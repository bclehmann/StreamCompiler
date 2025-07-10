use core::panic;

use inkwell::{builder::Builder, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::{Linkage, Module}, passes::{PassBuilderOptions, PassManager, PassManagerSubType}, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, types::VectorType, values::{AnyValue, BasicValue, FloatValue, FunctionValue, IntValue, VectorValue}, AddressSpace, OptimizationLevel};

use crate::{compiler::expression::{ExprCompiler, ScalarExprCompiler, VectorExprCompiler}, parser::{BinaryOperator, Clause, ClauseType, Expr}};

pub type ProgramFunction = unsafe extern "C" fn(*const f64, i32) -> ();

static mut ID: u64 = 0;

fn get_id() -> u64 {
    unsafe {
        ID += 1;
        ID
    }
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub olevel: OptimizationLevel,
    pub float_precision: u32,
    expr_compiler: ScalarExprCompiler,
    vector_expr_compiler: VectorExprCompiler,
}

#[derive(Debug)]
pub struct JittedProgram<'jit> {
    pub is_vectorized: bool,
    function: JitFunction<'jit, ProgramFunction>,
}

impl JittedProgram<'_> {
    pub fn new<'jit>(is_vectorized: bool, function: JitFunction<'jit, ProgramFunction>) -> JittedProgram<'jit> {
        JittedProgram {
            is_vectorized,
            function,
        }
    }

    pub unsafe fn call(&self, input: *const f64, len: i32) {
        self.function.call(input, len);
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, olevel: OptimizationLevel, float_precision: u32) -> Self {
        let module = context.create_module("stream_compiler");
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(olevel).expect("Failed to create execution engine");
        
        let runtime_module = Module::parse_bitcode_from_path("src/runtime/out/io.bc", context)
            .expect("Failed to parse runtime module");

        module.link_in_module(runtime_module).expect("Failed to link runtime module");

        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            olevel,
            float_precision,
            expr_compiler: ScalarExprCompiler,
            vector_expr_compiler: VectorExprCompiler,
        }
    }

    fn get_machine(&self) -> TargetMachine {
        Target::initialize_native(&InitializationConfig::default()).expect("Failed to initialize native target");
        let triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&triple).unwrap();
        target
            .create_target_machine(
                &triple,
                &cpu,
                &features,
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap()
    }

    fn dump_module(&self) {
        self.module.print_to_file("out.ll").expect("Failed to write module to file");
        self.module.write_bitcode_to_path("out.bc");

        let machine = self.get_machine();
        machine.write_to_file(&self.module, FileType::Assembly, "out.S".as_ref()).unwrap();
    }

    pub fn compile_program(&'ctx self, program: &'ctx[Clause]) -> JittedProgram<'ctx>
    {
        if program.is_empty() || program.iter().any(|clause| clause.clause_type == ClauseType::Filter) {
            let function = self.compile_program_scalar(program); // does not yet support vectorization on filter clauses
            JittedProgram::new(false, function)
        }
        else {
            let function = self.compile_program_vec4(program);
            JittedProgram::new(true, function)
        }
    }

    pub fn compile_program_scalar(&'ctx self, program: &'ctx[Clause]) -> JitFunction<'ctx, ProgramFunction>
    {
        struct CompiledClause<'a> {
            clause: &'a Clause<'a>,
            function: FunctionValue<'a>,
        }

        let mut compiled_clauses = Vec::new();
        for clause in program {
            if let Some(function) = self.compile_clause(clause) {
                compiled_clauses.push(CompiledClause {
                    function,
                    clause
                });
            } else {
                panic!("Failed to compile clause: {:?}", clause);
            }
        }

        let program_fn_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i32_type().into(),
            ],
            false
        );
        let program_fn_name_string = format!("program_{}", get_id());
        let program_fn_name = program_fn_name_string.as_str();
        let function = self.module.add_function(program_fn_name, program_fn_type, Some(Linkage::External));

        let println_double_var_precision = self.module.get_function("print_double_newline_variable_precision")
            .expect("Could not get print_double_newline_variable_precision function from runtime");
        
        let entry = self.context.append_basic_block(function, "entry");
        let program_exit_bb = self.context.append_basic_block(function, "program_exit");
        self.builder.position_at_end(entry);

        let input_ptr = function.get_first_param().expect("Function should have one parameter").into_pointer_value();
        let input_len = function.get_nth_param(1).expect("Function should have a second parameter").into_int_value();

        let loop_bb = self.context.append_basic_block(function, "loop");
        let loop_end_bb = self.context.append_basic_block(function, "loop_end");
        self.builder.build_unconditional_branch(loop_bb);
        self.builder.position_at_end(loop_bb);
        let loop_index = self.builder.build_phi(self.context.i32_type(), "loop_index").expect("Failed to create loop index");
        loop_index.add_incoming(&[
            (&self.context.i32_type().const_zero(), entry),
            (&self.builder.build_int_add(
                loop_index.as_basic_value().into_int_value(),
                self.context.i32_type().const_int(1, false),
                "next_index"
            ).expect("Could not build increment"), loop_end_bb),
        ]);

        let loop_body_bb = self.context.append_basic_block(function, "loop_body");
        self.builder.build_conditional_branch(
            self.builder.build_int_compare(
                inkwell::IntPredicate::ULT,
                loop_index.as_basic_value().into_int_value(),
                input_len,
                "loop_condition"
            ).expect("Could not build loop condition"),
            loop_body_bb,
           program_exit_bb 
        );
        self.builder.position_at_end(loop_body_bb);

        let mut next_input = self.builder.build_load(
            self.context.f64_type(),
            unsafe {
                self.builder.build_gep(
                    self.context.f64_type(),
                    input_ptr,
                    &[loop_index.as_basic_value().into_int_value()],
                    "input_ptr"
                ).expect("Could not build GEP for input")
            },
            "next_input"
        ).expect("Failed to load next input").into_float_value();

        for clause in compiled_clauses {
            let clause_entry_bb = self.context.append_basic_block(function, "clause_entry");
            self.builder.build_unconditional_branch(clause_entry_bb);
            self.builder.position_at_end(clause_entry_bb);

            let result_f64 = self.builder.build_call(
                clause.function,
                &[next_input.into()],
                "call_clause"
            ).unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into_float_value();

            match clause.clause.clause_type {
                ClauseType::Filter => {
                    let early_exit_bb = self.context.append_basic_block(function, "early_exit");
                    let continue_bb = self.context.append_basic_block(function, "continue");

                    let result_u1 = self.float_as_bool(result_f64);

                    self.builder.build_conditional_branch(result_u1, continue_bb, early_exit_bb);

                    self.builder.position_at_end(early_exit_bb);
                    self.builder.build_unconditional_branch(loop_end_bb);

                    self.builder.position_at_end(continue_bb); // Put it in place for the next clause
                },
                ClauseType::Map => {
                    next_input = result_f64; // Update next input for the next clause
                }
            }
        };

        self.builder.build_call(
            println_double_var_precision,
            &[
                next_input.into(),
                self.context.i32_type().const_int(self.float_precision as u64, false).into()
            ],
            "println_double_call"
        );

        self.builder.build_unconditional_branch(loop_end_bb);
        self.builder.position_at_end(loop_end_bb);
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(program_exit_bb);
        self.builder.build_return(None);

        let olevel_str = match self.olevel {
            OptimizationLevel::None => "O0",
            OptimizationLevel::Less => "O1",
            OptimizationLevel::Default => "O2",
            OptimizationLevel::Aggressive => "O3",
        };

        let passes = &[
            format!("default<{}>", olevel_str),
        ];

        self.module.run_passes(passes.join(",").as_str(), &self.get_machine(), PassBuilderOptions::create()).expect("Failed to run passes on module");
        // self.dump_module();
        unsafe { self.execution_engine.get_function(program_fn_name).ok().unwrap() }
    }

    pub fn compile_program_vec4(&'ctx self, program: &'ctx[Clause]) -> JitFunction<'ctx, ProgramFunction>
    {
        struct CompiledClause<'a> {
            clause: &'a Clause<'a>,
            function: FunctionValue<'a>,
        }

        let mut compiled_clauses = Vec::new();
        for clause in program {
            if let Some(function) = self.compile_clause_vec4(clause) {
                compiled_clauses.push(CompiledClause {
                    function,
                    clause
                });
            } else {
                panic!("Failed to compile clause: {:?}", clause);
            }
        }

        let program_fn_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i32_type().into(),
            ],
            false
        );
        let program_fn_name_string = format!("program_{}", get_id());
        let program_fn_name = program_fn_name_string.as_str();
        let function = self.module.add_function(program_fn_name, program_fn_type, Some(Linkage::External));

        let println_double_var_precision = self.module.get_function("print_double_newline_variable_precision")
            .expect("Could not get print_double_newline_variable_precision function from runtime");

        let println_double4_var_precision = self.module.get_function("print_4doubles_newline_variable_precision")
            .expect("Could not get print_4doubles_newline_variable_precision function from runtime");
        
        let entry = self.context.append_basic_block(function, "entry");
        let program_exit_bb = self.context.append_basic_block(function, "program_exit");
        self.builder.position_at_end(entry);

        let input_ptr = function.get_first_param().expect("Function should have one parameter").into_pointer_value();
        let input_len = function.get_nth_param(1).expect("Function should have a second parameter").into_int_value();

        let loop_bb = self.context.append_basic_block(function, "loop");
        let loop_end_bb = self.context.append_basic_block(function, "loop_end");
        self.builder.build_unconditional_branch(loop_bb);
        self.builder.position_at_end(loop_bb);
        let loop_index = self.builder.build_phi(self.context.i32_type(), "loop_index").expect("Failed to create loop index");
        loop_index.add_incoming(&[
            (&self.context.i32_type().const_zero(), entry),
            (&self.builder.build_int_add(
                loop_index.as_basic_value().into_int_value(),
                self.context.i32_type().const_int(1, false),
                "next_index"
            ).expect("Could not build increment"), loop_end_bb),
        ]);

        let loop_body_bb = self.context.append_basic_block(function, "loop_body");
        self.builder.build_conditional_branch(
            self.builder.build_int_compare(
                inkwell::IntPredicate::ULT,
                loop_index.as_basic_value().into_int_value(),
                self.builder.build_int_signed_div(
                    input_len,
                    self.context.i32_type().const_int(4, false),
                    "loop_condition_div"
                ).expect("Could not build loop condition division"),
                "loop_condition"
            ).expect("Could not build loop condition"),
            loop_body_bb,
           program_exit_bb 
        );
        self.builder.position_at_end(loop_body_bb);

        let mut next_input = self.builder.build_load(
            self.context.f64_type().vec_type(4),
            unsafe {
                self.builder.build_gep(
                    self.context.f64_type().vec_type(4),
                    input_ptr,
                    &[loop_index.as_basic_value().into_int_value()],
                    "input_ptr"
                ).expect("Could not build GEP for input")
            },
            "next_input"
        ).expect("Failed to load next input").into_vector_value();

        for clause in compiled_clauses {
            let clause_entry_bb = self.context.append_basic_block(function, "clause_entry");
            self.builder.build_unconditional_branch(clause_entry_bb);
            self.builder.position_at_end(clause_entry_bb);

            let result_f64v4 = self.builder.build_call(
                clause.function,
                &[next_input.into()],
                "call_clause"
            ).unwrap()
            .try_as_basic_value()
            .unwrap_left()
            .into_vector_value();

            match clause.clause.clause_type {
                ClauseType::Filter => {
                    panic!("Vec4 filtering is not supported yet, please use scalar filtering instead");
                    /*
                    let early_exit_bb = self.context.append_basic_block(function, "early_exit");
                    let continue_bb = self.context.append_basic_block(function, "continue");

                    let result_u1 = self.float_as_bool(result_f64);

                    self.builder.build_conditional_branch(result_u1, continue_bb, early_exit_bb);

                    self.builder.position_at_end(early_exit_bb);
                    self.builder.build_unconditional_branch(loop_end_bb);

                    self.builder.position_at_end(continue_bb); // Put it in place for the next clause
                    */
                },
                ClauseType::Map => {
                    next_input = result_f64v4; // Update next input for the next clause
                }
            }
        };

        let f64_0 = self.builder.build_extract_element(next_input, self.context.i32_type().const_int(0, false), "result0").unwrap();
        let f64_1 = self.builder.build_extract_element(next_input, self.context.i32_type().const_int(1, false), "result1").unwrap();
        let f64_2 = self.builder.build_extract_element(next_input, self.context.i32_type().const_int(2, false), "result2").unwrap();
        let f64_3 = self.builder.build_extract_element(next_input, self.context.i32_type().const_int(3, false), "result3").unwrap();

        self.builder.build_call(
            println_double4_var_precision,
            &[
                f64_0.into(),
                f64_1.into(),
                f64_2.into(),
                f64_3.into(),
                self.context.i32_type().const_int(self.float_precision as u64, false).into()
            ],
            "println_double4_call"
        );

        self.builder.build_unconditional_branch(loop_end_bb);
        self.builder.position_at_end(loop_end_bb);
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(program_exit_bb);
        self.builder.build_return(None);

        let olevel_str = match self.olevel {
            OptimizationLevel::None => "O0",
            OptimizationLevel::Less => "O1",
            OptimizationLevel::Default => "O2",
            OptimizationLevel::Aggressive => "O3",
        };

        let passes = &[
            format!("default<{}>", olevel_str),
        ];

        self.module.run_passes(passes.join(",").as_str(), &self.get_machine(), PassBuilderOptions::create()).expect("Failed to run passes on module");
        // self.dump_module();
        unsafe { self.execution_engine.get_function(program_fn_name).ok().unwrap() }
    }

    fn compile_clause_vec4(&self, clause: &Clause) -> Option<FunctionValue<'_>> {
        let vec4_type = self.context.f64_type().vec_type(4);
        let fn_type = vec4_type.fn_type(&[vec4_type.into()], false);
        let fn_name = format!("#clause_vec4_{}", get_id());
        let function = self.module.add_function(&fn_name, fn_type, Some(Linkage::Private));

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        
        let x = function.get_first_param().expect("Function should have one parameter").into_vector_value();
        match clause.clause_type {
            ClauseType::Filter => {
                panic!("Vec4 filtering is not supported yet, please use scalar filtering instead");
                /*
                let condition = self.compile_expression(&clause.expression, x);
                if condition.is_none() {
                    panic!("Failed to compile filter expression");
                }
                let condition_value = condition.unwrap();
                
                // If the condition is false, return 0.0 (filter out)
                let zero = f64_type.const_float(0.0);
                let one = f64_type.const_float(1.0);
                let result = self.builder.build_select(self.float_as_bool(condition_value), one, zero, "filter_result");
                
                self.builder.build_return(Some(&result.unwrap())).expect("Failed to build return for filter clause");
                */
            },
            ClauseType::Map => {
                let mapped_value = self.compile_expression_vec4(&clause.expression, x);
                if mapped_value.is_none() {
                    panic!("Failed to compile map expression");
                }
                
                self.builder.build_return(Some(&mapped_value.unwrap())).expect("Failed to build return for map clause");
            }
        };

        return Some(function);
    }

    fn compile_clause(&self, clause: &Clause) -> Option<FunctionValue<'_>> {
        let f64_type = self.context.f64_type();
        let fn_type = f64_type.fn_type(&[f64_type.into()], false);
        let fn_name = format!("#clause_{}", get_id());
        let function = self.module.add_function(&fn_name, fn_type, Some(Linkage::Private));

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        
        let x = function.get_first_param().expect("Function should have one parameter").into_float_value();
        match clause.clause_type {
            ClauseType::Filter => {
                let condition = self.compile_expression(&clause.expression, x);
                if condition.is_none() {
                    panic!("Failed to compile filter expression");
                }
                let condition_value = condition.unwrap();
                
                // If the condition is false, return 0.0 (filter out)
                let zero = f64_type.const_float(0.0);
                let one = f64_type.const_float(1.0);
                let result = self.builder.build_select(self.float_as_bool(condition_value), one, zero, "filter_result");
                
                self.builder.build_return(Some(&result.unwrap())).expect("Failed to build return for filter clause");
            },
            ClauseType::Map => {
                let mapped_value = self.compile_expression(&clause.expression, x);
                if mapped_value.is_none() {
                    panic!("Failed to compile map expression");
                }
                
                self.builder.build_return(Some(&mapped_value.unwrap())).expect("Failed to build return for map clause");
            }
        };

        return Some(function);
    }

    fn compile_expression_vec4<'cg>(&'cg self, expr: &Expr, input: VectorValue<'cg>) -> Option<VectorValue<'cg>> {
        self.vector_expr_compiler.compile_expression(self, expr, input.into()).map(|v| v.into())
    }

    fn compile_expression<'cg>(&'cg self, expr: &Expr, input: FloatValue<'cg>) -> Option<FloatValue<'cg>> {
        self.expr_compiler.compile_expression(self, expr, input.into()).map(|v| v.into())
    }

    fn float_as_bool<'cg>(&'cg self, value: FloatValue<'cg>) -> IntValue<'cg> {
        self.expr_compiler.float_as_bool(self, value.into()).into()
    }

    fn float_as_bool_vec4<'cg>(&'cg self, value: VectorValue<'cg>) -> IntValue<'cg> {
        self.vector_expr_compiler.float_as_bool(self, value.into()).into()
    }
}
