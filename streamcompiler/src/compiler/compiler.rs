use core::panic;

use inkwell::{builder::Builder, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::{Linkage, Module}, passes::PassBuilderOptions, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, types::{BasicType, BasicTypeEnum}, values::{BasicMetadataValueEnum, FloatValue, FunctionValue, IntValue, VectorValue}, AddressSpace, OptimizationLevel};

use crate::{compiler::expression::{ExprCompiler, ScalarExprCompiler, VectorExprCompiler}, parser::{Clause, ClauseType, Expr}};

pub type ProgramFunction = unsafe extern "C" fn(*const f64, i32) -> ();

static mut ID: u64 = 0;

fn get_id() -> u64 {
    unsafe {
        ID += 1;
        ID
    }
}

const VEC_WIDTH: u32 = 8;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
    pub olevel: OptimizationLevel,
    pub float_precision: u32,
    expr_compiler: ScalarExprCompiler,
    vector_expr_compiler: VectorExprCompiler<VEC_WIDTH>,
}

#[derive(Debug)]
pub struct JittedProgram<'jit> {
    pub vector_width: Option<u32>,
    function: JitFunction<'jit, ProgramFunction>,
}

impl JittedProgram<'_> {
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

    pub fn compile(&'ctx self, program: &'ctx[Clause]) -> JittedProgram<'ctx>
    {
        let (vec_width, compiled_program) = if program.is_empty() || program.iter().any(|clause| clause.clause_type == ClauseType::Filter) {
            (None, self.compile_program::<1>(program))
        } else {
            (Some(VEC_WIDTH), self.compile_program::<VEC_WIDTH>(program))
        };

        JittedProgram { vector_width: vec_width, function: compiled_program }
    }

    fn compile_program<const VEC_WIDTH: u32>(&'ctx self, program: &'ctx[Clause]) -> JitFunction<'ctx, ProgramFunction>
    {
        if VEC_WIDTH != 1 && VEC_WIDTH != 4 && VEC_WIDTH != 8 {
            panic!("Unsupported vector width: {}. Only 1 (scalar), 4 and 8 are supported.", VEC_WIDTH);
        }

        struct CompiledClause<'a> {
            clause: &'a Clause<'a>,
            function: FunctionValue<'a>,
        }

        let fn_type = self.context.void_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i32_type().into(),
            ],
            false
        );

        let (input_pointee_type, function, fn_name) = if VEC_WIDTH == 1 {
            let fn_name = format!("program_scalar_{}", get_id());
            let function = self.module.add_function(&fn_name, fn_type, Some(Linkage::External));

            (self.context.f64_type().as_basic_type_enum(), function, fn_name)
        } else {
            let fn_name = format!("program_vec{}_{}", VEC_WIDTH, get_id());
            let function = self.module.add_function(&fn_name, fn_type, Some(Linkage::External));

            (self.context.f64_type().vec_type(VEC_WIDTH as u32).as_basic_type_enum(), function, fn_name)
        };

        let mut compiled_clauses = Vec::new();
        for clause in program {
            if let Some(function) = self.compile_clause::<VEC_WIDTH>(clause) {
                compiled_clauses.push(CompiledClause {
                    function,
                    clause
                });
            } else {
                panic!("Failed to compile clause: {:?}", clause);
            }
        }

        let println_double_n_var_precision = self.module.get_function(&format!("print_{}doubles_newline_variable_precision", VEC_WIDTH))
            .expect("Could not get print_Ndoubles_newline_variable_precision function from runtime");
        
        let entry = self.context.append_basic_block(function, "entry");
        let program_exit_bb = self.context.append_basic_block(function, "program_exit");
        self.builder.position_at_end(entry);

        let input_ptr = function.get_first_param().expect("Function should have one parameter").into_pointer_value();
        let input_len = function.get_nth_param(1).expect("Function should have a second parameter").into_int_value();

        let loop_bb = self.context.append_basic_block(function, "loop");
        let loop_end_bb = self.context.append_basic_block(function, "loop_end");
        self.builder.build_unconditional_branch(loop_bb).expect("Failed to build unconditional branch to loop");
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
                self.builder.build_int_unsigned_div(
                    input_len,
                    self.context.i32_type().const_int(VEC_WIDTH as u64, false),
                    "loop_condition_div"
                ).expect("Could not build loop condition division"),
                "loop_condition"
            ).expect("Could not build loop condition"),
            loop_body_bb,
           program_exit_bb 
        ).expect("Failed to build conditional branch for loop");
        self.builder.position_at_end(loop_body_bb);

        let mut next_input = self.builder.build_load(
            input_pointee_type,
            unsafe {
                self.builder.build_gep(
                    input_pointee_type,
                    input_ptr,
                    &[loop_index.as_basic_value().into_int_value()],
                    "input_ptr"
                ).expect("Could not build GEP for input")
            },
            "next_input"
        ).expect("Failed to load next input");

        for clause in compiled_clauses {
            let clause_entry_bb = self.context.append_basic_block(function, "clause_entry");
            self.builder.build_unconditional_branch(clause_entry_bb).expect("Failed to build unconditional branch to clause entry");
            self.builder.position_at_end(clause_entry_bb);

            let result = self.builder.build_call(
                clause.function,
                &[next_input.into()],
                "call_clause"
            ).unwrap()
            .try_as_basic_value()
            .unwrap_left();


            match clause.clause.clause_type {
                ClauseType::Filter => {
                    match input_pointee_type.as_basic_type_enum() {
                        BasicTypeEnum::VectorType(_) => {
                            panic!("Vec filtering is not supported yet, please use scalar filtering instead");
                        },
                        BasicTypeEnum::FloatType(t) => {
                            if t != self.context.f64_type() {
                                panic!("Expected f64 type for input parameter, got {:?}", t);
                            }

                            let early_exit_bb = self.context.append_basic_block(function, "early_exit");
                            let continue_bb = self.context.append_basic_block(function, "continue");

                            let result_u1 = self.float_as_bool(result.into_float_value());

                            self.builder.build_conditional_branch(result_u1, continue_bb, early_exit_bb).expect("Failed to build conditional branch for filter clause");

                            self.builder.position_at_end(early_exit_bb);
                            self.builder.build_unconditional_branch(loop_end_bb).expect("Failed to build unconditional branch to loop end");

                            self.builder.position_at_end(continue_bb); // Put it in place for the next clause
                        },
                        _ => panic!("Expected float or vector type for input parameter"),
                    }
                },
                ClauseType::Map => {
                    next_input = result; // Update next input for the next clause
                }
            }
        };

        let double_args = match input_pointee_type {
            BasicTypeEnum::VectorType(t) => {
                if t.get_element_type().into_float_type() != self.context.f64_type() {
                    panic!("Expected vector of f64 type for input parameter, got {:?}", t);
                }
                if t.get_size() != VEC_WIDTH as u32 {
                    panic!("Expected vector of f64 type with width {}, got {:?}", VEC_WIDTH, t);
                }

                (0..VEC_WIDTH).map(|i| {
                    From::from(self.builder.build_extract_element(
                        next_input.into_vector_value(),
                        self.context.i32_type().const_int(i as u64, false),
                        &format!("result{}", i)
                    ).unwrap())
                }).collect::<Vec<BasicMetadataValueEnum>>()
            },
            BasicTypeEnum::FloatType(_) => {
                vec![next_input.into()]
            },
            _ => panic!("Expected float or vector type for input parameter"),
        };

        let precision = self.context.i32_type().const_int(self.float_precision as u64, false).into();
        let precision_vec = vec!(precision);

        let args = [
            double_args,
            precision_vec
        ].concat();

        self.builder.build_call(
            println_double_n_var_precision,
            args.as_slice(),
            "println_doubleN_call"
        ).expect("Failed to build call to println_doubleN_var_precision");

        self.builder.build_unconditional_branch(loop_end_bb).expect("Failed to build unconditional branch to loop end");
        self.builder.position_at_end(loop_end_bb);
        self.builder.build_unconditional_branch(loop_bb).expect("Failed to build unconditional branch to loop");

        self.builder.position_at_end(program_exit_bb);
        self.builder.build_return(None).expect("Failed to build return at program exit");

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
        unsafe { self.execution_engine.get_function(&fn_name).ok().unwrap() }
    }

    fn compile_clause<const VEC_WIDTH: u32>(&self, clause: &Clause) -> Option<FunctionValue<'_>> {
        let function = if VEC_WIDTH > 1 {
            let t = self.context.f64_type().vec_type(VEC_WIDTH);
            let fn_type = t.fn_type(&[t.into()], false);
            let fn_name = format!("#clause_vec{VEC_WIDTH}_{}", get_id());
            self.module.add_function(&fn_name, fn_type, Some(Linkage::Private))
        } else {
            let t = self.context.f64_type();
            let fn_type = t.fn_type(&[t.into()], false);
            let fn_name = format!("#clause_scalar_{}", get_id());
            self.module.add_function(&fn_name, fn_type, Some(Linkage::Private))
        };
 
        let x = function.get_first_param().expect("Function should have one parameter");
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        
        match clause.clause_type {
            ClauseType::Filter => {
                match x.get_type() {
                    BasicTypeEnum::VectorType(_) => {
                        panic!("Vec filtering is not supported yet, please use scalar filtering instead");
                    },
                    BasicTypeEnum::FloatType(t) => {
                        if t != self.context.f64_type() {
                            panic!("Expected f64 type for input parameter, got {:?}", t);
                        }

                        let condition = self.compile_expression(&clause.expression, x.into_float_value());
                        if condition.is_none() {
                            panic!("Failed to compile filter expression");
                        }
                        let condition_value = condition.unwrap();
                        
                        // If the condition is false, return 0.0 (filter out)
                        let zero = t.const_float(0.0);
                        let one = t.const_float(1.0);
                        let result = self.builder.build_select(self.float_as_bool(condition_value), one, zero, "filter_result");
                        
                        self.builder.build_return(Some(&result.unwrap())).expect("Failed to build return for filter clause");
                    },
                    _ => panic!("Expected float or vector type for input parameter"),
                }
            },
            ClauseType::Map => {
                match x.get_type() {
                    BasicTypeEnum::VectorType(t) => {
                        if t.get_element_type().into_float_type() != self.context.f64_type() {
                            panic!("Expected vector of f64 type for input parameter, got {:?}", t);
                        }
                        if t.get_size() != VEC_WIDTH as u32 {
                            panic!("Expected vector of f64 type with width {}, got {:?}", VEC_WIDTH, t);
                        }

                        let mapped_value = self.compile_expression_vec(&clause.expression, x.into_vector_value());
                        if mapped_value.is_none() {
                            panic!("Failed to compile map expression");
                        }
                        
                        self.builder.build_return(Some(&mapped_value.unwrap())).expect("Failed to build return for map clause");
                    },
                    BasicTypeEnum::FloatType(t) => {
                        if t != self.context.f64_type() {
                            panic!("Expected f64 type for input parameter, got {:?}", t);
                        }

                        let mapped_value = self.compile_expression(&clause.expression, x.into_float_value());
                        if mapped_value.is_none() {
                            panic!("Failed to compile map expression");
                        }
                        
                        self.builder.build_return(Some(&mapped_value.unwrap())).expect("Failed to build return for map clause");
                    },
                    _ => panic!("Expected float or vector type for input parameter"),
                }
            }
        };

        return Some(function);
    }

    fn compile_expression_vec<'cg>(&'cg self, expr: &Expr, input: VectorValue<'cg>) -> Option<VectorValue<'cg>> {
        self.vector_expr_compiler.compile_expression(self, expr, input.into()).map(|v| v.into())
    }

    fn compile_expression<'cg>(&'cg self, expr: &Expr, input: FloatValue<'cg>) -> Option<FloatValue<'cg>> {
        self.expr_compiler.compile_expression(self, expr, input.into()).map(|v| v.into())
    }

    fn float_as_bool<'cg>(&'cg self, value: FloatValue<'cg>) -> IntValue<'cg> {
        self.expr_compiler.float_as_bool(self, value.into()).into()
    }

    fn float_as_bool_vec<'cg>(&'cg self, value: VectorValue<'cg>) -> IntValue<'cg> {
        self.vector_expr_compiler.float_as_bool(self, value.into()).into()
    }
}
