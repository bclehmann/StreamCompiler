
use core::panic;

use inkwell::{builder::Builder, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::{Linkage, Module}, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, values::{AnyValue, BasicValue, FloatValue, FunctionValue, IntValue}, AddressSpace, OptimizationLevel};

use crate::parser::{BinaryOperator, Clause, ClauseType, Expr};

#[repr(C)]
#[derive(Debug)]
struct ProgramResult {
    pub result: f64,
    pub filtered_out: bool,
}

type ClauseFunction = unsafe extern "C" fn(f64) -> f64;
pub type ProgramFunction = unsafe extern "C" fn(*const f64, i32) -> ();

static mut ID: u64 = 0;

fn get_id() -> u64 {
    unsafe {
        ID += 1;
        ID
    }
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, olevel: OptimizationLevel) -> Self {
        let module = context.create_module("stream_compiler");
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(olevel).expect("Failed to create execution engine");
        
        let printf_type = context
            .i32_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], true);
        module.add_function("printf", printf_type, Some(Linkage::External));

        CodeGen {
            context,
            module,
            builder,
            execution_engine,
        }
    }

    fn dump_module(&self) {
        self.module.print_to_file("out.ll").expect("Failed to write module to file");
        self.module.write_bitcode_to_path("out.bc");

        Target::initialize_native(&InitializationConfig::default()).expect("Failed to initialize native target");
        let triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&triple).unwrap();
        let machine = target
            .create_target_machine(
                &triple,
                &cpu,
                &features,
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        
            // create a module and do JIT stuff

        machine.write_to_file(&self.module, FileType::Assembly, "out.S".as_ref()).unwrap();
    }

    pub fn compile_program(&'ctx self, program: &'ctx[Clause]) -> JitFunction<'ctx, ProgramFunction>
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
        
        let entry = self.context.append_basic_block(function, "entry");
        let program_exit_bb = self.context.append_basic_block(function, "program_exit");
        self.builder.position_at_end(entry);

        let format_specifier_global = self.module.add_global(self.context.ptr_type(AddressSpace::default()), Some(AddressSpace::default()), "format_specifier");
        format_specifier_global.set_initializer(&self.builder.build_global_string_ptr("%f\n", "format_specifier").expect("Could not build format specifier"));
        let format_specifier = self.builder.build_load(
            self.context.ptr_type(AddressSpace::default()),
            format_specifier_global.as_pointer_value(),
            "format_specifier_load"
        ).expect("Could not load format specifier");

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
            self.module.get_function("printf").expect("Could not get printf"),
            &[
                format_specifier.into(),
                next_input.as_basic_value_enum().into()
            ],
            "printf_call"
        );

        self.builder.build_unconditional_branch(loop_end_bb);
        self.builder.position_at_end(loop_end_bb);
        self.builder.build_unconditional_branch(loop_bb);

        self.builder.position_at_end(program_exit_bb);
        self.builder.build_return(None);

        // self.dump_module();
        unsafe { self.execution_engine.get_function(program_fn_name).ok().unwrap() }
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

    fn compile_expression(&self, expr: &Expr, input: FloatValue<'ctx>) -> Option<FloatValue<'ctx>> {
        match expr {
            Expr::Value(v) => Some(self.context.f64_type().const_float(*v)),
            Expr::Identifier("x") => Some(input), // Input value is passed as parameter
            Expr::Identifier(i) => panic!("Unknown identifier encountered: {}", i),
            Expr::BinaryOperation(op) => {
                let left = self.compile_expression(&op.left, input).expect("Failed to compile left expression");
                let right = self.compile_expression(&op.right, input).expect("Failed to compile right expression");
                
                let result = match op.operator {
                    BinaryOperator::Add => self.builder.build_float_add(left, right, "add"),
                    BinaryOperator::Subtract => self.builder.build_float_sub(left, right, "sub"),
                    BinaryOperator::Multiply => self.builder.build_float_mul(left, right, "mul"),
                    BinaryOperator::Divide => self.builder.build_float_div(left, right, "div"),
                    BinaryOperator::Modulo => self.builder.build_float_rem(left, right, "mod"),
                    BinaryOperator::LogicalAnd => {
                        let left_bool = self.float_as_bool(left);
                        let right_bool = self.float_as_bool(right);

                        let result_u1 = self.builder.build_and(left_bool, right_bool, "and").expect("Could not AND"); // Convert to int for logical operations
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "and_float")
                    },
                    BinaryOperator::LogicalOr => {
                        let left_bool = self.float_as_bool(left);
                        let right_bool = self.float_as_bool(right);

                        let result_u1 = self.builder.build_or(left_bool, right_bool, "or").expect("Could not OR"); // Convert to int for logical operations
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "or_float")
                    },
                    BinaryOperator::Equals => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::OEQ, left, right, "eq").expect("Could not compare for equality");
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "eq_float")
                    }
                    BinaryOperator::NotEqual => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::UNE, left, right, "ne").expect("Could not compare for inequeality"); // Unequal or both NaN
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "eq_float")
                    }
                    BinaryOperator::LessThan => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::OLT, left, right, "lt").expect("Could not compare for less than");
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "lt_float")
                    }
                    BinaryOperator::LessEqual => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::OLE, left, right, "le").expect("Could not compare for less than or equal to");
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "le_float")
                    }
                    BinaryOperator::GreaterThan => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::OGT, left, right, "gt").expect("Could not compare for greater than");
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "gt_float")
                    }
                    BinaryOperator::GreaterEqual => {
                        let result_u1 = self.builder.build_float_compare(inkwell::FloatPredicate::OGE, left, right, "ge").expect("Could not compare for greater than or equal to");
                        self.builder.build_unsigned_int_to_float(result_u1, self.context.f64_type(), "ge_float")
                    }
                };
                
                Some(result.expect("Could not build expression"))
            }
        }
    }

    fn float_as_bool(&self, value: FloatValue<'ctx>) -> IntValue<'ctx> {
        let zero = self.context.f64_type().const_float(0.0);
        self.builder.build_float_compare(inkwell::FloatPredicate::ONE, value, zero, "is_non_zero").expect("Could not convert to bool") // Returns false if either operand is NaN
    }
}
