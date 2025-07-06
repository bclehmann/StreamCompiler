
use core::panic;

use inkwell::{builder::Builder, context::Context, execution_engine::{ExecutionEngine, JitFunction}, module::{Linkage, Module}, values::{FloatValue, FunctionValue, IntValue}, AddressSpace, OptimizationLevel};

use crate::parser::{BinaryOperator, Clause, ClauseType, Expr};

#[repr(C)]
#[derive(Debug)]
struct ProgramResult {
    pub result: f64,
    pub filtered_out: bool,
}

type ClauseFunction = unsafe extern "C" fn(f64) -> f64;
type ProgramFunction = unsafe extern "C" fn(f64) -> *const ProgramResult;

static mut ID: u64 = 0;

fn get_id() -> u64 {
    unsafe {
        ID += 1;
        ID
    }
}

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("stream_compiler");
        let builder = context.create_builder();
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None).expect("Failed to create execution engine");
        
        CodeGen {
            context,
            module,
            builder,
            execution_engine,
        }
    }

    fn compile_program(&'ctx self, program: &'ctx[Clause]) -> JitFunction<'ctx, ProgramFunction>
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

        let program_result_type = self.context.struct_type(&[self.context.f64_type().into(), self.context.bool_type().into()], false);
        let return_type = self.context.ptr_type(AddressSpace::default());
        let program_fn_type = return_type.fn_type(&[self.context.f64_type().into()], false);
        let program_fn_name = "program";
        let function = self.module.add_function(program_fn_name, program_fn_type, Some(Linkage::External));
        
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let input = function.get_first_param().expect("Function should have one parameter").into_float_value();
        let result_value = self.builder.build_alloca(self.context.f64_type(), "result_value").expect("Could not allocate result value");
        let filtered_out = self.builder.build_alloca(self.context.bool_type(), "filtered_out").expect("Could not alloca filtered_out");
        let result_struct = self.builder.build_alloca(program_result_type, "result_struct").expect("Could not alloca result struct"); // This is only written before return so I have to write less GEPs

        self.builder.build_store(result_value, input);
        self.builder.build_store(filtered_out, self.context.bool_type().const_zero());

        self.builder.build_store(
            self.builder.build_struct_gep(
                program_result_type,
                result_struct,
                0,
                "result_struct.result"
            ).expect("Could not build result_struct.result GEP"),
            self.context.f64_type().const_float(0.0)
        );
        self.builder.build_store(
            self.builder.build_struct_gep(
                program_result_type,
                result_struct,
                1,
                "result_struct.filtered_out"
            ).expect("Could not build result_struct.filtered_out GEP"),
            self.context.i8_type().const_int(0, false)
        );

        for clause in compiled_clauses {
            let result_f64 = self.builder.build_call(
                clause.function,
                &[
                    self.builder.build_load(
                        self.context.f64_type(),
                        result_value,
                        "result_value"
                    ).expect("Could not build load")
                    .into()
                ],
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
                    self.builder.build_store(
                        self.builder.build_struct_gep(
                            program_result_type,
                            result_struct,
                            1,
                            "result_struct.filtered_out"
                        ).expect("Could not build result_struct.filtered_out GEP"),
                        self.context.i8_type().const_int(1, false)
                    );

                    self.builder.build_return(Some(&result_struct));

                    self.builder.position_at_end(continue_bb); // Put it in place for the next clause
                },
                ClauseType::Map => {
                    self.builder.build_store(
                        result_value,
                        result_f64
                    );
                }
            }
        };

        self.builder.build_store(
            self.builder.build_struct_gep(
                program_result_type,
                result_struct,
                0,
                "result_struct.result"
            ).expect("Could not build result_struct.result GEP"),
            self.builder.build_load(
                self.context.f64_type(),
                result_value,
                "final_load"
            ).expect("Could not build result_value load")
        );

        self.builder.build_return(Some(&result_struct));

        self.module.print_to_stderr();

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

pub fn compile_and_run_program<T: Iterator<Item=f64>>(program: &[Clause], input: T) {
    let context = Context::create();
    let codegen = Box::leak(Box::new(CodeGen::new(&context))); // Rust thinks `program` may outlive this scope, so we just leak the memory

    let program = codegen.compile_program(program);
    for x in input {
        unsafe {
            let result = program.call(x);
            if !(*result).filtered_out {
                println!("{}", (*result).result);
            }
            println!("{:?}", *result);
        }
    }

    return;
}