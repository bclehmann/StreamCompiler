use inkwell::{context::Context, types::VectorType, values::{FloatValue, VectorValue}};

use crate::{compiler::{expression::{expr::ExprResult, ExprCompiler}, CodeGen}, parser::{BinaryOperator, Expr}};

pub struct VectorExprCompiler<const VEC_WIDTH: u32>;

impl<const VEC_WIDTH: u32> VectorExprCompiler<VEC_WIDTH> {
    fn fill_vec<'a>(self: &'a Self, value: FloatValue<'a>) -> VectorValue<'a> {
        let values = vec!(value; VEC_WIDTH as usize);
        VectorType::const_vector(&values)
    }

    fn const_vec<'a>(self: &'a Self, context: &'a Context, value: f64) -> VectorValue<'a> {
        let as_f64 = context.f64_type().const_float(value);
        self.fill_vec(as_f64)
    }
}

impl<const VEC_WIDTH: u32> ExprCompiler for VectorExprCompiler<VEC_WIDTH> {
    fn float_as_bool<'cg>(self: &'cg Self, codegen: &'cg CodeGen, value: ExprResult<'cg>) -> ExprResult<'cg> {
        let zero = self.const_vec(codegen.context, 0.0);
        codegen.builder.build_float_compare(inkwell::FloatPredicate::ONE, value.into(), zero, "is_non_zero").expect("Could not convert to bool").into() // Returns false if either operand is NaN
    }

    fn compile_expression<'cg>(self: &'cg Self, codegen: &'cg CodeGen, expr: &Expr, input: ExprResult<'cg>) -> Option<ExprResult<'cg>> {
        let input = match input {
            ExprResult::DoubleVec(v) => v,
            _ => panic!("Expected a VectorValue, got {:?}", input),
        };


        match expr {
            Expr::Value(v) => {
                Some(self.const_vec(codegen.context, *v).into())
            },
            Expr::Identifier("x") => Some(input.into()), // Input value is passed as parameter
            Expr::Identifier(i) => panic!("Unknown identifier encountered: {}", i),
            Expr::BinaryOperation(op) => {
                let left = match self.compile_expression(codegen, &op.left, input.into()).expect("Failed to compile left expression") {
                    ExprResult::DoubleVec(v) => v,
                    _ => panic!("Expected a FloatValue, got {:?}", op.left),
                };
                let right = match self.compile_expression(codegen, &op.right, input.into()).expect("Failed to compile right expression") {
                    ExprResult::DoubleVec(v) => v,
                    _ => panic!("Expected a FloatValue, got {:?}", op.right),
                };
                
                let result = match op.operator {
                    BinaryOperator::Add => codegen.builder.build_float_add(left, right, "add"),
                    BinaryOperator::Subtract => codegen.builder.build_float_sub(left, right, "sub"),
                    BinaryOperator::Multiply => codegen.builder.build_float_mul(left, right, "mul"),
                    BinaryOperator::Divide => codegen.builder.build_float_div(left, right, "div"),
                    BinaryOperator::Modulo => codegen.builder.build_float_rem(left, right, "mod"),
                    BinaryOperator::LogicalAnd => {
                        let left_bool = match self.float_as_bool(codegen, left.into()) {
                            ExprResult::BooleanVec(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };
                        let right_bool = match self.float_as_bool(codegen, right.into()) {
                            ExprResult::BooleanVec(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };

                        let result_u1v = codegen.builder.build_and(left_bool, right_bool, "and").expect("Could not AND"); // Convert to int for logical operations
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "and_float")
                    },
                    BinaryOperator::LogicalOr => {
                        let left_bool = match self.float_as_bool(codegen, left.into()) {
                            ExprResult::BooleanVec(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };
                        let right_bool = match self.float_as_bool(codegen, right.into()) {
                            ExprResult::BooleanVec(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };

                        let result_u1v = codegen.builder.build_or(left_bool, right_bool, "or").expect("Could not OR");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "or_float")
                    },
                    BinaryOperator::Equals => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::OEQ, left, right, "eq").expect("Could not compare for equality");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "eq_float")
                    },
                    BinaryOperator::NotEqual => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::UNE, left, right, "ne").expect("Could not compare for inequeality"); // Unequal or both NaN
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "ne_float")
                    },
                    BinaryOperator::LessThan => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::OLT, left, right, "lt").expect("Could not compare for less than");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "lt_float")
                    },
                    BinaryOperator::LessEqual => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::OLE, left, right, "le").expect("Could not compare for less than or equal to");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "le_float")
                    },
                    BinaryOperator::GreaterThan => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::OGT, left, right, "gt").expect("Could not compare for greater than");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "gt_float")
                    },
                    BinaryOperator::GreaterEqual => {
                        let result_u1v = codegen.builder.build_float_compare(inkwell::FloatPredicate::OGE, left, right, "ge").expect("Could not compare for greater than or equal to");
                        codegen.builder.build_unsigned_int_to_float(result_u1v, codegen.context.f64_type().vec_type(VEC_WIDTH), "ge_float")
                    },
                };
                
                Some(result.expect("Could not build expression").into())
            }
        }
    }
}
