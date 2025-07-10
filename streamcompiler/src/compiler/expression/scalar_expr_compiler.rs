use crate::{compiler::{expression::{expr::ExprResult, ExprCompiler}, CodeGen}, parser::{BinaryOperator, Expr}};


pub struct ScalarExprCompiler;

impl ExprCompiler for ScalarExprCompiler {
    fn float_as_bool<'cg>(self: &'cg Self, codegen: &'cg CodeGen, value: ExprResult<'cg>) -> ExprResult<'cg> {
        let value = match value {
            ExprResult::Double(v) => v,
            _ => panic!("Expected a FloatValue, got {:?}", value),
        };

        let zero = codegen.context.f64_type().const_float(0.0);
        codegen.builder.build_float_compare(inkwell::FloatPredicate::ONE, value, zero, "is_non_zero").expect("Could not convert to bool").into() // Returns false if either operand is NaN
    }

    fn compile_expression<'cg>(self: &'cg Self, codegen: &'cg CodeGen, expr: &Expr, input: ExprResult<'cg>) -> Option<ExprResult<'cg>> {
        let input = match input {
            ExprResult::Double(v) => v,
            _ => panic!("Expected a FloatValue, got {:?}", input),
        };

        match expr {
            Expr::Value(v) => Some(codegen.context.f64_type().const_float(*v).into()),
            Expr::Identifier("x") => Some(input.into()), // Input value is passed as parameter
            Expr::Identifier(i) => panic!("Unknown identifier encountered: {}", i),
            Expr::BinaryOperation(op) => {
                let left = match self.compile_expression(codegen, &op.left, input.into()).expect("Failed to compile left expression") {
                    ExprResult::Double(v) => v,
                    _ => panic!("Expected a FloatValue, got {:?}", op.left),
                };
                let right = match self.compile_expression(codegen, &op.right, input.into()).expect("Failed to compile right expression") {
                    ExprResult::Double(v) => v,
                    _ => panic!("Expected a FloatValue, got {:?}", op.right),
                };

                let result = match op.operator {
                    BinaryOperator::Add => codegen.builder.build_float_add(left, right, "add").into(),
                    BinaryOperator::Subtract => codegen.builder.build_float_sub(left, right, "sub").into(),
                    BinaryOperator::Multiply => codegen.builder.build_float_mul(left, right, "mul").into(),
                    BinaryOperator::Divide => codegen.builder.build_float_div(left, right, "div").into(),
                    BinaryOperator::Modulo => codegen.builder.build_float_rem(left, right, "mod").into(),
                    BinaryOperator::LogicalAnd => {
                        let left_bool = match self.float_as_bool(codegen, left.into()) {
                            ExprResult::Boolean(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };
                        let right_bool = match self.float_as_bool(codegen, right.into()) {
                            ExprResult::Boolean(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };

                        let result_u1 = codegen.builder.build_and(left_bool, right_bool, "and").expect("Could not AND"); // Convert to int for logical operations
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "and_float")
                    },
                    BinaryOperator::LogicalOr => {
                        let left_bool = match self.float_as_bool(codegen, left.into()) {
                            ExprResult::Boolean(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };
                        let right_bool = match self.float_as_bool(codegen, right.into()) {
                            ExprResult::Boolean(v) => v,
                            _ => panic!("Expected a Boolean IntValue, got {:?}", left),
                        };

                        let result_u1 = codegen.builder.build_or(left_bool, right_bool, "or").expect("Could not OR"); // Convert to int for logical operations
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "or_float")
                    },
                    BinaryOperator::Equals => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::OEQ, left, right, "eq").expect("Could not compare for equality");
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "eq_float")
                    }
                    BinaryOperator::NotEqual => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::UNE, left, right, "ne").expect("Could not compare for inequeality"); // Unequal or both NaN
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "eq_float")
                    }
                    BinaryOperator::LessThan => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::OLT, left, right, "lt").expect("Could not compare for less than");
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "lt_float")
                    }
                    BinaryOperator::LessEqual => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::OLE, left, right, "le").expect("Could not compare for less than or equal to");
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "le_float")
                    }
                    BinaryOperator::GreaterThan => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::OGT, left, right, "gt").expect("Could not compare for greater than");
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "gt_float")
                    }
                    BinaryOperator::GreaterEqual => {
                        let result_u1 = codegen.builder.build_float_compare(inkwell::FloatPredicate::OGE, left, right, "ge").expect("Could not compare for greater than or equal to");
                        codegen.builder.build_unsigned_int_to_float(result_u1, codegen.context.f64_type(), "ge_float")
                    }
                };
                
                Some(result.expect("Could not build expression").into())
            }
        }
    }
}
