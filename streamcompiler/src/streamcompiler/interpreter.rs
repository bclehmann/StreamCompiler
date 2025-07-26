use crate::parser;
use parser::{Clause, ClauseType, Expr, BinaryOperator, Value};

fn interpret_expression(expr: &Expr, input: Value) -> Value {
    match expr {
        Expr::Value(v) => *v,
        Expr::Identifier("x") => input, // The only valid identifier, the input value
        Expr::Identifier(_) => panic!("Unknown identifier encountered"),
        Expr::BinaryOperation(op) => {
            let left = interpret_expression(&op.left, input);
            let right = interpret_expression(&op.right, input);
            match op.operator {
                BinaryOperator::Add => left + right,
                BinaryOperator::Subtract => left - right,
                BinaryOperator::Multiply => left * right,
                BinaryOperator::Divide => left / right,
                BinaryOperator::Modulo => left % right,
                BinaryOperator::LogicalAnd => if left != 0.0 && right != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::LogicalOr => if left != 0.0 || right != 0.0 { 1.0 } else { 0.0 },
                BinaryOperator::Equals => if left == right { 1.0 } else { 0.0 },
                BinaryOperator::NotEqual => if left != right { 1.0 } else { 0.0 },
                BinaryOperator::LessThan => if left < right { 1.0 } else { 0.0 },
                BinaryOperator::LessEqual => if left <= right { 1.0 } else { 0.0 },
                BinaryOperator::GreaterThan => if left > right { 1.0 } else { 0.0 },
                BinaryOperator::GreaterEqual => if left >= right { 1.0 } else { 0.0 },
            }
        },
    }
}

pub fn interpret_program_on_input(program: &[Clause], input: f64) -> Option<f64> {
    let mut result = input;

    for clause in program {
        match clause.clause_type {
            ClauseType::Filter => {
                if interpret_expression(&clause.expression, result) == 0.0 {
                    return None; // Filter condition not met
                }
            }
            ClauseType::Map => {
                result = interpret_expression(&clause.expression, result);
            }
        }
    }

    Some(result)
}