use crate::parser;
use parser::{Clause, ClauseType};
use crate::interpreter::interpret_expression;

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