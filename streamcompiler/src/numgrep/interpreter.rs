use crate::interpreter::interpret_expression;
use crate::parser::{Clause, ClauseType};

pub fn interpret_program_on_input(program: &[Clause], input_str: &str, input_floats: &[f64]) {
    for num in input_floats {
        let mut matches = true;

        for clause in program {
            match clause.clause_type {
                ClauseType::Filter => {
                    if interpret_expression(&clause.expression, *num) == 0.0 {
                        matches = false;
                        break;
                    }
                }
                ClauseType::Map => {
                    panic!("Map clauses are not supported in numgrep");
                }
            }
        }

        if matches {
            println!("{}", input_str);
            return;
        }
    }
}