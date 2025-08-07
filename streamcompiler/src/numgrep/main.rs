#[cfg(feature="jit")]
use inkwell::OptimizationLevel;
#[cfg(feature="jit")]
use crate::numgrep::compiler::Runner;
use std::ffi::CString;
use crate::numgrep::interpreter::interpret_program_on_input;
use crate::parser::ClauseType;

fn floats_within_string(s: &str) -> Vec<f64> {
    s.split(|c: char| !c.is_numeric() && c != '.' && c != '-')
        .filter_map(|word| word.parse::<f64>().ok())
        .collect()
}

fn get_input() -> Vec<(CString, Vec<f64>)> {
        let stdin = std::io::stdin();
        let input_str = std::io::read_to_string(stdin)
            .expect("Failed to read from stdin");
        let input_lines = input_str.lines();

        input_lines
            .map(|line| (CString::new(line).expect("Could not build cstring"), floats_within_string(line)))
            .filter(|(_, floats)| !floats.is_empty())
            .collect::<Vec<(CString, Vec<f64>)>>()
}

pub fn entrypoint(
    program_text: &str,
    should_interpret: bool,
    optimization_level: u8,
) {
    let program = crate::parser::lex_and_parse(&program_text, Some(ClauseType::Filter));

    if let Ok(ast) = program {
        #[cfg(feature="jit")]
        if !should_interpret {
            let optimization_level = match optimization_level {
                0 => OptimizationLevel::None,
                1 => OptimizationLevel::Less,
                2 => OptimizationLevel::Default,
                3 => OptimizationLevel::Aggressive,
                _ => panic!("Invalid optimization level: {}", optimization_level),
            };
            let runner = Runner::new(&ast, optimization_level);
            let input = get_input();
            runner.run(input.as_slice());
            return;
        }

        let input = get_input();
        for (string, floats) in input {
            interpret_program_on_input(&ast, string.to_str().expect("Could not convert CString to str"), floats.as_slice());
        }
    } else {
        panic!("Failed to parse program: {}", program.unwrap_err());
    }
}