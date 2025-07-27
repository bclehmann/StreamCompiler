use std::ffi::CString;
use inkwell::OptimizationLevel;
use crate::numgrep::runner::Runner;

fn floats_within_string(s: &str) -> Vec<f64> {
    s.split(|c: char| !c.is_numeric() && c != '.' && c != '-')
        .filter_map(|word| word.parse::<f64>().ok())
        .collect()
}

pub fn entrypoint(
    program_text: &str,
    optimization_level: OptimizationLevel,
) {
    let program = crate::parser::lex_and_parse(&program_text);

    if let Ok(ast) = program {
        let runner = Runner::new(&ast, optimization_level);

        let stdin = std::io::stdin();
        let input_str = std::io::read_to_string(stdin)
            .expect("Failed to read from stdin");
        let input_lines = input_str.lines();

        let parsed = input_lines
            .map(|line| (CString::new(line).expect("Could not build cstring"), floats_within_string(line)))
            .filter(|(_, floats)| !floats.is_empty())
            .collect::<Vec<(CString, Vec<f64>)>>();

        runner.run(parsed.as_slice());
    } else {
        panic!("Failed to parse program: {}", program.unwrap_err());
    }
}