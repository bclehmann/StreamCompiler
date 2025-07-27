use inkwell::OptimizationLevel;
use crate::numgrep::runner::Runner;

fn floats_within_string(s: &str) -> Vec<f64> {
    s.split(|c: char| !c.is_numeric() && c != '.' && c != '-')
        .filter_map(|word| word.parse::<f64>().ok())
        .collect()
}

pub fn entrypoint() {
    let program_text = std::env::args().nth(1).expect("No program text provided");
    let program = crate::parser::lex_and_parse(&program_text);

    let olevel = {
        let unparsed = std::env::args().nth(2).unwrap_or("O0".to_string());

        match unparsed.as_str() {
            "O0" => Some(inkwell::OptimizationLevel::None),
            "O1" => Some(inkwell::OptimizationLevel::Less),
            "O2" => Some(inkwell::OptimizationLevel::Default),
            "O3" => Some(inkwell::OptimizationLevel::Aggressive),
            _ => None,
        }
    };

    if let Ok(ast) = program {
        let runner = Runner::new(&ast, olevel.unwrap_or(OptimizationLevel::None));

        let stdin = std::io::stdin();
        let input_str = std::io::read_to_string(stdin)
            .expect("Failed to read from stdin");
        let input_lines = input_str.lines();

        let parsed = input_lines
            .map(|line| (line, floats_within_string(line)))
            .filter(|(_, floats)| !floats.is_empty())
            .collect::<Vec<(&str, Vec<f64>)>>();

        runner.run(parsed.as_slice());
    } else {
        panic!("Failed to parse program: {}", program.unwrap_err());
    }
}