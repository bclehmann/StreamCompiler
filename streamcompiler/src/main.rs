use crate::runner::Runner;

extern crate pest;

#[macro_use]
extern crate pest_derive;

mod parser;
mod runner;
mod interpreter;
mod compiler;

fn main() {
    let program_text = std::env::args().nth(1).expect("No program text provided");
    let program = parser::lex_and_parse(&program_text);
    let should_interpret= std::env::args().nth(2).unwrap_or("interpret".into()).eq_ignore_ascii_case("interpret");

    let olevel = if should_interpret {
        None
    } else {
        let unparsed = std::env::args().nth(3)
            .unwrap_or("O0".to_string());

        match unparsed.as_str() {
            "O0" => Some(inkwell::OptimizationLevel::None),
            "O1" => Some(inkwell::OptimizationLevel::Less),
            "O2" => Some(inkwell::OptimizationLevel::Default),
            "O3" => Some(inkwell::OptimizationLevel::Aggressive),
            _ => None
        }
    };
    let precise_compiled_floats = std::env::args().nth(4).unwrap_or("imprecise".into()).eq_ignore_ascii_case("precise");

    if let Ok(ast) = program {
        let runner: Box<dyn Runner> = if !should_interpret {
            runner::CompilerRunner::new(
                &ast,
                olevel.unwrap_or(inkwell::OptimizationLevel::None),
                if precise_compiled_floats { 17 } else { 6 }
            )
        } else {
            runner::InterpreterRunner::new_boxed(&ast)
        };

        let stdin = std::io::stdin();
        let input_str= std::io::read_to_string(stdin)
            .expect("Failed to read from stdin");
        let input_lines = input_str
            .lines();
        let input = input_lines
            .map(|line| line.parse::<f64>().expect("Could not parse input"));

        runner.run(&input.collect::<Vec<f64>>());
    } else {
        panic!("Failed to parse program: {}", program.unwrap_err());
    }
}
