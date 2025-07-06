extern crate pest;

#[macro_use]
extern crate pest_derive;

mod parser;
mod interpreter;
mod compiler;

fn main() {
    let stdin = std::io::stdin();
    let input_str= std::io::read_to_string(stdin)
        .expect("Failed to read from stdin");
    let input_lines = input_str
        .lines();
    let input = input_lines
        .map(|line| line.parse::<f64>().expect("Could not parse input"));
    let program_text = std::env::args().nth(1).expect("No program text provided");

    let program = parser::lex_and_parse(&program_text);

    let should_compile = std::env::args().nth(2).unwrap_or("interpret".into()).eq_ignore_ascii_case("compile");

    match program {
        Ok(program) => {
            if should_compile {
                compiler::compile_and_run_program(&program, input);
            } else {
                for input in input {
                    match interpreter::interpret_program_on_input(&program, input) {
                        Some(result) => println!("{}", result),
                        None => {}
                    }
                }
            }
        }
        Err(e) => panic!("Failed to parse program: {}", e),
    }
}
