use inkwell::OptimizationLevel;
use crate::parser;
use crate::streamcompiler::runner;
use crate::streamcompiler::runner::Runner;

pub fn entrypoint(
    program_text: &str,
    should_interpret: bool,
    optimization_level: OptimizationLevel,
    precise_compiled_floats: bool,
) {
    let program = parser::lex_and_parse(program_text);

    if let Ok(ast) = program {
        let runner: Box<dyn Runner> = if !should_interpret {
            runner::CompilerRunner::new(
                &ast,
                optimization_level,
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