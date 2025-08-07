#[cfg(feature="jit")]
use inkwell::OptimizationLevel;
use crate::parser;
use crate::streamcompiler::runner;

fn get_runner<'a>(
    program: &'a[parser::Clause],
    should_interpret: bool,
    optimization_level: u8,
    precise_compiled_floats: bool,
) -> Box<dyn runner::Runner + 'a> {
    #[cfg(feature="jit")]
    if !should_interpret {
        let optimization_level = match optimization_level {
            0 => OptimizationLevel::None,
            1 => OptimizationLevel::Less,
            2 => OptimizationLevel::Default,
            3 => OptimizationLevel::Aggressive,
            _ => panic!("Invalid optimization level: {}", optimization_level),
        };
        return runner::CompilerRunner::new(
            program,
            optimization_level,
            if precise_compiled_floats { 17 } else { 6 }
        );
    }

    runner::InterpreterRunner::new_boxed(program)
}

pub fn entrypoint(
    program_text: &str,
    should_interpret: bool,
    optimization_level: u8,
    precise_compiled_floats: bool,
) {
    let program = parser::lex_and_parse(program_text, None);

    if let Ok(ast) = program {
        let runner = get_runner(&ast, should_interpret, optimization_level, precise_compiled_floats);

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