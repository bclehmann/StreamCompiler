use inkwell::context::Context;
use inkwell::OptimizationLevel;

use crate::interpreter;
use crate::parser::Clause;
use crate::compiler::{CodeGen, JittedProgram};

pub trait Runner {
    fn run(&self, input: &[f64]);
}

#[derive(Debug)]
pub struct InterpreterRunner<'a> {
    program: &'a[Clause<'a>],
}

impl<'a> InterpreterRunner<'a>
{
    pub fn new_boxed(program: &'a [Clause]) -> Box<dyn Runner + 'a> {
        Box::new(InterpreterRunner { program })
    }

    pub fn new(program: &'a [Clause]) -> InterpreterRunner<'a> {
        InterpreterRunner { program }
    }
}

impl<'a> Runner for InterpreterRunner<'a> {
    fn run(&self, input: &[f64]) {
        for &v in input {
            match interpreter::interpret_program_on_input(&self.program, v) {
                Some(result) => println!("{}", result),
                None => {}
            }
        }
    }
}

#[derive(Debug)]
pub struct CompilerRunner<'a> {
    interpreter: InterpreterRunner<'a>,
    jitted_program: JittedProgram<'a>,
}

impl<'a> CompilerRunner<'a> {
    pub fn new(program: &'a [Clause], olevel: OptimizationLevel, float_precision: u32) -> Box<dyn Runner + 'a> {
        let context = Box::leak(Box::new(Context::create())); // `program` will outlive this scope, so we just leak the memory
        let codegen = Box::leak(Box::new(CodeGen::new(context, olevel, float_precision))); 
    
        let jitted_program = codegen.compile_program(program);
        let interpreter = InterpreterRunner::new(program);

        Box::new(CompilerRunner { interpreter, jitted_program })
    }
}

impl<'a> Runner for CompilerRunner<'a> {
    fn run(&self, input: &[f64]) {
        if self.jitted_program.is_vectorized {
            let aligned_input_len = input.len() - input.len() % 4;
            let first_part = &input[..aligned_input_len];
            let rest_part = &input[aligned_input_len..];

            unsafe { self.jitted_program.call(first_part.as_ptr(), first_part.len() as i32); }
            self.interpreter.run(rest_part); // We need to keep this aligned, and compiling a scalar version is probably unnecessary
        } else {
            unsafe { self.jitted_program.call(input.as_ptr(), input.len() as i32); }
        };
    }
}
