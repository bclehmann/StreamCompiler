use std::vec;

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

use crate::interpreter;
use crate::parser::Clause;
use crate::compiler::{CodeGen, ProgramFunction};

pub trait Runner {
    fn run(&self, input: &[f64]);
}


pub struct InterpreterRunner<'a> {
    program: Vec<Clause<'a>>,
}

impl<'a> InterpreterRunner<'a>
{
    pub fn new(program: Vec<Clause<'a>>) -> Box<dyn Runner + 'a> {
        Box::new(InterpreterRunner { program })
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

pub struct CompilerRunner<'a> {
    program: JitFunction<'a, ProgramFunction>,
}

impl<'a> CompilerRunner<'a> {
    pub fn new(program: &'a [Clause], olevel: OptimizationLevel) -> Box<dyn Runner + 'a> {
        let context = Box::leak(Box::new(Context::create())); // `program` will outlive this scope, so we just leak the memory
        let codegen = Box::leak(Box::new(CodeGen::new(context, olevel))); 
    
        let program= codegen.compile_program(program);

        Box::new(CompilerRunner { program })
    }
}

impl<'a> Runner for CompilerRunner<'a> {
    fn run(&self, input: &[f64]) {
        let mut result = vec![0.0; input.len()];
        let mut filtered_out = vec![false; input.len()];
        unsafe { self.program.call(input.as_ptr(), input.len() as i32, result.as_mut_ptr(), filtered_out.as_mut_ptr()); }

        for i in 0..input.len() {
            if filtered_out[i] {
                continue;
            }
            println!("{}", result[i]);
        }
    }
}
