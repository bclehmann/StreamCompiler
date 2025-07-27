use inkwell::context::Context;
use inkwell::OptimizationLevel;
use crate::compiler::{CodeGen, JittedNumgrepProgram};
use crate::parser::Clause;

#[derive(Debug)]
pub struct Runner<'a> {
    jitted_program: JittedNumgrepProgram<'a>,
}

impl<'a> Runner<'a> {
    pub fn new(program: &'a [Clause], olevel: OptimizationLevel) -> Self {
        let context = Box::leak(Box::new(Context::create())); // `program` will outlive this scope, so we just leak the memory
        let codegen = Box::leak(Box::new(CodeGen::new(context, olevel, 6, false))); // float precision and runtime don't matter for numgrep, so we just set default float precision and strip the runtime

        let jitted_program = codegen.compile_numgrep(program);

        Runner {
            jitted_program,
        }
    }

    pub fn run(&self, input: &[(&str, Vec<f64>)]) {
        let input_floats = input.iter()
            .flat_map(|(_, floats)| floats.iter().cloned())
            .collect::<Vec<f64>>();

        let should_include = vec![false; input_floats.len()];
        unsafe { self.jitted_program.call(input_floats.as_ptr(), should_include.as_ptr(), input_floats.len() as i32); }

        let mut output_index = 0;
        for i in 0..input.len() {
            for j in 0..input[i].1.len() {
                if should_include[output_index + j] {
                    println!("{}", input[i].0);
                    break;
                }
            }

            output_index += input[i].1.len();
        }
    }
}