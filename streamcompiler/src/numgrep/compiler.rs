use std::ffi::CString;
use inkwell::context::Context;
use inkwell::OptimizationLevel;
use crate::compiler::{CodeGen, JittedNumgrepProgram, NumgrepInput};
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

    pub fn run(&self, input: &[(CString, Vec<f64>)]) {
        let input = input.iter()
            .map(|r| NumgrepInput {
                string: r.0.as_ptr() as *const _,
                input: r.1.as_ptr(),
                input_len: r.1.len() as i32,
            })
            .into_iter()
            .collect::<Vec<NumgrepInput>>();

        unsafe { self.jitted_program.call(input.as_ptr(), input.len() as i32) ; }
    }
}