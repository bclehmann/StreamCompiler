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

        match jitted_program.vector_width {
            Some(8) => (),
            Some(_) => panic!("Vector width other than 8 is not supported for numgrep"),
            None => (),
        }

        Runner {
            jitted_program,
        }
    }

    pub fn run(&self, input: &[(&str, Vec<f64>)]) {
        let input_floats = input.iter()
            .flat_map(|(_, floats)| floats.iter().cloned())
            .collect::<Vec<f64>>();

        // TODO: Handle when input_floats is not a multiple of the vector width
        let should_include = vec![0u8; input_floats.len() / self.jitted_program.vector_width.unwrap_or(1) as usize];
        unsafe { self.jitted_program.call(input_floats.as_ptr(), should_include.as_ptr(), input_floats.len() as i32); }

        println!("should_include: {:?}", should_include);

        #[inline]
        fn is_set(should_include: &[u8], index: usize, vec_width: usize) -> bool {
            if vec_width != 1 {
                should_include[index / vec_width] & (1u8 << (index % vec_width)) != 0
            } else {
                should_include[index] != 0
            }
        }

        let mut output_index = 0;
        for i in 0..input.len() {
            for j in 0..input[i].1.len() {
                if is_set(&should_include, output_index + j, self.jitted_program.vector_width.unwrap_or(1) as usize) {
                    println!("{}", input[i].0);
                    break;
                }
            }

            output_index += input[i].1.len();
        }
    }
}