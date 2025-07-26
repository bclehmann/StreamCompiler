extern crate pest;

#[macro_use]
extern crate pest_derive;

mod parser;
mod compiler;
mod streamcompiler;
mod numgrep;

fn main() {
    match std::env::args().nth(0) {
        Some(arg) if arg.ends_with("numgrep") => {
            panic!("numgrep not implemented yet")
        },
        _ => {
            streamcompiler::main::entrypoint();
        },
    }
}
