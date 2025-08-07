extern crate pest;

#[macro_use]
extern crate pest_derive;

use clap::Parser;
use crate::Commands::{NumGrep, StreamCompiler};

mod parser;
#[cfg(feature="jit")]
mod compiler;
mod streamcompiler;
mod numgrep;
mod interpreter;

#[derive(clap::Parser)]
#[derive(Debug)]
#[command(version = "0.1.0")]
struct Cli {
    program_text: String,

    #[arg(long)]
    command: Option<String>,

    #[arg(short = 'O', default_value = "0")]
    optimization_level: u8,

    #[arg(long)]
    interpret: bool,

    #[arg(long)]
    imprecise: bool,
}

enum Commands {
    StreamCompiler,
    NumGrep,
}

fn main() {
    let cli = Cli::parse();

    let command = cli.command.as_ref()
        .and_then(|cmd| {
            if cmd.eq_ignore_ascii_case("numgrep") {
                Some(NumGrep)
            } else if cmd.eq_ignore_ascii_case("streamcompiler") {
                Some(StreamCompiler)
            } else {
                None
            }
        })
        .or_else(|| match std::env::args().nth(0) {
            Some(arg) if arg.to_ascii_lowercase().ends_with("numgrep") => Some(NumGrep),
            _ => None,
        })
        .unwrap_or(StreamCompiler);

    #[cfg(not(feature="jit"))]
    if !cli.interpret {
        eprintln!("Warning: JIT compilation is not enabled. The program will run in interpreted mode. Build with the `--features jit` flag to enable JIT compilation or run with --interpret to silence this warning.");
    }

    match command {
        StreamCompiler => {
            streamcompiler::main::entrypoint(
                &cli.program_text,
                cli.interpret,
                cli.optimization_level,
                !cli.imprecise,
            );
        },
        NumGrep => {
            numgrep::main::entrypoint(
                &cli.program_text,
                cli.interpret,
                cli.optimization_level,
            );
        },
    }
}
