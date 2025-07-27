extern crate pest;

#[macro_use]
extern crate pest_derive;

use clap::Parser;
use crate::Commands::{NumGrep, StreamCompiler};

mod parser;
mod compiler;
mod streamcompiler;
mod numgrep;

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
        .and_then(|cmd| {;
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

    let olevel = match cli.optimization_level {
            0 => Some(inkwell::OptimizationLevel::None),
            1 => Some(inkwell::OptimizationLevel::Less),
            2 => Some(inkwell::OptimizationLevel::Default),
            3 => Some(inkwell::OptimizationLevel::Aggressive),
            _ => None
    }.unwrap_or(inkwell::OptimizationLevel::None);

    match command {
        StreamCompiler => {
            streamcompiler::main::entrypoint(
                &cli.program_text,
                cli.interpret,
                olevel,
                !cli.imprecise,
            );
        },
        NumGrep => {
            numgrep::main::entrypoint(
                &cli.program_text,
                olevel,
            );
        },
    }
}
