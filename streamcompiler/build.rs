use std::env;
use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=src/runtime");
    let clangdir = env::var("CLANGDIR");
    let clang_path = match clangdir {
        Ok(path) => format!("{path}/clang"),
        Err(_) => {
            "clang".to_string() // Default to whatever is in PATH and pray
        }
    };

    let command_output = Command::new(clang_path)
        .args(
            [
                "-emit-llvm",
                "-O3",
                "-c",
                "src/runtime/io.cpp",
                "-o",
                "src/runtime/out/io.bc",
            ],
        )
        .output()
        .expect("Failed to compile runtime IO code with rustc");

    if !command_output.status.success() {
        panic!(
            "rustc failed with status: {}\nstdout: {}\nstderr: {}",
            command_output.status,
            String::from_utf8_lossy(&command_output.stdout),
            String::from_utf8_lossy(&command_output.stderr)
        );
    }
}