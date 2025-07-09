use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=src/runtime");
    let command_output = Command::new("clang")
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