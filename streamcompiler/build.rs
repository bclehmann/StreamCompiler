use std::process::Command;

fn main() {
    println!("cargo::rerun-if-changed=src/runtime");
    let command_output = Command::new("rustc")
        .args(
            [
                "src/runtime/io.rs",
                "--crate-type=lib",
                "--emit",
                "llvm-bc",
                "-o",
                "src/runtime/io.bc",
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