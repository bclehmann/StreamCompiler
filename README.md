## StreamCompiler

This is a simple JIT compiler and interpreter for me to get back into LLVM. It takes a program like `filter x % 2 | map x + 1 | ...` and runs it on a list of numbers taken from stdin (new-line separated).

I may consider adding some heuristics (e.g. interpret by default but JIT compile for input that's larger than n lines) but in the meantime this is what I cooked up over a weekend.

## numgrep

This also adds a `numgrep` command, which is a simple grep-like utility that only supports filter clauses and it prints all lines containing a number that matches the filter.

It currently is implemented by checking `argv[0]`, so you should create a symlink to the `streamcompiler` binary called `numgrep` to use it. You can also pass `--command numgrep` to use it without a symlink.

## Usage

```
Usage: streamcompiler [OPTIONS] <PROGRAM_TEXT>

Arguments:
  <PROGRAM_TEXT>

Options:
      --command <COMMAND>
  -O <OPTIMIZATION_LEVEL>      [default: 0]
      --interpret
      --imprecise
  -h, --help                   Print help
  -V, --version                Print version
```