# Rusk

Rusk is an experimental programming language and runtime, implemented in Rust.
It aims to combine:

- **Rust-like syntax** (block expressions, `match`, explicit mutability controls), with
- **TypeScript-like ergonomics** (type inference, generics, interface-driven abstraction), and
- **Algebraic effects** as a first-class mechanism for control-flow extensions (exceptions,
  async, generators, etc.).

This repository is a reference implementation that compiles `.rusk` source code into a compact
bytecode module (`.rbc`) lowered from a mid-level IR (“MIR”), and then executes that bytecode with
a small VM.

Rusk is designed to be embedded (CLI, WASM, embedded devices), so I/O and platform integration are
provided via **host functions**:

- The **compiler** is given host *prototypes* (names + signatures) before compilation, so name
  resolution and typechecking can succeed.
- The **runtime** (VM / interpreter) is given concrete host implementations at runtime; it will
  trap if the module declares host imports that are not installed.

In this repo, the `rusk` CLI registers a minimal host-defined `std` module with
`std::print(string) -> unit` and `std::println(string) -> unit`.

## The Language

The source language is specified in [`RUSK_SPEC.md`](RUSK_SPEC.md) (currently v0.4). MIR is
specified in [`MIR_SPEC.md`](MIR_SPEC.md) (currently v0.2 draft).

A tiny example:

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## Project Layout

- `src/main.rs`: `rusk` CLI (`rusk <file.rusk|file.rbc>`)
- `src/bin/ruskc.rs`: `ruskc` compiler CLI (emits `.rbc`)
- `crates/rusk-compiler/`: parser/typechecker + lowering from Rusk → MIR
- `crates/rusk-mir/`: MIR data structures (and optional serialization)
- `crates/rusk-interpreter/`: MIR interpreter + GC + core runtime intrinsics
- `crates/rusk-bytecode/`: bytecode module + `.rbc` serialization + verifier
- `crates/rusk-vm/`: bytecode VM runtime (step API, host imports, effects)
- `crates/rusk-host/`: reusable host-module declarations + installers (e.g. `std::print`)
- `fixtures/` and `tests/`: executable fixtures and regression tests

## Quick Start

```sh
cargo run --bin rusk -- fixtures/020_arrays_get_set.rusk
cargo run --bin ruskc -- fixtures/020_arrays_get_set.rusk
cargo run --bin rusk -- fixtures/020_arrays_get_set.rbc
cargo test
```
