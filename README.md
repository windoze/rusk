# Rusk

Rusk is an experimental programming language and runtime, implemented in Rust.
It aims to combine:

- **Rust-like syntax** (block expressions, `match`, explicit mutability controls), with
- **TypeScript-like ergonomics** (type inference, generics, interface-driven abstraction), and
- **Algebraic effects** as a first-class mechanism for control-flow extensions (exceptions,
  async, generators, etc.).

This repository is a reference implementation that compiles `.rusk` source code into a small
mid-level IR (“MIR”) and then executes that MIR with an interpreter. The interpreter provides a
set of *host functions* that act as the minimal “standard library” used by compiler desugarings.

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

- `src/main.rs`: `rusk` CLI (`rusk <file.rusk|file.mir>`)
- `crates/rusk-compiler/`: parser/typechecker + lowering from Rusk → MIR
- `crates/rusk-mir/`: MIR data structures (and optional serialization)
- `crates/rusk-interpreter/`: MIR interpreter + GC + core host functions
- `fixtures/` and `tests/`: executable fixtures and regression tests

## Quick Start

```sh
cargo run --bin rusk -- fixtures/020_arrays_get_set.rusk
cargo test
```
