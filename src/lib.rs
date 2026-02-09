#![forbid(unsafe_code)]

/// Rusk mid-level intermediate representation (MIR).
pub mod mir;

/// Source text span utilities used by the front-end.
pub mod source;

mod ast;
mod lexer;
mod parser;
mod typeck;

/// Script front-end: parses and compiles Rusk source to MIR.
pub mod compiler;

/// Garbage collection (GC) abstraction and implementations.
pub mod gc;

/// A small interpreter for executing Rusk MIR.
pub mod interpreter;

/// Standard-library host functions used by the compiler desugarings.
pub mod stdlib;

pub use interpreter::{Interpreter, RuntimeError, Value};
