#![forbid(unsafe_code)]

mod ast;
mod lexer;
mod modules;
mod parser;
mod typeck;

/// Script front-end: parses and compiles Rusk source to MIR.
pub mod compiler;

// Re-export commonly used types
pub use compiler::{CompileError, compile_file_to_mir, compile_to_mir};

/// Source text span utilities used by the front-end.
pub mod source;

/// Source map for tracking source locations.
pub mod source_map;
