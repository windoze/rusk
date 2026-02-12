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

#[cfg(feature = "serde")]
#[derive(Debug)]
pub struct SaveError(bitcode::Error);

#[cfg(feature = "serde")]
impl core::fmt::Display for SaveError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "failed to save module: {}", self.0)
    }
}

#[cfg(feature = "serde")]
impl core::error::Error for SaveError {}

#[cfg(feature = "serde")]
pub fn to_bytes(module: &rusk_mir::Module) -> Result<Vec<u8>, SaveError> {
    bitcode::serialize(module).map_err(SaveError)
}
