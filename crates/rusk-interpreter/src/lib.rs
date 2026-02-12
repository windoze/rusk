#![forbid(unsafe_code)]
#![cfg_attr(not(feature = "std"), no_std)]

/// Garbage collection (GC) abstraction and implementations.
pub mod gc;

/// A small interpreter for executing Rusk MIR.
pub mod interpreter;

/// Core-library host functions used by the compiler desugarings.
pub mod corelib;

// Re-export commonly used types
pub use corelib::register_core_host_fns;
pub use gc::{GcHeap, GcRef, MarkSweepHeap};
pub use interpreter::{Interpreter, RuntimeError, Value};

#[cfg(feature = "serde")]
#[derive(Debug)]
pub struct LoadError(bitcode::Error);

#[cfg(feature = "serde")]
impl core::fmt::Display for LoadError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "failed to load module: {}", self.0)
    }
}

#[cfg(feature = "serde")]
impl core::error::Error for LoadError {}

#[cfg(feature = "serde")]
pub fn load_module(bytes: &[u8]) -> Result<rusk_mir::Module, LoadError> {
    bitcode::deserialize(bytes).map_err(LoadError)
}
