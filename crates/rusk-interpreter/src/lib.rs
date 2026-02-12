#![forbid(unsafe_code)]
#![cfg_attr(feature = "no_std", no_std)]

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
