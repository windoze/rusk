#![forbid(unsafe_code)]

//! Host module declarations + VM installers for common environments.
//!
//! This crate contains small helpers that:
//! - declare host modules to the compiler via [`rusk_compiler::CompileOptions`]
//! - install matching host import implementations into a [`rusk_vm::Vm`]

/// Standard I/O host module (`_std_host`).
pub mod std_io;
