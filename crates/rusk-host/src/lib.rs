#![forbid(unsafe_code)]

//! Host module declarations + VM installers for common environments.
//!
//! This crate contains small helpers that:
//! - install host import implementations into a [`rusk_vm::Vm`]

/// Standard I/O host import implementations (`std::print`, `std::println`).
pub mod std_io;
