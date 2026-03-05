#![forbid(unsafe_code)]

mod ast;
mod bytecode_lower;
#[cfg(test)]
mod capture_analysis_tests;
pub mod host;
#[cfg(test)]
mod inliner_tests;
mod lexer;
mod mir_opt;
mod modules;
mod parser;
mod typeck;
pub mod vfs;

/// Script front-end: parses and compiles Rusk source to bytecode (via an internal MIR).
mod compiler;
mod derive_expand;

// Re-export commonly used types
pub use compiler::{
    CompileError, CompileMetrics, compile_file_to_bytecode, compile_file_to_bytecode_with_options,
    compile_file_to_bytecode_with_options_and_metrics, compile_to_bytecode,
    compile_to_bytecode_with_options, compile_to_bytecode_with_options_and_metrics,
};
pub use host::{
    CompileOptions, Cont, ExternalEffectDecl, HostFnSig, HostParamTypes, HostReturnType, HostType,
    HostTypeOf,
};
pub use rusk_bytecode::OptLevel;

/// Source text span utilities used by the front-end.
pub mod source;

/// Source map for tracking source locations.
pub mod source_map;

/// Public analysis APIs intended for editor tooling (e.g. LSP).
pub mod analysis;

/// Developer tooling (formatter, linter, diagnostic rendering).
pub mod tooling;

// NOTE: MIR serialization APIs intentionally not exposed: MIR is an internal compiler IR.
