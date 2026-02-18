#![forbid(unsafe_code)]

mod ast;
mod bytecode_lower;
#[cfg(test)]
mod capture_analysis_tests;
pub mod host;
mod lexer;
mod mir_opt;
mod modules;
mod parser;
mod typeck;

/// Script front-end: parses and compiles Rusk source to bytecode (via an internal MIR).
mod compiler;

// Re-export commonly used types
pub use compiler::{
    CompileError, CompileMetrics, compile_file_to_bytecode, compile_file_to_bytecode_with_options,
    compile_file_to_bytecode_with_options_and_metrics, compile_to_bytecode,
    compile_to_bytecode_with_options, compile_to_bytecode_with_options_and_metrics,
};
pub use host::{
    CompileOptions, ExternalEffectDecl, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType,
    HostVisibility,
};
pub use rusk_bytecode::OptLevel;

/// Source text span utilities used by the front-end.
pub mod source;

/// Source map for tracking source locations.
pub mod source_map;

// NOTE: MIR serialization APIs intentionally not exposed: MIR is an internal compiler IR.
