use rusk_bytecode::to_bytes;
use rusk_compiler::analysis as compiler_analysis;
use rusk_compiler::tooling::diagnostics as tooling_diagnostics;
use rusk_compiler::{CompileError, CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use std::env;
use std::fs;
use std::path::Path;
use std::process;
use std::sync::Arc;

fn main() {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: ruskc <file.rusk>");
        process::exit(2);
    };
    if args.next().is_some() {
        eprintln!("error: expected exactly one input file");
        process::exit(2);
    }

    let input_path = Path::new(&path);
    let mut options = CompileOptions::default();
    std_io::register_host_module(&mut options);
    let module = match compile_file_to_bytecode_with_options(input_path, &options) {
        Ok(m) => m,
        Err(e) => {
            emit_compile_diagnostics(input_path, &options, &e);
            process::exit(1);
        }
    };

    // 序列化 bytecode module
    let bytes = match to_bytes(&module) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("serialization error: {e}");
            process::exit(1);
        }
    };

    // 生成输出文件名：将 .rusk 扩展名替换为 .rbc
    let output_path = input_path.with_extension("rbc");

    // 写入文件
    if let Err(e) = fs::write(&output_path, bytes) {
        eprintln!("failed to write output file: {e}");
        process::exit(1);
    }

    eprintln!("compiled {} to {}", path, output_path.display());
}

fn emit_compile_diagnostics(path: &Path, options: &CompileOptions, fallback: &CompileError) {
    let analysis = compiler_analysis::analyze_entry_file(
        path,
        options,
        Arc::new(rusk_compiler::vfs::FsSourceProvider),
    );
    if analysis.diagnostics.is_empty() {
        eprintln!("compile error: {fallback}");
        return;
    }

    for (idx, diag) in analysis.diagnostics.iter().enumerate() {
        if idx > 0 {
            eprintln!();
        }
        let rendered = tooling_diagnostics::render_human(diag, &analysis.source_map);
        eprint!("{rendered}");
    }
}
