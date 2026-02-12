use rusk_compiler::{compile_file_to_mir, to_bytes};
use std::env;
use std::fs;
use std::path::Path;
use std::process;

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
    let module = match compile_file_to_mir(input_path) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("compile error: {e}");
            process::exit(1);
        }
    };

    // 序列化 MIR
    let bytes = match to_bytes(&module) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("serialization error: {e}");
            process::exit(1);
        }
    };

    // 生成输出文件名：将 .rusk 扩展名替换为 .mir
    let output_path = input_path.with_extension("mir");

    // 写入文件
    if let Err(e) = fs::write(&output_path, bytes) {
        eprintln!("failed to write output file: {e}");
        process::exit(1);
    }

    eprintln!("compiled {} to {}", path, output_path.display());
}
