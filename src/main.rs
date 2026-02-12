use rusk_compiler::{CompileOptions, compile_file_to_mir_with_options};
use rusk_host::std_io;
use rusk_interpreter::{Interpreter, Value, from_bytes, register_core_host_fns};
use std::env;
use std::fs;
use std::path::Path;
use std::process;

fn main() {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: rusk <file.rusk|file.mir>");
        process::exit(2);
    };
    if args.next().is_some() {
        eprintln!("error: expected exactly one input file");
        process::exit(2);
    }

    let input_path = Path::new(&path);
    let extension = input_path.extension().and_then(|s| s.to_str());

    let module = match extension {
        Some("mir") => {
            // 直接加载 .mir 文件
            let bytes = match fs::read(input_path) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("failed to read file: {e}");
                    process::exit(1);
                }
            };
            match from_bytes(&bytes) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("load error: {e}");
                    process::exit(1);
                }
            }
        }
        Some("rusk") => {
            // 编译 .rusk 文件
            let mut options = CompileOptions::default();
            std_io::register_host_module(&mut options);
            match compile_file_to_mir_with_options(input_path, &options) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("compile error: {e}");
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("error: input file must have .rusk or .mir extension");
            process::exit(2);
        }
    };

    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);
    std_io::install(&mut interp);

    match interp.run_function("main", vec![]) {
        Ok(Value::Unit) => {}
        Ok(other) => {
            // `main` defaults to returning `unit` (explicitly or via omission), but printing a
            // non-unit return is useful during development.
            println!("{other:?}");
        }
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}
