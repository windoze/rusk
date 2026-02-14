use rusk_bytecode::from_bytes;
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use rusk_vm::{StepResult, Vm, vm_step};
use std::env;
use std::fs;
use std::path::Path;
use std::process;

fn main() {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: rusk <file.rusk|file.rbc>");
        process::exit(2);
    };
    if args.next().is_some() {
        eprintln!("error: expected exactly one input file");
        process::exit(2);
    }

    let input_path = Path::new(&path);
    let extension = input_path.extension().and_then(|s| s.to_str());

    let module = match extension {
        Some("rbc") => {
            // 直接加载 .rbc 文件
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
            match compile_file_to_bytecode_with_options(input_path, &options) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("compile error: {e}");
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("error: input file must have .rusk or .rbc extension");
            process::exit(2);
        }
    };

    let mut vm = match Vm::new(module.clone()) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("vm init error: {e}");
            process::exit(1);
        }
    };
    std_io::install_vm(&module, &mut vm);

    match vm_step(&mut vm, None) {
        StepResult::Done { value } => {
            if value != rusk_vm::AbiValue::Unit {
                // `main` defaults to returning `unit` (explicitly or via omission), but printing a
                // non-unit return is useful during development.
                println!("{value:?}");
            }
        }
        StepResult::Trap { message } => {
            eprintln!("runtime error: {message}");
            process::exit(1);
        }
        StepResult::Request {
            effect_id, args, ..
        } => {
            let name = module
                .external_effect(effect_id)
                .map(|d| format!("{}.{}", d.interface, d.method))
                .unwrap_or_else(|| format!("<unknown {}>", effect_id.0));
            eprintln!("runtime error: external effect request: {name} args={args:?}");
            process::exit(1);
        }
        StepResult::Yield { .. } => {
            eprintln!("runtime error: unexpected yield");
            process::exit(1);
        }
    }
}
