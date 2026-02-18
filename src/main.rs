use rusk_bytecode::from_bytes;
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use rusk_vm::{StepResult, Vm, vm_step};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

fn main() {
    let mut args = env::args_os().skip(1).peekable();

    let mut sysroot: Option<PathBuf> = None;
    let mut load_std = true;

    while let Some(arg) = args.peek() {
        let s = arg.to_string_lossy();
        match s.as_ref() {
            "--help" | "-h" => {
                eprintln!(
                    "usage: rusk [--sysroot <path>] [--no-std] <file.rusk|file.rbc> [args...]"
                );
                process::exit(0);
            }
            "--sysroot" => {
                let _ = args.next();
                let Some(path) = args.next() else {
                    eprintln!("error: `--sysroot` expects a path argument");
                    process::exit(2);
                };
                sysroot = Some(PathBuf::from(path));
            }
            "--no-std" => {
                let _ = args.next();
                load_std = false;
            }
            "--" => {
                let _ = args.next();
                break;
            }
            _ if s.starts_with("--") => {
                eprintln!("error: unknown option `{s}`");
                process::exit(2);
            }
            _ => break,
        }
    }

    let Some(path) = args.next() else {
        eprintln!("usage: rusk [--sysroot <path>] [--no-std] <file.rusk|file.rbc> [args...]");
        process::exit(2);
    };

    let input_path = PathBuf::from(path);
    let argv0 = absolute_path_string(&input_path);
    let mut argv: Vec<String> = Vec::with_capacity(1);
    argv.push(argv0);
    argv.extend(args.map(|a| a.to_string_lossy().into_owned()));

    let extension = input_path.extension().and_then(|s| s.to_str());

    let module = match extension {
        Some("rbc") => {
            // 直接加载 .rbc 文件
            let bytes = match fs::read(&input_path) {
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
            let mut options = CompileOptions {
                sysroot,
                load_std,
                ..Default::default()
            };
            if load_std {
                std_io::register_host_module(&mut options);
            }
            match compile_file_to_bytecode_with_options(&input_path, &options) {
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

    let Some(entry_fn) = module.function(module.entry) else {
        eprintln!(
            "vm init error: invalid entry function id {}",
            module.entry.0
        );
        process::exit(1);
    };
    let mut vm = match entry_fn.param_count {
        0 => Vm::new(module.clone()),
        1 => Vm::new_with_argv(module.clone(), argv),
        n => Err(rusk_vm::VmError::InvalidState {
            message: format!("unsupported entry arity: expected 0 or 1 param, got {n}"),
        }),
    }
    .unwrap_or_else(|e| {
        eprintln!("vm init error: {e}");
        process::exit(1);
    });
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

fn absolute_path_string(path: &Path) -> String {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    };
    abs.to_string_lossy().into_owned()
}
