use rusk_bytecode::to_bytes;
use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
    compile_file_to_bytecode_with_options,
};
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let examples_dir = crate_dir.join("examples");

    build_basic_run(&examples_dir);
    build_host_imports(&examples_dir);
    build_counter(&examples_dir);

    eprintln!("ok: examples compiled");
}

fn compile_example(entry: &Path, options: &CompileOptions) -> Vec<u8> {
    let module = compile_file_to_bytecode_with_options(entry, options)
        .unwrap_or_else(|e| panic!("compile `{}`: {e}", entry.display()));
    to_bytes(&module).unwrap_or_else(|e| panic!("encode `{}`: {e}", entry.display()))
}

fn write_bytes(path: &Path, bytes: Vec<u8>) {
    fs::write(path, bytes).unwrap_or_else(|e| panic!("write `{}`: {e}", path.display()));
}

fn build_basic_run(examples_dir: &Path) {
    let dir = examples_dir.join("basic-run");
    let src = dir.join("program.rusk");
    let out = dir.join("program.rbc");

    let options = CompileOptions::default();
    write_bytes(&out, compile_example(&src, &options));
}

fn build_host_imports(examples_dir: &Path) {
    let dir = examples_dir.join("host-imports");
    let src = dir.join("program.rusk");
    let out = dir.join("program.rbc");

    let mut options = CompileOptions::default();
    options
        .register_host_module(
            "js",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![
                    HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "log".to_string(),
                        sig: HostFnSig {
                            params: vec![HostType::String],
                            ret: HostType::Unit,
                        },
                    },
                    HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "add_int".to_string(),
                        sig: HostFnSig {
                            params: vec![HostType::Int, HostType::Int],
                            ret: HostType::Int,
                        },
                    },
                ],
            },
        )
        .expect("register js host module");

    write_bytes(&out, compile_example(&src, &options));
}

fn build_counter(examples_dir: &Path) {
    let dir = examples_dir.join("counter");
    let src = dir.join("program.rusk");
    let out = dir.join("program.rbc");

    let mut options = CompileOptions::default();
    options
        .register_host_module(
            "dom",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![HostFunctionDecl {
                    visibility: HostVisibility::Public,
                    name: "set_text".to_string(),
                    sig: HostFnSig {
                        params: vec![HostType::String, HostType::String],
                        ret: HostType::Unit,
                    },
                }],
            },
        )
        .expect("register dom host module");

    options
        .register_external_effect(
            "Dom",
            "wait_event",
            HostFnSig {
                params: Vec::new(),
                ret: HostType::String,
            },
        )
        .expect("register Dom.wait_event external effect");

    write_bytes(&out, compile_example(&src, &options));
}

