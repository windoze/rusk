use rusk_compiler::{CompileOptions, HostFnSig, HostModuleDecl, HostType, HostVisibility};
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../../fixtures/200_bytecode_call_host_add_int_ok.rusk");
    println!("cargo:rerun-if-changed=../../fixtures/210_bytecode_external_effect_add_int_ok.rusk");
    println!(
        "cargo:rerun-if-changed=../../fixtures/211_bytecode_external_effect_echo_string_ok.rusk"
    );
    println!(
        "cargo:rerun-if-changed=../../fixtures/213_bytecode_external_effect_echo_bytes_ok.rusk"
    );
    println!(
        "cargo:rerun-if-changed=../../fixtures/206_bytecode_call_host_continuation_roundtrip_ok.rusk"
    );
    println!(
        "cargo:rerun-if-changed=../../fixtures/207_bytecode_host_tail_resume_pinned_continuation_ok.rusk"
    );
    println!("cargo:rerun-if-changed=../../fixtures/256_typerep_never_typearg_runtime_error.rusk");

    let mut options = CompileOptions::default();

    options
        .register_host_module(
            "test",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![
                    rusk_compiler::HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "add_int".to_string(),
                        sig: HostFnSig {
                            params: vec![HostType::Int, HostType::Int],
                            ret: HostType::Int,
                        },
                    },
                    rusk_compiler::HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "bytes_eq".to_string(),
                        sig: HostFnSig {
                            params: vec![HostType::Bytes, HostType::Bytes],
                            ret: HostType::Bool,
                        },
                    },
                    rusk_compiler::HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "store_cont".to_string(),
                        sig: HostFnSig {
                            params: vec![HostType::Cont {
                                param: Box::new(HostType::Int),
                                ret: Box::new(HostType::Int),
                            }],
                            ret: HostType::Unit,
                        },
                    },
                    rusk_compiler::HostFunctionDecl {
                        visibility: HostVisibility::Public,
                        name: "take_cont".to_string(),
                        sig: HostFnSig {
                            params: Vec::new(),
                            ret: HostType::Cont {
                                param: Box::new(HostType::Int),
                                ret: Box::new(HostType::Int),
                            },
                        },
                    },
                ],
            },
        )
        .expect("register test host module");

    options
        .register_external_effect(
            "TestFfi",
            "add",
            HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int,
            },
        )
        .expect("register TestFfi.add");
    options
        .register_external_effect(
            "TestFfi",
            "echo",
            HostFnSig {
                params: vec![HostType::String],
                ret: HostType::String,
            },
        )
        .expect("register TestFfi.echo");
    options
        .register_external_effect(
            "TestFfi",
            "echo_bytes",
            HostFnSig {
                params: vec![HostType::Bytes],
                ret: HostType::Bytes,
            },
        )
        .expect("register TestFfi.echo_bytes");

    let out_dir = PathBuf::from(std::env::var_os("OUT_DIR").expect("OUT_DIR is set"));

    compile_fixture(
        "../../fixtures/200_bytecode_call_host_add_int_ok.rusk",
        &options,
        out_dir.join("call_host_add_int.rbc"),
    );
    compile_fixture(
        "../../fixtures/206_bytecode_call_host_continuation_roundtrip_ok.rusk",
        &options,
        out_dir.join("call_host_continuation_roundtrip.rbc"),
    );
    compile_fixture(
        "../../fixtures/207_bytecode_host_tail_resume_pinned_continuation_ok.rusk",
        &options,
        out_dir.join("host_tail_resume_pinned_continuation.rbc"),
    );
    compile_fixture(
        "../../fixtures/210_bytecode_external_effect_add_int_ok.rusk",
        &options,
        out_dir.join("effect_add_int.rbc"),
    );
    compile_fixture(
        "../../fixtures/211_bytecode_external_effect_echo_string_ok.rusk",
        &options,
        out_dir.join("effect_echo_string.rbc"),
    );
    compile_fixture(
        "../../fixtures/213_bytecode_external_effect_echo_bytes_ok.rusk",
        &options,
        out_dir.join("effect_echo_bytes_and_bytes_eq.rbc"),
    );
    compile_fixture(
        "../../fixtures/256_typerep_never_typearg_runtime_error.rusk",
        &options,
        out_dir.join("panic_never_typerep.rbc"),
    );
}

fn compile_fixture(entry_path: &str, options: &CompileOptions, out_path: PathBuf) {
    let entry_path = PathBuf::from(entry_path);
    let module = rusk_compiler::compile_file_to_bytecode_with_options(&entry_path, options)
        .unwrap_or_else(|e| panic!("compile `{}`: {e}", entry_path.display()));
    let bytes = rusk_bytecode::to_bytes(&module)
        .unwrap_or_else(|e| panic!("encode `{}`: {e}", entry_path.display()));
    std::fs::write(&out_path, bytes)
        .unwrap_or_else(|e| panic!("write `{}`: {e}", out_path.display()));
}
