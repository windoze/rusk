use rusk_bytecode::{AbiType, HostFnSig};
use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};
use rusk_vm::{StepResult, Vm, vm_step};

#[test]
fn extern_fn_declaration_records_host_import_signature() {
    let src = r#"
mod host {
    pub extern fn println(s: string) -> unit;
}

fn main() -> unit {
    host::println("hi");
    ()
}
"#;

    let options = CompileOptions {
        load_std: false,
        ..Default::default()
    };
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");

    let import_id = module
        .host_import_id("host::println")
        .expect("host::println");
    let sig = &module.host_import(import_id).expect("host import").sig;
    assert_eq!(
        sig,
        &HostFnSig {
            params: vec![AbiType::String],
            ret: AbiType::Unit,
        }
    );
}

#[test]
fn extern_fn_in_nested_modules_uses_full_path_name() {
    let src = r#"
mod wasi {
    pub mod io {
        pub extern fn read(path: string) -> string;
    }
}

fn main() -> string {
    wasi::io::read("x")
}
"#;

    let options = CompileOptions {
        load_std: false,
        ..Default::default()
    };
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");

    let import_id = module
        .host_import_id("wasi::io::read")
        .expect("wasi::io::read");
    let sig = &module.host_import(import_id).expect("host import").sig;
    assert_eq!(
        sig,
        &HostFnSig {
            params: vec![AbiType::String],
            ret: AbiType::String,
        }
    );
}

#[test]
fn vm_traps_if_declared_host_import_missing() {
    let src = r#"
mod host {
    pub extern fn println(s: string) -> unit;
}

fn main() -> unit {
    host::println("hi");
    ()
}
"#;

    let options = CompileOptions {
        load_std: false,
        ..Default::default()
    };
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");

    let mut vm = Vm::new(module).expect("vm init");
    let got = vm_step(&mut vm, None);
    let StepResult::Trap { message } = got else {
        panic!("expected trap, got {got:?}");
    };
    assert!(
        message.contains("missing host import implementation") && message.contains("host::println"),
        "{message}"
    );
}

#[test]
fn extern_fn_visibility_is_enforced() {
    let src = r#"
mod host {
    extern fn secret(s: string) -> unit;

    pub fn ok() -> unit {
        secret("hi");
        ()
    }
}

fn main() -> unit {
    host::ok();
    host::secret("no");
    ()
}
"#;

    let options = CompileOptions {
        load_std: false,
        ..Default::default()
    };
    let err = compile_to_bytecode_with_options(src, &options).unwrap_err();
    assert!(err.message.contains("`secret` is private"), "{err}");
}
