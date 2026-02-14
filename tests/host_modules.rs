use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
    compile_file_to_mir_with_options, compile_to_mir, compile_to_mir_with_options,
};
use rusk_interpreter::{Interpreter, RuntimeError, Value};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};

fn std_host_module() -> HostModuleDecl {
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![HostFunctionDecl {
            visibility: HostVisibility::Public,
            name: "println".to_string(),
            sig: HostFnSig {
                params: vec![HostType::String],
                ret: HostType::Unit,
            },
        }],
    }
}

#[test]
fn std_is_not_built_in_without_host_registration() {
    let err = compile_to_mir(
        r#"
fn main() {
    std::println("hi");
    ()
}
"#,
    )
    .unwrap_err();
    assert!(
        err.message.contains("unknown value `std::println`"),
        "{err}"
    );
}

#[test]
fn registering_nested_host_module_is_rejected() {
    let mut options = CompileOptions::default();
    let err = options
        .register_host_module("wasi::io", std_host_module())
        .unwrap_err();
    assert!(
        err.contains("nested host modules are not supported"),
        "{err}"
    );
}

#[test]
fn compiles_with_host_module_and_records_host_imports() {
    let mut options = CompileOptions::default();
    options
        .register_host_module("std", std_host_module())
        .unwrap();

    let module = compile_to_mir_with_options(
        r#"
fn main() {
    std::println("hi");
    ()
}
"#,
        &options,
    )
    .unwrap();

    let import_id = module.host_import_id("std::println").unwrap();
    let sig = &module.host_import(import_id).unwrap().sig;
    assert_eq!(
        sig,
        &HostFnSig {
            params: vec![HostType::String],
            ret: HostType::Unit,
        }
    );
}

#[test]
fn interpreter_fails_fast_if_declared_host_missing() {
    let mut options = CompileOptions::default();
    options
        .register_host_module("std", std_host_module())
        .unwrap();

    let module = compile_to_mir_with_options(
        r#"
fn main() {
    std::println("hi");
    ()
}
"#,
        &options,
    )
    .unwrap();

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::MissingHostFunctions {
            names: vec!["std::println".to_string()]
        }
    );
}

#[test]
fn nested_modules_must_use_crate_prefix_for_host_modules() {
    let mut options = CompileOptions::default();
    options
        .register_host_module("std", std_host_module())
        .unwrap();

    // `std::...` is not in scope inside `mod m` unless you use `crate::std` or import it.
    let err = compile_to_mir_with_options(
        r#"
mod m {
    pub fn f() {
        std::println("hi");
        ()
    }
}

fn main() {
    m::f();
    ()
}
"#,
        &options,
    )
    .unwrap_err();
    assert!(
        err.message.contains("unknown value `std::println`"),
        "{err}"
    );

    // `crate::std::...` resolves.
    compile_to_mir_with_options(
        r#"
mod m {
    pub fn f() {
        crate::std::println("hi");
        ()
    }
}

fn main() {
    m::f();
    ()
}
"#,
        &options,
    )
    .unwrap();
}

#[test]
fn host_module_conflicts_with_source_module_inline() {
    let mut options = CompileOptions::default();
    options
        .register_host_module("std", std_host_module())
        .unwrap();

    let err = compile_to_mir_with_options(
        r#"
mod std {}
fn main() { () }
"#,
        &options,
    )
    .unwrap_err();
    assert!(
        err.message
            .contains("host module `std` conflicts with an existing source module"),
        "{err}"
    );
}

#[test]
fn host_module_conflicts_with_source_module_file() {
    let ts = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let mut dir = std::env::temp_dir();
    dir.push(format!("rusk_host_module_test_{ts}"));
    fs::create_dir_all(&dir).unwrap();

    let main_path = dir.join("main.rusk");
    let std_path = dir.join("std.rusk");

    fs::write(
        &main_path,
        r#"
mod std;
fn main() { () }
"#,
    )
    .unwrap();
    fs::write(&std_path, "pub fn dummy() { () }\n").unwrap();

    let mut options = CompileOptions::default();
    options
        .register_host_module("std", std_host_module())
        .unwrap();

    let err = compile_file_to_mir_with_options(&main_path, &options).unwrap_err();
    assert!(
        err.message
            .contains("host module `std` conflicts with an existing source module"),
        "{err}"
    );
}

#[test]
fn host_function_visibility_is_enforced() {
    let mut options = CompileOptions::default();
    options
        .register_host_module(
            "std",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![HostFunctionDecl {
                    visibility: HostVisibility::Private,
                    name: "print".to_string(),
                    sig: HostFnSig {
                        params: vec![HostType::String],
                        ret: HostType::Unit,
                    },
                }],
            },
        )
        .unwrap();

    let err = compile_to_mir_with_options(
        r#"
fn main() {
    std::print("hi");
    ()
}
"#,
        &options,
    )
    .unwrap_err();
    assert!(err.message.contains("`print` is private"), "{err}");
}

#[test]
fn wrapper_modules_can_reexport_flat_host_modules_into_nested_namespaces() {
    let mut options = CompileOptions::default();
    options
        .register_host_module(
            "_wasi_io_host",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![HostFunctionDecl {
                    visibility: HostVisibility::Public,
                    name: "read".to_string(),
                    sig: HostFnSig {
                        params: vec![HostType::String],
                        ret: HostType::String,
                    },
                }],
            },
        )
        .unwrap();

    compile_to_mir_with_options(
        r#"
mod wasi {
    pub mod io {
        pub use crate::_wasi_io_host::read;
    }
}

fn main() -> string {
    wasi::io::read("x")
}
"#,
        &options,
    )
    .unwrap();
}

#[test]
fn interpreter_lists_registered_host_functions() {
    let mut interp = Interpreter::new(rusk_mir::Module::default());
    interp.register_host_fn("b", |_interp, _args| Ok(Value::Unit));
    interp.register_host_fn("a", |_interp, _args| Ok(Value::Unit));

    let names: Vec<&str> = interp.host_function_names().collect();
    assert_eq!(names, vec!["a", "b"]);
}
