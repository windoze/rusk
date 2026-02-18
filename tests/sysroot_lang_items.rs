use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
    compile_to_bytecode_with_options,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

fn create_temp_dir(prefix: &str) -> PathBuf {
    let base = std::env::temp_dir();
    for attempt in 0..100 {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time is after unix epoch")
            .as_nanos();
        let dir = base.join(format!("{prefix}_{nanos}_{attempt}"));
        if fs::create_dir(&dir).is_ok() {
            return dir;
        }
    }
    panic!(
        "failed to create a unique temp dir under `{}`",
        base.display()
    );
}

fn write_file(path: &Path, contents: &str) {
    if let Some(dir) = path.parent() {
        fs::create_dir_all(dir).unwrap_or_else(|e| panic!("create dir {}: {e}", dir.display()));
    }
    fs::write(path, contents).unwrap_or_else(|e| panic!("write {}: {e}", path.display()));
}

fn std_host_module() -> HostModuleDecl {
    HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "print".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::String],
                    ret: HostType::Unit,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "println".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::String],
                    ret: HostType::Unit,
                },
            },
        ],
    }
}

#[test]
fn std_sysroot_is_loaded_when_std_host_is_declared() {
    let mut options = CompileOptions::default();
    options
        .register_host_module("_std_host", std_host_module())
        .unwrap();

    let module = compile_to_bytecode_with_options(
        r#"
fn main() {
    std::println("hi");
    ()
}
"#,
        &options,
    )
    .unwrap();

    assert!(
        module.host_import_id("_std_host::println").is_some(),
        "expected `_std_host::println` host import to exist"
    );
}

#[test]
fn sysroot_validation_rejects_bad_iterator_next_signature() {
    let dir = create_temp_dir("rusk_sysroot_bad_iter");

    write_file(
        &dir.join("core/mod.rusk"),
        r#"
pub mod prelude;
pub mod iter;
"#,
    );
    write_file(&dir.join("core/prelude.rusk"), "");
    write_file(
        &dir.join("core/iter/mod.rusk"),
        r#"
pub interface Iterator {
    type Item;
    fn next() -> Self::Item;
}
"#,
    );

    let options = CompileOptions {
        sysroot: Some(dir.clone()),
        ..Default::default()
    };

    let err = compile_to_bytecode_with_options("fn main() { () }\n", &options).unwrap_err();
    assert!(
        err.message
            .contains("`core::iter::Iterator::next` must return `Option<Self::Item>`"),
        "unexpected error: {err}"
    );

    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn sysroot_validation_rejects_non_readonly_core_ops_add() {
    let dir = create_temp_dir("rusk_sysroot_bad_ops");

    write_file(
        &dir.join("core/mod.rusk"),
        r#"
pub mod prelude;
pub mod iter;
pub mod fmt;
pub mod ops;
"#,
    );
    write_file(&dir.join("core/prelude.rusk"), "");
    write_file(
        &dir.join("core/iter/mod.rusk"),
        r#"
pub interface Iterator {
    type Item;
    fn next() -> Option<Self::Item>;
}
"#,
    );
    write_file(
        &dir.join("core/fmt.rusk"),
        r#"
pub interface ToString {
    readonly fn to_string() -> string;
}
"#,
    );
    write_file(
        &dir.join("core/ops.rusk"),
        r#"
pub interface Add { fn add(other: Self) -> Self; }

pub interface Sub { readonly fn sub(other: Self) -> Self; }
pub interface Mul { readonly fn mul(other: Self) -> Self; }
pub interface Div { readonly fn div(other: Self) -> Self; }
pub interface Rem { readonly fn rem(other: Self) -> Self; }

pub interface Neg { readonly fn neg() -> Self; }
pub interface Not { readonly fn not() -> Self; }

pub interface Lt { readonly fn lt(other: Self) -> bool; }
pub interface Gt { readonly fn gt(other: Self) -> bool; }
pub interface Le { readonly fn le(other: Self) -> bool; }
pub interface Ge { readonly fn ge(other: Self) -> bool; }

pub interface Eq { readonly fn eq(other: Self) -> bool; }
pub interface Ne { readonly fn ne(other: Self) -> bool; }
"#,
    );

    let options = CompileOptions {
        sysroot: Some(dir.clone()),
        ..Default::default()
    };

    let err = compile_to_bytecode_with_options("fn main() { () }\n", &options).unwrap_err();
    assert!(
        err.message
            .contains("`core::ops::Add::add` must be `readonly`"),
        "unexpected error: {err}"
    );

    let _ = fs::remove_dir_all(&dir);
}
