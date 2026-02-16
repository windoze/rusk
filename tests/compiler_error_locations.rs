use rusk_compiler::{compile_file_to_bytecode, compile_to_bytecode};
use std::fs;
use std::path::PathBuf;
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

#[test]
fn compile_to_bytecode_renders_string_locations_with_line_and_column() {
    let src = "fn main() {\n#\n}\n";
    let err = compile_to_bytecode(src).expect_err("expected a compile error");

    assert_eq!(
        err.rendered_location.as_deref(),
        Some("<string>: <2:1> - <2:2>")
    );

    let display = err.to_string();
    assert!(
        display.starts_with("<string>: <2:1> - <2:2>: "),
        "unexpected display: {display}"
    );
    assert!(
        display.contains("unexpected character `#`"),
        "unexpected display: {display}"
    );
}

#[test]
fn compile_file_to_bytecode_renders_module_file_locations() {
    let dir = create_temp_dir("rusk_error_loc");
    let main_path = dir.join("main.rusk");
    let foo_path = dir.join("foo.rusk");

    fs::write(&main_path, "mod foo;\nfn main() { () }\n").expect("write main");
    fs::write(&foo_path, "#\n").expect("write foo");

    let err = compile_file_to_bytecode(&main_path).expect_err("expected a compile error");
    let display = err.to_string();

    assert!(
        err.rendered_location
            .as_deref()
            .is_some_and(|loc| loc.contains("foo.rusk: <1:1> - <1:2>")),
        "unexpected rendered_location: {:?}",
        err.rendered_location
    );
    assert!(
        display.contains("foo.rusk: <1:1> - <1:2>:"),
        "unexpected display: {display}"
    );
    assert!(
        display.contains("unexpected character `#`"),
        "unexpected display: {display}"
    );

    let _ = fs::remove_dir_all(&dir);
}
