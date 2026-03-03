use std::fs;
use std::path::PathBuf;
use std::process::Command;
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

fn rusk_bin() -> &'static str {
    env!("CARGO_BIN_EXE_rusk")
}

#[test]
fn rusk_fmt_check_then_write() {
    let dir = create_temp_dir("rusk_fmt_cli");
    let file = dir.join("main.rusk");

    let input = fs::read_to_string("tests/fixtures/tooling/fmt/ugly.rusk").expect("read fixture");
    let expected = fs::read_to_string("tests/fixtures/tooling/fmt/ugly.formatted.rusk")
        .expect("read expected");
    fs::write(&file, input).expect("write temp file");

    let out = Command::new(rusk_bin())
        .args(["fmt", "--check"])
        .arg(&file)
        .output()
        .expect("run rusk fmt --check");
    assert!(
        !out.status.success(),
        "expected non-zero exit status for --check"
    );

    let out = Command::new(rusk_bin())
        .args(["fmt"])
        .arg(&file)
        .output()
        .expect("run rusk fmt");
    assert!(
        out.status.success(),
        "stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let got = fs::read_to_string(&file).expect("read formatted file");
    assert_eq!(got, expected);

    let out = Command::new(rusk_bin())
        .args(["fmt", "--check"])
        .arg(&file)
        .output()
        .expect("run rusk fmt --check (post-write)");
    assert!(
        out.status.success(),
        "expected exit 0 after formatting; stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );

    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn rusk_lint_exit_codes_and_json() {
    let dir = create_temp_dir("rusk_lint_cli");
    let file = dir.join("main.rusk");
    let src =
        fs::read_to_string("tests/fixtures/tooling/lint/warnings.rusk").expect("read fixture");
    fs::write(&file, src).expect("write temp file");

    let out = Command::new(rusk_bin())
        .args(["lint"])
        .arg(&file)
        .output()
        .expect("run rusk lint");
    assert!(
        out.status.success(),
        "expected exit 0 for warnings; stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    for want in [
        "warning[unused_variable]",
        "warning[unreachable_code]",
        "warning[needless_bool]",
        "warning[redundant_else]",
        "warning[suspicious_comparison]",
    ] {
        assert!(
            stderr.contains(want),
            "expected lint output to contain `{want}`; got stderr:\n{stderr}"
        );
    }

    let out = Command::new(rusk_bin())
        .args(["lint", "--deny-warnings"])
        .arg(&file)
        .output()
        .expect("run rusk lint --deny-warnings");
    assert!(
        !out.status.success(),
        "expected non-zero exit when denying warnings"
    );

    let out = Command::new(rusk_bin())
        .args(["lint", "--json"])
        .arg(&file)
        .output()
        .expect("run rusk lint --json");
    assert!(
        out.status.success(),
        "expected exit 0 for --json; stderr={}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("\"schema_version\": 1"),
        "unexpected json: {stdout}"
    );
    assert!(
        stdout.contains("\"unused_variable\""),
        "unexpected json: {stdout}"
    );

    let _ = fs::remove_dir_all(&dir);
}

#[test]
fn rusk_compile_errors_use_rich_diagnostics() {
    let dir = create_temp_dir("rusk_diag_cli");
    let file = dir.join("bad.rusk");
    fs::write(&file, "fn main() -> int {\n#\n}\n").expect("write temp file");

    let out = Command::new(rusk_bin())
        .arg(&file)
        .output()
        .expect("run rusk bad.rusk");
    assert!(
        !out.status.success(),
        "expected compile to fail for invalid input"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("error:"), "stderr:\n{stderr}");
    assert!(stderr.contains("bad.rusk:2:1"), "stderr:\n{stderr}");
    assert!(stderr.contains("^"), "stderr:\n{stderr}");

    let _ = fs::remove_dir_all(&dir);
}
