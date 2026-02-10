use rusk::compiler::compile_to_mir;
use rusk::stdlib::register_std_host_fns;
use rusk::{Interpreter, Value};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
enum Expectation {
    Ok(Value),
    CompileError { contains: String },
    RuntimeError { contains: String },
}

fn parse_expectation(source: &str, path: &Path) -> Expectation {
    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        let Some(comment) = trimmed.strip_prefix("//") else {
            break;
        };
        let directive = comment.trim();
        let Some(rest) = directive.strip_prefix("expect:") else {
            continue;
        };
        let rest = rest.trim();

        if let Some(value_spec) = rest.strip_prefix("ok") {
            let value_spec = value_spec.trim();
            if value_spec.is_empty() {
                panic!(
                    "fixture {}: `// expect: ok ...` requires a value spec (e.g. `int 1`, `unit`)",
                    path.display()
                );
            }
            return Expectation::Ok(parse_expected_value(value_spec, path));
        }

        if let Some(contains) = rest.strip_prefix("compile_error") {
            let contains = contains.trim();
            if contains.is_empty() {
                panic!(
                    "fixture {}: `// expect: compile_error ...` requires a substring to match",
                    path.display()
                );
            }
            return Expectation::CompileError {
                contains: contains.to_string(),
            };
        }

        if let Some(contains) = rest.strip_prefix("runtime_error") {
            let contains = contains.trim();
            if contains.is_empty() {
                panic!(
                    "fixture {}: `// expect: runtime_error ...` requires a substring to match",
                    path.display()
                );
            }
            return Expectation::RuntimeError {
                contains: contains.to_string(),
            };
        }

        panic!(
            "fixture {}: unknown expectation directive `{rest}`",
            path.display()
        );
    }

    panic!(
        "fixture {}: missing expectation comment; add e.g. `// expect: ok int 1`",
        path.display()
    );
}

fn parse_expected_value(spec: &str, path: &Path) -> Value {
    let spec = spec.trim();
    if spec == "unit" {
        return Value::Unit;
    }

    if let Some(rest) = spec.strip_prefix("bool") {
        let v = rest.trim();
        return match v {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => panic!(
                "fixture {}: invalid bool expectation `{spec}`; use `bool true` or `bool false`",
                path.display()
            ),
        };
    }

    if let Some(rest) = spec.strip_prefix("int") {
        let v = rest.trim();
        let n: i64 = v.parse().unwrap_or_else(|_| {
            panic!(
                "fixture {}: invalid int expectation `{spec}`; use e.g. `int 123`",
                path.display()
            )
        });
        return Value::Int(n);
    }

    if let Some(rest) = spec.strip_prefix("string") {
        let mut s = rest.trim();
        if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
            s = &s[1..s.len() - 1];
        }
        return Value::String(s.to_string());
    }

    panic!(
        "fixture {}: unsupported expected value `{spec}`; supported: unit, bool, int, string",
        path.display()
    );
}

#[test]
fn fixtures() {
    let fixture_dir = Path::new("fixtures");
    let mut files: Vec<PathBuf> = fs::read_dir(fixture_dir)
        .unwrap_or_else(|e| {
            panic!(
                "failed to read fixtures dir `{}`: {e}",
                fixture_dir.display()
            )
        })
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            match path.extension().and_then(|s| s.to_str()) {
                Some("rusk") => Some(path),
                _ => None,
            }
        })
        .collect();

    files.sort();
    if files.is_empty() {
        panic!("no fixtures found under `{}`", fixture_dir.display());
    }

    for path in files {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("fixture {}: read failed: {e}", path.display()));
        let expect = parse_expectation(&source, &path);

        match expect {
            Expectation::CompileError { contains } => {
                let err = compile_to_mir(&source).expect_err("expected compile error");
                assert!(
                    err.message.contains(&contains),
                    "fixture {}: compile error mismatch\n  want message containing: {contains:?}\n  got: {}",
                    path.display(),
                    err.message
                );
            }
            Expectation::Ok(want) => {
                let module = compile_to_mir(&source)
                    .unwrap_or_else(|e| panic!("fixture {}: compile failed: {e}", path.display()));
                let mut interp = Interpreter::new(module);
                register_std_host_fns(&mut interp);
                let got = interp
                    .run_function("main", vec![])
                    .unwrap_or_else(|e| panic!("fixture {}: runtime failed: {e}", path.display()));
                assert_eq!(got, want, "fixture {}: result mismatch", path.display());
            }
            Expectation::RuntimeError { contains } => {
                let module = compile_to_mir(&source)
                    .unwrap_or_else(|e| panic!("fixture {}: compile failed: {e}", path.display()));
                let mut interp = Interpreter::new(module);
                register_std_host_fns(&mut interp);
                let err = interp
                    .run_function("main", vec![])
                    .expect_err("expected runtime error");
                let msg = err.to_string();
                assert!(
                    msg.contains(&contains),
                    "fixture {}: runtime error mismatch\n  want message containing: {contains:?}\n  got: {msg}",
                    path.display()
                );
            }
        }
    }
}
