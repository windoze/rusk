use rusk_compiler::{compile_file_to_mir, compile_to_mir};
use rusk_interpreter::corelib::register_core_host_fns;
use rusk_interpreter::{Interpreter, Value};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
enum Expectation {
    Ok(Value),
    CompileError { contains: String },
    RuntimeError { contains: String },
}

fn parse_expectation(source: &str, path: &Path) -> Expectation {
    for (line_idx, line) in source.lines().enumerate() {
        // Allow Unix-style shebang on the first line (e.g. `#!/usr/bin/env rusk`).
        if line_idx == 0 && line.starts_with("#!") {
            continue;
        }
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

#[derive(Clone, Debug)]
enum FixtureKind {
    SingleFile(PathBuf),
    DirMain(PathBuf),
}

#[derive(Clone, Debug)]
struct FixtureCase {
    path: PathBuf,
    kind: FixtureKind,
}

fn discover_fixtures(fixture_dir: &Path) -> Vec<FixtureCase> {
    let mut out = Vec::new();
    let entries = fs::read_dir(fixture_dir).unwrap_or_else(|e| {
        panic!(
            "failed to read fixtures dir `{}`: {e}",
            fixture_dir.display()
        )
    });

    for entry in entries {
        let entry = entry.unwrap_or_else(|e| {
            panic!(
                "failed to read fixtures dir entry under `{}`: {e}",
                fixture_dir.display()
            )
        });
        let path = entry.path();
        if path.is_file() {
            if path.extension().and_then(|s| s.to_str()) == Some("rusk") {
                out.push(FixtureCase {
                    path: path.clone(),
                    kind: FixtureKind::SingleFile(path),
                });
            }
            continue;
        }

        if path.is_dir() {
            let main = path.join("main.rusk");
            if main.is_file() {
                out.push(FixtureCase {
                    path: path.clone(),
                    kind: FixtureKind::DirMain(main),
                });
                continue;
            }

            let has_rusk = fs::read_dir(&path).ok().is_some_and(|iter| {
                iter.filter_map(Result::ok).any(|e| {
                    e.path().is_file()
                        && e.path().extension().and_then(|s| s.to_str()) == Some("rusk")
                })
            });
            if has_rusk {
                panic!(
                    "fixture directory `{}` contains `.rusk` files but no `main.rusk` entry",
                    path.display()
                );
            }
        }
    }

    out.sort_by(|a, b| a.path.cmp(&b.path));
    out
}

#[test]
fn fixtures() {
    let fixture_dir = Path::new("fixtures");
    let cases = discover_fixtures(fixture_dir);
    if cases.is_empty() {
        panic!("no fixtures found under `{}`", fixture_dir.display());
    }

    for case in cases {
        let (entry_path, source, module) = match &case.kind {
            FixtureKind::SingleFile(path) => {
                let source = fs::read_to_string(path)
                    .unwrap_or_else(|e| panic!("fixture {}: read failed: {e}", path.display()));
                let module = compile_to_mir(&source);
                (path.as_path(), source, module)
            }
            FixtureKind::DirMain(main_path) => {
                let source = fs::read_to_string(main_path).unwrap_or_else(|e| {
                    panic!("fixture {}: read failed: {e}", main_path.display())
                });
                let module = compile_file_to_mir(main_path);
                (main_path.as_path(), source, module)
            }
        };

        let expect = parse_expectation(&source, entry_path);

        match expect {
            Expectation::CompileError { contains } => {
                let err = module.expect_err("expected compile error");
                assert!(
                    err.message.contains(&contains),
                    "fixture {}: compile error mismatch\n  want message containing: {contains:?}\n  got: {}",
                    entry_path.display(),
                    err.message
                );
            }
            Expectation::Ok(want) => {
                let module = module.unwrap_or_else(|e| {
                    panic!("fixture {}: compile failed: {e}", entry_path.display())
                });
                let mut interp = Interpreter::new(module);
                register_core_host_fns(&mut interp);
                let got = interp.run_function("main", vec![]).unwrap_or_else(|e| {
                    panic!("fixture {}: runtime failed: {e}", entry_path.display())
                });
                assert_eq!(
                    got,
                    want,
                    "fixture {}: result mismatch",
                    entry_path.display()
                );
            }
            Expectation::RuntimeError { contains } => {
                let module = module.unwrap_or_else(|e| {
                    panic!("fixture {}: compile failed: {e}", entry_path.display())
                });
                let mut interp = Interpreter::new(module);
                register_core_host_fns(&mut interp);
                let err = interp
                    .run_function("main", vec![])
                    .expect_err("expected runtime error");
                let msg = err.to_string();
                assert!(
                    msg.contains(&contains),
                    "fixture {}: runtime error mismatch\n  want message containing: {contains:?}\n  got: {msg}",
                    entry_path.display()
                );
            }
        }
    }
}
