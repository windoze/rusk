mod common;

use rusk_compiler::{
    CompileOptions, compile_file_to_bytecode_with_options, compile_to_bytecode_with_options,
};
use rusk_vm::{AbiValue, StepResult, Vm, vm_drop_continuation, vm_resume, vm_step};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
enum Expectation {
    Ok(AbiValue),
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

fn parse_expected_value(spec: &str, path: &Path) -> AbiValue {
    let spec = spec.trim();
    if spec == "unit" {
        return AbiValue::Unit;
    }

    if let Some(rest) = spec.strip_prefix("bool") {
        let v = rest.trim();
        return match v {
            "true" => AbiValue::Bool(true),
            "false" => AbiValue::Bool(false),
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
        return AbiValue::Int(n);
    }

    if let Some(rest) = spec.strip_prefix("string") {
        let mut s = rest.trim();
        if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
            s = &s[1..s.len() - 1];
        }
        return AbiValue::String(s.to_string());
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

fn is_bytecode_fixture(path: &Path) -> bool {
    let _ = path;
    true
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
            if path.extension().and_then(|s| s.to_str()) == Some("rusk")
                && is_bytecode_fixture(&path)
            {
                out.push(FixtureCase {
                    path: path.clone(),
                    kind: FixtureKind::SingleFile(path),
                });
            }
            continue;
        }

        if path.is_dir() {
            let main = path.join("main.rusk");
            if main.is_file() && is_bytecode_fixture(&path) {
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
fn bytecode_fixtures() {
    let fixture_dir = Path::new("fixtures");
    let cases = discover_fixtures(fixture_dir);
    if cases.is_empty() {
        panic!(
            "no bytecode fixtures found under `{}`",
            fixture_dir.display()
        );
    }

    let mut options = CompileOptions::default();
    common::register_test_host_module(&mut options);
    common::register_test_external_effects(&mut options);

    for case in cases {
        let (entry_path, source, module) = match &case.kind {
            FixtureKind::SingleFile(path) => {
                let source = fs::read_to_string(path)
                    .unwrap_or_else(|e| panic!("fixture {}: read failed: {e}", path.display()));
                let module = compile_to_bytecode_with_options(&source, &options);
                (path.as_path(), source, module)
            }
            FixtureKind::DirMain(main_path) => {
                let source = fs::read_to_string(main_path).unwrap_or_else(|e| {
                    panic!("fixture {}: read failed: {e}", main_path.display())
                });
                let module = compile_file_to_bytecode_with_options(main_path, &options);
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
                let mut vm = Vm::new(module.clone()).unwrap_or_else(|e| {
                    panic!("fixture {}: vm init failed: {e}", entry_path.display())
                });
                common::install_test_host_fns_vm(&module, &mut vm);
                common::install_core_host_fns_vm(&module, &mut vm);
                let got = run_to_completion(&module, &mut vm).unwrap_or_else(|e| {
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
                let mut vm = Vm::new(module.clone()).unwrap_or_else(|e| {
                    panic!("fixture {}: vm init failed: {e}", entry_path.display())
                });
                common::install_test_host_fns_vm(&module, &mut vm);
                common::install_core_host_fns_vm(&module, &mut vm);
                let err = run_to_completion(&module, &mut vm).expect_err("expected trap");
                let message = err;
                assert!(
                    message.contains(&contains),
                    "fixture {}: runtime error mismatch\n  want message containing: {contains:?}\n  got: {message}",
                    entry_path.display()
                );
            }
        }
    }
}

fn run_to_completion(
    module: &rusk_bytecode::ExecutableModule,
    vm: &mut Vm,
) -> Result<AbiValue, String> {
    loop {
        match vm_step(vm, None) {
            StepResult::Done { value } => return Ok(value),
            StepResult::Trap { message } => return Err(message),
            StepResult::Yield { .. } => return Err("unexpected yield".to_string()),
            StepResult::Request { effect_id, args, k } => {
                let Some(decl) = module.external_effect(effect_id) else {
                    let _ = vm_drop_continuation(vm, k);
                    return Err(format!("unknown external effect id {}", effect_id.0));
                };

                let resume_value = match (decl.interface.as_str(), decl.method.as_str()) {
                    ("TestFfi", "add") => match args.as_slice() {
                        [AbiValue::Int(a), AbiValue::Int(b)] => AbiValue::Int(a + b),
                        other => {
                            let _ = vm_drop_continuation(vm, k);
                            return Err(format!("TestFfi.add: bad args: {other:?}"));
                        }
                    },
                    ("TestFfi", "echo") => match args.as_slice() {
                        [AbiValue::String(s)] => AbiValue::String(s.clone()),
                        other => {
                            let _ = vm_drop_continuation(vm, k);
                            return Err(format!("TestFfi.echo: bad args: {other:?}"));
                        }
                    },
                    ("TestFfi", "echo_bytes") => match args.as_slice() {
                        [AbiValue::Bytes(b)] => AbiValue::Bytes(b.clone()),
                        other => {
                            let _ = vm_drop_continuation(vm, k);
                            return Err(format!("TestFfi.echo_bytes: bad args: {other:?}"));
                        }
                    },
                    other => {
                        let _ = vm_drop_continuation(vm, k);
                        return Err(format!("unhandled external effect: {other:?}"));
                    }
                };

                vm_resume(vm, k, resume_value).map_err(|e| e.to_string())?;
            }
        }
    }
}
