use rusk_bytecode::from_bytes;
use rusk_compiler::analysis as compiler_analysis;
use rusk_compiler::tooling::{diagnostics as tooling_diagnostics, formatter, linter};
use rusk_compiler::{CompileOptions, compile_file_to_bytecode_with_options};
use rusk_host::std_io;
use rusk_vm::{StepResult, Vm, vm_step};
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::Arc;

fn main() {
    let mut args = env::args_os().skip(1).peekable();

    if let Some(arg0) = args.peek().map(|a| a.to_string_lossy().into_owned()) {
        match arg0.as_str() {
            "fmt" => {
                let _ = args.next();
                cmd_fmt(args);
                return;
            }
            "lint" => {
                let _ = args.next();
                cmd_lint(args);
                return;
            }
            _ => {}
        }
    }

    let mut sysroot: Option<PathBuf> = None;
    let mut load_std = true;

    while let Some(arg) = args.peek() {
        let s = arg.to_string_lossy();
        match s.as_ref() {
            "--help" | "-h" => {
                eprintln!(
                    "usage:\n  rusk [--sysroot <path>] [--no-std] <file.rusk|file.rbc> [args...]\n  rusk fmt [--check] [--write] [--stdin] [--stdout] <paths...>\n  rusk lint [--deny-warnings] [--json] <paths...>"
                );
                process::exit(0);
            }
            "--sysroot" => {
                let _ = args.next();
                let Some(path) = args.next() else {
                    eprintln!("error: `--sysroot` expects a path argument");
                    process::exit(2);
                };
                sysroot = Some(PathBuf::from(path));
            }
            "--no-std" => {
                let _ = args.next();
                load_std = false;
            }
            "--" => {
                let _ = args.next();
                break;
            }
            _ if s.starts_with("--") => {
                eprintln!("error: unknown option `{s}`");
                process::exit(2);
            }
            _ => break,
        }
    }

    let Some(path) = args.next() else {
        eprintln!("usage: rusk [--sysroot <path>] [--no-std] <file.rusk|file.rbc> [args...]");
        process::exit(2);
    };

    let input_path = PathBuf::from(path);
    let argv0 = absolute_path_string(&input_path);
    let mut argv: Vec<String> = Vec::with_capacity(1);
    argv.push(argv0);
    argv.extend(args.map(|a| a.to_string_lossy().into_owned()));

    let extension = input_path.extension().and_then(|s| s.to_str());

    let module = match extension {
        Some("rbc") => {
            // 直接加载 .rbc 文件
            let bytes = match fs::read(&input_path) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("failed to read file: {e}");
                    process::exit(1);
                }
            };
            match from_bytes(&bytes) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("load error: {e}");
                    process::exit(1);
                }
            }
        }
        Some("rusk") => {
            // 编译 .rusk 文件
            let mut options = CompileOptions {
                sysroot,
                load_std,
                ..Default::default()
            };
            if load_std {
                std_io::register_host_module(&mut options);
            }
            match compile_file_to_bytecode_with_options(&input_path, &options) {
                Ok(m) => m,
                Err(e) => {
                    emit_compile_diagnostics(&input_path, &options, &e);
                    process::exit(1);
                }
            }
        }
        _ => {
            eprintln!("error: input file must have .rusk or .rbc extension");
            process::exit(2);
        }
    };

    let Some(entry_fn) = module.function(module.entry) else {
        eprintln!(
            "vm init error: invalid entry function id {}",
            module.entry.0
        );
        process::exit(1);
    };
    let mut vm = match entry_fn.param_count {
        0 => Vm::new(module.clone()),
        1 => Vm::new_with_argv(module.clone(), argv),
        n => Err(rusk_vm::VmError::InvalidState {
            message: format!("unsupported entry arity: expected 0 or 1 param, got {n}"),
        }),
    }
    .unwrap_or_else(|e| {
        eprintln!("vm init error: {e}");
        process::exit(1);
    });
    std_io::install_vm(&module, &mut vm);

    match vm_step(&mut vm, None) {
        StepResult::Done { value } => {
            if value != rusk_vm::AbiValue::Unit {
                // `main` defaults to returning `unit` (explicitly or via omission), but printing a
                // non-unit return is useful during development.
                println!("{value:?}");
            }
        }
        StepResult::Trap { message } => {
            eprintln!("runtime error: {message}");
            process::exit(1);
        }
        StepResult::Request {
            effect_id, args, ..
        } => {
            let name = module
                .external_effect(effect_id)
                .map(|d| format!("{}.{}", d.interface, d.method))
                .unwrap_or_else(|| format!("<unknown {}>", effect_id.0));
            eprintln!("runtime error: external effect request: {name} args={args:?}");
            process::exit(1);
        }
        StepResult::Yield { .. } => {
            eprintln!("runtime error: unexpected yield");
            process::exit(1);
        }
    }
}

fn absolute_path_string(path: &Path) -> String {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    };
    abs.to_string_lossy().into_owned()
}

fn cmd_fmt(args: impl Iterator<Item = std::ffi::OsString>) {
    let mut check = false;
    let mut write = false;
    let mut stdin = false;
    let mut stdout = false;
    let mut paths: Vec<PathBuf> = Vec::new();

    for arg in args {
        let s = arg.to_string_lossy();
        match s.as_ref() {
            "--help" | "-h" => {
                eprintln!("usage: rusk fmt [--check] [--write] [--stdin] [--stdout] <paths...>");
                process::exit(0);
            }
            "--check" => check = true,
            "--write" => write = true,
            "--stdin" => stdin = true,
            "--stdout" => stdout = true,
            _ if s.starts_with("--") => {
                eprintln!("error: unknown option `{s}`");
                process::exit(2);
            }
            _ => paths.push(PathBuf::from(arg)),
        }
    }

    if stdin && !paths.is_empty() {
        eprintln!("error: `rusk fmt --stdin` does not accept file paths");
        process::exit(2);
    }
    if check && write {
        eprintln!("error: `--check` and `--write` are mutually exclusive");
        process::exit(2);
    }
    if stdout && write {
        eprintln!("error: `--stdout` and `--write` are mutually exclusive");
        process::exit(2);
    }
    if stdout && paths.len() > 1 {
        eprintln!("error: `--stdout` only supports a single input file");
        process::exit(2);
    }

    let default_write = !check && !stdout && !stdin;
    let write = write || default_write;

    let options = formatter::FormatOptions::default();

    if stdin {
        let mut input = String::new();
        if let Err(err) = std::io::stdin().read_to_string(&mut input) {
            eprintln!("error: failed to read stdin: {err}");
            process::exit(1);
        }
        let out = formatter::format_source(
            rusk_compiler::source_map::SourceName::Virtual("<stdin>".to_string()),
            &input,
            &options,
        );
        if !out.diagnostics.is_empty() || out.formatted.is_none() {
            emit_tool_diagnostics(&out.diagnostics, &out.source_map);
            process::exit(1);
        }
        let formatted = out.formatted.unwrap();
        if check && formatted != input {
            process::exit(1);
        }
        print!("{formatted}");
        process::exit(0);
    }

    if paths.is_empty() {
        eprintln!("usage: rusk fmt [--check] [--write] [--stdin] [--stdout] <paths...>");
        process::exit(2);
    }

    let mut files: Vec<PathBuf> = Vec::new();
    for path in &paths {
        collect_rusk_files(path, &mut files);
    }
    if files.is_empty() {
        eprintln!("error: no `.rusk` files found");
        process::exit(2);
    }
    if stdout && files.len() != 1 {
        eprintln!("error: `--stdout` only supports a single input file");
        process::exit(2);
    }

    let mut had_error = false;
    let mut needs_change = false;

    for file in files {
        let src = match fs::read_to_string(&file) {
            Ok(s) => s,
            Err(err) => {
                eprintln!("error: failed to read {}: {err}", file.display());
                had_error = true;
                continue;
            }
        };

        let out = formatter::format_source(
            rusk_compiler::source_map::SourceName::Path(file.clone()),
            &src,
            &options,
        );
        if !out.diagnostics.is_empty() || out.formatted.is_none() {
            emit_tool_diagnostics(&out.diagnostics, &out.source_map);
            had_error = true;
            continue;
        }
        let formatted = out.formatted.unwrap();
        if formatted != src {
            needs_change = true;
            if stdout {
                print!("{formatted}");
            } else if write && let Err(err) = fs::write(&file, formatted) {
                eprintln!("error: failed to write {}: {err}", file.display());
                had_error = true;
            }
        }
    }

    if had_error {
        process::exit(1);
    }
    if check && needs_change {
        process::exit(1);
    }
    process::exit(0);
}

fn cmd_lint(args: impl Iterator<Item = std::ffi::OsString>) {
    let mut deny_warnings = false;
    let mut json = false;
    let mut paths: Vec<PathBuf> = Vec::new();

    for arg in args {
        let s = arg.to_string_lossy();
        match s.as_ref() {
            "--help" | "-h" => {
                eprintln!("usage: rusk lint [--deny-warnings] [--json] <paths...>");
                process::exit(0);
            }
            "--deny-warnings" => deny_warnings = true,
            "--json" => json = true,
            _ if s.starts_with("--") => {
                eprintln!("error: unknown option `{s}`");
                process::exit(2);
            }
            _ => paths.push(PathBuf::from(arg)),
        }
    }

    if paths.is_empty() {
        eprintln!("usage: rusk lint [--deny-warnings] [--json] <paths...>");
        process::exit(2);
    }

    let mut files: Vec<PathBuf> = Vec::new();
    for path in &paths {
        collect_rusk_files(path, &mut files);
    }
    if files.is_empty() {
        eprintln!("error: no `.rusk` files found");
        process::exit(2);
    }

    let options = linter::LintOptions { deny_warnings };

    let mut all_diags: Vec<(
        compiler_analysis::Diagnostic,
        rusk_compiler::source_map::SourceMap,
    )> = Vec::new();
    let mut has_error = false;
    let mut has_warning = false;

    for file in files {
        let out = linter::lint_file(&file, &options);
        for diag in &out.diagnostics {
            match diag.severity {
                compiler_analysis::DiagnosticSeverity::Error => has_error = true,
                compiler_analysis::DiagnosticSeverity::Warning => has_warning = true,
            }
        }
        if json {
            for diag in out.diagnostics {
                all_diags.push((diag, out.source_map.clone()));
            }
        } else {
            emit_tool_diagnostics(&out.diagnostics, &out.source_map);
        }
    }

    if json {
        emit_tool_diagnostics_json(&all_diags);
    }

    if has_error || (deny_warnings && has_warning) {
        process::exit(1);
    }
    process::exit(0);
}

fn collect_rusk_files(path: &Path, out: &mut Vec<PathBuf>) {
    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("rusk") {
            out.push(path.to_path_buf());
        }
        return;
    }
    if !path.is_dir() {
        return;
    }
    let entries = match fs::read_dir(path) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let p = entry.path();
        if p.is_dir() {
            collect_rusk_files(&p, out);
        } else if p.extension().and_then(|s| s.to_str()) == Some("rusk") {
            out.push(p);
        }
    }
    out.sort();
    out.dedup();
}

fn emit_compile_diagnostics(
    path: &Path,
    options: &CompileOptions,
    fallback: &rusk_compiler::CompileError,
) {
    let analysis = compiler_analysis::analyze_entry_file(
        path,
        options,
        Arc::new(rusk_compiler::vfs::FsSourceProvider),
    );
    if analysis.diagnostics.is_empty() {
        eprintln!("compile error: {fallback}");
        return;
    }
    emit_tool_diagnostics(&analysis.diagnostics, &analysis.source_map);
}

fn emit_tool_diagnostics(
    diags: &[compiler_analysis::Diagnostic],
    source_map: &rusk_compiler::source_map::SourceMap,
) {
    for (idx, diag) in diags.iter().enumerate() {
        if idx > 0 {
            eprintln!();
        }
        let rendered = tooling_diagnostics::render_human(diag, source_map);
        eprint!("{rendered}");
    }
}

fn emit_tool_diagnostics_json(
    diags: &[(
        compiler_analysis::Diagnostic,
        rusk_compiler::source_map::SourceMap,
    )],
) {
    use serde_json::json;

    let mut items = Vec::new();
    for (diag, source_map) in diags {
        let range = diag
            .range
            .clone()
            .or_else(|| source_map.lookup_span(diag.span));
        let (file, start, end) = if let Some(r) = range {
            let file = match r.name {
                rusk_compiler::source_map::SourceName::Path(p) => p.display().to_string(),
                rusk_compiler::source_map::SourceName::Virtual(v) => v,
            };
            (file, r.start, r.end)
        } else {
            (
                "<unknown>".to_string(),
                rusk_compiler::source_map::LineCol { line: 0, col: 0 },
                rusk_compiler::source_map::LineCol { line: 0, col: 0 },
            )
        };
        let sev = match diag.severity {
            compiler_analysis::DiagnosticSeverity::Error => "error",
            compiler_analysis::DiagnosticSeverity::Warning => "warning",
        };
        items.push(json!({
            "severity": sev,
            "code": diag.code,
            "message": diag.message,
            "help": diag.help,
            "notes": diag.notes,
            "label": diag.label,
            "location": {
                "file": file,
                "start": { "line": start.line, "col": start.col },
                "end": { "line": end.line, "col": end.col },
            }
        }));
    }

    let payload = json!({
        "schema_version": 1,
        "diagnostics": items
    });
    println!(
        "{}",
        serde_json::to_string_pretty(&payload).unwrap_or_else(|_| payload.to_string())
    );
}
