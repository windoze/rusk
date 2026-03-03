use rusk_compiler::analysis::DiagnosticSeverity;
use rusk_compiler::source_map::SourceName;
use rusk_compiler::tooling::linter::{LintOptions, lint_source};
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;

#[test]
fn linter_emits_expected_lints() {
    let path = Path::new("tests/fixtures/tooling/lint/warnings.rusk");
    let src = fs::read_to_string(path).expect("read lint fixture");

    let out = lint_source(
        SourceName::Virtual("<fixture>".to_string()),
        &src,
        &LintOptions::default(),
    );
    assert!(
        !out.diagnostics.is_empty(),
        "expected lints for fixture input"
    );
    assert!(
        out.diagnostics
            .iter()
            .all(|d| d.severity == DiagnosticSeverity::Warning),
        "expected warnings by default"
    );

    let got: BTreeSet<&str> = out
        .diagnostics
        .iter()
        .filter_map(|d| d.code.as_deref())
        .collect();

    for want in [
        "unused_variable",
        "unreachable_code",
        "needless_bool",
        "redundant_else",
        "suspicious_comparison",
    ] {
        assert!(got.contains(want), "missing lint `{want}`; got={got:?}");
    }
}

#[test]
fn linter_deny_warnings_upgrades_severity() {
    let path = Path::new("tests/fixtures/tooling/lint/warnings.rusk");
    let src = fs::read_to_string(path).expect("read lint fixture");

    let out = lint_source(
        SourceName::Virtual("<fixture>".to_string()),
        &src,
        &LintOptions {
            deny_warnings: true,
        },
    );
    assert!(
        out.diagnostics
            .iter()
            .all(|d| d.severity == DiagnosticSeverity::Error),
        "expected all warnings to be upgraded to errors"
    );
}
