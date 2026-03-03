use rusk_compiler::source_map::SourceName;
use rusk_compiler::tooling::diagnostics::render_human;
use rusk_compiler::tooling::formatter::{FormatOptions, format_source};
use std::fs;
use std::path::Path;

#[test]
fn formatter_golden_and_idempotent() {
    let input_path = Path::new("tests/fixtures/tooling/fmt/ugly.rusk");
    let expected_path = Path::new("tests/fixtures/tooling/fmt/ugly.formatted.rusk");

    let input = fs::read_to_string(input_path).expect("read input fixture");
    let expected = fs::read_to_string(expected_path).expect("read expected fixture");

    let options = FormatOptions::default();
    let out = format_source(
        SourceName::Virtual("<fixture>".to_string()),
        &input,
        &options,
    );
    assert!(
        out.diagnostics.is_empty(),
        "unexpected diagnostics:\n{}",
        out.diagnostics
            .iter()
            .map(|d| render_human(d, &out.source_map))
            .collect::<Vec<_>>()
            .join("\n")
    );
    let formatted = out.formatted.expect("formatter output");
    assert_eq!(formatted, expected);

    // Roundtrip: formatting already formatted output should be a no-op.
    let out2 = format_source(
        SourceName::Virtual("<fixture>".to_string()),
        &formatted,
        &options,
    );
    assert!(
        out2.diagnostics.is_empty(),
        "unexpected diagnostics on roundtrip"
    );
    assert_eq!(out2.formatted.expect("formatted output"), formatted);
}

#[test]
fn formatter_reports_parse_errors_with_snippet() {
    let src = "fn main() -> int {\n#\n}\n";
    let options = FormatOptions::default();
    let out = format_source(SourceName::Virtual("<string>".to_string()), src, &options);
    assert!(out.formatted.is_none(), "expected formatting to fail");
    assert_eq!(out.diagnostics.len(), 1, "expected a single diagnostic");
    let rendered = render_human(&out.diagnostics[0], &out.source_map);
    assert!(
        rendered.contains("error[parse_error]"),
        "rendered: {rendered}"
    );
    assert!(
        rendered.contains("--> <string>:2:1"),
        "rendered: {rendered}"
    );
    assert!(rendered.contains("^"), "rendered: {rendered}");
}
