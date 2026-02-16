use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options_and_metrics};

#[test]
fn compiler_metrics_exist_and_are_consistent() {
    let src = r#"
fn main() {
    let x = 1;
    x;
}
"#;

    let options = CompileOptions::default();
    let (_module, metrics) =
        compile_to_bytecode_with_options_and_metrics(src, &options).expect("compile");

    // These are timing measurements; they may be zero on fast machines / coarse timers, but the
    // total should always be >= the sum of the staged timers we record.
    assert!(metrics.total_time >= metrics.parse_time);
    assert!(metrics.total_time >= metrics.typecheck_time);
    assert!(metrics.total_time >= metrics.lower_time);
    assert!(metrics.total_time >= metrics.parse_time + metrics.typecheck_time + metrics.lower_time);
}
