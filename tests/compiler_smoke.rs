use rusk::compiler::compile_to_mir;
use rusk::{Interpreter, Value};

#[test]
fn compiles_and_runs_a_minimal_program() {
    let src = r#"
        fn main() -> unit {
            ()
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).expect("run");
    assert_eq!(out, Value::Unit);
}
