use rusk::compiler::compile_to_mir;
use rusk::corelib::register_core_host_fns;
use rusk::{Interpreter, Value};

#[test]
fn for_loop_iterates_using_iterator_protocol() {
    let src = r#"
        fn sum() -> int {
            let total = 0;
            let xs = [1, 2, 3];
            for x in xs {
                total = total + x;
            };
            total
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);

    let out = interp.run_function("sum", vec![]).expect("run");
    assert_eq!(out, Value::Int(6));
}

#[test]
fn for_loop_over_readonly_array_is_allowed() {
    let src = r#"
        fn sum() -> int {
            let total = 0;
            readonly xs = [1, 2, 3];
            for x in xs {
                total = total + x;
            };
            total
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);

    let out = interp.run_function("sum", vec![]).expect("run");
    assert_eq!(out, Value::Int(6));
}
