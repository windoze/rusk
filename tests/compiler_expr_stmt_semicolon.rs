use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

#[test]
fn block_like_expression_statements_can_omit_trailing_semicolon() {
    let src = r#"
        fn main() -> int {
            let x = 0;

            if true { x = 1; } else { x = 2; }

            match x {
                1 => { x = 10; },
                _ => { x = 20; },
            }

            { x = 11; }

            x
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(11)
        }
    );
}

#[test]
fn let_statement_still_requires_a_semicolon_even_with_block_initializer() {
    let src = r#"
        fn main() -> unit {
            let x = match 1 {
                1 => 1,
                _ => 2,
            }
            ()
        }
    "#;

    let err = compile_to_bytecode(src).expect_err("missing semicolon after `let` should fail");
    assert!(
        err.message.contains("expected Semi"),
        "unexpected error message: {err:?}"
    );
}
