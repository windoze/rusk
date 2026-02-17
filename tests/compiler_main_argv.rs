use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

#[test]
fn main_can_receive_argv_from_host() {
    let src = r#"
        fn main(argv: [string]) -> string {
            argv[1]
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let argv = vec![String::new(), "hello".to_string()];
    let mut vm = Vm::new_with_argv(module, argv).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::String("hello".to_string())
        }
    );
}

#[test]
fn main_argv_must_be_string_array() {
    let src = r#"
        fn main(argv: [int]) -> unit {
            ()
        }
    "#;

    let err = compile_to_bytecode(src).expect_err("expected compile error");
    assert!(
        err.message
            .contains("entry function `main` parameter must be `[string]`"),
        "{err}"
    );
}
