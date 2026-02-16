use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

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

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(6)
        }
    );
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

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(6)
        }
    );
}
