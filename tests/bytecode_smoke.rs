use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

#[test]
fn bytecode_smoke_runs_simple_int_add() {
    let src = r#"
        fn main() -> int {
            1 + 2
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: AbiValue::Int(3)
        }
    );
}
