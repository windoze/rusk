use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

#[test]
fn compiles_and_runs_a_minimal_program() {
    let src = r#"
        fn main() -> unit {
            ()
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Unit
        }
    );
}
