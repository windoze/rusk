use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn run_to_completion(vm: &mut Vm) -> AbiValue {
    loop {
        match vm_step(vm, None) {
            StepResult::Done { value } => return value,
            StepResult::Trap { message } => panic!("unexpected trap: {message}"),
            StepResult::Request {
                effect_id, args, ..
            } => {
                panic!("unexpected external effect request: {effect_id:?} args={args:?}")
            }
            StepResult::Yield { .. } => continue,
        }
    }
}

#[test]
fn vcall_fast_path_is_used_for_hash_and_eq_on_primitives() {
    let src = r#"
        fn hash_generic<T: core::hash::Hash>(x: T) -> int {
            core::hash::Hash::hash(x)
        }

        fn eq_generic<T: core::ops::Eq>(a: T, b: T) -> bool {
            core::ops::Eq::eq(a, b)
        }

        fn main() -> int {
            let i = 0;
            let sum = 0;
            while i < 1000 {
                if eq_generic(123, 123) {
                    sum = sum + hash_generic(123);
                } else {
                    sum = sum + 0;
                };
                i = i + 1;
            };
            sum - sum
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    vm.enable_metrics(true);
    vm.reset_metrics();

    let result = run_to_completion(&mut vm);
    assert_eq!(result, AbiValue::Int(0));

    let metrics = vm.take_metrics();
    assert!(
        metrics.vcall_instructions > 0,
        "expected at least one vcall, got metrics={metrics:?}"
    );
    assert!(
        metrics.vcall_fast_path_hits > 0,
        "expected vcall fast path hits, got metrics={metrics:?}"
    );
}
