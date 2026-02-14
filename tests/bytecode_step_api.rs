use rusk_compiler::{CompileOptions, HostFnSig, HostType, compile_to_bytecode_with_options};
use rusk_vm::{AbiValue, StepResult, Vm, vm_drop_continuation, vm_resume, vm_step};

fn options_externalize_testffi_add() -> CompileOptions {
    let mut options = CompileOptions::default();
    options
        .register_external_effect(
            "TestFfi",
            "add",
            HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int,
            },
        )
        .unwrap();
    options
}

fn compile_add_program() -> rusk_bytecode::ExecutableModule {
    let src = r#"
        interface TestFfi { fn add(a: int, b: int) -> int; }

        fn main() -> int {
            @TestFfi.add(1, 2)
        }
    "#;
    compile_to_bytecode_with_options(src, &options_externalize_testffi_add()).expect("compile")
}

#[test]
fn unhandled_externalized_effect_returns_request() {
    let module = compile_add_program();
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { effect_id, args, .. } = got else {
        panic!("expected request, got {got:?}");
    };

    let decl = module.external_effect(effect_id).expect("effect decl");
    assert_eq!(decl.interface, "TestFfi");
    assert_eq!(decl.method, "add");
    assert_eq!(args, vec![AbiValue::Int(1), AbiValue::Int(2)]);
}

#[test]
fn resume_continues_after_request() {
    let module = compile_add_program();
    let mut vm = Vm::new(module).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { k, .. } = got else {
        panic!("expected request, got {got:?}");
    };

    vm_resume(&mut vm, k, AbiValue::Int(3)).expect("resume");
    assert_eq!(
        vm_step(&mut vm, None),
        StepResult::Done {
            value: AbiValue::Int(3)
        }
    );
}

#[test]
fn step_while_suspended_traps() {
    let module = compile_add_program();
    let mut vm = Vm::new(module).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { .. } = got else {
        panic!("expected request, got {got:?}");
    };

    let got = vm_step(&mut vm, None);
    let StepResult::Trap { message } = got else {
        panic!("expected trap, got {got:?}");
    };
    assert!(message.contains("suspended"), "{message}");
}

#[test]
fn resume_wrong_handle_fails() {
    let module = compile_add_program();
    let mut vm = Vm::new(module).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { k, .. } = got else {
        panic!("expected request, got {got:?}");
    };

    let wrong = rusk_vm::ContinuationHandle {
        index: k.index,
        generation: k.generation.wrapping_add(1),
    };
    let err = vm_resume(&mut vm, wrong, AbiValue::Int(3)).expect_err("expected error");
    assert!(err.to_string().contains("continuation"), "{err}");
}

#[test]
fn resume_twice_fails() {
    let module = compile_add_program();
    let mut vm = Vm::new(module).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { k, .. } = got else {
        panic!("expected request, got {got:?}");
    };

    vm_resume(&mut vm, k.clone(), AbiValue::Int(3)).expect("resume");
    let err = vm_resume(&mut vm, k, AbiValue::Int(3)).expect_err("expected error");
    assert!(err.to_string().contains("not suspended"), "{err}");
}

#[test]
fn drop_cancels_vm() {
    let module = compile_add_program();
    let mut vm = Vm::new(module).expect("vm init");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { k, .. } = got else {
        panic!("expected request, got {got:?}");
    };

    vm_drop_continuation(&mut vm, k).expect("drop");
    let got = vm_step(&mut vm, None);
    let StepResult::Trap { message } = got else {
        panic!("expected trap, got {got:?}");
    };
    assert!(message.contains("cancelled"), "{message}");
}

