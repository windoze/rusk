use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};
use rusk_vm::{
    AbiValue, EffectDispatchTable, StepResult, Vm, vm_resume, vm_step, vm_step_with_effects,
};

fn run_to_completion(vm: &mut Vm) -> Result<AbiValue, String> {
    loop {
        match vm_step(vm, None) {
            StepResult::Done { value } => return Ok(value),
            StepResult::Trap { message } => return Err(message),
            StepResult::Yield { .. } => continue,
            StepResult::Request { effect_id, .. } => {
                return Err(format!(
                    "unexpected external effect request: id={}",
                    effect_id.0
                ));
            }
        }
    }
}

#[test]
fn typed_host_import_roundtrip() {
    let src = r#"
mod host {
    pub extern fn add(a: int, b: int) -> int;
}

fn main() -> int {
    host::add(40, 2)
}
"#;

    let options = CompileOptions::default();
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let id = module
        .host_import_id("host::add")
        .expect("host::add import");
    vm.register_host_import_typed(id, |(a, b): (i64, i64)| Ok(a + b))
        .expect("register host::add");

    let got = run_to_completion(&mut vm).expect("run");
    assert_eq!(got, AbiValue::Int(42));
}

#[test]
fn register_host_import_typed_rejects_signature_mismatch() {
    let src = r#"
mod host {
    pub extern fn add(a: int, b: int) -> int;
}

fn main() -> int {
    host::add(1, 2)
}
"#;

    let options = CompileOptions::default();
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");
    let mut vm = Vm::new(module.clone()).expect("vm init");
    let id = module
        .host_import_id("host::add")
        .expect("host::add import");

    let err = vm
        .register_host_import_typed::<(bool, bool), bool>(id, |(a, b)| Ok(a && b))
        .unwrap_err();

    match err {
        rusk_vm::VmError::InvalidState { message } => {
            assert!(message.contains("signature mismatch"));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn effect_dispatch_table_one_step_lookup() {
    let src = r#"
interface TestFfi { fn add(a: int, b: int) -> int; }

fn main() -> int {
    @TestFfi.add(1, 2)
}
"#;

    let mut options = CompileOptions::default();
    options
        .register_external_effect_typed::<(i64, i64), i64>("TestFfi", "add")
        .unwrap();

    let module = compile_to_bytecode_with_options(src, &options).expect("compile");
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let mut effects = EffectDispatchTable::new(&module);
    effects
        .register_typed::<(i64, i64), i64>(&module, "TestFfi", "add", |(a, b)| Ok(a + b))
        .unwrap();

    let got = match vm_step(&mut vm, None) {
        StepResult::Request { effect_id, args, k } => {
            let resume_value = vm
                .with_host_context(|cx| effects.dispatch(cx, effect_id, &args))
                .expect("dispatch");
            vm_resume(&mut vm, k, resume_value).expect("resume");
            run_to_completion(&mut vm).expect("run")
        }
        other => panic!("expected Request, got {other:?}"),
    };

    assert_eq!(got, AbiValue::Int(3));
}

#[test]
fn vm_step_with_effects_handles_requests() {
    let src = r#"
interface TestFfi { fn add(a: int, b: int) -> int; }

fn main() -> int {
    @TestFfi.add(20, 22)
}
"#;

    let mut options = CompileOptions::default();
    options
        .register_external_effect_typed::<(i64, i64), i64>("TestFfi", "add")
        .unwrap();

    let module = compile_to_bytecode_with_options(src, &options).expect("compile");
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let mut effects = EffectDispatchTable::new(&module);
    effects
        .register_typed::<(i64, i64), i64>(&module, "TestFfi", "add", |(a, b)| Ok(a + b))
        .unwrap();

    let step = vm_step_with_effects(&mut vm, None, &mut effects).expect("step with effects");
    match step {
        StepResult::Done { value } => assert_eq!(value, AbiValue::Int(42)),
        other => panic!("expected Done, got {other:?}"),
    }
}
