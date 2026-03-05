use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};
use rusk_vm::{AbiValue, HostError, StepResult, Vm, vm_step};
use std::cell::Cell;
use std::rc::Rc;

#[test]
fn generic_specialization_dispatches_to_host_impl_for_exact_type_args() {
    let src = r#"
        mod spec {
            pub extern fn id_int(x: int) -> int;
        }

        fn id<T>(x: T) -> T {
            let _ = core::intrinsics::unit_eq((), ());
            x
        }

        fn _touch() -> int { spec::id_int(0) }

        fn main() -> int { id(123) }
    "#;
    let options = CompileOptions::default();
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");

    let mut vm = Vm::new(module.clone()).expect("vm init");

    let calls = Rc::new(Cell::new(0u32));
    let calls_cell = Rc::clone(&calls);

    let host_id = module
        .host_import_id("spec::id_int")
        .expect("host import id");
    vm.register_host_import_typed(host_id, move |(v,): (i64,)| -> Result<i64, HostError> {
        calls_cell.set(calls_cell.get() + 1);
        Ok(v)
    })
    .expect("register host import");

    let fn_id = module.function_id("id").expect("id function id");
    let int_ty = vm
        .intern_type_rep(&rusk_bytecode::TypeRepLit::Int, &[])
        .expect("intern typerep");
    vm.register_generic_specialization(fn_id, vec![int_ty], host_id)
        .expect("register specialization");

    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: AbiValue::Int(123),
        }
    );
    assert_eq!(calls.get(), 1, "specialized host import should be called");
}

#[test]
fn generic_specialization_falls_back_when_type_args_do_not_match() {
    let src = r#"
        mod spec {
            pub extern fn id_int(x: int) -> int;
        }

        fn id<T>(x: T) -> T {
            let _ = core::intrinsics::unit_eq((), ());
            x
        }

        fn _touch() -> int { spec::id_int(0) }

        fn main() -> string { id("ok") }
    "#;
    let options = CompileOptions::default();
    let module = compile_to_bytecode_with_options(src, &options).expect("compile");

    let mut vm = Vm::new(module.clone()).expect("vm init");

    let calls = Rc::new(Cell::new(0u32));
    let calls_cell = Rc::clone(&calls);

    let host_id = module
        .host_import_id("spec::id_int")
        .expect("host import id");
    vm.register_host_import_typed(host_id, move |(v,): (i64,)| -> Result<i64, HostError> {
        calls_cell.set(calls_cell.get() + 1);
        Ok(v)
    })
    .expect("register host import");

    let fn_id = module.function_id("id").expect("id function id");
    let int_ty = vm
        .intern_type_rep(&rusk_bytecode::TypeRepLit::Int, &[])
        .expect("intern typerep");
    vm.register_generic_specialization(fn_id, vec![int_ty], host_id)
        .expect("register specialization");

    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: AbiValue::String("ok".to_string()),
        }
    );
    assert_eq!(calls.get(), 0, "specialized host import must not be called");
}
