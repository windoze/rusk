use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
    compile_to_bytecode_with_options,
};
use rusk_vm::{AbiValue, HostError, StepResult, Vm, vm_step};
use std::cell::Cell;
use std::rc::Rc;

fn options_with_specialization_host_import() -> CompileOptions {
    let mut options = CompileOptions::default();
    options
        .register_host_module(
            "spec",
            HostModuleDecl {
                visibility: HostVisibility::Public,
                functions: vec![HostFunctionDecl {
                    visibility: HostVisibility::Public,
                    name: "id_int".to_string(),
                    sig: HostFnSig {
                        params: vec![HostType::Int],
                        ret: HostType::Int,
                    },
                }],
            },
        )
        .unwrap();
    options
}

#[test]
fn generic_specialization_dispatches_to_host_impl_for_exact_type_args() {
    let src = r#"
        fn id<T>(x: T) -> T {
            let _ = core::intrinsics::unit_eq((), ());
            x
        }

        fn _touch() -> int { spec::id_int(0) }

        fn main() -> int { id(123) }
    "#;
    let module = compile_to_bytecode_with_options(src, &options_with_specialization_host_import())
        .expect("compile");

    let mut vm = Vm::new(module.clone()).expect("vm init");

    let calls = Rc::new(Cell::new(0u32));
    let calls_cell = Rc::clone(&calls);

    let host_id = module
        .host_import_id("spec::id_int")
        .expect("host import id");
    vm.register_host_import(host_id, move |args: &[AbiValue]| match args {
        [AbiValue::Int(v)] => {
            calls_cell.set(calls_cell.get() + 1);
            Ok(AbiValue::Int(*v))
        }
        other => Err(HostError {
            message: format!("spec::id_int: bad args: {other:?}"),
        }),
    })
    .expect("register host import");

    let fn_id = module.function_id("id").expect("id function id");
    let int_ty = vm.intern_type_rep(&rusk_mir::TypeRepLit::Int, &[]);
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
        fn id<T>(x: T) -> T {
            let _ = core::intrinsics::unit_eq((), ());
            x
        }

        fn _touch() -> int { spec::id_int(0) }

        fn main() -> string { id("ok") }
    "#;
    let module = compile_to_bytecode_with_options(src, &options_with_specialization_host_import())
        .expect("compile");

    let mut vm = Vm::new(module.clone()).expect("vm init");

    let calls = Rc::new(Cell::new(0u32));
    let calls_cell = Rc::clone(&calls);

    let host_id = module
        .host_import_id("spec::id_int")
        .expect("host import id");
    vm.register_host_import(host_id, move |args: &[AbiValue]| match args {
        [AbiValue::Int(v)] => {
            calls_cell.set(calls_cell.get() + 1);
            Ok(AbiValue::Int(*v))
        }
        other => Err(HostError {
            message: format!("spec::id_int: bad args: {other:?}"),
        }),
    })
    .expect("register host import");

    let fn_id = module.function_id("id").expect("id function id");
    let int_ty = vm.intern_type_rep(&rusk_mir::TypeRepLit::Int, &[]);
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
