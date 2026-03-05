use rusk_compiler::{CompileOptions, HostFnSig, HostType, compile_to_bytecode_with_options};
use rusk_vm::{
    AbiValue, ContinuationHandle, HostError, StepResult, Vm, vm_drop_pinned_continuation,
    vm_resume, vm_resume_pinned_continuation_tail, vm_step,
};
use std::cell::RefCell;
use std::rc::Rc;

fn compile_with_host_continuations(src: &str) -> rusk_bytecode::ExecutableModule {
    let mut options = CompileOptions::default();

    options
        .register_external_effect(
            "TestFfi",
            "yield",
            HostFnSig {
                params: Vec::new(),
                ret: HostType::Unit,
            },
        )
        .expect("register external effect");

    compile_to_bytecode_with_options(src, &options).expect("compile")
}

#[test]
fn host_can_store_multiple_pinned_continuations_and_resume_after_gc() {
    let src = r#"
        mod host {
            pub extern fn store_cont(k: cont(string) -> string) -> unit;
            pub extern fn pop_cont() -> cont(string) -> string;
        }

        interface E { fn boom() -> string; }
        interface TestFfi { fn yield() -> unit; }

        fn capture(n: int) -> unit {
            let prefix = f"p{n}";
            match @E.boom() {
                @E.boom() -> k => { host::store_cont(k); "captured" }
                s => f"{prefix}:{s}"
            };
            ()
        }

        fn main() -> string {
            capture(1);
            capture(2);

            // Give the host a chance to run GC while the continuations are held only on the host.
            @TestFfi.yield();

            let k1 = host::pop_cont();
            let a = k1("A");
            let k2 = host::pop_cont();
            let b = k2("B");
            f"{a}|{b}"
        }
    "#;

    let module = compile_with_host_continuations(src);

    let stored: Rc<RefCell<Vec<ContinuationHandle>>> = Rc::new(RefCell::new(Vec::new()));
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let store_id = module.host_import_id("host::store_cont").expect("store id");
    let stored_for_store = Rc::clone(&stored);
    vm.register_host_import_typed(
        store_id,
        move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
            stored_for_store.borrow_mut().push(k);
            Ok(())
        },
    )
    .expect("register store");

    let pop_id = module.host_import_id("host::pop_cont").expect("pop id");
    let stored_for_pop = Rc::clone(&stored);
    vm.register_host_import_typed(pop_id, move |()| -> Result<ContinuationHandle, HostError> {
        stored_for_pop.borrow_mut().pop().ok_or_else(|| HostError {
            message: "host::pop_cont: empty".to_string(),
        })
    })
    .expect("register pop");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { args, k, .. } = got else {
        panic!("expected yield request, got {got:?}");
    };
    assert!(args.is_empty(), "yield args should be empty: {args:?}");

    let handles_before_resume = stored.borrow().clone();
    assert_eq!(
        handles_before_resume.len(),
        2,
        "expected two stored continuations, got {handles_before_resume:?}"
    );
    assert_ne!(handles_before_resume[0], handles_before_resume[1]);
    assert_ne!(handles_before_resume[0].index, 0);
    assert_ne!(handles_before_resume[1].index, 0);

    // Ensure pinned continuations are part of the VM root set.
    vm.collect_garbage_now();

    vm_resume(&mut vm, k, AbiValue::Unit).expect("resume yield");
    assert_eq!(
        vm_step(&mut vm, None),
        StepResult::Done {
            value: AbiValue::String("p2:A|p1:B".to_string())
        }
    );

    // Host-side explicit dropping releases pinned slots and invalidates stale handles.
    for h in &handles_before_resume {
        vm_drop_pinned_continuation(&mut vm, h.clone()).expect("drop pinned continuation");
        let err = vm_drop_pinned_continuation(&mut vm, h.clone()).expect_err("double-drop errors");
        assert!(
            err.to_string().contains("invalid continuation"),
            "unexpected error: {err}"
        );
    }
}

#[test]
fn dropping_a_pinned_continuation_invalidates_the_handle() {
    let src = r#"
        mod host {
            pub extern fn store_cont(k: cont(string) -> string) -> unit;
            pub extern fn pop_cont() -> cont(string) -> string;
        }

        interface E { fn boom() -> string; }
        interface TestFfi { fn yield() -> unit; }

        fn capture(n: int) -> unit {
            let prefix = f"p{n}";
            match @E.boom() {
                @E.boom() -> k => { host::store_cont(k); "captured" }
                s => f"{prefix}:{s}"
            };
            ()
        }

        fn main() -> string {
            capture(1);
            capture(2);
            @TestFfi.yield();
            let k1 = host::pop_cont();
            k1("A")
        }
    "#;

    let module = compile_with_host_continuations(src);

    let stored: Rc<RefCell<Vec<ContinuationHandle>>> = Rc::new(RefCell::new(Vec::new()));
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let store_id = module.host_import_id("host::store_cont").expect("store id");
    let stored_for_store = Rc::clone(&stored);
    vm.register_host_import_typed(
        store_id,
        move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
            stored_for_store.borrow_mut().push(k);
            Ok(())
        },
    )
    .expect("register store");

    let pop_id = module.host_import_id("host::pop_cont").expect("pop id");
    let stored_for_pop = Rc::clone(&stored);
    vm.register_host_import_typed(pop_id, move |()| -> Result<ContinuationHandle, HostError> {
        stored_for_pop.borrow_mut().pop().ok_or_else(|| HostError {
            message: "host::pop_cont: empty".to_string(),
        })
    })
    .expect("register pop");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { args: _, k, .. } = got else {
        panic!("expected yield request, got {got:?}");
    };

    // Drop the most-recently stored continuation, which `pop_cont` will return first.
    let dropped = stored.borrow()[1].clone();
    vm_drop_pinned_continuation(&mut vm, dropped).expect("drop pinned");

    vm_resume(&mut vm, k, AbiValue::Unit).expect("resume yield");
    let got = vm_step(&mut vm, None);
    let StepResult::Trap { message } = got else {
        panic!("expected trap, got {got:?}");
    };
    assert!(
        message.contains("invalid pinned continuation handle"),
        "unexpected trap message: {message}"
    );
}

#[test]
fn reusing_a_consumed_pinned_continuation_handle_traps() {
    let src = r#"
        mod host {
            pub extern fn store(k: cont(int) -> int) -> unit;
            pub extern fn peek() -> cont(int) -> int;
        }

        interface E { fn boom() -> int; }
        interface TestFfi { fn yield() -> unit; }

        fn main() -> int {
            match @E.boom() {
                @E.boom() -> k => { host::store(k); 0 }
                x => x
            };
            @TestFfi.yield();

            let k1 = host::peek();
            let a = k1(1);

            // Ask the host for the same handle again. The continuation is one-shot, so this must fail.
            let _k2 = host::peek();
            a
        }
    "#;

    let module = compile_with_host_continuations(src);

    let stored: Rc<RefCell<Option<ContinuationHandle>>> = Rc::new(RefCell::new(None));
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let store_id = module.host_import_id("host::store").expect("store id");
    let stored_for_store = Rc::clone(&stored);
    vm.register_host_import_typed(
        store_id,
        move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
            *stored_for_store.borrow_mut() = Some(k);
            Ok(())
        },
    )
    .expect("register store");

    let peek_id = module.host_import_id("host::peek").expect("peek id");
    let stored_for_peek = Rc::clone(&stored);
    vm.register_host_import_typed(
        peek_id,
        move |()| -> Result<ContinuationHandle, HostError> {
            stored_for_peek.borrow().clone().ok_or_else(|| HostError {
                message: "host::peek: empty".to_string(),
            })
        },
    )
    .expect("register peek");

    let got = vm_step(&mut vm, None);
    let StepResult::Request { k, .. } = got else {
        panic!("expected yield request, got {got:?}");
    };
    vm_resume(&mut vm, k, AbiValue::Unit).expect("resume yield");

    let got = vm_step(&mut vm, None);
    let StepResult::Trap { message } = got else {
        panic!("expected trap, got {got:?}");
    };
    assert!(
        message.contains("already consumed"),
        "unexpected trap message: {message}"
    );
}

#[test]
fn host_can_tail_resume_a_pinned_continuation_directly() {
    let src = r#"
        mod host {
            pub extern fn store(k: cont(int) -> int) -> unit;
        }

        interface E { fn boom() -> int; }

        fn main() -> int {
            match @E.boom() {
                @E.boom() -> k => { host::store(k); 0 }
                x => x
            }
        }
    "#;

    let module = compile_with_host_continuations(src);

    let stored: Rc<RefCell<Option<ContinuationHandle>>> = Rc::new(RefCell::new(None));
    let mut vm = Vm::new(module.clone()).expect("vm init");

    let store_id = module.host_import_id("host::store").expect("store id");
    let stored_for_store = Rc::clone(&stored);
    vm.register_host_import_typed(
        store_id,
        move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
            *stored_for_store.borrow_mut() = Some(k);
            Ok(())
        },
    )
    .expect("register store");

    // Run to completion without resuming in-language; the continuation is stored on the host.
    assert_eq!(
        vm_step(&mut vm, None),
        StepResult::Done {
            value: AbiValue::Int(0)
        }
    );

    let k = stored.borrow().clone().expect("stored continuation handle");

    // Host-driven tail-resume: inject 42 into the captured `perform`.
    //
    // This API is schedule-only: it does not step the VM. In this test the VM has already
    // completed, so the resumed continuation becomes a fresh top-level execution.
    vm_resume_pinned_continuation_tail(&mut vm, k.clone(), AbiValue::Int(42))
        .expect("tail resume pinned continuation");
    assert_eq!(
        vm_step(&mut vm, None),
        StepResult::Done {
            value: AbiValue::Int(42)
        }
    );

    // The handle is consumed and automatically unpinned by the tail-resume API.
    let err = vm_drop_pinned_continuation(&mut vm, k).expect_err("stale handle is invalid");
    assert!(
        err.to_string().contains("invalid continuation"),
        "unexpected error: {err}"
    );
}

#[test]
fn host_tail_resume_splices_onto_the_running_stack_and_discards_return_value() {
    let src = r#"
        mod host {
            pub extern fn store(k: cont(int) -> int) -> unit;
            pub extern fn set(x: int) -> unit;
            pub extern fn get() -> int;
        }

        interface E { fn boom() -> int; }

        fn capture() -> unit {
            match @E.boom() {
                @E.boom() -> k => { host::store(k); 0 }
                x => { host::set(x); x }
            };
            ()
        }

        fn main() -> int {
            capture();

            // Keep the VM running so the host can schedule a tail-resume onto a non-empty stack.
            let i = 0;
            while i < 10000 {
                i = i + 1;
            };

            host::get()
        }
    "#;

    let module = compile_with_host_continuations(src);

    let stored: Rc<RefCell<Option<ContinuationHandle>>> = Rc::new(RefCell::new(None));
    let last_set: Rc<RefCell<i64>> = Rc::new(RefCell::new(0));

    let mut vm = Vm::new(module.clone()).expect("vm init");

    let store_id = module.host_import_id("host::store").expect("store id");
    let stored_for_store = Rc::clone(&stored);
    vm.register_host_import_typed(
        store_id,
        move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
            *stored_for_store.borrow_mut() = Some(k);
            Ok(())
        },
    )
    .expect("register store");

    let set_id = module.host_import_id("host::set").expect("set id");
    let last_set_for_set = Rc::clone(&last_set);
    vm.register_host_import_typed(set_id, move |(n,): (i64,)| -> Result<(), HostError> {
        *last_set_for_set.borrow_mut() = n;
        Ok(())
    })
    .expect("register set");

    let get_id = module.host_import_id("host::get").expect("get id");
    let last_set_for_get = Rc::clone(&last_set);
    vm.register_host_import_typed(get_id, move |()| -> Result<i64, HostError> {
        Ok(*last_set_for_get.borrow())
    })
    .expect("register get");

    // Drive the VM for a bit (fuel-limited), stopping once we've observed that `capture` exported a
    // pinned continuation handle. At this point the VM is still running with a non-empty stack.
    let mut observed = None;
    for _ in 0..100_000 {
        match vm_step(&mut vm, Some(1)) {
            StepResult::Yield { .. } => {}
            StepResult::Request { .. } => panic!("unexpected external request while stepping"),
            StepResult::Trap { message } => panic!("unexpected trap while stepping: {message}"),
            StepResult::Done { value } => {
                panic!("unexpected completion before storing continuation: {value:?}")
            }
        }

        if let Some(k) = stored.borrow().clone() {
            observed = Some(k);
            break;
        }
    }

    let k = observed.expect("expected stored pinned continuation handle");
    assert_ne!(k.index, 0);

    // Schedule a tail-resume *onto* the running VM stack. The continuation's return value is
    // discarded; the base computation continues and returns `host::get()` after the loop.
    vm_resume_pinned_continuation_tail(&mut vm, k.clone(), AbiValue::Int(42))
        .expect("tail resume pinned continuation");

    assert_eq!(
        vm_step(&mut vm, None),
        StepResult::Done {
            value: AbiValue::Int(42)
        }
    );

    // The handle is consumed and automatically unpinned by the tail-resume API.
    let err = vm_drop_pinned_continuation(&mut vm, k).expect_err("stale handle is invalid");
    assert!(
        err.to_string().contains("invalid continuation"),
        "unexpected error: {err}"
    );
}
