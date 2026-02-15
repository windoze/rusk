use rusk_compiler::compile_to_mir;
use rusk_interpreter::corelib::register_core_host_fns;
use rusk_interpreter::{Interpreter, RuntimeError, Value};

fn run0(src: &str, fn_name: &str) -> Result<Value, RuntimeError> {
    let module = compile_to_mir(src).map_err(|e| RuntimeError::Trap {
        message: format!("compile error: {e}"),
    })?;
    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);
    interp.run_function(fn_name, vec![])
}

#[test]
fn supports_literals_and_numeric_ops() {
    let src = r#"
        fn int_math() -> int { 1 + 2 * 3 - 4 / 2 }
        fn float_math() -> float { 1.5 + 2.0 * 2.0 }
        fn float_cmp() -> bool { 1.5 < 2.0 && 2.0 >= 2.0 }
    "#;

    assert_eq!(
        run0(src, "int_math").expect("run"),
        Value::Int(1 + 2 * 3 - 4 / 2)
    );
    assert_eq!(
        run0(src, "float_math").expect("run"),
        Value::Float(1.5 + 2.0 * 2.0)
    );
    assert_eq!(run0(src, "float_cmp").expect("run"), Value::Bool(true));
}

#[test]
fn supports_primitive_equality() {
    let src = r#"
        fn bool_eq() -> bool { true == false || true != false }
        fn string_eq() -> bool { "a" == "a" && "a" != "b" }
        fn bytes_eq() -> bool { b"hi" == b"hi" && b"hi" != b"ho" }
        fn unit_eq() -> bool { () == () && () != () == false }
    "#;

    assert_eq!(run0(src, "bool_eq").expect("run"), Value::Bool(true));
    assert_eq!(run0(src, "string_eq").expect("run"), Value::Bool(true));
    assert_eq!(run0(src, "bytes_eq").expect("run"), Value::Bool(true));
    assert_eq!(run0(src, "unit_eq").expect("run"), Value::Bool(true));
}

#[test]
fn supports_arrays_indexing_and_assignment() {
    let src = r#"
        fn get() -> int {
            let xs = [10, 20, 30];
            xs[1]
        }

        fn set() -> int {
            let xs = [10, 20, 30];
            xs[1] = 99;
            xs[1]
        }
    "#;

    assert_eq!(run0(src, "get").expect("run"), Value::Int(20));
    assert_eq!(run0(src, "set").expect("run"), Value::Int(99));
}

#[test]
fn traps_on_array_index_out_of_bounds() {
    let src = r#"
        fn oob() -> int {
            let xs = [1, 2, 3];
            xs[999]
        }
    "#;

    let err = run0(src, "oob").expect_err("should trap");
    assert!(
        matches!(err, RuntimeError::IndexOutOfBounds { .. }),
        "{err:?}"
    );
}

#[test]
fn supports_structs_fields_and_methods() {
    let src = r#"
        struct Point { x: int, y: int }

        impl Point {
            fn sum() -> int { self.x + self.y }
        }

        interface Add {
            fn add(n: int) -> int;
        }

        impl Add for Point {
            fn add(n: int) -> int { self.sum() + n }
        }

        fn test() -> int {
            let p = Point { x: 1, y: 2 };
            p.x = 10;
            p.sum() + Add::add(p, 5) + p.add(7)
        }
    "#;

    let out = run0(src, "test").expect("run");
    // After mutation, p.sum() == 12; Add::add(p,5)==17; p.add(7)==19
    assert_eq!(out, Value::Int(12 + 17 + 19));
}

#[test]
fn method_call_sugar_is_ambiguous_error() {
    let src = r#"
        struct S { x: int }

        interface A { fn foo() -> int; }
        interface B { fn foo() -> int; }

        impl A for S { fn foo() -> int { self.x + 1 } }
        impl B for S { fn foo() -> int { self.x + 2 } }

        fn test() -> int {
            let s = S { x: 0 };
            s.foo()
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message.contains("ambiguous method"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn closures_capture_let_bindings() {
    let src = r#"
        fn test() -> int {
            let x = 0;
            let add = |n: int| {
                x = x + n;
                x
            };
            add(1);
            add(2);
            x
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(3));
}

#[test]
fn function_items_can_be_used_as_values() {
    let src = r#"
        fn add1(x: int) -> int { x + 1 }

        fn apply(f: fn(int) -> int, x: int) -> int { f(x) }

        fn test() -> int { apply(add1, 41) }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(42));
}

#[test]
fn unit_return_type_can_be_omitted_on_function_items() {
    let src = r#"
        fn test() {
            ()
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Unit);
}

#[test]
fn fn_type_syntax_can_omit_unit_return_type() {
    let src = r#"
        fn sink(f: fn(int), x: int) -> unit {
            f(x);
            ()
        }

        fn g(n: int) {
            ()
        }

        fn test() -> unit {
            sink(g, 1);
            ()
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Unit);
}

#[test]
fn interface_method_signatures_can_omit_unit_return_type() {
    let src = r#"
        struct S {}

        interface I {
            fn bar(n: int);
        }

        impl I for S {
            fn bar(n: int) {
                ()
            }
        }

        fn test() -> unit {
            let s = S {};
            I::bar(s, 42);
            ()
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Unit);
}

#[test]
fn generic_functions_infer_type_arguments() {
    let src = r#"
        fn id<T>(x: T) -> T { x }
        fn test_int() -> int { id(123) }
        fn test_string() -> string { id("ok") }
    "#;

    assert_eq!(run0(src, "test_int").expect("run"), Value::Int(123));
    assert_eq!(
        run0(src, "test_string").expect("run"),
        Value::String("ok".to_string())
    );
}

#[test]
fn generic_calls_error_when_type_arguments_cannot_be_inferred() {
    let src = r#"
        fn marker<T>() { () }

        fn test() -> unit {
            marker();
            ()
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message
            .contains("cannot infer type arguments for generic call"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn generic_calls_support_explicit_type_arguments_with_turbofish() {
    let src = r#"
        mod m {
            pub fn marker<T>() { () }
        }

        struct S {}

        impl S {
            static fn id<T>(x: T) -> T { x }

            fn id_instance<T>(x: T) -> T { x }
        }

        fn test() -> int {
            m::marker::<int>();

            let s = S {};
            let a = S::id::<int>(123);
            let b = s.id_instance::<int>(a);
            b
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(123));
}

#[test]
fn rejects_uninitialized_let_bindings() {
    let src = r#"
        fn bad() -> int {
            let x;
            x
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message
            .contains("`let` bindings require an initializer"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn readonly_prevents_assignment_through_view() {
    let src = r#"
        struct Box { v: int }
        fn test() -> unit {
            readonly b = Box { v: 1 };
            b.v = 2;
            ()
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message.contains("readonly view"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn effects_resume_and_handler_result() {
    let src = r#"
        interface Tick { fn tick(n: int) -> int; }

        fn test_resume() -> int {
            match @Tick.tick(1) + @Tick.tick(2) {
                @Tick.tick(n) => resume(n * 10)
                v => v
            }
        }

        fn test_no_resume() -> int {
            match @Tick.tick(1) {
                @Tick.tick(_) => 99
                v => v
            }
        }
    "#;

    assert_eq!(run0(src, "test_resume").expect("run"), Value::Int(30));
    assert_eq!(run0(src, "test_no_resume").expect("run"), Value::Int(99));
}

#[test]
fn match_helpers_can_mutate_captured_let_bindings() {
    let src = r#"
        interface Tick { fn tick(n: int) -> int; }

        fn test() -> int {
            let x = 0;

            match @Tick.tick(7) {
                @Tick.tick(n) => { x = n; resume(0) }
                v => v
            };

            x
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(7));
}

#[test]
fn traps_on_unhandled_effect_outside_match() {
    let src = r#"
        interface E { fn boom() -> unit; }
        fn bad() -> unit {
            @E.boom()
        }
    "#;

    let err = run0(src, "bad").expect_err("should error");
    assert!(
        matches!(err, RuntimeError::UnhandledEffect { .. }),
        "{err:?}"
    );
}

#[test]
fn effect_handlers_are_scoped_to_scrutinee_only() {
    let src = r#"
        interface E { fn boom() -> unit; }

        fn bad() -> unit {
            match 0 {
                @E.boom() => resume(())
                0 => { @E.boom() }
                _ => ()
            }
        }
    "#;

    let err = run0(src, "bad").expect_err("should error");
    assert!(
        matches!(err, RuntimeError::UnhandledEffect { .. }),
        "{err:?}"
    );
}

#[test]
fn resume_is_one_shot() {
    let src = r#"
        interface E { fn boom() -> unit; }

        fn bad() -> unit {
            match @E.boom() {
                @E.boom() => {
                    resume(());
                    resume(())
                }
                _ => ()
            }
        }
    "#;

    let err = run0(src, "bad").expect_err("should error");
    assert!(matches!(err, RuntimeError::InvalidResume), "{err:?}");
}

#[test]
fn can_store_and_resume_a_captured_continuation_later() {
    let src = r#"
        struct Cell<T> { v: T }

        interface E { fn boom() -> int; }

        fn test_default_binder() -> int {
            let cell = Cell { v: Option::None };
            match @E.boom() {
                @E.boom() => { cell.v = Option::Some(resume); 0 }
                x => x
            };
            match cell.v {
                Option::Some(k) => k(41)
                Option::None => 0
            }
        }

        fn test_explicit_binder() -> int {
            let cell = Cell { v: Option::None };
            match @E.boom() {
                @E.boom() -> k => { cell.v = Option::Some(k); 0 }
                x => x
            };
            match cell.v {
                Option::Some(k) => k(42)
                Option::None => 0
            }
        }
    "#;

    assert_eq!(
        run0(src, "test_default_binder").expect("run"),
        Value::Int(41)
    );
    assert_eq!(
        run0(src, "test_explicit_binder").expect("run"),
        Value::Int(42)
    );
}

#[test]
fn calling_a_continuation_value_is_one_shot() {
    let src = r#"
        struct Cell<T> { v: T }

        interface E { fn boom() -> int; }

        fn bad() -> unit {
            let cell = Cell { v: Option::None };
            match @E.boom() {
                @E.boom() -> k => { cell.v = Option::Some(k); 0 }
                x => x
            };
            match cell.v {
                Option::Some(k) => {
                    k(1);
                    k(2);
                    ()
                }
                Option::None => ()
            }
        }
    "#;

    let err = run0(src, "bad").expect_err("should error");
    assert!(matches!(err, RuntimeError::InvalidResume), "{err:?}");
}

#[test]
fn evaluation_order_is_left_to_right_for_array_literals() {
    let src = r#"
        interface Tick { fn tick(n: int) -> unit; }

        fn test() -> int {
            let acc = 0;
            match [@Tick.tick(1), @Tick.tick(2), @Tick.tick(3)] {
                @Tick.tick(n) => { acc = acc * 10 + n; resume(()) }
                _ => acc
            }
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(123));
}

#[test]
fn formatted_strings_desugar_via_core_intrinsics() {
    let src = r#"
        fn test() -> string {
            f"hello {1} world {true}"
        }
    "#;

    assert_eq!(
        run0(src, "test").expect("run"),
        Value::String("hello 1 world true".to_string())
    );
}

#[test]
fn enum_construction_via_call_and_matching() {
    let src = r#"
        fn some() -> int {
            match Option::Some(42) {
                Option::Some(n) => n
                Option::None => 0
            }
        }

        fn none() -> int {
            let x: Option<int> = Option::None;
            match x {
                Option::Some(_) => 0
                Option::None => 7
            }
        }
    "#;

    assert_eq!(run0(src, "some").expect("run"), Value::Int(42));
    assert_eq!(run0(src, "none").expect("run"), Value::Int(7));
}

#[test]
fn array_prefix_patterns_work() {
    let src = r#"
        fn test() -> int {
            match [1, 2, 3] {
                [a, b, ..] => a * 10 + b
                _ => 0
            }
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(12));
}

#[test]
fn match_non_exhaustive_is_compile_error() {
    let src = r#"
        fn test() -> int {
            match 1 {
                0 => 0
            }
        }
    "#;

    let err = compile_to_mir(src).expect_err("should be a compile error");
    assert!(
        err.message.contains("non-exhaustive match"),
        "unexpected compile error: {err}"
    );
}

#[test]
fn loop_break_and_continue_work() {
    let src = r#"
        fn test_loop() -> int {
            let x = 0;
            loop {
                x = x + 1;
                if x == 3 { break; };
            };
            x
        }

        fn test_while() -> int {
            let i = 0;
            let sum = 0;
            while i < 5 {
                i = i + 1;
                if i == 3 { continue; };
                sum = sum + i;
            };
            sum
        }
    "#;

    assert_eq!(run0(src, "test_loop").expect("run"), Value::Int(3));
    assert_eq!(
        run0(src, "test_while").expect("run"),
        Value::Int(1 + 2 + 4 + 5)
    );
}

#[test]
fn break_outside_loop_is_type_error() {
    let src = r#"
        fn bad() -> unit {
            break;
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message.contains("outside of a loop"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn lambda_params_can_be_inferred_from_call_site() {
    let src = r#"
        fn apply(f: fn(int) -> int, x: int) -> int { f(x) }
        fn test() -> int { apply(|n| { n + 1 }, 41) }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(42));
}

#[test]
fn using_generic_function_as_value_is_rejected() {
    let src = r#"
        fn id<T>(x: T) -> T { x }
        fn test() -> unit {
            let f = id;
            f(1);
            ()
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message.contains("generic function"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn readonly_array_rejects_index_assignment() {
    let src = r#"
        fn test() -> unit {
            readonly xs = [1, 2];
            xs[0] = 3;
            ()
        }
    "#;

    let err = compile_to_mir(src).expect_err("should fail");
    assert!(
        err.message.contains("readonly view"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn effect_arm_order_is_source_order() {
    let src = r#"
        interface E { fn boom(n: int) -> int; }

        fn test() -> int {
            match @E.boom(1) {
                @E.boom(_) => 1
                @E.boom(1) => 2
                v => v
            }
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), Value::Int(1));
}
