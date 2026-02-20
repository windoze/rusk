use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn ensure_has_main(src: &str) -> String {
    if src.contains("fn main(") {
        return src.to_string();
    }

    format!(
        r#"{src}

fn main() -> unit {{
    ()
}}
"#
    )
}

fn compile(src: &str) -> Result<rusk_bytecode::ExecutableModule, rusk_compiler::CompileError> {
    let src = ensure_has_main(src);
    compile_to_bytecode(&src)
}

fn run0(src: &str, fn_name: &str) -> Result<AbiValue, String> {
    let mut module = compile(src).map_err(|e| format!("compile error: {e}"))?;
    module.entry = module
        .function_id(fn_name)
        .ok_or_else(|| format!("unknown function `{fn_name}`"))?;

    let mut vm = Vm::new(module).map_err(|e| format!("vm init error: {e}"))?;
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => Ok(value),
        StepResult::Trap { message } => Err(message),
        StepResult::Request { effect_id, .. } => Err(format!(
            "unexpected external effect request (id={})",
            effect_id.0
        )),
        StepResult::Yield { .. } => Err("unexpected yield".to_string()),
    }
}

#[test]
fn byte_conversions_truncate_and_check() {
    let src = r#"
        fn trunc_low_8_bits() -> int { (-1).to_byte().to_int() }

        fn try_byte_ok() -> int {
            match 255.try_byte() {
                Option::Some(b) => b.to_int(),
                Option::None => -1,
            }
        }

        fn try_byte_oob_is_none() -> int {
            match 256.try_byte() {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }
    "#;

    assert_eq!(
        run0(src, "trunc_low_8_bits").expect("run"),
        AbiValue::Int(255)
    );
    assert_eq!(run0(src, "try_byte_ok").expect("run"), AbiValue::Int(255));
    assert_eq!(
        run0(src, "try_byte_oob_is_none").expect("run"),
        AbiValue::Int(0)
    );
}

#[test]
fn char_conversions_check_scalar_values() {
    let src = r#"
        fn try_char_ok() -> int {
            match 65.try_char() {
                Option::Some(c) => c.to_int(),
                Option::None => -1,
            }
        }

        fn try_char_surrogate_is_none() -> int {
            match 55296.try_char() {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }

        fn to_char_surrogate_traps() -> int {
            let c = 55296.to_char();
            c.to_int()
        }
    "#;

    assert_eq!(run0(src, "try_char_ok").expect("run"), AbiValue::Int(65));
    assert_eq!(
        run0(src, "try_char_surrogate_is_none").expect("run"),
        AbiValue::Int(0)
    );

    let err = run0(src, "to_char_surrogate_traps").expect_err("should trap");
    assert!(err.contains("surrogate"), "{err}");
}

#[test]
fn byte_and_char_support_equality_ops() {
    let src = r#"
        fn byte_eq() -> bool {
            1.to_byte() == 1.to_byte() && 1.to_byte() != 2.to_byte()
        }

        fn char_eq() -> bool {
            65.to_char() == 65.to_char() && 65.to_char() != 66.to_char()
        }
    "#;

    assert_eq!(run0(src, "byte_eq").expect("run"), AbiValue::Bool(true));
    assert_eq!(run0(src, "char_eq").expect("run"), AbiValue::Bool(true));
}

#[test]
fn infers_integer_literals_to_byte_from_context() {
    let src = r#"
        fn take_byte(x: byte) -> int { x.to_int() }

        fn annotated_literal_is_byte() -> int {
            let y: byte = 42;
            y.to_int()
        }

        fn inferred_via_call_arg() -> int {
            let x = 42;
            take_byte(x)
        }
    "#;

    assert_eq!(
        run0(src, "annotated_literal_is_byte").expect("run"),
        AbiValue::Int(42)
    );
    assert_eq!(
        run0(src, "inferred_via_call_arg").expect("run"),
        AbiValue::Int(42)
    );
}

#[test]
fn rejects_out_of_range_integer_literals_for_byte_inference() {
    let src = r#"
        fn bad() -> unit {
            let _b: byte = 256;
            ()
        }
    "#;

    let err = compile(src).expect_err("should fail");
    assert!(
        err.message.contains("does not fit into `byte`"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn supports_char_literals_and_escapes() {
    let src = r#"
        fn basic() -> int { 'a'.to_int() }
        fn hex_escape() -> int { '\x41'.to_int() }
        fn unicode_escape() -> int { '\u{1F600}'.to_int() }
    "#;

    assert_eq!(run0(src, "basic").expect("run"), AbiValue::Int(97));
    assert_eq!(run0(src, "hex_escape").expect("run"), AbiValue::Int(65));
    assert_eq!(
        run0(src, "unicode_escape").expect("run"),
        AbiValue::Int(0x1F600)
    );
}

#[test]
fn rejects_invalid_unicode_code_points_in_char_literals() {
    let src = r#"
        fn bad() -> unit {
            let _xs = ['\u{D800}', '\u{DFFF}', '\u{110000}'];
            ()
        }
    "#;

    let err = compile(src).expect_err("should fail");
    assert!(
        err.message.contains("invalid unicode code point"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn rejects_multi_codepoint_char_literals() {
    let src = r#"
        fn bad1() -> unit {
            let _c = 'ab';
            ()
        }

        fn bad2() -> unit {
            let _c = '\u{0065}\u{0301}';
            ()
        }
    "#;

    let err = compile(src).expect_err("should fail");
    assert!(
        err.message
            .contains("char literal must contain exactly one"),
        "unexpected error: {err:?}"
    );
}

#[test]
fn bytes_index_get_and_slice() {
    let src = r#"
        fn index_ok() -> int {
            readonly bs = b"ABC";
            bs[1].to_int()
        }

        fn index_oob_traps() -> int {
            readonly bs = b"A";
            bs[1].to_int()
        }

        fn get_oob_is_none() -> int {
            readonly bs = b"A";
            match bs.get(1) {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }

        fn slice_ok() -> bool {
            readonly bs = b"ABCDE";
            bs.slice(1, Option::Some(4)) == b"BCD"
        }

        fn slice_to_end_ok() -> bool {
            readonly bs = b"ABCDE";
            bs.slice(2, Option::None) == b"CDE"
        }

        fn slice_oob_traps() -> bool {
            readonly bs = b"ABCDE";
            bs.slice(0, Option::Some(999)) == b""
        }
    "#;

    assert_eq!(run0(src, "index_ok").expect("run"), AbiValue::Int(66));

    let err = run0(src, "index_oob_traps").expect_err("should trap");
    assert!(err.contains("index out of bounds"), "{err}");

    assert_eq!(run0(src, "get_oob_is_none").expect("run"), AbiValue::Int(0));
    assert_eq!(run0(src, "slice_ok").expect("run"), AbiValue::Bool(true));
    assert_eq!(
        run0(src, "slice_to_end_ok").expect("run"),
        AbiValue::Bool(true)
    );

    let err = run0(src, "slice_oob_traps").expect_err("should trap");
    assert!(err.contains("out of bounds"), "{err}");
}

#[test]
fn bytes_array_conversions_are_copying() {
    let src = r#"
        fn to_array_copies() -> bool {
            readonly bs = b"AB";
            let xs = bs.to_array();
            xs[0] = 90.to_byte();
            bs == b"AB"
        }

        fn from_array_copies() -> bool {
            let xs = [65.to_byte(), 66.to_byte()];
            readonly ro = xs;
            let bs = bytes::from_array(ro);
            xs[0] = 90.to_byte();
            bs == b"AB"
        }
    "#;

    assert_eq!(
        run0(src, "to_array_copies").expect("run"),
        AbiValue::Bool(true)
    );
    assert_eq!(
        run0(src, "from_array_copies").expect("run"),
        AbiValue::Bool(true)
    );
}

#[test]
fn string_slice_uses_byte_offsets_and_checks_utf8_boundaries() {
    let src = r#"
        fn slice_ok() -> bool {
            "é".slice(0, Option::Some(2)) == "é"
        }

        fn slice_to_end_ok() -> bool {
            "hello".slice(2, Option::None) == "llo"
        }

        fn slice_non_boundary_traps() -> string {
            "é".slice(0, Option::Some(1))
        }
    "#;

    assert_eq!(run0(src, "slice_ok").expect("run"), AbiValue::Bool(true));
    assert_eq!(
        run0(src, "slice_to_end_ok").expect("run"),
        AbiValue::Bool(true)
    );

    let err = run0(src, "slice_non_boundary_traps").expect_err("should trap");
    assert!(err.contains("UTF-8 boundary"), "{err}");
}

#[test]
fn slice_views_keep_backing_buffers_alive_across_gc() {
    let src = r#"
        fn gc_test() -> int {
            let bs = b"ABCDE";
            let bsl = bs.slice(1, Option::Some(2));
            bs = b"Z";

            let s = "hé";
            let ssl = s.slice(1, Option::Some(3));
            s = "";

            let i = 0;
            while i < 60000 {
                let tmp = [i];
                i = i + 1;
            };

            let ok = if ssl == "é" { 1 } else { 0 };
            bsl[0].to_int() + ok * 1000
        }
    "#;

    assert_eq!(run0(src, "gc_test").expect("run"), AbiValue::Int(66 + 1000));
}
