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
fn array_methods_are_available_without_core_intrinsics() {
    let src = r#"
        fn push_pop() -> int {
            let xs = [1, 2];
            xs.push(3);
            let l1 = xs.len();
            let p = match xs.pop() {
                Option::Some(n) => n,
                Option::None => 0,
            };
            let l2 = xs.len();
            l1 * 100 + p * 10 + l2
        }

        fn insert_remove() -> int {
            let xs = [1, 2, 3];
            xs.insert(1, 9); // [1, 9, 2, 3]
            let r = xs.remove(2); // => 2, xs=[1, 9, 3]
            xs[0] * 10000 + xs[1] * 1000 + xs[2] * 100 + r * 10 + xs.len()
        }

        fn resize_fill() -> int {
            let xs = [1, 2];
            xs.resize(5, 7); // [1,2,7,7,7]
            xs.resize(3, 0); // [1,2,7]
            xs[0] * 10000 + xs[1] * 1000 + xs[2] * 100 + xs.len()
        }

        fn extend_from_other() -> int {
            let xs = [1];
            xs.extend([2, 3]);
            xs[0] * 10000 + xs[1] * 1000 + xs[2] * 100 + xs.len()
        }

        fn clear_len() -> int {
            let xs = [1, 2, 3];
            xs.clear();
            xs.len()
        }

        fn slice_sum() -> int {
            let xs = [10, 20, 30, 40];
            let ys = xs.slice(1, 3);
            ys[0] * 100 + ys[1] * 10 + ys.len()
        }

        fn concat_sum() -> int {
            let xs = [1, 2];
            let ys = xs.concat([3, 4]);
            ys[0] * 10000 + ys[1] * 1000 + ys[2] * 100 + ys[3] * 10 + ys.len()
        }

        fn concat_ro_sum() -> int {
            readonly xs = [1, 2];
            readonly other = [3, 4];
            let ys = xs.concat_ro(other);
            ys[0] * 10000 + ys[1] * 1000 + ys[2] * 100 + ys[3] * 10 + ys.len()
        }

        fn copy_sum() -> int {
            let xs = [1, 2, 3];
            let ys = xs.copy();
            ys[0] * 1000 + ys[1] * 100 + ys[2] * 10 + ys.len()
        }

        fn copy_ro_sum() -> int {
            readonly xs = [1, 2, 3];
            let ys = xs.copy_ro();
            ys[0] * 1000 + ys[1] * 100 + ys[2] * 10 + ys.len()
        }
    "#;

    assert_eq!(run0(src, "push_pop").expect("run"), AbiValue::Int(332));
    assert_eq!(
        run0(src, "insert_remove").expect("run"),
        AbiValue::Int(19323)
    );
    assert_eq!(run0(src, "resize_fill").expect("run"), AbiValue::Int(12703));
    assert_eq!(
        run0(src, "extend_from_other").expect("run"),
        AbiValue::Int(12303)
    );
    assert_eq!(run0(src, "clear_len").expect("run"), AbiValue::Int(0));
    assert_eq!(run0(src, "slice_sum").expect("run"), AbiValue::Int(2302));
    assert_eq!(run0(src, "concat_sum").expect("run"), AbiValue::Int(12344));
    assert_eq!(
        run0(src, "concat_ro_sum").expect("run"),
        AbiValue::Int(12344)
    );
    assert_eq!(run0(src, "copy_sum").expect("run"), AbiValue::Int(1233));
    assert_eq!(run0(src, "copy_ro_sum").expect("run"), AbiValue::Int(1233));
}

#[test]
fn string_add_and_construction_helpers_work() {
    let src = r#"
        fn str_add() -> string { "hello, " + "world" }

        fn str_add_via_iface_call() -> string { core::ops::Add::add("a", "b") }

        fn from_chars() -> string {
            let chars = [104.to_char(), 233.to_char()];
            readonly ro = chars;
            string::from_chars(ro)
        }
    "#;

    assert_eq!(
        run0(src, "str_add").expect("run"),
        AbiValue::String("hello, world".to_string())
    );
    assert_eq!(
        run0(src, "str_add_via_iface_call").expect("run"),
        AbiValue::String("ab".to_string())
    );
    assert_eq!(
        run0(src, "from_chars").expect("run"),
        AbiValue::String("hé".to_string())
    );
}

#[test]
fn string_decoders_utf8_and_utf16_work() {
    let src = r#"
        fn utf8_lossy() -> string {
            readonly arr = [240.to_byte(), 40.to_byte(), 140.to_byte(), 40.to_byte()];
            let b = bytes::from_array(arr);
            string::from_utf8(b)
        }

        fn utf8_strict_invalid_is_none() -> int {
            readonly arr = [240.to_byte(), 40.to_byte(), 140.to_byte(), 40.to_byte()];
            let b = bytes::from_array(arr);
            match string::from_utf8_strict(b) {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }

        fn utf8_strict_valid_is_some() -> int {
            match string::from_utf8_strict(b"hello") {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }

        fn utf16_pair_le() -> string {
            // U+10437 (𐐷) = D801 DC37
            readonly units = [55297, 56375];
            string::from_utf16_le(units)
        }

        fn utf16_pair_be() -> string {
            readonly units = [55297, 56375];
            string::from_utf16_be(units)
        }

        fn utf16_lossy_invalid_surrogate() -> string {
            readonly units = [55297];
            string::from_utf16_le(units)
        }

        fn utf16_strict_invalid_is_none() -> int {
            readonly units = [55297];
            match string::from_utf16_le_strict(units) {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }

        fn utf16_strict_out_of_range_is_none() -> int {
            readonly units = [70000];
            match string::from_utf16_le_strict(units) {
                Option::Some(_) => 1,
                Option::None => 0,
            }
        }
    "#;

    assert_eq!(
        run0(src, "utf8_lossy").expect("run"),
        AbiValue::String("�(�(".to_string())
    );
    assert_eq!(
        run0(src, "utf8_strict_invalid_is_none").expect("run"),
        AbiValue::Int(0)
    );
    assert_eq!(
        run0(src, "utf8_strict_valid_is_some").expect("run"),
        AbiValue::Int(1)
    );
    assert_eq!(
        run0(src, "utf16_pair_le").expect("run"),
        AbiValue::String("𐐷".to_string())
    );
    assert_eq!(
        run0(src, "utf16_pair_be").expect("run"),
        AbiValue::String("𐐷".to_string())
    );
    assert_eq!(
        run0(src, "utf16_lossy_invalid_surrogate").expect("run"),
        AbiValue::String("�".to_string())
    );
    assert_eq!(
        run0(src, "utf16_strict_invalid_is_none").expect("run"),
        AbiValue::Int(0)
    );
    assert_eq!(
        run0(src, "utf16_strict_out_of_range_is_none").expect("run"),
        AbiValue::Int(0)
    );
}

#[test]
fn hash_interface_is_available_for_primitives_and_user_types() {
    let src = r#"
        struct Point { x: int, y: int }

        impl core::hash::Hash for Point {
            readonly fn hash() -> int {
                core::hash::combine(self.x.hash(), self.y.hash())
            }
        }

        fn hash_int_is_deterministic() -> bool {
            123.hash() == 123.hash()
        }

        fn hash_int_is_not_constant() -> bool {
            123.hash() != 124.hash()
        }

        fn hash_string_is_deterministic() -> bool {
            "abc".hash() == "abc".hash()
        }

        fn hash_bytes_is_deterministic() -> bool {
            b"abc".hash() == b"abc".hash()
        }

        fn point_hash_matches_combine() -> bool {
            let p = Point { x: 1, y: 2 };
            p.hash() == core::hash::combine(1.hash(), 2.hash())
        }
    "#;

    assert_eq!(
        run0(src, "hash_int_is_deterministic").expect("run"),
        AbiValue::Bool(true)
    );
    assert_eq!(
        run0(src, "hash_int_is_not_constant").expect("run"),
        AbiValue::Bool(true)
    );
    assert_eq!(
        run0(src, "hash_string_is_deterministic").expect("run"),
        AbiValue::Bool(true)
    );
    assert_eq!(
        run0(src, "hash_bytes_is_deterministic").expect("run"),
        AbiValue::Bool(true)
    );
    assert_eq!(
        run0(src, "point_hash_matches_combine").expect("run"),
        AbiValue::Bool(true)
    );
}
