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
fn bytes_len_is_available_via_core_len_interface() {
    let src = r#"
        fn bytes_len() -> int {
            readonly bs = b"ABC";
            bs.len()
        }

        fn bytes_slice_len() -> int {
            readonly bs = b"ABCDE";
            bs.slice(1, Option::Some(4)).len()
        }
    "#;

    assert_eq!(run0(src, "bytes_len").expect("run"), AbiValue::Int(3));
    assert_eq!(run0(src, "bytes_slice_len").expect("run"), AbiValue::Int(3));
}

#[test]
fn array_len_is_available_via_core_len_interface() {
    let src = r#"
        fn array_len() -> int {
            let xs = [1, 2, 3, 4];
            xs.len()
        }

        fn readonly_array_len() -> int {
            let xs = [1, 2, 3];
            readonly ro = xs;
            ro.len()
        }
    "#;

    assert_eq!(run0(src, "array_len").expect("run"), AbiValue::Int(4));
    assert_eq!(
        run0(src, "readonly_array_len").expect("run"),
        AbiValue::Int(3)
    );
}

#[test]
fn string_chars_returns_char_iterator() {
    let src = r#"
        fn count_ascii() -> int { "hello".chars().count() }

        fn count_unicode_scalars() -> int { "hé".chars().count() }

        fn first_char() -> int {
            match "hé".chars().next() {
                Option::Some(c) => c.to_int(),
                Option::None => -1,
            }
        }
    "#;

    assert_eq!(run0(src, "count_ascii").expect("run"), AbiValue::Int(5));
    assert_eq!(
        run0(src, "count_unicode_scalars").expect("run"),
        AbiValue::Int(2)
    );
    assert_eq!(run0(src, "first_char").expect("run"), AbiValue::Int(104));
}
