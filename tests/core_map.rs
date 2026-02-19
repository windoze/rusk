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
fn map_basic_insert_get_update_remove_and_grow_work() {
    let src = r#"
        struct Key(int);

        impl core::hash::Hash for Key {
            readonly fn hash() -> int { 0 } // force collisions
        }

        impl core::ops::Eq for Key {
            readonly fn eq(other: Self) -> bool { self.0 == other.0 }
        }

        fn len_and_is_empty() -> int {
	            let m: core::map::Map<int, int> = core::map::Map::new();
            let a = if m.is_empty() { 1 } else { 0 };
            m.insert(1, 10);
            let l1 = m.len();
            let b = if m.is_empty() { 1 } else { 0 };
            let l2 = core::len::Len::len(m);
            a * 1000 + l1 * 100 + b * 10 + l2
        }

        fn basic_int_map() -> int {
	            let m: core::map::Map<int, int> = core::map::Map::new();
            m.insert(1, 10);
            m.insert(2, 20);

            let a = match m.get(1) { Option::Some(v) => v, Option::None => 0 };
            let b = match m.get(2) { Option::Some(v) => v, Option::None => 0 };
            let c = match m.get(3) { Option::Some(v) => v, Option::None => 30 };
            a * 100 + b * 10 + c
        }

        fn update_returns_old() -> int {
	            let m: core::map::Map<string, int> = core::map::Map::new();
            m.insert("a", 1);
            let old = match m.insert("a", 2) { Option::Some(v) => v, Option::None => 0 };
            let cur = match m.get("a") { Option::Some(v) => v, Option::None => 0 };
            old * 10 + cur
        }

        fn remove_works() -> int {
	            let m: core::map::Map<int, int> = core::map::Map::new();
            m.insert(1, 10);
            m.insert(2, 20);
            let r = match m.remove(1) { Option::Some(v) => v, Option::None => 0 };
            let missing = match m.get(1) { Option::Some(_) => 1, Option::None => 0 };
            let still = match m.get(2) { Option::Some(v) => v, Option::None => 0 };
            r * 100 + missing * 10 + still
        }

        fn collisions_work() -> int {
	            let m: core::map::Map<Key, int> = core::map::Map::new();
            m.insert(Key(1), 11);
            m.insert(Key(2), 22);
            m.insert(Key(3), 33);

            let a = match m.get(Key(1)) { Option::Some(v) => v, Option::None => 0 };
            let b = match m.get(Key(2)) { Option::Some(v) => v, Option::None => 0 };
            let c = match m.get(Key(3)) { Option::Some(v) => v, Option::None => 0 };
            a + b + c
        }

        fn grow_and_get() -> int {
	            let m: core::map::Map<int, int> = core::map::Map::new();

            let i = 0;
            while i < 50 {
                m.insert(i, i + 1);
                i = i + 1;
            };

            let sum = 0;
            let j = 0;
            while j < 50 {
                let v = match m.get(j) { Option::Some(v) => v, Option::None => 0 };
                sum = sum + v;
                j = j + 1;
            };
            sum
        }
    "#;

    assert_eq!(
        run0(src, "len_and_is_empty").expect("run"),
        AbiValue::Int(1101)
    );
    assert_eq!(
        run0(src, "basic_int_map").expect("run"),
        AbiValue::Int(1230)
    );
    assert_eq!(
        run0(src, "update_returns_old").expect("run"),
        AbiValue::Int(12)
    );
    assert_eq!(run0(src, "remove_works").expect("run"), AbiValue::Int(1020));
    assert_eq!(
        run0(src, "collisions_work").expect("run"),
        AbiValue::Int(66)
    );
    assert_eq!(run0(src, "grow_and_get").expect("run"), AbiValue::Int(1275));
}
