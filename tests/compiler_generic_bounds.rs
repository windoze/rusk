use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn run0(src: &str, fn_name: &str) -> Result<AbiValue, String> {
    let mut module = compile_to_bytecode(src).map_err(|e| format!("compile error: {e}"))?;
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
fn struct_generic_bounds_work_and_are_enforced() {
    let ok_src = r#"
interface MyHash {
    readonly fn hash() -> int;
}

interface MyEq {
    readonly fn eq(other: Self) -> bool;
}

struct Key { x: int }

impl MyHash for Key {
    readonly fn hash() -> int { 7 }
}

impl MyEq for Key {
    readonly fn eq(other: Self) -> bool { self.x == other.x }
}

struct Box<T: MyHash + MyEq> { v: T }

impl<T: MyHash + MyEq> Box<T> {
    fn v_hash() -> int { MyHash::hash(self.v) }
}

fn main() -> int {
    let b = Box { v: Key { x: 1 } };
    b.v_hash()
}
"#;
    assert_eq!(run0(ok_src, "main").expect("run"), AbiValue::Int(7));

    let err_src = r#"
interface MyHash {
    readonly fn hash() -> int;
}

interface MyEq {
    readonly fn eq(other: Self) -> bool;
}

struct NoImpl { x: int }

struct Box<T: MyHash + MyEq> { v: T }

fn main() -> unit {
    let _b: Box<NoImpl> = Box { v: NoImpl { x: 1 } };
    ()
}
"#;
    let err = compile_to_bytecode(err_src).expect_err("expected compile error");
    assert!(
        err.message.contains("does not satisfy bound `MyHash`"),
        "unexpected error: {err}"
    );
}

#[test]
fn impl_generic_bounds_work_and_are_enforced() {
    let ok_src = r#"
interface MyHash {
    readonly fn hash() -> int;
}

struct Key { x: int }

impl MyHash for Key {
    readonly fn hash() -> int { 9 }
}

struct Wrap<T> { v: T }

impl<T: MyHash> Wrap<T> {
    fn h() -> int { MyHash::hash(self.v) }
}

fn main() -> int {
    let w = Wrap { v: Key { x: 0 } };
    w.h()
}
"#;
    assert_eq!(run0(ok_src, "main").expect("run"), AbiValue::Int(9));

    let err_src = r#"
interface MyHash {
    readonly fn hash() -> int;
}

struct NoImpl { x: int }

struct Wrap<T> { v: T }

impl<T: MyHash> Wrap<T> {
    fn h() -> int { MyHash::hash(self.v) }
}

fn main() -> int {
    let w = Wrap { v: NoImpl { x: 1 } };
    w.h()
}
"#;
    let err = compile_to_bytecode(err_src).expect_err("expected compile error");
    assert!(
        err.message.contains("does not implement `MyHash`"),
        "unexpected error: {err}"
    );
}
