use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn ensure_has_main(src: &str) -> String {
    if src.contains("fn main") {
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

fn run0(src: &str, fn_name: &str) -> Result<AbiValue, String> {
    let src = ensure_has_main(src);
    let mut module = compile_to_bytecode(&src).map_err(|e| format!("compile error: {e}"))?;
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
fn try_catch_finally_runs_catch_then_finally_on_throw() {
    let src = r#"
        fn test() -> int {
            let state = [0];

            let _r: Result<int, int> =
                try {
                    throw(5)
                }
                .catch |e| {
                    state[0] = e;
                }
                .finally {
                    state[0] = state[0] + 1;
                };

            state[0]
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), AbiValue::Int(6));
}

#[test]
fn try_catch_finally_runs_finally_on_success() {
    let src = r#"
        fn test() -> int {
            let state = [0];

            let r =
                try {
                    state[0] = 10;
                    7
                }
                .catch |e: int| {
                    // should not run
                    state[0] = 99;
                    let _ = e;
                }
                .finally {
                    state[0] = state[0] + 1;
                };

            match r {
                Result::Ok(v) => state[0] + v,
                Result::Err(_) => -1,
            }
        }
    "#;

    assert_eq!(run0(src, "test").expect("run"), AbiValue::Int(18));
}
