use rusk_bytecode::Instruction;
use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn count_push_handler_clauses(
    module: &rusk_bytecode::ExecutableModule,
) -> Vec<&rusk_bytecode::HandlerClause> {
    let mut out = Vec::new();
    for func in &module.functions {
        for instr in &func.code {
            if let Instruction::PushHandler { clauses } = instr {
                for clause in clauses {
                    out.push(clause);
                }
            }
        }
    }
    out
}

fn handler_clauses_for_effect<'m>(
    module: &'m rusk_bytecode::ExecutableModule,
    interface: &str,
    method: &str,
) -> Vec<&'m rusk_bytecode::HandlerClause> {
    count_push_handler_clauses(module)
        .into_iter()
        .filter(|clause| clause.effect.interface == interface && clause.effect.method == method)
        .collect()
}

#[test]
fn abortive_handler_omits_continuation_param_in_bytecode() {
    let src = r#"
        interface Tick { fn tick(n: int) -> int; }

        fn main() -> int {
            match @Tick.tick(1) {
                @Tick.tick(_) => 99
                v => v
            }
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let clauses = handler_clauses_for_effect(&module, "Tick", "tick");
    assert_eq!(clauses.len(), 1, "expected exactly one handler clause");
    assert_eq!(
        clauses[0].param_regs.len(),
        0,
        "abortive clause should omit continuation param"
    );
}

#[test]
fn continuation_is_kept_when_stored_in_bytecode() {
    let src = r#"
        struct Cell<T> { v: T }
        interface E { fn boom() -> int; }

        fn main() -> int {
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
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let clauses = handler_clauses_for_effect(&module, "E", "boom");
    assert_eq!(clauses.len(), 1, "expected exactly one handler clause");
    assert_eq!(
        clauses[0].param_regs.len(),
        1,
        "clause that references the continuation must include it as a param"
    );
}

#[test]
fn tail_resume_keeps_stack_bounded_in_yield_loop() {
    let n: i64 = 2000;
    let expected_sum: i64 = (n - 1) * n / 2;

    let src = format!(
        r#"
        interface Yield {{ fn yield(n: int) -> unit; }}

        fn do_yields(n: int) -> unit {{
            let i = 0;
            while i < n {{
                @Yield.yield(i);
                i = i + 1;
            }};
            ()
        }}

        fn main() -> int {{
            let acc = 0;
            match do_yields({n}) {{
                @Yield.yield(x) => {{ acc = acc + x; resume(()) }}
                _ => acc
            }}
        }}
    "#
    );

    let module = compile_to_bytecode(&src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    vm.enable_metrics(true);
    vm.reset_metrics();

    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: AbiValue::Int(expected_sum),
        }
    );

    let metrics = vm.take_metrics();
    assert!(
        metrics.resume_tail_instructions > 0,
        "expected tail resumes, got metrics: {metrics:?}"
    );
    assert_eq!(
        metrics.continuations_captured, n as u64,
        "expected one captured continuation per yield"
    );
    assert!(
        metrics.max_frames_len <= 32,
        "expected bounded frame stack, got max_frames_len={}",
        metrics.max_frames_len
    );
    assert!(
        metrics.max_handlers_len <= 32,
        "expected bounded handler stack, got max_handlers_len={}",
        metrics.max_handlers_len
    );
}

#[test]
fn abortive_handler_skips_continuation_capture_in_vm() {
    let src = r#"
        interface Tick { fn tick(n: int) -> int; }

        fn main() -> int {
            match @Tick.tick(1) {
                @Tick.tick(_) => 99
                v => v
            }
        }
    "#;

    let module = compile_to_bytecode(src).expect("compile");
    let mut vm = Vm::new(module).expect("vm init");
    vm.enable_metrics(true);
    vm.reset_metrics();

    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: AbiValue::Int(99)
        }
    );

    let metrics = vm.take_metrics();
    assert_eq!(metrics.continuations_captured, 0);
    assert_eq!(metrics.continuations_skipped_abortive, 1);
}
