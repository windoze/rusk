use rusk_compiler::{CompileOptions, OptLevel, compile_to_bytecode_with_options};
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn compile_with_opt_level(src: &str, opt_level: OptLevel) -> rusk_bytecode::ExecutableModule {
    let options = CompileOptions {
        opt_level,
        ..Default::default()
    };
    compile_to_bytecode_with_options(src, &options).expect("compile")
}

fn run_to_completion(module: &rusk_bytecode::ExecutableModule) -> StepResult {
    let mut vm = Vm::new(module.clone()).expect("vm init");
    match vm_step(&mut vm, None) {
        StepResult::Done { value } => StepResult::Done { value },
        StepResult::Trap { message } => StepResult::Trap { message },
        StepResult::Yield { .. } => panic!("unexpected yield"),
        StepResult::Request { .. } => panic!("unexpected external request"),
    }
}

#[test]
fn peephole_o1_rewrites_resume_return_into_resume_tail() {
    let src = r#"
interface Tick { fn tick(n: int) -> int; }

fn main() -> int {
    match @Tick.tick(1) {
        @Tick.tick(n) => resume(n + 1)
        v => v
    }
}
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o1 = compile_with_opt_level(src, OptLevel::O1);

    let count_tail = |m: &rusk_bytecode::ExecutableModule| {
        m.functions
            .iter()
            .flat_map(|f| f.code.iter())
            .filter(|i| matches!(i, rusk_bytecode::Instruction::ResumeTail { .. }))
            .count()
    };

    assert_eq!(
        count_tail(&module_o0),
        0,
        "O0 should not contain ResumeTail"
    );
    if count_tail(&module_o1) == 0 {
        eprintln!("O1 module contains no ResumeTail; dumping match helper bytecode:");
        for func in &module_o1.functions {
            if !func.name.contains("$match") {
                continue;
            }
            eprintln!("function {}:", func.name);
            for (pc, instr) in func.code.iter().enumerate() {
                eprintln!("  {pc}: {instr:?}");
            }
        }
    }
    assert!(
        count_tail(&module_o1) > 0,
        "O1 should contain ResumeTail after peephole optimization"
    );
    assert_eq!(
        run_to_completion(&module_o1),
        StepResult::Done {
            value: AbiValue::Int(2)
        }
    );
}

#[test]
fn peephole_o1_matches_o0_semantics_on_branch_switch_and_handler() {
    let src = r#"
interface Tick { fn tick(n: int) -> int; }

fn main() -> int {
    let x = 1;
    let y = if x > 0 { 1 } else { 2 };
    let z = match y { 1 => 10, _ => 20 };
    match @Tick.tick(z) {
        @Tick.tick(n) => resume(n + 1)
        v => v
    }
}
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o1 = compile_with_opt_level(src, OptLevel::O1);

    rusk_bytecode::verify_module(&module_o0).expect("verify O0");
    rusk_bytecode::verify_module(&module_o1).expect("verify O1");

    let got_o0 = run_to_completion(&module_o0);
    let got_o1 = run_to_completion(&module_o1);

    assert_eq!(
        got_o0,
        StepResult::Done {
            value: AbiValue::Int(11)
        }
    );
    assert_eq!(got_o1, got_o0);
}

#[test]
fn peephole_o2_matches_o0_semantics_on_branch_switch_and_handler() {
    let src = r#"
interface Tick { fn tick(n: int) -> int; }

fn main() -> int {
    let x = 1;
    let y = if x > 0 { 1 } else { 2 };
    let z = match y { 1 => 10, _ => 20 };
    match @Tick.tick(z) {
        @Tick.tick(n) => resume(n + 1)
        v => v
    }
}
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o2 = compile_with_opt_level(src, OptLevel::O2);

    rusk_bytecode::verify_module(&module_o0).expect("verify O0");
    rusk_bytecode::verify_module(&module_o2).expect("verify O2");

    let got_o0 = run_to_completion(&module_o0);
    let got_o2 = run_to_completion(&module_o2);

    assert_eq!(
        got_o0,
        StepResult::Done {
            value: AbiValue::Int(11)
        }
    );
    assert_eq!(got_o2, got_o0);
}

#[test]
fn peephole_o2_matches_o0_semantics_on_while_loop_phi_like_updates() {
    let src = r#"
fn main() -> int {
    let n = 1000;
    let i = 0;
    let acc = 0;
    while i < n {
        acc = acc + i;
        i = i + 1;
    };
    acc
}
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o2 = compile_with_opt_level(src, OptLevel::O2);

    rusk_bytecode::verify_module(&module_o0).expect("verify O0");
    rusk_bytecode::verify_module(&module_o2).expect("verify O2");

    let got_o0 = run_to_completion(&module_o0);
    let got_o2 = run_to_completion(&module_o2);

    // sum_{i=0}^{999} i = 999*1000/2 = 499500
    assert_eq!(
        got_o0,
        StepResult::Done {
            value: AbiValue::Int(499500),
        }
    );
    assert_eq!(got_o2, got_o0);
}

#[test]
fn peephole_o1_threads_jumpif_trampoline_edges() {
    let src = r#"
fn main() -> int {
    let x = 1;
    if x > 0 { 1 } else { 2 }
}
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o1 = compile_with_opt_level(src, OptLevel::O1);

    let main_o0 = module_o0.function(module_o0.entry).expect("main O0");
    let main_o1 = module_o1.function(module_o1.entry).expect("main O1");

    let (then_pc_o0, else_pc_o0) = main_o0
        .code
        .iter()
        .find_map(|i| match i {
            rusk_bytecode::Instruction::JumpIf {
                then_pc, else_pc, ..
            } => Some((*then_pc, *else_pc)),
            _ => None,
        })
        .expect("expected a JumpIf in O0 bytecode");

    let (then_pc_o1, else_pc_o1) = main_o1
        .code
        .iter()
        .find_map(|i| match i {
            rusk_bytecode::Instruction::JumpIf {
                then_pc, else_pc, ..
            } => Some((*then_pc, *else_pc)),
            _ => None,
        })
        .expect("expected a JumpIf in O1 bytecode");

    assert!(
        matches!(
            main_o0.code[then_pc_o0 as usize],
            rusk_bytecode::Instruction::Jump { .. }
        ),
        "O0 then_pc should target an edge trampoline jump"
    );
    assert!(
        matches!(
            main_o0.code[else_pc_o0 as usize],
            rusk_bytecode::Instruction::Jump { .. }
        ),
        "O0 else_pc should target an edge trampoline jump"
    );

    assert!(
        !matches!(
            main_o1.code[then_pc_o1 as usize],
            rusk_bytecode::Instruction::Jump { .. }
        ),
        "O1 then_pc should skip the trampoline jump"
    );
    assert!(
        !matches!(
            main_o1.code[else_pc_o1 as usize],
            rusk_bytecode::Instruction::Jump { .. }
        ),
        "O1 else_pc should skip the trampoline jump"
    );
}

#[test]
fn peephole_o1_constant_folds_simple_int_add() {
    let src = r#"
fn main() -> int { 1 + 2 }
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o1 = compile_with_opt_level(src, OptLevel::O1);

    let main_o0 = module_o0.function(module_o0.entry).expect("main O0");
    let main_o1 = module_o1.function(module_o1.entry).expect("main O1");

    let o0_int_adds = main_o0
        .code
        .iter()
        .filter(|i| matches!(i, rusk_bytecode::Instruction::IntAdd { .. }))
        .count();
    let o1_int_adds = main_o1
        .code
        .iter()
        .filter(|i| matches!(i, rusk_bytecode::Instruction::IntAdd { .. }))
        .count();

    assert!(o0_int_adds >= 1, "expected O0 to contain IntAdd");
    assert_eq!(o1_int_adds, 0, "expected O1 to fold away IntAdd");
    assert_eq!(
        run_to_completion(&module_o1),
        StepResult::Done {
            value: AbiValue::Int(3),
        }
    );
}

#[test]
fn peephole_o1_does_not_fold_division_by_zero() {
    let src = r#"
fn main() -> int { 1 / 0 }
"#;

    let module_o0 = compile_with_opt_level(src, OptLevel::O0);
    let module_o1 = compile_with_opt_level(src, OptLevel::O1);

    let main_o1 = module_o1.function(module_o1.entry).expect("main O1");
    assert!(
        main_o1
            .code
            .iter()
            .any(|i| matches!(i, rusk_bytecode::Instruction::IntDiv { .. })),
        "expected O1 to keep IntDiv for division-by-zero semantics"
    );

    let StepResult::Trap { message: msg0 } = run_to_completion(&module_o0) else {
        panic!("expected O0 to trap");
    };
    let StepResult::Trap { message: msg1 } = run_to_completion(&module_o1) else {
        panic!("expected O1 to trap");
    };

    assert!(msg0.contains("division by zero"), "O0 msg={msg0}");
    assert!(msg1.contains("division by zero"), "O1 msg={msg1}");
}
