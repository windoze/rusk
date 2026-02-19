use rusk_bytecode::{CallTarget, Instruction, OptLevel};
use rusk_compiler::{CompileOptions, compile_to_bytecode_with_options};
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

fn compile_o2(src: &str) -> rusk_bytecode::ExecutableModule {
    let options = CompileOptions {
        opt_level: OptLevel::O2,
        ..CompileOptions::default()
    };
    compile_to_bytecode_with_options(src, &options).expect("compile")
}

fn func_by_name<'a>(
    module: &'a rusk_bytecode::ExecutableModule,
    name: &str,
) -> &'a rusk_bytecode::Function {
    let id = module
        .function_id(name)
        .unwrap_or_else(|| panic!("missing function `{name}`"));
    module.function(id).expect("valid function id")
}

fn count_instr(
    func: &rusk_bytecode::Function,
    mut pred: impl FnMut(&Instruction) -> bool,
) -> usize {
    func.code.iter().filter(|i| pred(i)).count()
}

#[test]
fn option_unboxed_return_eliminates_heap_allocation() {
    let src = r#"
        fn maybe_inc(x: int) -> Option<int> {
            if x > 0 { Option::Some(x + 1) } else { Option::None }
        }

        fn main() -> int {
            let a = match maybe_inc(1) {
                Option::Some(v) => v,
                Option::None => 0,
            };
            let b = match maybe_inc(-1) {
                Option::Some(v) => v,
                Option::None => 0,
            };
            a + b
        }
    "#;

    let module = compile_o2(src);
    let option_id = module.type_id("Option").expect("Option type id");

    // Callee: should have an internal `$unboxed` variant with no `MakeEnum Option`.
    let unboxed = func_by_name(&module, "maybe_inc$unboxed");
    assert_eq!(
        count_instr(
            unboxed,
            |i| matches!(i, Instruction::MakeEnum { enum_type_id, .. } if *enum_type_id == option_id)
        ),
        0,
        "unboxed variant should not allocate Option"
    );
    assert!(
        unboxed
            .code
            .iter()
            .any(|i| matches!(i, Instruction::ReturnMulti { values } if values.len() == 2)),
        "unboxed variant should return 2 values"
    );

    // Caller: should use `CallMulti` + `JumpIf` instead of `Switch`.
    let main = func_by_name(&module, "main");
    let unboxed_id = module.function_id("maybe_inc$unboxed").expect("unboxed id");

    assert!(
        main.code.iter().any(|i| matches!(
            i,
            Instruction::CallMulti { func: CallTarget::Bc(fid), dsts, .. }
                if *fid == unboxed_id && dsts.len() == 2
        )),
        "main should call maybe_inc$unboxed via CallMulti"
    );
    assert_eq!(
        count_instr(main, |i| matches!(i, Instruction::Switch { .. })),
        0,
        "main should not use Switch for immediate Option match at O2"
    );
    assert!(
        main.code
            .iter()
            .any(|i| matches!(i, Instruction::JumpIf { .. })),
        "main should branch on the unboxed tag"
    );

    // Behavior: still correct.
    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(2)
        }
    );
}

#[test]
fn option_make_enum_followed_by_match_is_sroa_eliminated() {
    let src = r#"
        fn main() -> int {
            let a = match Option::Some(5) {
                Option::Some(v) => v,
                Option::None => 0,
            };
            let b = match Option::None {
                Option::Some(v) => v,
                Option::None => 1,
            };
            a + b
        }
    "#;

    let module = compile_o2(src);
    let option_id = module.type_id("Option").expect("Option type id");
    let main = func_by_name(&module, "main");

    assert_eq!(
        count_instr(
            main,
            |i| matches!(i, Instruction::MakeEnum { enum_type_id, .. } if *enum_type_id == option_id)
        ),
        0,
        "immediate match should not allocate Option at O2"
    );
    assert_eq!(
        count_instr(main, |i| matches!(i, Instruction::Switch { .. })),
        0,
        "immediate Option match should lower to JumpIf at O2"
    );

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(6)
        }
    );
}

#[test]
fn checked_cast_followed_by_match_avoids_allocating_outer_option() {
    let src = r#"
        interface I { fn id() -> int; }

        struct S { n: int }
        struct T { n: int }

        impl I for S {
            fn id() -> int { self.n }
        }

        impl I for T {
            fn id() -> int { self.n }
        }

        fn main() -> int {
            let x: I = (S { n: 1 } as I);
            let y: I = (T { n: 2 } as I);
            let a = match x as? S {
                Option::Some(_) => 1,
                Option::None => 0,
            };
            let b = match y as? S {
                Option::Some(_) => 1,
                Option::None => 0,
            };
            a + b
        }
    "#;

    let module = compile_o2(src);
    let main = func_by_name(&module, "main");

    assert_eq!(
        count_instr(main, |i| matches!(i, Instruction::CheckedCast { .. })),
        0,
        "checked-cast + immediate match should be lowered to IsType + JumpIf at O2"
    );
    assert!(
        main.code
            .iter()
            .any(|i| matches!(i, Instruction::IsType { .. })),
        "checked-cast match fast path should use IsType"
    );
    assert_eq!(
        count_instr(main, |i| matches!(i, Instruction::Switch { .. })),
        0,
        "checked-cast match fast path should not use Switch"
    );

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(1)
        }
    );
}
