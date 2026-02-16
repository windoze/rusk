use crate::compiler::compile_to_mir;
use rusk_mir::Instruction;

fn count_index_gets(func: &rusk_mir::Function) -> usize {
    func.blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter(|instr| matches!(instr, Instruction::IndexGet { .. }))
        .count()
}

#[test]
fn lambda_does_not_capture_unused_bindings() {
    let src = r#"
        fn test() -> int {
            const a = 1;
            const b = 2;
            let f = |x: int| { x };
            f(3)
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let lambdas: Vec<&rusk_mir::Function> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("$lambda::"))
        .collect();
    assert_eq!(lambdas.len(), 1, "expected exactly one lambda");
    assert_eq!(
        count_index_gets(lambdas[0]),
        0,
        "capture-free lambda should not load from env"
    );
}

#[test]
fn lambda_captures_only_used_free_vars() {
    let src = r#"
        fn test() -> int {
            const a = 40;
            const b = 999;
            let f = |x: int| { a + x };
            f(2)
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let lambdas: Vec<&rusk_mir::Function> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("$lambda::"))
        .collect();
    assert_eq!(lambdas.len(), 1, "expected exactly one lambda");
    assert_eq!(
        count_index_gets(lambdas[0]),
        1,
        "lambda should only load the one captured binding"
    );
}

#[test]
fn match_helpers_do_not_capture_unused_bindings() {
    let src = r#"
        interface E { fn boom() -> int; }

        fn test() -> int {
            const a = 1;
            const b = 2;
            match @E.boom() {
                @E.boom() => resume(1)
                v => v
            }
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let helpers: Vec<&rusk_mir::Function> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("$match::"))
        .collect();
    assert_eq!(helpers.len(), 1, "expected exactly one match helper");
    assert!(
        helpers[0].params.is_empty(),
        "capture-free match helper should take no captured params"
    );
}
