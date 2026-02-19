use crate::compiler::compile_to_mir;
use rusk_mir::{CallTarget, Instruction};

fn contains_call_to(func: &rusk_mir::Function, callee: rusk_mir::FunctionId) -> bool {
    func.blocks.iter().any(|block| {
        block.instructions.iter().any(|instr| match instr {
            Instruction::CallId {
                func: CallTarget::Mir(id),
                ..
            } => *id == callee,
            Instruction::Call { func, .. } => func == "helper",
            _ => false,
        })
    })
}

#[test]
fn inlines_small_multiblock_helpers() {
    let src = r#"
        fn helper(x: int) -> int {
            if x < 0 { x + 10 } else { x }
        }

        fn main() -> int {
            helper(-1)
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let main_id = module.function_id("main").expect("main id");
    let helper_id = module.function_id("helper").expect("helper id");

    let main = module.function(main_id).expect("main function");
    let helper = module.function(helper_id).expect("helper function");
    assert!(
        helper.blocks.len() > 1,
        "expected helper to lower to multiple blocks"
    );

    assert!(
        !contains_call_to(main, helper_id),
        "expected helper call to be inlined"
    );
}
