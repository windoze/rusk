use rusk_compiler::{CompileOptions, compile_to_mir_with_options_and_metrics};
use rusk_interpreter::{Interpreter, InterpreterMetrics, Value};
use rusk_mir::{BasicBlock, ConstValue, Function, Instruction, Local, Module, Operand, Terminator};

#[test]
fn compiler_metrics_exist_and_are_consistent() {
    let src = r#"
fn main() {
    let x = 1;
    x;
}
"#;

    let options = CompileOptions::default();
    let (_module, metrics) =
        compile_to_mir_with_options_and_metrics(src, &options).expect("compile");

    // These are timing measurements; they may be zero on fast machines / coarse timers, but the
    // total should always be >= the sum of the staged timers we record.
    assert!(metrics.total_time >= metrics.parse_time);
    assert!(metrics.total_time >= metrics.typecheck_time);
    assert!(metrics.total_time >= metrics.lower_time);
    assert!(metrics.total_time >= metrics.parse_time + metrics.typecheck_time + metrics.lower_time);
}

#[test]
fn interpreter_metrics_count_executed_instructions_and_terminators() {
    let mut module = Module::default();
    module
        .add_function(Function {
            name: "main".to_string(),
            params: Vec::new(),
            ret_type: None,
            locals: 1,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: Vec::new(),
                instructions: vec![Instruction::Const {
                    dst: Local(0),
                    value: ConstValue::Int(1),
                }],
                terminator: Terminator::Return {
                    value: Operand::Local(Local(0)),
                },
            }],
        })
        .unwrap();

    let mut interp = Interpreter::new(module);
    interp.reset_metrics();
    let result = interp.run_function("main", vec![]).expect("run");
    assert!(matches!(result, Value::Int(1)));

    let metrics = interp.take_metrics();
    assert_eq!(
        metrics,
        InterpreterMetrics {
            executed_instructions: 1,
            executed_terminators: 1,
            block_entries: 0,
            allocations: 0,
            gc_cycles: 0,
            gc_nanos: 0,
            call_instructions: 0,
            icall_instructions: 0,
            vcall_instructions: 0,
            host_calls: 0,
            mir_calls: 0,
            br_terminators: 0,
            cond_br_terminators: 0,
            switch_terminators: 0,
            return_terminators: 1,
            trap_terminators: 0,
        }
    );
}
