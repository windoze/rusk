use rusk_bytecode::{ConstValue, ExecutableModule, Function, Instruction};
use rusk_vm::{StepResult, Vm, VmMetrics, vm_step};

fn module_with_function(code: Vec<Instruction>, reg_count: u32) -> ExecutableModule {
    let func = Function {
        name: "main".to_string(),
        reg_count,
        param_count: 0,
        code,
    };
    let mut module = ExecutableModule::default();
    module.add_function(func).unwrap();
    module.entry = rusk_bytecode::FunctionId(0);
    module
}

#[test]
fn vm_metrics_are_disabled_by_default() {
    let module = module_with_function(vec![Instruction::Return { value: 0 }], /*reg_count*/ 1);
    let mut vm = Vm::new(module).expect("vm init");
    assert_eq!(vm.metrics(), &VmMetrics::default());

    let _ = vm_step(&mut vm, None);
    assert_eq!(
        vm.metrics(),
        &VmMetrics::default(),
        "metrics should remain zero unless enabled"
    );
}

#[test]
fn vm_metrics_count_basic_instructions() {
    let module = module_with_function(
        vec![
            Instruction::Const {
                dst: 0,
                value: ConstValue::Int(1),
            },
            Instruction::Copy { dst: 1, src: 0 },
            Instruction::Return { value: 1 },
        ],
        /*reg_count*/ 2,
    );

    let mut vm = Vm::new(module).expect("vm init");
    vm.enable_metrics(true);
    vm.reset_metrics();

    let got = vm_step(&mut vm, None);
    assert_eq!(
        got,
        StepResult::Done {
            value: rusk_vm::AbiValue::Int(1),
        }
    );

    let metrics = vm.take_metrics();
    assert_eq!(metrics.executed_instructions, 3);
    assert_eq!(metrics.const_instructions, 1);
    assert_eq!(metrics.copy_instructions, 1);
    assert_eq!(metrics.return_instructions, 1);
    assert_eq!(metrics.other_instructions, 0);
}
