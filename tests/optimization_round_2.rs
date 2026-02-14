use rusk_compiler::compile_to_mir;
use rusk_interpreter::{Interpreter, RuntimeError, Value, register_core_host_fns};
use rusk_mir::{
    BasicBlock, CallTarget, ConstValue, EffectSpec, Function, HandlerClause, HostFnSig, HostImport,
    HostType, Instruction, Local, Module, Operand, Pattern, Terminator, Type,
};

fn l(i: usize) -> Local {
    Local(i)
}

#[test]
fn core_intrinsic_callid_does_not_count_as_host_call() {
    let mut module = Module::default();
    let int_add_id = module
        .add_host_import(HostImport {
            name: "core::intrinsics::int_add".to_string(),
            sig: HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int,
            },
        })
        .unwrap();

    module
        .add_function(Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 3,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(1),
                    },
                    Instruction::Const {
                        dst: l(1),
                        value: ConstValue::Int(2),
                    },
                    Instruction::CallId {
                        dst: Some(l(2)),
                        func: CallTarget::Host(int_add_id),
                        args: vec![Operand::Local(l(0)), Operand::Local(l(1))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(2)),
                },
            }],
        })
        .unwrap();

    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);

    interp.reset_metrics();
    let out = interp.run_function("main", vec![]).expect("run");
    assert_eq!(out, Value::Int(3));

    let metrics = interp.take_metrics();
    assert_eq!(metrics.call_instructions, 1);
    assert_eq!(
        metrics.host_calls, 0,
        "core intrinsics should not go through host closure dispatch"
    );
}

#[test]
fn argument_buffer_is_cleared_between_calls() {
    let mut module = Module::default();
    module
        .add_function(Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 4,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(1),
                    },
                    Instruction::Const {
                        dst: l(1),
                        value: ConstValue::Int(2),
                    },
                    Instruction::Call {
                        dst: Some(l(2)),
                        func: "sum2".to_string(),
                        args: vec![Operand::Local(l(0)), Operand::Local(l(1))],
                    },
                    Instruction::Call {
                        dst: Some(l(3)),
                        func: "zero".to_string(),
                        args: vec![],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(3)),
                },
            }],
        })
        .unwrap();

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("sum2", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("sum2: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("zero", |_interp, args| match args {
        [] => Ok(Value::Int(0)),
        other => Err(RuntimeError::Trap {
            message: format!("zero: bad args: {other:?}"),
        }),
    });

    let out = interp.run_function("main", vec![]).expect("run");
    assert_eq!(out, Value::Int(0));
}

#[test]
fn host_call_can_resume_continuation_without_clobbering_args() {
    let mut module = Module::default();

    // fn process() -> int {
    //   let x = perform Logger.log("hi");
    //   return inner(x);
    // }
    module
        .add_function(Function {
            name: "process".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Perform {
                        dst: Some(l(0)),
                        effect: EffectSpec {
                            interface: "Logger".to_string(),
                            interface_args: vec![],
                            method: "log".to_string(),
                        },
                        args: vec![Operand::Literal(ConstValue::String("hi".to_string()))],
                    },
                    Instruction::Call {
                        dst: Some(l(1)),
                        func: "inner".to_string(),
                        args: vec![Operand::Local(l(0))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        })
        .unwrap();

    // fn main() -> int {
    //   push_handler H0 { Logger.log(%msg) -> block_log }
    //   let r = call process()
    //   pop_handler
    //   return r
    //
    // block_log(%msg, %k):
    //   return host_handle(%msg, %k)
    // }
    module
        .add_function(Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 4,
            blocks: vec![
                BasicBlock {
                    label: "entry".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "Logger".to_string(),
                                    interface_args: vec![],
                                    method: "log".to_string(),
                                },
                                arg_patterns: vec![Pattern::Bind],
                                target: rusk_mir::BlockId(1),
                            }],
                        },
                        Instruction::Call {
                            dst: Some(l(2)),
                            func: "process".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(2)),
                    },
                },
                BasicBlock {
                    label: "block_log".to_string(),
                    params: vec![l(0), l(1)],
                    instructions: vec![Instruction::Call {
                        dst: Some(l(3)),
                        func: "host_handle".to_string(),
                        args: vec![Operand::Local(l(0)), Operand::Local(l(1))],
                    }],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(3)),
                    },
                },
            ],
        })
        .unwrap();

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("inner", |_interp, args| match args {
        [Value::Int(v)] => Ok(Value::Int(v + 1)),
        other => Err(RuntimeError::Trap {
            message: format!("inner: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("host_handle", |interp, args| match args {
        [Value::String(msg), Value::Continuation(token)] => {
            let msg_len = msg.len() as i64;
            let value = interp.resume_continuation(token.clone(), Value::Int(msg_len))?;
            if msg.len() as i64 != msg_len {
                return Err(RuntimeError::Trap {
                    message: "host_handle: msg mutated across resume".to_string(),
                });
            }
            Ok(value)
        }
        other => Err(RuntimeError::Trap {
            message: format!("host_handle: bad args: {other:?}"),
        }),
    });

    let out = interp.run_function("main", vec![]).expect("run");
    assert_eq!(out, Value::Int(3));
}

#[test]
fn compiler_lowers_int_ops_to_mir_opcodes() {
    let src = r#"
        fn test() -> int {
            let i = 0;
            while i < 10 {
                i = i + 1;
            };
            i
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    assert!(
        module.host_import_id("core::intrinsics::int_add").is_none(),
        "int_add should no longer be a host import once lowered to a MIR opcode"
    );
    assert!(
        module.host_import_id("core::intrinsics::int_lt").is_none(),
        "int_lt should no longer be a host import once lowered to a MIR opcode"
    );

    let func_id = module.function_id("test").expect("function id");
    let func = module.function(func_id).expect("function");
    let mut saw_add = false;
    let mut saw_lt = false;
    for block in &func.blocks {
        for instr in &block.instructions {
            match instr {
                Instruction::IntAdd { .. } => saw_add = true,
                Instruction::IntLt { .. } => saw_lt = true,
                _ => {}
            }
        }
    }
    assert!(saw_add, "expected at least one IntAdd instruction");
    assert!(saw_lt, "expected at least one IntLt instruction");

    let mut interp = Interpreter::new(module);
    interp.reset_metrics();
    let out = interp.run_function("test", vec![]).expect("run");
    assert_eq!(out, Value::Int(10));

    let metrics = interp.take_metrics();
    assert_eq!(
        metrics.call_instructions, 0,
        "int ops should not execute via call instructions"
    );
    assert_eq!(
        metrics.host_calls, 0,
        "int ops should not execute via host calls"
    );
}
