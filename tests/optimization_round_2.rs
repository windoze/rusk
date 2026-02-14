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
            locals: 6,
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
                    Instruction::Const {
                        dst: l(2),
                        value: ConstValue::Int(3),
                    },
                    Instruction::Const {
                        dst: l(3),
                        value: ConstValue::Int(4),
                    },
                    Instruction::Call {
                        dst: Some(l(4)),
                        func: "sum3".to_string(),
                        args: vec![
                            Operand::Local(l(0)),
                            Operand::Local(l(1)),
                            Operand::Local(l(2)),
                        ],
                    },
                    Instruction::Call {
                        dst: Some(l(5)),
                        func: "sum4".to_string(),
                        args: vec![
                            Operand::Local(l(0)),
                            Operand::Local(l(1)),
                            Operand::Local(l(2)),
                            Operand::Local(l(3)),
                        ],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(5)),
                },
            }],
        })
        .unwrap();

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("sum3", |_interp, args| match args {
        [Value::Int(a), Value::Int(b), Value::Int(c)] => Ok(Value::Int(a + b + c)),
        other => Err(RuntimeError::Trap {
            message: format!("sum3: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("sum4", |_interp, args| match args {
        [Value::Int(a), Value::Int(b), Value::Int(c), Value::Int(d)] => {
            Ok(Value::Int(a + b + c + d))
        }
        other => Err(RuntimeError::Trap {
            message: format!("sum4: bad args: {other:?}"),
        }),
    });

    let out = interp.run_function("main", vec![]).expect("run");
    assert_eq!(out, Value::Int(10));
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
            locals: 5,
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
                    instructions: vec![
                        Instruction::Const {
                            dst: l(3),
                            value: ConstValue::Int(123),
                        },
                        Instruction::Call {
                            dst: Some(l(4)),
                            func: "host_handle".to_string(),
                            args: vec![
                                Operand::Local(l(0)),
                                Operand::Local(l(1)),
                                Operand::Local(l(3)),
                            ],
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(4)),
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
        [
            Value::String(msg),
            Value::Continuation(token),
            Value::Int(extra),
        ] => {
            if *extra != 123 {
                return Err(RuntimeError::Trap {
                    message: format!("host_handle: unexpected extra arg: {extra}"),
                });
            }
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

#[test]
fn compiler_lowers_int_and_bool_ops_to_mir_opcodes() {
    let src = r#"
        fn test(a: int, b: int, p: bool, q: bool) -> int {
            let c = a + b;
            let d = a - b;
            let e = a * b;
            let f = a / b;
            let g = a % b;

            let h = if a < b { 1 } else { 0 };
            let i = if a <= b { 1 } else { 0 };
            let j = if a > b { 1 } else { 0 };
            let k = if a >= b { 1 } else { 0 };
            let l = if a == b { 1 } else { 0 };
            let m = if a != b { 1 } else { 0 };

            let r = !p;
            let s = p == q;
            let t = p != q;

            c + d + e + f + g + h + i + j + k + l + m
                + (if r { 1 } else { 0 })
                + (if s { 1 } else { 0 })
                + (if t { 1 } else { 0 })
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    for name in [
        "core::intrinsics::int_add",
        "core::intrinsics::int_sub",
        "core::intrinsics::int_mul",
        "core::intrinsics::int_div",
        "core::intrinsics::int_mod",
        "core::intrinsics::int_lt",
        "core::intrinsics::int_le",
        "core::intrinsics::int_gt",
        "core::intrinsics::int_ge",
        "core::intrinsics::int_eq",
        "core::intrinsics::int_ne",
        "core::intrinsics::bool_not",
        "core::intrinsics::bool_eq",
        "core::intrinsics::bool_ne",
    ] {
        assert!(
            module.host_import_id(name).is_none(),
            "{name} should be lowered to a MIR opcode, not a host import",
        );
    }

    let func_id = module.function_id("test").expect("function id");
    let func = module.function(func_id).expect("function");
    let mut saw_add = false;
    let mut saw_sub = false;
    let mut saw_mul = false;
    let mut saw_div = false;
    let mut saw_mod = false;
    let mut saw_lt = false;
    let mut saw_le = false;
    let mut saw_gt = false;
    let mut saw_ge = false;
    let mut saw_eq = false;
    let mut saw_ne = false;
    let mut saw_not = false;
    let mut saw_bool_eq = false;
    let mut saw_bool_ne = false;
    for block in &func.blocks {
        for instr in &block.instructions {
            match instr {
                Instruction::IntAdd { .. } => saw_add = true,
                Instruction::IntSub { .. } => saw_sub = true,
                Instruction::IntMul { .. } => saw_mul = true,
                Instruction::IntDiv { .. } => saw_div = true,
                Instruction::IntMod { .. } => saw_mod = true,
                Instruction::IntLt { .. } => saw_lt = true,
                Instruction::IntLe { .. } => saw_le = true,
                Instruction::IntGt { .. } => saw_gt = true,
                Instruction::IntGe { .. } => saw_ge = true,
                Instruction::IntEq { .. } => saw_eq = true,
                Instruction::IntNe { .. } => saw_ne = true,
                Instruction::BoolNot { .. } => saw_not = true,
                Instruction::BoolEq { .. } => saw_bool_eq = true,
                Instruction::BoolNe { .. } => saw_bool_ne = true,
                _ => {}
            }
        }
    }

    assert!(saw_add, "expected at least one IntAdd instruction");
    assert!(saw_sub, "expected at least one IntSub instruction");
    assert!(saw_mul, "expected at least one IntMul instruction");
    assert!(saw_div, "expected at least one IntDiv instruction");
    assert!(saw_mod, "expected at least one IntMod instruction");
    assert!(saw_lt, "expected at least one IntLt instruction");
    assert!(saw_le, "expected at least one IntLe instruction");
    assert!(saw_gt, "expected at least one IntGt instruction");
    assert!(saw_ge, "expected at least one IntGe instruction");
    assert!(saw_eq, "expected at least one IntEq instruction");
    assert!(saw_ne, "expected at least one IntNe instruction");
    assert!(saw_not, "expected at least one BoolNot instruction");
    assert!(saw_bool_eq, "expected at least one BoolEq instruction");
    assert!(saw_bool_ne, "expected at least one BoolNe instruction");

    let mut interp = Interpreter::new(module);
    interp.reset_metrics();
    let out = interp
        .run_function(
            "test",
            vec![
                Value::Int(10),
                Value::Int(3),
                Value::Bool(true),
                Value::Bool(false),
            ],
        )
        .expect("run");
    assert_eq!(out, Value::Int(58));

    let metrics = interp.take_metrics();
    assert_eq!(
        metrics.call_instructions, 0,
        "int/bool ops should not execute via call instructions"
    );
    assert_eq!(
        metrics.host_calls, 0,
        "int/bool ops should not execute via host calls"
    );
}

#[test]
fn int_division_by_zero_traps() {
    let src = r#"
        fn test(x: int) -> int {
            1 / x
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    assert!(
        module.host_import_id("core::intrinsics::int_div").is_none(),
        "int_div should be lowered to a MIR opcode, not a host import"
    );

    let func_id = module.function_id("test").expect("function id");
    let func = module.function(func_id).expect("function");
    assert!(
        func.blocks.iter().any(|block| block
            .instructions
            .iter()
            .any(|instr| matches!(instr, Instruction::IntDiv { .. }))),
        "expected IntDiv instruction"
    );

    let mut interp = Interpreter::new(module);
    let err = interp
        .run_function("test", vec![Value::Int(0)])
        .expect_err("should trap");
    match err {
        RuntimeError::Trap { message } => {
            assert!(
                message.contains("division by zero"),
                "unexpected trap message: {message:?}"
            );
        }
        other => panic!("expected trap, got {other:?}"),
    }
}

fn count_cell_allocs(func: &rusk_mir::Function) -> usize {
    func.blocks
        .iter()
        .flat_map(|block| block.instructions.iter())
        .filter(|instr| match instr {
            Instruction::MakeStruct { type_name, .. } => type_name == "$Cell",
            _ => false,
        })
        .count()
}

#[test]
fn uncaptured_let_bindings_do_not_allocate_cells() {
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
    let func_id = module.function_id("test").expect("function id");
    let func = module.function(func_id).expect("function");
    assert_eq!(
        count_cell_allocs(func),
        0,
        "uncaptured `let` bindings should not allocate `$Cell`"
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("test", vec![]).expect("run");
    assert_eq!(out, Value::Int(10));
}

#[test]
fn captured_let_bindings_allocate_cells() {
    let src = r#"
        fn test() -> int {
            let x = 0;
            let add = |n: int| {
                x = x + n;
                x
            };
            add(1);
            add(2);
            x
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let func_id = module.function_id("test").expect("function id");
    let func = module.function(func_id).expect("function");
    assert_eq!(
        count_cell_allocs(func),
        1,
        "expected only the captured `let x` to allocate a `$Cell`"
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("test", vec![]).expect("run");
    assert_eq!(out, Value::Int(3));
}

#[test]
fn tiny_functions_are_inlined_to_eliminate_call_overhead() {
    let src = r#"
        fn inc(x: int) -> int { x + 1 }

        fn test() -> int {
            let i = 0;
            let acc = 0;
            while i < 1000 {
                acc = inc(acc);
                i = i + 1;
            };
            acc
        }
    "#;

    let module = compile_to_mir(src).expect("compile");
    let test_id = module.function_id("test").expect("test id");
    let inc_id = module.function_id("inc").expect("inc id");
    let func = module.function(test_id).expect("test function");
    assert!(
        func.blocks
            .iter()
            .all(|block| block.instructions.iter().all(|instr| {
                !matches!(
                    instr,
                    Instruction::CallId {
                        func: CallTarget::Mir(id),
                        ..
                    } if *id == inc_id
                )
            })),
        "expected calls to `inc` to be inlined"
    );

    let mut interp = Interpreter::new(module);
    interp.reset_metrics();
    let out = interp.run_function("test", vec![]).expect("run");
    assert_eq!(out, Value::Int(1000));

    let metrics = interp.take_metrics();
    assert_eq!(
        metrics.call_instructions, 0,
        "inlining should remove call instructions in the hot loop"
    );
    assert_eq!(metrics.host_calls, 0);
}
