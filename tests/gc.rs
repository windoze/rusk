use rusk::mir::*;
use rusk::{Interpreter, RuntimeError, Value};

fn l(i: usize) -> Local {
    Local(i)
}

fn b(i: usize) -> BlockId {
    BlockId(i)
}

#[test]
fn gc_collects_unreachable_objects() {
    let mut module = Module::default();
    module.functions.insert(
        "main".to_string(),
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![ConstValue::Int(1)]),
                    },
                    Instruction::Const {
                        dst: l(1),
                        value: ConstValue::Array(vec![ConstValue::Int(2)]),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Unit);

    assert_eq!(interp.heap_live_objects(), 2);
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 0);
}

#[test]
fn gc_preserves_objects_reachable_via_rooted_values() {
    let mut module = Module::default();
    module.functions.insert(
        "make".to_string(),
        Function {
            name: "make".to_string(),
            params: vec![],
            ret_type: Some(Type::Array),
            locals: 1,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![Instruction::Const {
                    dst: l(0),
                    value: ConstValue::Array(vec![
                        ConstValue::Int(1),
                        ConstValue::Int(2),
                        ConstValue::Int(3),
                    ]),
                }],
                terminator: Terminator::Return {
                    value: Operand::Local(l(0)),
                },
            }],
        },
    );
    module.functions.insert(
        "len_of".to_string(),
        Function {
            name: "len_of".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Readonly,
                ty: Some(Type::Array),
            }],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![Instruction::Len {
                    dst: l(1),
                    arr: Operand::Local(l(0)),
                }],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let arr = interp.run_function("make", vec![]).unwrap();
    let root = interp.root_value(arr.clone());

    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 1);

    let len = interp.run_function("len_of", vec![arr.clone()]).unwrap();
    assert_eq!(len, Value::Int(3));

    assert!(interp.unroot_value(root).is_some());
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 0);

    let err = interp.run_function("len_of", vec![arr]).unwrap_err();
    assert!(matches!(err, RuntimeError::DanglingRef { .. }));
}

#[test]
fn gc_traces_through_nested_heap_graphs() {
    let mut module = Module::default();
    module.functions.insert(
        "make_nested".to_string(),
        Function {
            name: "make_nested".to_string(),
            params: vec![],
            ret_type: Some(Type::Struct("Box".to_string())),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![ConstValue::Int(7)]),
                    },
                    Instruction::MakeStruct {
                        dst: l(1),
                        type_name: "Box".to_string(),
                        fields: vec![("inner".to_string(), Operand::Local(l(0)))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        },
    );
    module.functions.insert(
        "len_inner".to_string(),
        Function {
            name: "len_inner".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Readonly,
                ty: Some(Type::Struct("Box".to_string())),
            }],
            ret_type: Some(Type::Int),
            locals: 3,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::GetField {
                        dst: l(1),
                        obj: Operand::Local(l(0)),
                        field: "inner".to_string(),
                    },
                    Instruction::Len {
                        dst: l(2),
                        arr: Operand::Local(l(1)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(2)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let boxed = interp.run_function("make_nested", vec![]).unwrap();
    let root = interp.root_value(boxed.clone());

    assert_eq!(interp.heap_live_objects(), 2);
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 2);

    let len = interp
        .run_function("len_inner", vec![boxed.clone()])
        .unwrap();
    assert_eq!(len, Value::Int(1));

    assert!(interp.unroot_value(root).is_some());
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 0);

    let err = interp.run_function("len_inner", vec![boxed]).unwrap_err();
    assert!(matches!(err, RuntimeError::DanglingRef { .. }));
}

#[test]
fn gc_roots_values_captured_in_continuations() {
    let mut module = Module::default();
    module.functions.insert(
        "main".to_string(),
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: None,
            locals: 3,
            blocks: vec![
                BasicBlock {
                    label: "entry".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::Const {
                            dst: l(0),
                            value: ConstValue::Array(vec![ConstValue::Int(1)]),
                        },
                        Instruction::PushHandler {
                            handler_id: "H".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectId {
                                    interface: "Test".to_string(),
                                    method: "suspend".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Perform {
                            dst: None,
                            effect: EffectId {
                                interface: "Test".to_string(),
                                method: "suspend".to_string(),
                            },
                            args: vec![],
                        },
                        Instruction::PopHandler,
                        Instruction::Len {
                            dst: l(1),
                            arr: Operand::Local(l(0)),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
                BasicBlock {
                    label: "handler".to_string(),
                    params: vec![l(2)],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(2)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);

    let token = match interp.run_function("main", vec![]).unwrap() {
        Value::Continuation(k) => k,
        other => panic!("expected continuation token, got {other:?}"),
    };

    let root = interp.root_value(Value::Continuation(token.clone()));
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 1);

    let resumed = interp
        .resume_continuation(token, Value::Unit)
        .expect("resume should succeed");
    assert_eq!(resumed, Value::Int(1));

    // After resuming, the continuation becomes inert (one-shot), so it no longer
    // keeps the captured heap objects alive.
    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 0);

    assert!(interp.unroot_value(root).is_some());
}

#[test]
fn gc_prevents_stale_handle_use_after_reuse() {
    let mut module = Module::default();

    let make_box = |name: &str, x: i64| Function {
        name: name.to_string(),
        params: vec![],
        ret_type: Some(Type::Struct("Box".to_string())),
        locals: 1,
        blocks: vec![BasicBlock {
            label: "entry".to_string(),
            params: vec![],
            instructions: vec![Instruction::MakeStruct {
                dst: l(0),
                type_name: "Box".to_string(),
                fields: vec![("x".to_string(), Operand::Literal(ConstValue::Int(x)))],
            }],
            terminator: Terminator::Return {
                value: Operand::Local(l(0)),
            },
        }],
    };

    module
        .functions
        .insert("make1".to_string(), make_box("make1", 1));
    module
        .functions
        .insert("make2".to_string(), make_box("make2", 2));

    module.functions.insert(
        "read_x".to_string(),
        Function {
            name: "read_x".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Readonly,
                ty: Some(Type::Struct("Box".to_string())),
            }],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "entry".to_string(),
                params: vec![],
                instructions: vec![Instruction::GetField {
                    dst: l(1),
                    obj: Operand::Local(l(0)),
                    field: "x".to_string(),
                }],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let old = interp.run_function("make1", vec![]).unwrap();
    assert_eq!(interp.heap_live_objects(), 1);

    interp.collect_garbage_now();
    assert_eq!(interp.heap_live_objects(), 0);

    let new = interp.run_function("make2", vec![]).unwrap();
    assert_eq!(interp.heap_live_objects(), 1);

    let x2 = interp.run_function("read_x", vec![new.clone()]).unwrap();
    assert_eq!(x2, Value::Int(2));

    let err = interp.run_function("read_x", vec![old]).unwrap_err();
    assert!(matches!(err, RuntimeError::DanglingRef { .. }));
}

#[test]
fn rooted_value_handles_are_one_shot() {
    let mut interp = Interpreter::new(Module::default());
    let handle = interp.root_value(Value::Int(123));
    assert_eq!(interp.unroot_value(handle), Some(Value::Int(123)));
    assert_eq!(interp.unroot_value(handle), None);
}
