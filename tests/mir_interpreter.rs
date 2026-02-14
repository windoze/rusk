use rusk_interpreter::{Interpreter, RuntimeError, Value};
use rusk_mir::*;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

fn l(i: usize) -> Local {
    Local(i)
}

fn b(i: usize) -> BlockId {
    BlockId(i)
}

fn add_fn(module: &mut Module, func: Function) {
    module.add_function(func).unwrap();
}

#[test]
fn runs_simple_host_call() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 3,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(2),
                    },
                    Instruction::Const {
                        dst: l(1),
                        value: ConstValue::Int(3),
                    },
                    Instruction::Call {
                        dst: Some(l(2)),
                        func: "add".to_string(),
                        args: vec![Operand::Local(l(0)), Operand::Local(l(1))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(2)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("add", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
        _ => Err(RuntimeError::Trap {
            message: "bad args".to_string(),
        }),
    });

    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(5));
}

#[test]
fn struct_fields_can_be_read_and_written() {
    let mut module = Module::default();

    let fields = vec![
        ("x".to_string(), Operand::Literal(ConstValue::Int(1))),
        ("y".to_string(), Operand::Literal(ConstValue::Int(2))),
    ];

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 3,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "Point".to_string(),
                        type_args: Vec::new(),
                        fields,
                    },
                    Instruction::GetField {
                        dst: l(1),
                        obj: Operand::Local(l(0)),
                        field: "x".to_string(),
                    },
                    Instruction::SetField {
                        obj: Operand::Local(l(0)),
                        field: "x".to_string(),
                        value: Operand::Literal(ConstValue::Int(10)),
                    },
                    Instruction::GetField {
                        dst: l(2),
                        obj: Operand::Local(l(0)),
                        field: "x".to_string(),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(2)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(10));
}

#[test]
fn readonly_write_traps() {
    let mut module = Module::default();

    let fields = vec![("x".to_string(), Operand::Literal(ConstValue::Int(1)))];

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "Box".to_string(),
                        type_args: Vec::new(),
                        fields,
                    },
                    Instruction::AsReadonly {
                        dst: l(1),
                        src: l(0),
                    },
                    Instruction::SetField {
                        obj: Operand::Local(l(1)),
                        field: "x".to_string(),
                        value: Operand::Literal(ConstValue::Int(2)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::ReadonlyWrite);
}

#[test]
fn array_len_index_get_and_set_work() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 4,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![
                            ConstValue::Int(1),
                            ConstValue::Int(2),
                            ConstValue::Int(3),
                        ]),
                    },
                    Instruction::Len {
                        dst: l(1),
                        arr: Operand::Local(l(0)),
                    },
                    Instruction::IndexGet {
                        dst: l(2),
                        arr: Operand::Local(l(0)),
                        idx: Operand::Literal(ConstValue::Int(1)),
                    },
                    Instruction::IndexSet {
                        arr: Operand::Local(l(0)),
                        idx: Operand::Literal(ConstValue::Int(1)),
                        value: Operand::Literal(ConstValue::Int(9)),
                    },
                    Instruction::IndexGet {
                        dst: l(3),
                        arr: Operand::Local(l(0)),
                        idx: Operand::Literal(ConstValue::Int(1)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(3)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(9));
}

#[test]
fn make_array_allocates_with_runtime_elements() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
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
                        value: ConstValue::Int(10),
                    },
                    Instruction::Const {
                        dst: l(1),
                        value: ConstValue::Int(20),
                    },
                    Instruction::MakeArray {
                        dst: l(2),
                        items: vec![Operand::Local(l(0)), Operand::Local(l(1))],
                    },
                    Instruction::IndexGet {
                        dst: l(3),
                        arr: Operand::Local(l(2)),
                        idx: Operand::Literal(ConstValue::Int(1)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(3)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(20));
}

#[test]
fn make_enum_allocates_with_runtime_fields() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "entry".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::Const {
                            dst: l(0),
                            value: ConstValue::Int(42),
                        },
                        Instruction::MakeEnum {
                            dst: l(1),
                            enum_name: "Option".to_string(),
                            type_args: Vec::new(),
                            variant: "Some".to_string(),
                            fields: vec![Operand::Local(l(0))],
                        },
                    ],
                    terminator: Terminator::Switch {
                        value: Operand::Local(l(1)),
                        cases: vec![SwitchCase {
                            pattern: Pattern::Enum {
                                enum_name: "Option".to_string(),
                                variant: "Some".to_string(),
                                fields: vec![Pattern::Bind],
                            },
                            target: b(1),
                        }],
                        default: b(2),
                    },
                },
                BasicBlock {
                    label: "some".to_string(),
                    params: vec![l(0)],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(0)),
                    },
                },
                BasicBlock {
                    label: "default".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Int(0)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(42));
}

#[test]
fn switch_binds_values_into_target_block_params() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Enum {
                            enum_name: "Option".to_string(),
                            variant: "Some".to_string(),
                            fields: vec![ConstValue::Int(42)],
                        },
                    }],
                    terminator: Terminator::Switch {
                        value: Operand::Local(l(0)),
                        cases: vec![
                            SwitchCase {
                                pattern: Pattern::Enum {
                                    enum_name: "Option".to_string(),
                                    variant: "Some".to_string(),
                                    fields: vec![Pattern::Bind],
                                },
                                target: b(1),
                            },
                            SwitchCase {
                                pattern: Pattern::Enum {
                                    enum_name: "Option".to_string(),
                                    variant: "None".to_string(),
                                    fields: vec![],
                                },
                                target: b(2),
                            },
                        ],
                        default: b(3),
                    },
                },
                BasicBlock {
                    label: "block_some".to_string(),
                    params: vec![l(1)],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
                BasicBlock {
                    label: "block_none".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Int(0)),
                    },
                },
                BasicBlock {
                    label: "block_default".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Trap {
                        message: "no match".to_string(),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(42));
}

#[test]
fn br_passes_block_arguments() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(7),
                    }],
                    terminator: Terminator::Br {
                        target: b(1),
                        args: vec![Operand::Local(l(0))],
                    },
                },
                BasicBlock {
                    label: "block1".to_string(),
                    params: vec![l(1)],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(7));
}

#[test]
fn move_clears_the_source_local() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 3,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(1),
                    },
                    Instruction::Move {
                        dst: l(1),
                        src: l(0),
                    },
                    Instruction::Copy {
                        dst: l(2),
                        src: l(0),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::UninitializedLocal { local: l(0) });
}

#[test]
fn pop_handler_without_push_is_an_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::PopHandler],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::MismatchedPopHandler);
}

#[test]
fn effects_perform_and_resume() {
    let mut module = Module::default();

    // fn process() -> unit { perform Logger.log("hi"); perform Logger.log("bye"); return unit }
    add_fn(
        &mut module,
        Function {
            name: "process".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Perform {
                        dst: None,
                        effect: EffectSpec {
                            interface: "Logger".to_string(),
                            interface_args: Vec::new(),
                            method: "log".to_string(),
                        },
                        args: vec![Operand::Literal(ConstValue::String("hi".to_string()))],
                    },
                    Instruction::Perform {
                        dst: None,
                        effect: EffectSpec {
                            interface: "Logger".to_string(),
                            interface_args: Vec::new(),
                            method: "log".to_string(),
                        },
                        args: vec![Operand::Literal(ConstValue::String("bye".to_string()))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    // fn main() -> int {
    //   push_handler H0 { Logger.log(%msg) -> block_log }
    //   _ = call process()
    //   pop_handler
    //   return 99
    //
    // block_log(%msg, %k):
    //   _ = call record(%msg)
    //   %r = resume %k unit
    //   return %r
    // }
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 4,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "Logger".to_string(),
                                    interface_args: Vec::new(),
                                    method: "log".to_string(),
                                },
                                arg_patterns: vec![Pattern::Bind],
                                target: b(1),
                            }],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "process".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                        Instruction::Const {
                            dst: l(3),
                            value: ConstValue::Int(99),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(3)),
                    },
                },
                BasicBlock {
                    label: "block_log".to_string(),
                    params: vec![l(0), l(1)],
                    instructions: vec![
                        Instruction::Call {
                            dst: None,
                            func: "record".to_string(),
                            args: vec![Operand::Local(l(0))],
                        },
                        Instruction::Resume {
                            dst: Some(l(2)),
                            k: Operand::Local(l(1)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(2)),
                    },
                },
            ],
        },
    );

    let output: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(Vec::new()));
    let output_for_host = Rc::clone(&output);

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("record", move |_interp, args| {
        let [Value::String(s)] = args else {
            return Err(RuntimeError::Trap {
                message: "record expects (string)".to_string(),
            });
        };
        output_for_host.borrow_mut().push(s.clone());
        Ok(Value::Unit)
    });

    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(99));
    assert_eq!(&*output.borrow(), &["hi".to_string(), "bye".to_string()]);
}

#[test]
fn cond_br_selects_then_and_else() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "choose".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Mutable,
                ty: Some(Type::Bool),
            }],
            ret_type: Some(Type::Int),
            locals: 1,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::CondBr {
                        cond: Operand::Local(l(0)),
                        then_target: b(1),
                        then_args: vec![],
                        else_target: b(2),
                        else_args: vec![],
                    },
                },
                BasicBlock {
                    label: "then".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Int(1)),
                    },
                },
                BasicBlock {
                    label: "else".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Int(0)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    assert_eq!(
        interp
            .run_function("choose", vec![Value::Bool(true)])
            .unwrap(),
        Value::Int(1)
    );
    assert_eq!(
        interp
            .run_function("choose", vec![Value::Bool(false)])
            .unwrap(),
        Value::Int(0)
    );
}

#[test]
fn trap_terminator_produces_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Trap {
                    message: "boom".to_string(),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::Trap {
            message: "boom".to_string()
        }
    );
}

#[test]
fn call_unknown_function_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::Call {
                    dst: None,
                    func: "no_such_fn".to_string(),
                    args: vec![],
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::UnknownFunction {
            name: "no_such_fn".to_string()
        }
    );
}

#[test]
fn icall_invokes_function_reference() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "callee".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Int(123)),
                },
            }],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Mutable,
                ty: Some(Type::Fn),
            }],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::ICall {
                    dst: Some(l(1)),
                    fnptr: Operand::Local(l(0)),
                    args: vec![],
                }],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        },
    );

    let callee_id = module.function_id("callee").unwrap();
    let mut interp = Interpreter::new(module);
    let out = interp
        .run_function("main", vec![Value::Function(callee_id)])
        .unwrap();
    assert_eq!(out, Value::Int(123));
}

#[test]
fn vcall_invokes_resolved_method() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "Point_get_x".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Readonly,
                ty: Some(Type::Struct("Point".to_string())),
            }],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
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

    let method_id = module.function_id("Point_get_x").unwrap();
    module
        .methods
        .insert(("Point".to_string(), "get_x".to_string()), method_id);

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "Point".to_string(),
                        type_args: Vec::new(),
                        fields: vec![("x".to_string(), Operand::Literal(ConstValue::Int(5)))],
                    },
                    Instruction::VCall {
                        dst: Some(l(1)),
                        obj: Operand::Local(l(0)),
                        method: "get_x".to_string(),
                        method_type_args: Vec::new(),
                        args: vec![],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Local(l(1)),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    assert_eq!(out, Value::Int(5));
}

#[test]
fn get_field_missing_field_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "S".to_string(),
                        type_args: Vec::new(),
                        fields: vec![("x".to_string(), Operand::Literal(ConstValue::Int(1)))],
                    },
                    Instruction::GetField {
                        dst: l(1),
                        obj: Operand::Local(l(0)),
                        field: "y".to_string(),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::MissingField {
            field: "y".to_string()
        }
    );
}

#[test]
fn index_get_out_of_bounds_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![ConstValue::Int(1), ConstValue::Int(2)]),
                    },
                    Instruction::IndexGet {
                        dst: l(1),
                        arr: Operand::Local(l(0)),
                        idx: Operand::Literal(ConstValue::Int(9)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::IndexOutOfBounds { index: 9, len: 2 });
}

#[test]
fn len_on_non_array_is_type_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(1),
                    },
                    Instruction::Len {
                        dst: l(1),
                        arr: Operand::Local(l(0)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::TypeError {
            op: "len",
            expected: "ref(array)",
            got: rusk_interpreter::interpreter::ValueKind::Int
        }
    );
}

#[test]
fn switch_target_param_arity_mismatch_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Int(1),
                    }],
                    terminator: Terminator::Switch {
                        value: Operand::Local(l(0)),
                        cases: vec![SwitchCase {
                            pattern: Pattern::Bind,
                            target: b(1),
                        }],
                        default: b(2),
                    },
                },
                BasicBlock {
                    label: "bad_target".to_string(),
                    params: vec![], // should be 1 param for the binding
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "default".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::InvalidBlockArgs {
            target: b(1),
            expected: 0,
            got: 1
        }
    );
}

#[test]
fn push_handler_validates_target_param_arity() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![Instruction::PushHandler {
                        handler_id: "H0".to_string(),
                        clauses: vec![HandlerClause {
                            effect: EffectSpec {
                                interface: "E".to_string(),
                                interface_args: Vec::new(),
                                method: "op".to_string(),
                            },
                            arg_patterns: vec![Pattern::Bind],
                            target: b(1),
                        }],
                    }],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "handler".to_string(),
                    // should be 2 params (bind + k); only 1 means `push_handler` should trap
                    params: vec![l(0)],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    let RuntimeError::Trap { message } = err else {
        panic!("expected trap error");
    };
    assert!(message.contains("invalid handler target params"));
}

#[test]
fn effect_handler_clause_order_and_arg_patterns_work() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "do_both".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Perform {
                        dst: None,
                        effect: EffectSpec {
                            interface: "E".to_string(),
                            interface_args: Vec::new(),
                            method: "op".to_string(),
                        },
                        args: vec![Operand::Literal(ConstValue::Int(1))],
                    },
                    Instruction::Perform {
                        dst: None,
                        effect: EffectSpec {
                            interface: "E".to_string(),
                            interface_args: Vec::new(),
                            method: "op".to_string(),
                        },
                        args: vec![Operand::Literal(ConstValue::Int(2))],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 3,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![
                                HandlerClause {
                                    effect: EffectSpec {
                                        interface: "E".to_string(),
                                        interface_args: Vec::new(),
                                        method: "op".to_string(),
                                    },
                                    arg_patterns: vec![Pattern::Literal(ConstValue::Int(1))],
                                    target: b(1),
                                },
                                HandlerClause {
                                    effect: EffectSpec {
                                        interface: "E".to_string(),
                                        interface_args: Vec::new(),
                                        method: "op".to_string(),
                                    },
                                    arg_patterns: vec![Pattern::Bind],
                                    target: b(2),
                                },
                            ],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "do_both".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                // literal(1) match: 0 binds => only %k
                BasicBlock {
                    label: "block_one".to_string(),
                    params: vec![l(0)],
                    instructions: vec![
                        Instruction::Call {
                            dst: None,
                            func: "record_int".to_string(),
                            args: vec![Operand::Literal(ConstValue::Int(-1))],
                        },
                        Instruction::Resume {
                            dst: Some(l(2)),
                            k: Operand::Local(l(0)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(2)),
                    },
                },
                // bind match: 1 bind => %arg, %k
                BasicBlock {
                    label: "block_any".to_string(),
                    params: vec![l(0), l(1)],
                    instructions: vec![
                        Instruction::Call {
                            dst: None,
                            func: "record_int".to_string(),
                            args: vec![Operand::Local(l(0))],
                        },
                        Instruction::Resume {
                            dst: Some(l(2)),
                            k: Operand::Local(l(1)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(2)),
                    },
                },
            ],
        },
    );

    let seen: Rc<RefCell<Vec<i64>>> = Rc::new(RefCell::new(Vec::new()));
    let seen_for_host = Rc::clone(&seen);

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("record_int", move |_interp, args| {
        let [Value::Int(i)] = args else {
            return Err(RuntimeError::Trap {
                message: "record_int expects (int)".to_string(),
            });
        };
        seen_for_host.borrow_mut().push(*i);
        Ok(Value::Unit)
    });

    interp.run_function("main", vec![]).unwrap();
    assert_eq!(&*seen.borrow(), &[-1, 2]);
}

#[test]
fn nested_handlers_prefer_innermost() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "do_one".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::Perform {
                    dst: None,
                    effect: EffectSpec {
                        interface: "E".to_string(),
                        interface_args: Vec::new(),
                        method: "op".to_string(),
                    },
                    args: vec![],
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "with_inner".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H1".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "do_one".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "inner_handler".to_string(),
                    params: vec![l(0)], // only %k
                    instructions: vec![
                        Instruction::Call {
                            dst: None,
                            func: "record_int".to_string(),
                            args: vec![Operand::Literal(ConstValue::Int(1))],
                        },
                        Instruction::Resume {
                            dst: Some(l(1)),
                            k: Operand::Local(l(0)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
            ],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "with_inner".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "outer_handler".to_string(),
                    params: vec![l(0)], // only %k
                    instructions: vec![
                        Instruction::Call {
                            dst: None,
                            func: "record_int".to_string(),
                            args: vec![Operand::Literal(ConstValue::Int(2))],
                        },
                        Instruction::Resume {
                            dst: Some(l(1)),
                            k: Operand::Local(l(0)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
            ],
        },
    );

    let seen: Rc<RefCell<Vec<i64>>> = Rc::new(RefCell::new(Vec::new()));
    let seen_for_host = Rc::clone(&seen);

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("record_int", move |_interp, args| {
        let [Value::Int(i)] = args else {
            return Err(RuntimeError::Trap {
                message: "record_int expects (int)".to_string(),
            });
        };
        seen_for_host.borrow_mut().push(*i);
        Ok(Value::Unit)
    });

    interp.run_function("main", vec![]).unwrap();
    assert_eq!(&*seen.borrow(), &[1]);
}

#[test]
fn handler_can_skip_continuation_by_not_resuming() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "do_work".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Perform {
                        dst: None,
                        effect: EffectSpec {
                            interface: "E".to_string(),
                            interface_args: Vec::new(),
                            method: "op".to_string(),
                        },
                        args: vec![],
                    },
                    Instruction::Call {
                        dst: None,
                        func: "set_flag".to_string(),
                        args: vec![],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "do_work".to_string(),
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "handler".to_string(),
                    params: vec![l(0)], // only %k, ignored
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
            ],
        },
    );

    let flag = Rc::new(Cell::new(false));
    let flag_for_host = Rc::clone(&flag);

    let mut interp = Interpreter::new(module);
    interp.register_host_fn("set_flag", move |_interp, _args| {
        flag_for_host.set(true);
        Ok(Value::Unit)
    });

    interp.run_function("main", vec![]).unwrap();
    assert!(!flag.get());
}

#[test]
fn can_resume_continuation_from_host_and_it_is_one_shot() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Int),
            locals: 2,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Perform {
                            dst: Some(l(0)),
                            effect: EffectSpec {
                                interface: "E".to_string(),
                                interface_args: Vec::new(),
                                method: "op".to_string(),
                            },
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(0)),
                    },
                },
                BasicBlock {
                    label: "handler".to_string(),
                    params: vec![l(1)], // only %k
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let out = interp.run_function("main", vec![]).unwrap();
    let Value::Continuation(token) = out else {
        panic!("expected continuation token as the program result");
    };

    let resumed = interp
        .resume_continuation(token.clone(), Value::Int(9))
        .unwrap();
    assert_eq!(resumed, Value::Int(9));

    let err = interp
        .resume_continuation(token, Value::Int(10))
        .unwrap_err();
    assert_eq!(err, RuntimeError::InvalidResume);
}

#[test]
fn invalid_local_index_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::Const {
                    dst: l(0),
                    value: ConstValue::Int(1),
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::InvalidLocal {
            local: l(0),
            locals: 0
        }
    );
}

#[test]
fn invalid_block_id_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![],
                terminator: Terminator::Br {
                    target: b(1),
                    args: vec![],
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::InvalidBlock {
            block: b(1),
            blocks: 1
        }
    );
}

#[test]
fn cond_br_requires_bool_condition() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "choose".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Mutable,
                ty: None,
            }],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::CondBr {
                        cond: Operand::Local(l(0)),
                        then_target: b(1),
                        then_args: vec![],
                        else_target: b(2),
                        else_args: vec![],
                    },
                },
                BasicBlock {
                    label: "then".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "else".to_string(),
                    params: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp
        .run_function("choose", vec![Value::Int(1)])
        .unwrap_err();
    assert_eq!(
        err,
        RuntimeError::TypeError {
            op: "cond_br",
            expected: "bool",
            got: rusk_interpreter::interpreter::ValueKind::Int
        }
    );
}

#[test]
fn copy_from_uninitialized_local_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::Copy {
                    dst: l(1),
                    src: l(0),
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::UninitializedLocal { local: l(0) });
}

#[test]
fn set_field_missing_field_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "S".to_string(),
                        type_args: Vec::new(),
                        fields: vec![("x".to_string(), Operand::Literal(ConstValue::Int(1)))],
                    },
                    Instruction::SetField {
                        obj: Operand::Local(l(0)),
                        field: "y".to_string(),
                        value: Operand::Literal(ConstValue::Int(2)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::MissingField {
            field: "y".to_string()
        }
    );
}

#[test]
fn index_set_readonly_write_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![ConstValue::Int(1)]),
                    },
                    Instruction::AsReadonly {
                        dst: l(1),
                        src: l(0),
                    },
                    Instruction::IndexSet {
                        arr: Operand::Local(l(1)),
                        idx: Operand::Literal(ConstValue::Int(0)),
                        value: Operand::Literal(ConstValue::Int(2)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::ReadonlyWrite);
}

#[test]
fn index_set_out_of_bounds_is_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::Const {
                        dst: l(0),
                        value: ConstValue::Array(vec![ConstValue::Int(1)]),
                    },
                    Instruction::IndexSet {
                        arr: Operand::Local(l(0)),
                        idx: Operand::Literal(ConstValue::Int(9)),
                        value: Operand::Literal(ConstValue::Int(2)),
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::IndexOutOfBounds { index: 9, len: 1 });
}

#[test]
fn vcall_unresolved_method_is_trap() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 2,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![
                    Instruction::MakeStruct {
                        dst: l(0),
                        type_name: "Point".to_string(),
                        type_args: Vec::new(),
                        fields: vec![("x".to_string(), Operand::Literal(ConstValue::Int(1)))],
                    },
                    Instruction::VCall {
                        dst: Some(l(1)),
                        obj: Operand::Local(l(0)),
                        method: "nope".to_string(),
                        method_type_args: Vec::new(),
                        args: vec![],
                    },
                ],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    let RuntimeError::Trap { message } = err else {
        panic!("expected trap error");
    };
    assert_eq!(message, "unresolved vcall method: nope on Point");
}

#[test]
fn pop_handler_in_non_owner_frame_is_error() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "callee".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::PopHandler],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Call {
                            dst: None,
                            func: "callee".to_string(),
                            args: vec![],
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "handler".to_string(),
                    params: vec![l(0)], // only %k
                    instructions: vec![],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::MismatchedPopHandler);
}

#[test]
fn icall_with_non_function_value_is_type_error() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![Param {
                local: l(0),
                mutability: Mutability::Mutable,
                ty: None,
            }],
            ret_type: Some(Type::Unit),
            locals: 1,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::ICall {
                    dst: None,
                    fnptr: Operand::Local(l(0)),
                    args: vec![],
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp
        .run_function("main", vec![Value::Int(0)])
        .unwrap_err();
    assert_eq!(
        err,
        RuntimeError::TypeError {
            op: "icall",
            expected: "fn reference",
            got: rusk_interpreter::interpreter::ValueKind::Int
        }
    );
}

#[test]
fn unhandled_effect_traps() {
    let mut module = Module::default();
    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 0,
            blocks: vec![BasicBlock {
                label: "block0".to_string(),
                params: vec![],
                instructions: vec![Instruction::Perform {
                    dst: None,
                    effect: EffectSpec {
                        interface: "Logger".to_string(),
                        interface_args: Vec::new(),
                        method: "log".to_string(),
                    },
                    args: vec![Operand::Literal(ConstValue::String("x".to_string()))],
                }],
                terminator: Terminator::Return {
                    value: Operand::Literal(ConstValue::Unit),
                },
            }],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(
        err,
        RuntimeError::UnhandledEffect {
            interface: "Logger".to_string(),
            method: "log".to_string(),
        }
    );
}

#[test]
fn continuation_is_one_shot() {
    let mut module = Module::default();

    add_fn(
        &mut module,
        Function {
            name: "main".to_string(),
            params: vec![],
            ret_type: Some(Type::Unit),
            locals: 3,
            blocks: vec![
                BasicBlock {
                    label: "block0".to_string(),
                    params: vec![],
                    instructions: vec![
                        Instruction::PushHandler {
                            handler_id: "H0".to_string(),
                            clauses: vec![HandlerClause {
                                effect: EffectSpec {
                                    interface: "E".to_string(),
                                    interface_args: Vec::new(),
                                    method: "op".to_string(),
                                },
                                arg_patterns: vec![],
                                target: b(1),
                            }],
                        },
                        Instruction::Perform {
                            dst: None,
                            effect: EffectSpec {
                                interface: "E".to_string(),
                                interface_args: Vec::new(),
                                method: "op".to_string(),
                            },
                            args: vec![],
                        },
                        Instruction::PopHandler,
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Literal(ConstValue::Unit),
                    },
                },
                BasicBlock {
                    label: "block_handler".to_string(),
                    params: vec![l(0)], // only %k (0 binds + 1)
                    instructions: vec![
                        Instruction::Resume {
                            dst: Some(l(1)),
                            k: Operand::Local(l(0)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                        Instruction::Resume {
                            dst: Some(l(2)),
                            k: Operand::Local(l(0)),
                            value: Operand::Literal(ConstValue::Unit),
                        },
                    ],
                    terminator: Terminator::Return {
                        value: Operand::Local(l(1)),
                    },
                },
            ],
        },
    );

    let mut interp = Interpreter::new(module);
    let err = interp.run_function("main", vec![]).unwrap_err();
    assert_eq!(err, RuntimeError::InvalidResume);
}
