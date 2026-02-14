use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};
use rusk_interpreter::{Interpreter, RuntimeError, Value};
use rusk_vm::{AbiValue, HostError, Vm};

pub fn register_test_host_module(options: &mut CompileOptions) {
    let module = HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "add_int".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Int, HostType::Int],
                    ret: HostType::Int,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "concat_string".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::String, HostType::String],
                    ret: HostType::String,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "bool_not".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Bool],
                    ret: HostType::Bool,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "float_mul".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Float, HostType::Float],
                    ret: HostType::Float,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "float_eq".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Float, HostType::Float],
                    ret: HostType::Bool,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "unit".to_string(),
                sig: HostFnSig {
                    params: vec![],
                    ret: HostType::Unit,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "bytes_echo".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Bytes],
                    ret: HostType::Bytes,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "bytes_eq".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::Bytes, HostType::Bytes],
                    ret: HostType::Bool,
                },
            },
        ],
    };

    options
        .register_host_module("test", module)
        .expect("test host module declaration must be valid");
}

pub fn install_test_host_fns(interp: &mut Interpreter) {
    interp.register_host_fn("test::add_int", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("test::add_int: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::concat_string", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::String(format!("{a}{b}"))),
        other => Err(RuntimeError::Trap {
            message: format!("test::concat_string: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::bool_not", |_interp, args| match args {
        [Value::Bool(v)] => Ok(Value::Bool(!v)),
        other => Err(RuntimeError::Trap {
            message: format!("test::bool_not: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::float_mul", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a * b)),
        other => Err(RuntimeError::Trap {
            message: format!("test::float_mul: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::float_eq", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("test::float_eq: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::unit", |_interp, args| match args {
        [] => Ok(Value::Unit),
        other => Err(RuntimeError::Trap {
            message: format!("test::unit: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::bytes_echo", |_interp, args| match args {
        [Value::Bytes(b)] => Ok(Value::Bytes(b.clone())),
        other => Err(RuntimeError::Trap {
            message: format!("test::bytes_echo: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("test::bytes_eq", |_interp, args| match args {
        [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("test::bytes_eq: bad args: {other:?}"),
        }),
    });
}

pub fn install_test_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    if let Some(id) = module.host_import_id("test::add_int") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Int(a), AbiValue::Int(b)] => Ok(AbiValue::Int(a + b)),
            other => Err(HostError {
                message: format!("test::add_int: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::concat_string") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(a), AbiValue::String(b)] => Ok(AbiValue::String(format!("{a}{b}"))),
            other => Err(HostError {
                message: format!("test::concat_string: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bool_not") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Bool(v)] => Ok(AbiValue::Bool(!v)),
            other => Err(HostError {
                message: format!("test::bool_not: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::float_mul") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Float(a), AbiValue::Float(b)] => Ok(AbiValue::Float(a * b)),
            other => Err(HostError {
                message: format!("test::float_mul: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::float_eq") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Float(a), AbiValue::Float(b)] => Ok(AbiValue::Bool(a == b)),
            other => Err(HostError {
                message: format!("test::float_eq: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::unit") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [] => Ok(AbiValue::Unit),
            other => Err(HostError {
                message: format!("test::unit: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bytes_echo") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Bytes(b)] => Ok(AbiValue::Bytes(b.clone())),
            other => Err(HostError {
                message: format!("test::bytes_echo: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bytes_eq") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Bytes(a), AbiValue::Bytes(b)] => Ok(AbiValue::Bool(a == b)),
            other => Err(HostError {
                message: format!("test::bytes_eq: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
}

