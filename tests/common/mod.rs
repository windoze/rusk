use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};
use rusk_vm::{AbiValue, ContinuationHandle, HostError, Vm};

#[allow(dead_code)]
pub fn register_test_host_module(options: &mut CompileOptions) {
    let cont_int_to_int = HostType::Cont {
        param: Box::new(HostType::Int),
        ret: Box::new(HostType::Int),
    };

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
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "store_cont".to_string(),
                sig: HostFnSig {
                    params: vec![cont_int_to_int.clone()],
                    ret: HostType::Unit,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "take_cont".to_string(),
                sig: HostFnSig {
                    params: vec![],
                    ret: cont_int_to_int,
                },
            },
        ],
    };

    options
        .register_host_module("test", module)
        .expect("test host module declaration must be valid");
}

#[allow(unused)]
pub fn register_test_external_effects(options: &mut CompileOptions) {
    options
        .register_external_effect(
            "TestFfi",
            "add",
            HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int,
            },
        )
        .expect("register TestFfi.add");

    options
        .register_external_effect(
            "TestFfi",
            "echo",
            HostFnSig {
                params: vec![HostType::String],
                ret: HostType::String,
            },
        )
        .expect("register TestFfi.echo");

    options
        .register_external_effect(
            "TestFfi",
            "echo_bytes",
            HostFnSig {
                params: vec![HostType::Bytes],
                ret: HostType::Bytes,
            },
        )
        .expect("register TestFfi.echo_bytes");
}

#[allow(unused)]
pub fn install_test_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    let stored_cont: std::rc::Rc<std::cell::RefCell<Option<ContinuationHandle>>> =
        std::rc::Rc::new(std::cell::RefCell::new(None));

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

    if let Some(id) = module.host_import_id("test::store_cont") {
        let stored_cont = std::rc::Rc::clone(&stored_cont);
        vm.register_host_import(id, move |args: &[AbiValue]| match args {
            [AbiValue::Continuation(k)] => {
                *stored_cont.borrow_mut() = Some(k.clone());
                Ok(AbiValue::Unit)
            }
            other => Err(HostError {
                message: format!("test::store_cont: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::take_cont") {
        let stored_cont = std::rc::Rc::clone(&stored_cont);
        vm.register_host_import(id, move |args: &[AbiValue]| match args {
            [] => {
                let Some(k) = stored_cont.borrow().clone() else {
                    return Err(HostError {
                        message: "test::take_cont: missing stored continuation".to_string(),
                    });
                };
                Ok(AbiValue::Continuation(k))
            }
            other => Err(HostError {
                message: format!("test::take_cont: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
}

#[allow(unused)]
pub fn install_core_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    // Strings
    if let Some(id) = module.host_import_id("core::intrinsics::string_concat") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(a), AbiValue::String(b)] => Ok(AbiValue::String(format!("{a}{b}"))),
            other => Err(HostError {
                message: format!("core::intrinsics::string_concat: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::string_eq") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(a), AbiValue::String(b)] => Ok(AbiValue::Bool(a == b)),
            other => Err(HostError {
                message: format!("core::intrinsics::string_eq: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::string_ne") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(a), AbiValue::String(b)] => Ok(AbiValue::Bool(a != b)),
            other => Err(HostError {
                message: format!("core::intrinsics::string_ne: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    // Bytes
    if let Some(id) = module.host_import_id("core::intrinsics::bytes_eq") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Bytes(a), AbiValue::Bytes(b)] => Ok(AbiValue::Bool(a == b)),
            other => Err(HostError {
                message: format!("core::intrinsics::bytes_eq: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::bytes_ne") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Bytes(a), AbiValue::Bytes(b)] => Ok(AbiValue::Bool(a != b)),
            other => Err(HostError {
                message: format!("core::intrinsics::bytes_ne: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    // Unit
    if let Some(id) = module.host_import_id("core::intrinsics::unit_eq") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Unit, AbiValue::Unit] => Ok(AbiValue::Bool(true)),
            other => Err(HostError {
                message: format!("core::intrinsics::unit_eq: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::unit_ne") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Unit, AbiValue::Unit] => Ok(AbiValue::Bool(false)),
            other => Err(HostError {
                message: format!("core::intrinsics::unit_ne: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }

    // Floats
    if let Some(id) = module.host_import_id("core::intrinsics::float_lt") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Float(a), AbiValue::Float(b)] => Ok(AbiValue::Bool(a < b)),
            other => Err(HostError {
                message: format!("core::intrinsics::float_lt: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::float_ge") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::Float(a), AbiValue::Float(b)] => Ok(AbiValue::Bool(a >= b)),
            other => Err(HostError {
                message: format!("core::intrinsics::float_ge: bad args: {other:?}"),
            }),
        })
        .unwrap();
    }
}
