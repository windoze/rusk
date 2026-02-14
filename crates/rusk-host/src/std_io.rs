use rusk_compiler::{
    CompileOptions, HostFnSig, HostFunctionDecl, HostModuleDecl, HostType, HostVisibility,
};
use rusk_bytecode::ExecutableModule;
use rusk_interpreter::{Interpreter, RuntimeError, Value};
use rusk_vm::{AbiValue, HostError, Vm};
use std::io::{self, Write};

pub fn register_host_module(options: &mut CompileOptions) {
    let module = HostModuleDecl {
        visibility: HostVisibility::Public,
        functions: vec![
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "print".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::String],
                    ret: HostType::Unit,
                },
            },
            HostFunctionDecl {
                visibility: HostVisibility::Public,
                name: "println".to_string(),
                sig: HostFnSig {
                    params: vec![HostType::String],
                    ret: HostType::Unit,
                },
            },
        ],
    };

    options
        .register_host_module("std", module)
        .expect("std host module declaration must be valid");
}

pub fn install(interp: &mut Interpreter) {
    interp.register_host_fn("std::print", |_interp, args| match args {
        [Value::String(s)] => {
            let mut stdout = io::stdout();
            stdout
                .write_all(s.as_bytes())
                .map_err(|e| RuntimeError::Trap {
                    message: format!("std::print: io error: {e}"),
                })?;
            stdout.flush().ok();
            Ok(Value::Unit)
        }
        other => Err(RuntimeError::Trap {
            message: format!("std::print: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::println", |_interp, args| match args {
        [Value::String(s)] => {
            let mut stdout = io::stdout();
            stdout
                .write_all(s.as_bytes())
                .map_err(|e| RuntimeError::Trap {
                    message: format!("std::println: io error: {e}"),
                })?;
            stdout.write_all(b"\n").map_err(|e| RuntimeError::Trap {
                message: format!("std::println: io error: {e}"),
            })?;
            stdout.flush().ok();
            Ok(Value::Unit)
        }
        other => Err(RuntimeError::Trap {
            message: format!("std::println: bad args: {other:?}"),
        }),
    });
}

pub fn install_vm(module: &ExecutableModule, vm: &mut Vm) {
    if let Some(id) = module.host_import_id("std::print") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(s)] => {
                let mut stdout = io::stdout();
                stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                    message: format!("std::print: io error: {e}"),
                })?;
                stdout.flush().ok();
                Ok(AbiValue::Unit)
            }
            other => Err(HostError {
                message: format!("std::print: bad args: {other:?}"),
            }),
        })
        .expect("std::print host import id must be valid");
    }

    if let Some(id) = module.host_import_id("std::println") {
        vm.register_host_import(id, |args: &[AbiValue]| match args {
            [AbiValue::String(s)] => {
                let mut stdout = io::stdout();
                stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                    message: format!("std::println: io error: {e}"),
                })?;
                stdout.write_all(b"\n").map_err(|e| HostError {
                    message: format!("std::println: io error: {e}"),
                })?;
                stdout.flush().ok();
                Ok(AbiValue::Unit)
            }
            other => Err(HostError {
                message: format!("std::println: bad args: {other:?}"),
            }),
        })
        .expect("std::println host import id must be valid");
    }
}
