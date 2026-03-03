use rusk_bytecode::ExecutableModule;
use rusk_compiler::{CompileOptions, HostModuleDecl};
use rusk_vm::{HostError, Vm};
use std::io::{self, Write};

/// Registers the `_std_host` module (currently `print`/`println`) into compiler options.
///
/// This is intended to be paired with [`install_vm`] so the produced bytecode can resolve host
/// imports at runtime.
pub fn register_host_module(options: &mut CompileOptions) {
    let module = HostModuleDecl::public()
        .function::<(String,), ()>("print")
        .function::<(String,), ()>("println")
        .build();

    options
        .register_host_module("_std_host", module)
        .expect("_std_host host module declaration must be valid");
}

/// Installs `_std_host` host import implementations into the given VM.
///
/// The installer checks which host imports exist in the compiled module and registers only the
/// ones that are present.
pub fn install_vm(module: &ExecutableModule, vm: &mut Vm) {
    if let Some(id) = module.host_import_id("_std_host::print") {
        vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
            let mut stdout = io::stdout();
            stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                message: format!("_std_host::print: io error: {e}"),
            })?;
            stdout.flush().ok();
            Ok(())
        })
        .expect("_std_host::print host import id must be valid");
    }

    if let Some(id) = module.host_import_id("_std_host::println") {
        vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
            let mut stdout = io::stdout();
            stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                message: format!("_std_host::println: io error: {e}"),
            })?;
            stdout.write_all(b"\n").map_err(|e| HostError {
                message: format!("_std_host::println: io error: {e}"),
            })?;
            stdout.flush().ok();
            Ok(())
        })
        .expect("_std_host::println host import id must be valid");
    }
}
