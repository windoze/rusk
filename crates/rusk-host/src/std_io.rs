use rusk_bytecode::ExecutableModule;
use rusk_vm::{HostError, Vm};
use std::io::{self, Write};

/// Installs `std::print` / `std::println` host import implementations into the given VM.
///
/// The installer checks which host imports exist in the compiled module and registers only the
/// ones that are present.
pub fn install_vm(module: &ExecutableModule, vm: &mut Vm) {
    if let Some(id) = module.host_import_id("std::print") {
        vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
            let mut stdout = io::stdout();
            stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                message: format!("std::print: io error: {e}"),
            })?;
            stdout.flush().ok();
            Ok(())
        })
        .expect("std::print host import id must be valid");
    }

    if let Some(id) = module.host_import_id("std::println") {
        vm.register_host_import_typed(id, |(s,): (String,)| -> Result<(), HostError> {
            let mut stdout = io::stdout();
            stdout.write_all(s.as_bytes()).map_err(|e| HostError {
                message: format!("std::println: io error: {e}"),
            })?;
            stdout.write_all(b"\n").map_err(|e| HostError {
                message: format!("std::println: io error: {e}"),
            })?;
            stdout.flush().ok();
            Ok(())
        })
        .expect("std::println host import id must be valid");
    }
}
