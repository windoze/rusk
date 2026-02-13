use rusk_compiler::{CompileOptions, compile_file_to_mir_with_options};
use rusk_host::std_io;
use rusk_interpreter::corelib::register_core_host_fns;
use rusk_interpreter::{Interpreter, RuntimeError, Value};
use std::path::Path;

#[test]
fn example_17_effects_state_management_runs() {
    let mut options = CompileOptions::default();
    std_io::register_host_module(&mut options);

    let module = compile_file_to_mir_with_options(
        Path::new("examples/17-effects-state-management.rusk"),
        &options,
    )
    .expect("example 17 should compile");

    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);
    // Keep tests quiet even if `cargo test -- --nocapture` is enabled.
    interp.register_host_fn("std::print", |_interp, args| match args {
        [Value::String(_)] => Ok(Value::Unit),
        other => Err(RuntimeError::Trap {
            message: format!("std::print: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::println", |_interp, args| match args {
        [Value::String(_)] => Ok(Value::Unit),
        other => Err(RuntimeError::Trap {
            message: format!("std::println: bad args: {other:?}"),
        }),
    });

    let got = interp
        .run_function("main", vec![])
        .expect("example 17 should run");
    assert_eq!(got, Value::Unit);
}
