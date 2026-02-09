use rusk::compiler::compile_to_mir;
use rusk::stdlib::register_std_host_fns;
use rusk::{Interpreter, Value};
use std::env;
use std::fs;
use std::process;

fn main() {
    let mut args = env::args().skip(1);
    let Some(path) = args.next() else {
        eprintln!("usage: rusk <file.rusk>");
        process::exit(2);
    };
    if args.next().is_some() {
        eprintln!("error: expected exactly one input file");
        process::exit(2);
    }

    let source = match fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to read `{path}`: {e}");
            process::exit(2);
        }
    };

    let module = match compile_to_mir(&source) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("compile error: {e}");
            process::exit(1);
        }
    };

    let mut interp = Interpreter::new(module);
    register_std_host_fns(&mut interp);

    match interp.run_function("main", vec![]) {
        Ok(Value::Unit) => {}
        Ok(other) => {
            // The current spec requires `main() -> unit`, but printing a non-unit return
            // is useful during development.
            println!("{other:?}");
        }
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}
