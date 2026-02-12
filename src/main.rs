use rusk_compiler::compile_file_to_mir;
use rusk_interpreter::{Interpreter, Value, register_core_host_fns};
use std::env;
use std::path::Path;
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

    let module = match compile_file_to_mir(Path::new(&path)) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("compile error: {e}");
            process::exit(1);
        }
    };

    let mut interp = Interpreter::new(module);
    register_core_host_fns(&mut interp);

    match interp.run_function("main", vec![]) {
        Ok(Value::Unit) => {}
        Ok(other) => {
            // `main` defaults to returning `unit` (explicitly or via omission), but printing a
            // non-unit return is useful during development.
            println!("{other:?}");
        }
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}
