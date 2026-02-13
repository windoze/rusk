use rusk_compiler::{CompileMetrics, CompileOptions, compile_file_to_mir_with_options_and_metrics};
use rusk_host::std_io;
use rusk_interpreter::{
    Interpreter, InterpreterMetrics, Value, from_bytes, register_core_host_fns,
};
use std::env;
use std::fs;
use std::path::Path;
use std::process;
use std::rc::Rc;
use std::time::{Duration, Instant};

fn usage() -> ! {
    eprintln!("usage: rusk-measure [--json] [--warmup N] [--iters N] <file.rusk|file.mir>");
    process::exit(2);
}

fn escape_json_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                out.push_str(&format!("\\u{:04x}", ch as u32));
            }
            ch => out.push(ch),
        }
    }
    out
}

fn duration_ns(d: Duration) -> u128 {
    d.as_nanos()
}

fn add_metrics(dst: &mut InterpreterMetrics, src: &InterpreterMetrics) {
    dst.executed_instructions = dst
        .executed_instructions
        .saturating_add(src.executed_instructions);
    dst.executed_terminators = dst
        .executed_terminators
        .saturating_add(src.executed_terminators);
    dst.block_entries = dst.block_entries.saturating_add(src.block_entries);

    dst.allocations = dst.allocations.saturating_add(src.allocations);
    dst.gc_cycles = dst.gc_cycles.saturating_add(src.gc_cycles);
    dst.gc_nanos = dst.gc_nanos.saturating_add(src.gc_nanos);

    dst.call_instructions = dst.call_instructions.saturating_add(src.call_instructions);
    dst.icall_instructions = dst
        .icall_instructions
        .saturating_add(src.icall_instructions);
    dst.vcall_instructions = dst
        .vcall_instructions
        .saturating_add(src.vcall_instructions);
    dst.host_calls = dst.host_calls.saturating_add(src.host_calls);
    dst.mir_calls = dst.mir_calls.saturating_add(src.mir_calls);

    dst.br_terminators = dst.br_terminators.saturating_add(src.br_terminators);
    dst.cond_br_terminators = dst
        .cond_br_terminators
        .saturating_add(src.cond_br_terminators);
    dst.switch_terminators = dst
        .switch_terminators
        .saturating_add(src.switch_terminators);
    dst.return_terminators = dst
        .return_terminators
        .saturating_add(src.return_terminators);
    dst.trap_terminators = dst.trap_terminators.saturating_add(src.trap_terminators);
}

fn main() {
    let mut iters: usize = 1;
    let mut warmup: usize = 0;
    let mut json = false;
    let mut path: Option<String> = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--json" => json = true,
            "--iters" => {
                let n = args.next().unwrap_or_else(|| usage());
                iters = n.parse().unwrap_or_else(|_| usage());
            }
            "--warmup" => {
                let n = args.next().unwrap_or_else(|| usage());
                warmup = n.parse().unwrap_or_else(|_| usage());
            }
            _ if arg.starts_with('-') => usage(),
            _ => {
                if path.is_some() {
                    usage();
                }
                path = Some(arg);
            }
        }
    }

    let Some(path) = path else { usage() };
    let input_path = Path::new(&path);
    let extension = input_path.extension().and_then(|s| s.to_str());

    let (module, compile_metrics) = match extension {
        Some("rusk") => {
            let mut options = CompileOptions::default();
            std_io::register_host_module(&mut options);
            match compile_file_to_mir_with_options_and_metrics(input_path, &options) {
                Ok((m, metrics)) => (m, metrics),
                Err(e) => {
                    eprintln!("compile error: {e}");
                    process::exit(1);
                }
            }
        }
        Some("mir") => {
            let load_start = Instant::now();
            let bytes = match fs::read(input_path) {
                Ok(b) => b,
                Err(e) => {
                    eprintln!("failed to read file: {e}");
                    process::exit(1);
                }
            };
            let module = match from_bytes(&bytes) {
                Ok(m) => m,
                Err(e) => {
                    eprintln!("load error: {e}");
                    process::exit(1);
                }
            };
            let mut metrics = CompileMetrics::default();
            metrics.load_time = load_start.elapsed();
            metrics.total_time = metrics.load_time;
            (module, metrics)
        }
        _ => {
            eprintln!("error: input file must have .rusk or .mir extension");
            process::exit(2);
        }
    };

    if iters == 0 {
        eprintln!("error: --iters must be >= 1");
        process::exit(2);
    }

    let module = Rc::new(module);

    let total_runs = warmup.saturating_add(iters);
    let mut run_time_total = Duration::ZERO;
    let mut agg_metrics = InterpreterMetrics::default();
    let mut last_result: Option<Value> = None;

    for run_index in 0..total_runs {
        let mut interp = Interpreter::new_shared(Rc::clone(&module));
        register_core_host_fns(&mut interp);
        std_io::install(&mut interp);

        interp.reset_metrics();
        let run_start = Instant::now();
        let result = match interp.run_function("main", vec![]) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("runtime error: {e}");
                process::exit(1);
            }
        };
        let run_time = run_start.elapsed();
        let metrics = interp.take_metrics();

        if run_index >= warmup {
            run_time_total += run_time;
            add_metrics(&mut agg_metrics, &metrics);
            last_result = Some(result);
        }
    }

    let avg_run_time = run_time_total / (iters as u32);

    if json {
        let path_json = escape_json_string(&path);
        println!(
            "{{\"input\":\"{path_json}\",\"iters\":{iters},\"warmup\":{warmup},\
\"compile\":{{\"load_ns\":{load_ns},\"read_ns\":{read_ns},\"parse_ns\":{parse_ns},\
\"typecheck_ns\":{type_ns},\"lower_ns\":{lower_ns},\"total_ns\":{total_ns},\
\"files_read\":{files_read},\"bytes_read\":{bytes_read}}},\
\"run\":{{\"total_ns\":{run_total_ns},\"avg_ns\":{run_avg_ns},\
\"metrics\":{{\"executed_instructions\":{ei},\"executed_terminators\":{et},\"block_entries\":{be},\
\"allocations\":{allocs},\"gc_cycles\":{gcc},\"gc_ns\":{gcn},\
\"call_instructions\":{calli},\"icall_instructions\":{icalli},\"vcall_instructions\":{vcalli},\
\"host_calls\":{hostc},\"mir_calls\":{mirc},\
\"br_terminators\":{brt},\"cond_br_terminators\":{cbrt},\"switch_terminators\":{swt},\
\"return_terminators\":{rett},\"trap_terminators\":{trapt}}}}}}}",
            load_ns = duration_ns(compile_metrics.load_time),
            read_ns = duration_ns(compile_metrics.read_time),
            parse_ns = duration_ns(compile_metrics.parse_time),
            type_ns = duration_ns(compile_metrics.typecheck_time),
            lower_ns = duration_ns(compile_metrics.lower_time),
            total_ns = duration_ns(compile_metrics.total_time),
            files_read = compile_metrics.files_read,
            bytes_read = compile_metrics.bytes_read,
            run_total_ns = duration_ns(run_time_total),
            run_avg_ns = duration_ns(avg_run_time),
            ei = agg_metrics.executed_instructions,
            et = agg_metrics.executed_terminators,
            be = agg_metrics.block_entries,
            allocs = agg_metrics.allocations,
            gcc = agg_metrics.gc_cycles,
            gcn = agg_metrics.gc_nanos,
            calli = agg_metrics.call_instructions,
            icalli = agg_metrics.icall_instructions,
            vcalli = agg_metrics.vcall_instructions,
            hostc = agg_metrics.host_calls,
            mirc = agg_metrics.mir_calls,
            brt = agg_metrics.br_terminators,
            cbrt = agg_metrics.cond_br_terminators,
            swt = agg_metrics.switch_terminators,
            rett = agg_metrics.return_terminators,
            trapt = agg_metrics.trap_terminators,
        );
        return;
    }

    println!("Input: {path}");
    println!("Compile:");
    println!("  load:      {:?}", compile_metrics.load_time);
    println!(
        "  read:      {:?} (files: {}, bytes: {})",
        compile_metrics.read_time, compile_metrics.files_read, compile_metrics.bytes_read
    );
    println!("  parse:     {:?}", compile_metrics.parse_time);
    println!("  typecheck: {:?}", compile_metrics.typecheck_time);
    println!("  lower:     {:?}", compile_metrics.lower_time);
    println!("  total:     {:?}", compile_metrics.total_time);
    println!("Run:");
    println!("  iters:     {iters} (warmup: {warmup})");
    println!("  total:     {:?}", run_time_total);
    println!("  avg:       {:?}", avg_run_time);
    println!("  metrics:   {agg_metrics:?}");

    if let Some(result) = last_result
        && !matches!(result, Value::Unit)
    {
        println!("Result: {result:?}");
    }
}
