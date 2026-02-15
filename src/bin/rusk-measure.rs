use rusk_compiler::{CompileMetrics, CompileOptions, compile_file_to_mir_with_options_and_metrics};
use rusk_host::std_io;
use rusk_interpreter::{
    Interpreter, InterpreterMetrics, Value, from_bytes, register_core_host_fns,
};
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};
use std::env;
use std::fs;
use std::path::Path;
use std::process;
use std::rc::Rc;
use std::time::{Duration, Instant};

fn usage() -> ! {
    eprintln!(
        "usage: rusk-measure [--backend mir|bytecode] [--opt-level o0|o1|o2] [--metrics] [--json] [--warmup N] [--iters N] <file.rusk|file.mir|file.rbc>"
    );
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Backend {
    Mir,
    Bytecode,
}

fn main() {
    let mut iters: usize = 1;
    let mut warmup: usize = 0;
    let mut json = false;
    let mut metrics = false;
    let mut backend = Backend::Mir;
    let mut opt_level = rusk_bytecode::OptLevel::default();
    let mut path: Option<String> = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--backend" => {
                let Some(value) = args.next() else { usage() };
                backend = match value.as_str() {
                    "mir" => Backend::Mir,
                    "bytecode" => Backend::Bytecode,
                    _ => usage(),
                };
            }
            "--opt-level" => {
                let Some(value) = args.next() else { usage() };
                opt_level = match value.as_str() {
                    "o0" | "O0" => rusk_bytecode::OptLevel::O0,
                    "o1" | "O1" => rusk_bytecode::OptLevel::O1,
                    "o2" | "O2" => rusk_bytecode::OptLevel::O2,
                    _ => usage(),
                };
            }
            "--metrics" => metrics = true,
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

    if iters == 0 {
        eprintln!("error: --iters must be >= 1");
        process::exit(2);
    }
    match backend {
        Backend::Mir => {
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
                    eprintln!("error: MIR backend expects .rusk or .mir input");
                    process::exit(2);
                }
            };

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

            println!("Backend: mir");
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
        Backend::Bytecode => {
            let (module, compile_metrics) = match extension {
                Some("rusk") => {
                    let mut options = CompileOptions::default();
                    std_io::register_host_module(&mut options);
                    options.opt_level = opt_level;
                    let (mir, mut metrics) =
                        match compile_file_to_mir_with_options_and_metrics(input_path, &options) {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("compile error: {e}");
                                process::exit(1);
                            }
                        };

                    let bc_lower_start = Instant::now();
                    let mut bc_module = match rusk_bytecode::lower_mir_module(&mir) {
                        Ok(m) => m,
                        Err(e) => {
                            eprintln!("bytecode lowering error: {}", e.message);
                            process::exit(1);
                        }
                    };
                    let bc_lower_time = bc_lower_start.elapsed();
                    let opt_start = Instant::now();
                    if let Err(e) =
                        rusk_bytecode::peephole_optimize_module(&mut bc_module, opt_level)
                    {
                        eprintln!("bytecode opt error: {}", e.message);
                        process::exit(1);
                    }
                    let bc_opt_time = opt_start.elapsed();
                    metrics.lower_time += bc_lower_time;
                    metrics.lower_time += bc_opt_time;
                    metrics.total_time += bc_lower_time;
                    metrics.total_time += bc_opt_time;
                    (bc_module, metrics)
                }
                Some("rbc") => {
                    let load_start = Instant::now();
                    let bytes = match fs::read(input_path) {
                        Ok(b) => b,
                        Err(e) => {
                            eprintln!("failed to read file: {e}");
                            process::exit(1);
                        }
                    };
                    let module = match rusk_bytecode::from_bytes(&bytes) {
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
                Some("mir") => {
                    let load_start = Instant::now();
                    let bytes = match fs::read(input_path) {
                        Ok(b) => b,
                        Err(e) => {
                            eprintln!("failed to read file: {e}");
                            process::exit(1);
                        }
                    };
                    let mir = match from_bytes(&bytes) {
                        Ok(m) => m,
                        Err(e) => {
                            eprintln!("load error: {e}");
                            process::exit(1);
                        }
                    };
                    let bc_lower_start = Instant::now();
                    let mut module = match rusk_bytecode::lower_mir_module(&mir) {
                        Ok(m) => m,
                        Err(e) => {
                            eprintln!("bytecode lowering error: {}", e.message);
                            process::exit(1);
                        }
                    };
                    let bc_lower_time = bc_lower_start.elapsed();
                    let opt_start = Instant::now();
                    if let Err(e) = rusk_bytecode::peephole_optimize_module(&mut module, opt_level)
                    {
                        eprintln!("bytecode opt error: {}", e.message);
                        process::exit(1);
                    }
                    let bc_opt_time = opt_start.elapsed();
                    let mut metrics = CompileMetrics::default();
                    metrics.load_time = load_start.elapsed();
                    metrics.lower_time = bc_lower_time + bc_opt_time;
                    metrics.total_time = metrics.load_time + metrics.lower_time;
                    (module, metrics)
                }
                _ => {
                    eprintln!("error: bytecode backend expects .rusk, .rbc, or .mir input");
                    process::exit(2);
                }
            };

            let total_runs = warmup.saturating_add(iters);
            let mut run_time_total = Duration::ZERO;
            let mut last_result: Option<AbiValue> = None;
            let mut agg_metrics = rusk_vm::VmMetrics::default();

            for run_index in 0..total_runs {
                let mut vm = match Vm::new(module.clone()) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("vm init error: {e}");
                        process::exit(1);
                    }
                };
                std_io::install_vm(&module, &mut vm);
                if metrics {
                    vm.enable_metrics(true);
                    vm.reset_metrics();
                }

                let run_start = Instant::now();
                let step = vm_step(&mut vm, None);
                let run_time = run_start.elapsed();
                let run_metrics = if metrics {
                    Some(vm.take_metrics())
                } else {
                    None
                };

                let result = match step {
                    StepResult::Done { value } => value,
                    StepResult::Trap { message } => {
                        eprintln!("runtime error: {message}");
                        process::exit(1);
                    }
                    StepResult::Request {
                        effect_id, args, ..
                    } => {
                        let name = module
                            .external_effect(effect_id)
                            .map(|d| format!("{}.{}", d.interface, d.method))
                            .unwrap_or_else(|| format!("<unknown {}>", effect_id.0));
                        eprintln!("runtime error: external effect request: {name} args={args:?}");
                        process::exit(1);
                    }
                    StepResult::Yield { .. } => {
                        eprintln!("runtime error: unexpected yield");
                        process::exit(1);
                    }
                };

                if run_index >= warmup {
                    run_time_total += run_time;
                    last_result = Some(result);
                    if let Some(m) = run_metrics {
                        agg_metrics.add_from(&m);
                    }
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
\"metrics\":{{\"executed_instructions\":0,\"executed_terminators\":0,\"block_entries\":0,\
\"allocations\":0,\"gc_cycles\":0,\"gc_ns\":0,\
\"call_instructions\":0,\"icall_instructions\":0,\"vcall_instructions\":0,\
\"host_calls\":0,\"mir_calls\":0,\
\"br_terminators\":0,\"cond_br_terminators\":0,\"switch_terminators\":0,\
\"return_terminators\":0,\"trap_terminators\":0}},\
\"vm_metrics\":{{\"executed_instructions\":{vm_ei},\"const_instructions\":{vm_const},\
\"copy_instructions\":{vm_copy},\"move_instructions\":{vm_move},\"as_readonly_instructions\":{vm_ro},\
\"int_binop_instructions\":{vm_ibi},\"int_cmp_instructions\":{vm_icmp},\"bool_op_instructions\":{vm_bop},\
\"call_instructions\":{vm_call},\"icall_instructions\":{vm_icall},\"vcall_instructions\":{vm_vcall},\
\"push_handler_instructions\":{vm_pushh},\"pop_handler_instructions\":{vm_poph},\
\"perform_instructions\":{vm_perf},\"resume_instructions\":{vm_res},\
\"jump_instructions\":{vm_j},\"jumpif_instructions\":{vm_jif},\"switch_instructions\":{vm_sw},\
\"return_instructions\":{vm_ret},\"trap_instructions\":{vm_trap},\"other_instructions\":{vm_other}}}}}}}",
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
                    vm_ei = agg_metrics.executed_instructions,
                    vm_const = agg_metrics.const_instructions,
                    vm_copy = agg_metrics.copy_instructions,
                    vm_move = agg_metrics.move_instructions,
                    vm_ro = agg_metrics.as_readonly_instructions,
                    vm_ibi = agg_metrics.int_binop_instructions,
                    vm_icmp = agg_metrics.int_cmp_instructions,
                    vm_bop = agg_metrics.bool_op_instructions,
                    vm_call = agg_metrics.call_instructions,
                    vm_icall = agg_metrics.icall_instructions,
                    vm_vcall = agg_metrics.vcall_instructions,
                    vm_pushh = agg_metrics.push_handler_instructions,
                    vm_poph = agg_metrics.pop_handler_instructions,
                    vm_perf = agg_metrics.perform_instructions,
                    vm_res = agg_metrics.resume_instructions,
                    vm_j = agg_metrics.jump_instructions,
                    vm_jif = agg_metrics.jumpif_instructions,
                    vm_sw = agg_metrics.switch_instructions,
                    vm_ret = agg_metrics.return_instructions,
                    vm_trap = agg_metrics.trap_instructions,
                    vm_other = agg_metrics.other_instructions,
                );
                return;
            }

            println!("Backend: bytecode");
            println!("Opt level: {:?}", opt_level);
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
            if metrics {
                println!("  metrics:   {agg_metrics:?}");
            } else {
                println!("  metrics:   <not collected; pass --metrics>");
            }

            if let Some(result) = last_result
                && result != AbiValue::Unit
            {
                println!("Result: {result:?}");
            }
        }
    }
}
