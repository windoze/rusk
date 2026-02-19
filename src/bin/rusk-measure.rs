use rusk_compiler::{
    CompileMetrics, CompileOptions, compile_file_to_bytecode_with_options_and_metrics,
};
use rusk_host::std_io;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::time::{Duration, Instant};

fn usage() -> ! {
    eprintln!(
        "usage: rusk-measure [--opt-level o0|o1|o2] [--metrics] [--json] [--warmup N] [--iters N] <file.rusk|file.rbc>"
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

fn main() {
    let mut iters: usize = 1;
    let mut warmup: usize = 0;
    let mut json = false;
    let mut metrics = false;
    let mut opt_level = rusk_bytecode::OptLevel::default();
    let mut path: Option<String> = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
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
    let (module, compile_metrics) = match extension {
        Some("rusk") => {
            let mut options = CompileOptions::default();
            std_io::register_host_module(&mut options);
            options.opt_level = opt_level;
            match compile_file_to_bytecode_with_options_and_metrics(input_path, &options) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("compile error: {e}");
                    process::exit(1);
                }
            }
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
        _ => {
            eprintln!("error: input file must have .rusk or .rbc extension");
            process::exit(2);
        }
    };

    let argv0 = absolute_path_string(input_path);
    let argv = vec![argv0];
    let Some(entry_fn) = module.function(module.entry) else {
        eprintln!(
            "vm init error: invalid entry function id {}",
            module.entry.0
        );
        process::exit(1);
    };

    let total_runs = warmup.saturating_add(iters);
    let mut run_time_total = Duration::ZERO;
    let mut last_result: Option<AbiValue> = None;
    let mut agg_metrics = rusk_vm::VmMetrics::default();

    for run_index in 0..total_runs {
        let mut vm = match entry_fn.param_count {
            0 => Vm::new(module.clone()),
            1 => Vm::new_with_argv(module.clone(), argv.clone()),
            n => Err(rusk_vm::VmError::InvalidState {
                message: format!("unsupported entry arity: expected 0 or 1 param, got {n}"),
            }),
        }
        .unwrap_or_else(|e| {
            eprintln!("vm init error: {e}");
            process::exit(1);
        });
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
\"vm_metrics\":{{\"executed_instructions\":{vm_ei},\"const_instructions\":{vm_const},\
\"copy_instructions\":{vm_copy},\"move_instructions\":{vm_move},\"as_readonly_instructions\":{vm_ro},\
	\"int_binop_instructions\":{vm_ibi},\"int_cmp_instructions\":{vm_icmp},\"bool_op_instructions\":{vm_bop},\
	\"call_instructions\":{vm_call},\"icall_instructions\":{vm_icall},\"vcall_instructions\":{vm_vcall},\
	\"vcall_fast_path_hits\":{vm_vcall_fast},\
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
            vm_vcall_fast = agg_metrics.vcall_fast_path_hits,
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

fn absolute_path_string(path: &Path) -> String {
    let abs = if path.is_absolute() {
        path.to_path_buf()
    } else {
        env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    };
    abs.to_string_lossy().into_owned()
}
