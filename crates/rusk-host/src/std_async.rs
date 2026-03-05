use rusk_bytecode::ExecutableModule;
use rusk_compiler::CompileOptions;
use rusk_vm::{AbiValue, HostError, StepResult, Vm, vm_resume, vm_step};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::sync::mpsc;

const STD_HOST_ASYNC_MODULE: &str = "_std_host_async";
const WAIT_NEXT_IFACE: &str = "std::async::_HostAsync";
const WAIT_NEXT_METHOD: &str = "wait_next";

fn encode_pending() -> Vec<u8> {
    vec![0x00]
}

fn encode_ok(mut payload: Vec<u8>) -> Vec<u8> {
    let mut out = Vec::with_capacity(1 + payload.len());
    out.push(0x01);
    out.append(&mut payload);
    out
}

fn encode_err(msg: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(1 + msg.len());
    out.push(0x02);
    out.extend_from_slice(msg.as_bytes());
    out
}

fn encode_cancelled() -> Vec<u8> {
    vec![0x03]
}

/// Registers the `_std_host_async` host module into compiler options.
///
/// This is intended to be paired with [`TokioHostAsync::install_vm`] so the produced bytecode can
/// resolve host imports at runtime.
pub fn register_host_module(options: &mut CompileOptions) {
    let module = rusk_compiler::HostModuleDecl::public()
        .function::<(i64,), i64>("sleep_start_ms")
        .function::<(String,), i64>("http_get_start")
        .function::<(i64,), bool>("op_cancel")
        .function::<(i64,), Vec<u8>>("op_take")
        .build();

    options
        .register_host_module(STD_HOST_ASYNC_MODULE, module)
        .expect("_std_host_async host module declaration must be valid");
}

/// Registers `std::async::_HostAsync.wait_next` as an externalized effect.
pub fn register_external_effects(options: &mut CompileOptions) {
    options
        .register_external_effect_typed::<(), i64>(WAIT_NEXT_IFACE, WAIT_NEXT_METHOD)
        .expect("std::async::_HostAsync.wait_next external effect declaration must be valid");
}

/// Convenience helper: registers both `_std_host_async` host imports and the `wait_next` external
/// effect.
pub fn register(options: &mut CompileOptions) {
    register_host_module(options);
    register_external_effects(options);
}

#[derive(Debug)]
enum OpEntry {
    Pending { handle: tokio::task::JoinHandle<()> },
    Completed { result: Vec<u8> },
}

#[derive(Debug, Default)]
struct OpTable {
    next_id: i64,
    ops: HashMap<i64, OpEntry>,
}

impl OpTable {
    fn alloc_id(&mut self) -> i64 {
        let id = self.next_id;
        self.next_id = self.next_id.saturating_add(1);
        // Reserve 0 as "invalid".
        if id == 0 {
            return self.alloc_id();
        }
        id
    }
}

/// Tokio-backed async host ops for a single VM instance.
///
/// This provides:
/// - synchronous host imports (`sleep_start_ms`, `http_get_start`, `op_cancel`, `op_take`)
/// - an async completion queue used to implement `std::async::_HostAsync.wait_next`.
pub struct TokioHostAsync {
    table: Arc<Mutex<OpTable>>,
    tx: mpsc::UnboundedSender<i64>,
    rx: mpsc::UnboundedReceiver<i64>,
    #[cfg(feature = "http")]
    client: reqwest::Client,
}

impl TokioHostAsync {
    pub fn new() -> Self {
        let (tx, rx) = mpsc::unbounded_channel();
        Self {
            table: Arc::new(Mutex::new(OpTable {
                next_id: 1,
                ops: HashMap::new(),
            })),
            tx,
            rx,
            #[cfg(feature = "http")]
            client: reqwest::Client::new(),
        }
    }

    /// Installs `_std_host_async` host import implementations into the given VM.
    ///
    /// The installer checks which host imports exist in the compiled module and registers only the
    /// ones that are present.
    pub fn install_vm(&self, module: &ExecutableModule, vm: &mut Vm) {
        if let Some(id) = module.host_import_id("_std_host_async::sleep_start_ms") {
            let table = Arc::clone(&self.table);
            let tx = self.tx.clone();
            vm.register_host_import_typed(id, move |(ms,): (i64,)| -> Result<i64, HostError> {
                let handle = tokio::runtime::Handle::try_current().map_err(|e| HostError {
                    message: format!("_std_host_async::sleep_start_ms: no Tokio runtime: {e}"),
                })?;

                let op_id = {
                    let mut table = table.lock().map_err(|_| HostError {
                        message: "_std_host_async::sleep_start_ms: op table lock poisoned"
                            .to_string(),
                    })?;
                    table.alloc_id()
                };

                if ms < 0 {
                    let mut table = table.lock().map_err(|_| HostError {
                        message: "_std_host_async::sleep_start_ms: op table lock poisoned"
                            .to_string(),
                    })?;
                    table.ops.insert(
                        op_id,
                        OpEntry::Completed {
                            result: encode_err("sleep_ms: negative duration"),
                        },
                    );
                    let _ = tx.send(op_id);
                    return Ok(op_id);
                }

                let table2 = Arc::clone(&table);
                let tx2 = tx.clone();
                let sleep_ms: u64 = ms as u64;
                let join = handle.spawn(async move {
                    tokio::time::sleep(Duration::from_millis(sleep_ms)).await;
                    let result = encode_ok(Vec::new());

                    let should_send = {
                        let mut table = match table2.lock() {
                            Ok(t) => t,
                            Err(_) => return,
                        };
                        match table.ops.get_mut(&op_id) {
                            Some(entry @ OpEntry::Pending { .. }) => {
                                *entry = OpEntry::Completed { result };
                                true
                            }
                            Some(OpEntry::Completed { .. }) => false,
                            None => false,
                        }
                    };

                    if should_send {
                        let _ = tx2.send(op_id);
                    }
                });

                let mut table = table.lock().map_err(|_| HostError {
                    message: "_std_host_async::sleep_start_ms: op table lock poisoned".to_string(),
                })?;
                table.ops.insert(op_id, OpEntry::Pending { handle: join });
                Ok(op_id)
            })
            .expect("_std_host_async::sleep_start_ms host import id must be valid");
        }

        if let Some(id) = module.host_import_id("_std_host_async::http_get_start") {
            let table = Arc::clone(&self.table);
            let tx = self.tx.clone();
            #[cfg(feature = "http")]
            let client = self.client.clone();

            vm.register_host_import_typed(
                id,
                move |(_url,): (String,)| -> Result<i64, HostError> {
                    let op_id = {
                        let mut table = table.lock().map_err(|_| HostError {
                            message: "_std_host_async::http_get_start: op table lock poisoned"
                                .to_string(),
                        })?;
                        table.alloc_id()
                    };

                    #[cfg(not(feature = "http"))]
                    {
                        let mut table = table.lock().map_err(|_| HostError {
                            message: "_std_host_async::http_get_start: op table lock poisoned"
                                .to_string(),
                        })?;
                        table.ops.insert(
                            op_id,
                            OpEntry::Completed {
                                result: encode_err(
                                    "http disabled (compile with rusk-host feature `http`)",
                                ),
                            },
                        );
                        let _ = tx.send(op_id);
                        return Ok(op_id);
                    }

                    #[cfg(feature = "http")]
                    {
                        let handle =
                            tokio::runtime::Handle::try_current().map_err(|e| HostError {
                                message: format!(
                                    "_std_host_async::http_get_start: no Tokio runtime: {e}"
                                ),
                            })?;

                        let table2 = Arc::clone(&table);
                        let tx2 = tx.clone();
                        let join = handle.spawn(async move {
                            let result = match client.get(_url).send().await {
                                Ok(resp) => match resp.bytes().await {
                                    Ok(body) => encode_ok(body.to_vec()),
                                    Err(e) => encode_err(&format!("http body error: {e}")),
                                },
                                Err(e) => encode_err(&format!("http request error: {e}")),
                            };

                            let should_send = {
                                let mut table = match table2.lock() {
                                    Ok(t) => t,
                                    Err(_) => return,
                                };
                                match table.ops.get_mut(&op_id) {
                                    Some(entry @ OpEntry::Pending { .. }) => {
                                        *entry = OpEntry::Completed { result };
                                        true
                                    }
                                    Some(OpEntry::Completed { .. }) => false,
                                    None => false,
                                }
                            };

                            if should_send {
                                let _ = tx2.send(op_id);
                            }
                        });

                        let mut table = table.lock().map_err(|_| HostError {
                            message: "_std_host_async::http_get_start: op table lock poisoned"
                                .to_string(),
                        })?;
                        table.ops.insert(op_id, OpEntry::Pending { handle: join });
                        Ok(op_id)
                    }
                },
            )
            .expect("_std_host_async::http_get_start host import id must be valid");
        }

        if let Some(id) = module.host_import_id("_std_host_async::op_cancel") {
            let table = Arc::clone(&self.table);
            let tx = self.tx.clone();
            vm.register_host_import_typed(id, move |(op_id,): (i64,)| -> Result<bool, HostError> {
                let mut table = table.lock().map_err(|_| HostError {
                    message: "_std_host_async::op_cancel: op table lock poisoned".to_string(),
                })?;

                let Some(entry) = table.ops.get_mut(&op_id) else {
                    return Ok(false);
                };

                match entry {
                    OpEntry::Pending { handle } => {
                        handle.abort();
                        *entry = OpEntry::Completed {
                            result: encode_cancelled(),
                        };
                        let _ = tx.send(op_id);
                        Ok(true)
                    }
                    OpEntry::Completed { .. } => Ok(false),
                }
            })
            .expect("_std_host_async::op_cancel host import id must be valid");
        }

        if let Some(id) = module.host_import_id("_std_host_async::op_take") {
            let table = Arc::clone(&self.table);
            vm.register_host_import_typed(
                id,
                move |(op_id,): (i64,)| -> Result<Vec<u8>, HostError> {
                    let mut table = table.lock().map_err(|_| HostError {
                        message: "_std_host_async::op_take: op table lock poisoned".to_string(),
                    })?;

                    let Some(entry) = table.ops.remove(&op_id) else {
                        return Ok(encode_err("invalid op_id"));
                    };

                    match entry {
                        OpEntry::Pending { handle } => {
                            // Put it back; still pending.
                            table.ops.insert(op_id, OpEntry::Pending { handle });
                            Ok(encode_pending())
                        }
                        OpEntry::Completed { result } => Ok(result),
                    }
                },
            )
            .expect("_std_host_async::op_take host import id must be valid");
        }
    }

    /// Waits until any host operation completes, returning its `op_id`.
    pub async fn wait_next(&mut self) -> Option<i64> {
        self.rx.recv().await
    }
}

/// Runs the VM until it reaches a non-request boundary (`Done` / `Trap` / `Yield`), handling
/// `std::async::_HostAsync.wait_next` requests via `host_async`.
///
/// If another external effect request is encountered, it is returned as-is (leaving the VM
/// suspended). Callers can decide how to handle it.
pub async fn vm_step_with_wait_next(
    module: &ExecutableModule,
    vm: &mut Vm,
    host_async: &mut TokioHostAsync,
) -> StepResult {
    let wait_next_id = module.external_effect_id(WAIT_NEXT_IFACE, WAIT_NEXT_METHOD);

    loop {
        match vm_step(vm, None) {
            StepResult::Done { value } => return StepResult::Done { value },
            StepResult::Trap { message } => return StepResult::Trap { message },
            StepResult::Yield { remaining_fuel } => return StepResult::Yield { remaining_fuel },
            StepResult::Request { effect_id, args, k } => {
                if wait_next_id.is_some_and(|id| id == effect_id) {
                    let op_id = host_async.wait_next().await.unwrap_or(-1);
                    if let Err(e) = vm_resume(vm, k, AbiValue::Int(op_id)) {
                        return StepResult::Trap {
                            message: format!("vm resume failed: {e}"),
                        };
                    }
                    continue;
                }

                return StepResult::Request { effect_id, args, k };
            }
        }
    }
}
