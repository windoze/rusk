#![forbid(unsafe_code)]

use js_sys::{Array, BigInt, Function, Object, Reflect, Uint8Array};
use rusk_bytecode::{AbiType, EffectId, ExecutableModule, HostImportId};
use rusk_vm::{AbiValue, ContinuationHandle, HostError, VmError};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

const MAX_SAFE_INTEGER: u64 = 9_007_199_254_740_991; // 2^53 - 1

#[wasm_bindgen]
pub struct Vm {
    module: ExecutableModule,
    vm: rusk_vm::Vm,
    state: VmState,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum VmState {
    RunningOrSuspended,
    Done,
    Trapped,
}

#[wasm_bindgen]
impl Vm {
    #[wasm_bindgen(constructor)]
    pub fn new(rbc_bytes: &[u8]) -> Result<Self, JsValue> {
        let module =
            rusk_bytecode::from_bytes(rbc_bytes).map_err(|e| JsValue::from_str(&e.to_string()))?;
        let vm = rusk_vm::Vm::new(module.clone())
            .map_err(|e| JsValue::from_str(&format!("vm init failed: {e}")))?;
        Ok(Self {
            module,
            vm,
            state: VmState::RunningOrSuspended,
        })
    }

    #[wasm_bindgen(js_name = listHostImports)]
    pub fn list_host_imports(&self) -> Array {
        let out = Array::new();
        for (idx, import) in self.module.host_imports.iter().enumerate() {
            let obj = Object::new();

            let id_u32: u32 = idx.try_into().unwrap_or(u32::MAX);
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("id"),
                &JsValue::from_f64(id_u32 as f64),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("name"),
                &JsValue::from_str(&import.name),
            );

            let params = Array::new();
            for ty in &import.sig.params {
                params.push(&abi_type_to_js(*ty));
            }
            let _ = Reflect::set(&obj, &JsValue::from_str("params"), &params.into());
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("ret"),
                &abi_type_to_js(import.sig.ret),
            );

            out.push(&obj);
        }
        out
    }

    #[wasm_bindgen(js_name = listExternalEffects)]
    pub fn list_external_effects(&self) -> Array {
        let out = Array::new();
        for (idx, eff) in self.module.external_effects.iter().enumerate() {
            let obj = Object::new();

            let id_u32: u32 = idx.try_into().unwrap_or(u32::MAX);
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("id"),
                &JsValue::from_f64(id_u32 as f64),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("interface"),
                &JsValue::from_str(&eff.interface),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("method"),
                &JsValue::from_str(&eff.method),
            );

            let params = Array::new();
            for ty in &eff.sig.params {
                params.push(&abi_type_to_js(*ty));
            }
            let _ = Reflect::set(&obj, &JsValue::from_str("params"), &params.into());
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("ret"),
                &abi_type_to_js(eff.sig.ret),
            );

            out.push(&obj);
        }
        out
    }

    #[wasm_bindgen(js_name = registerHostImport)]
    pub fn register_host_import(&mut self, id: u32, f: Function) -> Result<(), JsValue> {
        let id = HostImportId(id);
        let Some(import) = self.module.host_import(id) else {
            return Err(JsValue::from_str(&format!(
                "invalid host import id {}",
                id.0
            )));
        };

        self.vm
            .register_host_import(
                id,
                JsHostFn {
                    name: import.name.clone(),
                    f,
                },
            )
            .map_err(|e| JsValue::from_str(&e.to_string()))
    }

    #[wasm_bindgen]
    pub fn step(&mut self, fuel: Option<JsValue>) -> Result<JsValue, JsValue> {
        let fuel_u64 = match fuel {
            None => None,
            Some(v) if v.is_undefined() || v.is_null() => None,
            Some(v) => Some(js_to_u64(&v)?),
        };

        let r = rusk_vm::vm_step(&mut self.vm, fuel_u64);
        self.state = match &r {
            rusk_vm::StepResult::Done { .. } => VmState::Done,
            rusk_vm::StepResult::Trap { .. } => VmState::Trapped,
            rusk_vm::StepResult::Request { .. } | rusk_vm::StepResult::Yield { .. } => {
                VmState::RunningOrSuspended
            }
        };

        Ok(step_result_to_js(r))
    }

    #[wasm_bindgen]
    pub fn resume(
        &mut self,
        k_index: u32,
        k_generation: u32,
        value: JsValue,
    ) -> Result<(), JsValue> {
        let value = js_to_abi_value(value).map_err(|message| JsValue::from_str(&message))?;
        rusk_vm::vm_resume(
            &mut self.vm,
            ContinuationHandle {
                index: k_index,
                generation: k_generation,
            },
            value,
        )
        .map_err(vm_error_to_js)?;
        self.state = VmState::RunningOrSuspended;
        Ok(())
    }

    #[wasm_bindgen(js_name = dropContinuation)]
    pub fn drop_continuation(&mut self, k_index: u32, k_generation: u32) -> Result<(), JsValue> {
        rusk_vm::vm_drop_continuation(
            &mut self.vm,
            ContinuationHandle {
                index: k_index,
                generation: k_generation,
            },
        )
        .map_err(vm_error_to_js)?;
        self.state = VmState::Trapped;
        Ok(())
    }

    #[wasm_bindgen(js_name = dropPinnedContinuation)]
    pub fn drop_pinned_continuation(
        &mut self,
        k_index: u32,
        k_generation: u32,
    ) -> Result<(), JsValue> {
        rusk_vm::vm_drop_pinned_continuation(
            &mut self.vm,
            ContinuationHandle {
                index: k_index,
                generation: k_generation,
            },
        )
        .map_err(vm_error_to_js)?;
        Ok(())
    }

    #[wasm_bindgen(js_name = resumePinnedContinuationTail)]
    pub fn resume_pinned_continuation_tail(
        &mut self,
        k_index: u32,
        k_generation: u32,
        value: JsValue,
    ) -> Result<(), JsValue> {
        let value = js_to_abi_value(value).map_err(|message| JsValue::from_str(&message))?;
        rusk_vm::vm_resume_pinned_continuation_tail(
            &mut self.vm,
            ContinuationHandle {
                index: k_index,
                generation: k_generation,
            },
            value,
        )
        .map_err(vm_error_to_js)?;
        self.state = VmState::RunningOrSuspended;
        Ok(())
    }

    #[wasm_bindgen(js_name = isDone)]
    pub fn is_done(&self) -> bool {
        matches!(self.state, VmState::Done)
    }

    #[wasm_bindgen(js_name = isTrapped)]
    pub fn is_trapped(&self) -> bool {
        matches!(self.state, VmState::Trapped)
    }
}

struct JsHostFn {
    name: String,
    f: Function,
}

impl rusk_vm::HostFn for JsHostFn {
    fn call(&mut self, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let js_args = Array::new();
        for arg in args {
            js_args.push(&abi_value_to_js(arg));
        }

        let result = self.f.call1(&JsValue::UNDEFINED, &js_args.into());
        let result = match result {
            Ok(v) => v,
            Err(e) => {
                return Err(HostError {
                    message: format!("js host import `{}` threw: {}", self.name, js_stringify(&e)),
                });
            }
        };

        js_to_abi_value(result).map_err(|message| HostError {
            message: format!(
                "js host import `{}` returned invalid AbiValue: {message}",
                self.name
            ),
        })
    }
}

fn abi_type_to_js(ty: AbiType) -> JsValue {
    JsValue::from_str(match ty {
        AbiType::Unit => "unit",
        AbiType::Bool => "bool",
        AbiType::Int => "int",
        AbiType::Float => "float",
        AbiType::String => "string",
        AbiType::Bytes => "bytes",
        AbiType::Continuation => "continuation",
    })
}

fn abi_value_to_js(v: &AbiValue) -> JsValue {
    match v {
        AbiValue::Unit => JsValue::UNDEFINED,
        AbiValue::Bool(b) => JsValue::from_bool(*b),
        AbiValue::Int(n) => JsValue::from(BigInt::from(*n)),
        AbiValue::Float(x) => JsValue::from_f64(*x),
        AbiValue::String(s) => JsValue::from_str(s),
        AbiValue::Bytes(b) => Uint8Array::from(b.as_slice()).into(),
        AbiValue::Continuation(k) => {
            let obj = Object::new();
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("index"),
                &JsValue::from_f64(k.index as f64),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("generation"),
                &JsValue::from_f64(k.generation as f64),
            );
            obj.into()
        }
    }
}

fn js_to_abi_value(v: JsValue) -> Result<AbiValue, String> {
    if v.is_undefined() || v.is_null() {
        return Ok(AbiValue::Unit);
    }
    if let Some(b) = v.as_bool() {
        return Ok(AbiValue::Bool(b));
    }
    if v.is_bigint() {
        let bi: BigInt = v
            .dyn_into()
            .map_err(|_| "expected bigint for int(i64)".to_string())?;
        let n = i64::try_from(bi).map_err(|_| "bigint out of range for int(i64)".to_string())?;
        return Ok(AbiValue::Int(n));
    }
    if let Some(n) = v.as_f64() {
        return Ok(AbiValue::Float(n));
    }
    if let Some(s) = v.as_string() {
        return Ok(AbiValue::String(s));
    }
    if v.is_instance_of::<Uint8Array>() {
        let bytes: Uint8Array = v
            .dyn_into()
            .map_err(|_| "expected Uint8Array for bytes".to_string())?;
        return Ok(AbiValue::Bytes(bytes.to_vec()));
    }
    if v.is_object() {
        // Continuation handle object: `{ index: u32, generation: u32 }`.
        let index_v = Reflect::get(&v, &JsValue::from_str("index")).unwrap_or(JsValue::UNDEFINED);
        let gen_v =
            Reflect::get(&v, &JsValue::from_str("generation")).unwrap_or(JsValue::UNDEFINED);
        if !index_v.is_undefined() && !gen_v.is_undefined() {
            let index = js_to_u32(&index_v).map_err(|e| format!("continuation.index: {e}"))?;
            let generation =
                js_to_u32(&gen_v).map_err(|e| format!("continuation.generation: {e}"))?;
            return Ok(AbiValue::Continuation(ContinuationHandle {
                index,
                generation,
            }));
        }
    }

    Err(format!(
        "unsupported AbiValue JS type (got {})",
        js_typeof(&v)
    ))
}

fn js_to_u32(v: &JsValue) -> Result<u32, String> {
    if v.is_bigint() {
        let bi: BigInt = v
            .clone()
            .dyn_into()
            .map_err(|_| "expected bigint".to_string())?;
        let n_i64 = i64::try_from(bi).map_err(|_| "bigint out of range for u32".to_string())?;
        return u32::try_from(n_i64).map_err(|_| "bigint out of range for u32".to_string());
    }

    let Some(n) = v.as_f64() else {
        return Err("expected number or bigint".to_string());
    };
    if !n.is_finite() {
        return Err("must be finite".to_string());
    }
    if n < 0.0 {
        return Err("must be >= 0".to_string());
    }
    if n.fract() != 0.0 {
        return Err("must be an integer".to_string());
    }
    let u = n as u32;
    if (u as f64) != n {
        return Err("out of range for u32".to_string());
    }
    Ok(u)
}

fn js_to_u64(v: &JsValue) -> Result<u64, JsValue> {
    if v.is_bigint() {
        let bi: BigInt = v
            .clone()
            .dyn_into()
            .map_err(|_| JsValue::from_str("fuel: expected bigint"))?;
        return u64::try_from(bi).map_err(|_| JsValue::from_str("fuel: bigint out of range"));
    }

    let Some(n) = v.as_f64() else {
        return Err(JsValue::from_str("fuel: expected number or bigint"));
    };
    if !n.is_finite() {
        return Err(JsValue::from_str("fuel: must be finite"));
    }
    if n < 0.0 {
        return Err(JsValue::from_str("fuel: must be >= 0"));
    }
    if n.fract() != 0.0 {
        return Err(JsValue::from_str("fuel: must be an integer"));
    }
    let u = n as u64;
    if (u as f64) != n {
        return Err(JsValue::from_str(
            "fuel: out of range (use bigint for large values)",
        ));
    }
    Ok(u)
}

fn u64_to_js(n: u64) -> JsValue {
    if n <= MAX_SAFE_INTEGER {
        JsValue::from_f64(n as f64)
    } else {
        JsValue::from(BigInt::from(n))
    }
}

fn step_result_to_js(r: rusk_vm::StepResult) -> JsValue {
    match r {
        rusk_vm::StepResult::Done { value } => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("done"));
            let _ = Reflect::set(&obj, &JsValue::from_str("value"), &abi_value_to_js(&value));
            obj.into()
        }
        rusk_vm::StepResult::Trap { message } => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("trap"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("message"),
                &JsValue::from_str(&message),
            );
            obj.into()
        }
        rusk_vm::StepResult::Request { effect_id, args, k } => request_to_js(effect_id, args, k),
        rusk_vm::StepResult::Yield { remaining_fuel } => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("yield"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("remainingFuel"),
                &u64_to_js(remaining_fuel),
            );
            obj.into()
        }
    }
}

fn request_to_js(effect_id: EffectId, args: Vec<AbiValue>, k: ContinuationHandle) -> JsValue {
    let obj = Object::new();
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("tag"),
        &JsValue::from_str("request"),
    );
    let _ = Reflect::set(
        &obj,
        &JsValue::from_str("effectId"),
        &JsValue::from_f64(effect_id.0 as f64),
    );

    let js_args = Array::new();
    for arg in args {
        js_args.push(&abi_value_to_js(&arg));
    }
    let _ = Reflect::set(&obj, &JsValue::from_str("args"), &js_args.into());

    let k_obj = Object::new();
    let _ = Reflect::set(
        &k_obj,
        &JsValue::from_str("index"),
        &JsValue::from_f64(k.index as f64),
    );
    let _ = Reflect::set(
        &k_obj,
        &JsValue::from_str("generation"),
        &JsValue::from_f64(k.generation as f64),
    );
    let _ = Reflect::set(&obj, &JsValue::from_str("k"), &k_obj.into());

    obj.into()
}

fn vm_error_to_js(e: VmError) -> JsValue {
    JsValue::from_str(&e.to_string())
}

fn js_stringify(v: &JsValue) -> String {
    match js_sys::JSON::stringify(v) {
        Ok(s) => s
            .as_string()
            .unwrap_or_else(|| "<non-string json>".to_string()),
        Err(_) => format!("{v:?}"),
    }
}

fn js_typeof(v: &JsValue) -> String {
    v.js_typeof()
        .as_string()
        .unwrap_or_else(|| "<unknown>".to_string())
}
