#![forbid(unsafe_code)]

use js_sys::{Array, BigInt, Function, Object, Reflect, Uint8Array};
use rusk_bytecode::{AbiType, EffectId, ExecutableModule, HostImportId};
use rusk_vm::{AbiValue, ContinuationHandle, HostContext, HostError, VmError};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;

const MAX_SAFE_INTEGER: u64 = 9_007_199_254_740_991; // 2^53 - 1

thread_local! {
    /// A process-wide ABI value table used to round-trip composite ABI values through JS.
    ///
    /// Composite ABI values (array/tuple/struct/enum) are opaque VM references with private
    /// internals, so we store them in Rust and hand JS a small `{ tag: "abiRef", abiId: ... }`
    /// object that can be passed back to the VM.
    static ABI_REF_TABLE: RefCell<HashMap<u32, AbiValue>> = RefCell::new(HashMap::new());
    static NEXT_ABI_REF_ID: Cell<u32> = Cell::new(1);
}

fn store_abi_ref(value: AbiValue) -> u32 {
    ABI_REF_TABLE.with(|table| {
        NEXT_ABI_REF_ID.with(|next| {
            let id = next.get();
            next.set(id.wrapping_add(1));
            table.borrow_mut().insert(id, value);
            id
        })
    })
}

fn load_abi_ref(id: u32) -> Option<AbiValue> {
    ABI_REF_TABLE.with(|table| table.borrow().get(&id).cloned())
}

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
                params.push(&abi_type_to_js(&self.module, ty));
            }
            let _ = Reflect::set(&obj, &JsValue::from_str("params"), &params.into());
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("ret"),
                &abi_type_to_js(&self.module, &import.sig.ret),
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
                params.push(&abi_type_to_js(&self.module, ty));
            }
            let _ = Reflect::set(&obj, &JsValue::from_str("params"), &params.into());
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("ret"),
                &abi_type_to_js(&self.module, &eff.sig.ret),
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
    fn call(
        &mut self,
        _cx: &mut HostContext<'_>,
        args: &[AbiValue],
    ) -> Result<AbiValue, HostError> {
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

fn abi_type_to_js(module: &ExecutableModule, ty: &AbiType) -> JsValue {
    match ty {
        AbiType::Unit => JsValue::from_str("unit"),
        AbiType::Bool => JsValue::from_str("bool"),
        AbiType::Int => JsValue::from_str("int"),
        AbiType::Float => JsValue::from_str("float"),
        AbiType::Byte => JsValue::from_str("byte"),
        AbiType::Char => JsValue::from_str("char"),
        AbiType::String => JsValue::from_str("string"),
        AbiType::Bytes => JsValue::from_str("bytes"),
        AbiType::Continuation => JsValue::from_str("continuation"),
        AbiType::Array(elem) => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("array"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("elem"),
                &abi_type_to_js(module, elem.as_ref()),
            );
            obj.into()
        }
        AbiType::Tuple(items) => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("tuple"));
            let arr = Array::new();
            for item in items {
                arr.push(&abi_type_to_js(module, item));
            }
            let _ = Reflect::set(&obj, &JsValue::from_str("items"), &arr.into());
            obj.into()
        }
        AbiType::Struct(type_id) => {
            let obj = Object::new();
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("tag"),
                &JsValue::from_str("struct"),
            );
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("typeId"),
                &JsValue::from_f64(type_id.0 as f64),
            );
            if let Some(name) = module.type_name(*type_id) {
                let _ = Reflect::set(&obj, &JsValue::from_str("name"), &JsValue::from_str(name));
            }
            obj.into()
        }
        AbiType::Enum(type_id) => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("enum"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("typeId"),
                &JsValue::from_f64(type_id.0 as f64),
            );
            if let Some(name) = module.type_name(*type_id) {
                let _ = Reflect::set(&obj, &JsValue::from_str("name"), &JsValue::from_str(name));
            }
            obj.into()
        }
    }
}

fn abi_value_to_js(v: &AbiValue) -> JsValue {
    match v {
        AbiValue::Unit => JsValue::UNDEFINED,
        AbiValue::Bool(b) => JsValue::from_bool(*b),
        AbiValue::Int(n) => JsValue::from(BigInt::from(*n)),
        AbiValue::Float(x) => JsValue::from_f64(*x),
        AbiValue::Byte(b) => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("byte"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("value"),
                &JsValue::from_f64((*b) as f64),
            );
            obj.into()
        }
        AbiValue::Char(c) => {
            let obj = Object::new();
            let _ = Reflect::set(&obj, &JsValue::from_str("tag"), &JsValue::from_str("char"));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("value"),
                &JsValue::from_str(&c.to_string()),
            );
            obj.into()
        }
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
        AbiValue::Array(_) | AbiValue::Tuple(_) | AbiValue::Struct(_) | AbiValue::Enum(_) => {
            // Composite ABI values are opaque VM references; store them in Rust and hand JS a
            // stable ref id that can be passed back to the VM.
            let (kind, cloned) = match v {
                AbiValue::Array(_) => ("array", v.clone()),
                AbiValue::Tuple(_) => ("tuple", v.clone()),
                AbiValue::Struct(_) => ("struct", v.clone()),
                AbiValue::Enum(_) => ("enum", v.clone()),
                _ => unreachable!(),
            };
            let id = store_abi_ref(cloned);
            let obj = Object::new();
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("tag"),
                &JsValue::from_str("abiRef"),
            );
            let _ = Reflect::set(&obj, &JsValue::from_str("kind"), &JsValue::from_str(kind));
            let _ = Reflect::set(
                &obj,
                &JsValue::from_str("abiId"),
                &JsValue::from_f64(id as f64),
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
        let tag_v = Reflect::get(&v, &JsValue::from_str("tag")).unwrap_or(JsValue::UNDEFINED);
        if let Some(tag) = tag_v.as_string() {
            match tag.as_str() {
                "byte" => {
                    let value_v =
                        Reflect::get(&v, &JsValue::from_str("value")).unwrap_or(JsValue::UNDEFINED);
                    let n = js_to_u32(&value_v).map_err(|e| format!("byte.value: {e}"))?;
                    let b: u8 = n
                        .try_into()
                        .map_err(|_| "byte.value: out of range for u8".to_string())?;
                    return Ok(AbiValue::Byte(b));
                }
                "char" => {
                    let value_v =
                        Reflect::get(&v, &JsValue::from_str("value")).unwrap_or(JsValue::UNDEFINED);
                    let s = value_v
                        .as_string()
                        .ok_or_else(|| "char.value: expected string".to_string())?;
                    let mut chars = s.chars();
                    let c = chars
                        .next()
                        .ok_or_else(|| "char.value: must not be empty".to_string())?;
                    if chars.next().is_some() {
                        return Err("char.value: must be a single Unicode scalar value".to_string());
                    }
                    return Ok(AbiValue::Char(c));
                }
                "abiRef" => {
                    let id_v =
                        Reflect::get(&v, &JsValue::from_str("abiId")).unwrap_or(JsValue::UNDEFINED);
                    let id = js_to_u32(&id_v).map_err(|e| format!("abiRef.abiId: {e}"))?;
                    return load_abi_ref(id).ok_or_else(|| {
                        format!("unknown abiRef id {id} (value is no longer available)")
                    });
                }
                _ => {}
            }
        }

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
