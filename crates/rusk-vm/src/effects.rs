use rusk_bytecode::{EffectId, ExecutableModule};

use crate::{
    AbiArgs, AbiTypeOf, AbiValue, HostError, HostFn, StepResult, Vm, VmError, vm_resume, vm_step,
};

/// A dense `EffectId -> handler` dispatch table for externalized effects.
///
/// This is a host-side helper: it avoids repeated per-request string matching by resolving
/// `(interface, method)` to an `EffectId` once at startup, then dispatching by table index.
pub struct EffectDispatchTable {
    handlers: Vec<Option<Box<dyn HostFn>>>,
}

impl EffectDispatchTable {
    /// Creates an empty dispatch table for the external effects declared in `module`.
    pub fn new(module: &ExecutableModule) -> Self {
        let len = module.external_effects.len();
        let mut handlers = Vec::with_capacity(len);
        handlers.resize_with(len, || None);
        Self { handlers }
    }

    /// Registers a typed handler for an external effect declared in `module`.
    ///
    /// This is strict by default: it returns an error if the module does not declare the effect or
    /// if the declared signature does not match the handler's typed signature.
    pub fn register_typed<Args, Ret>(
        &mut self,
        module: &ExecutableModule,
        interface: &str,
        method: &str,
        mut f: impl FnMut(Args) -> Result<Ret, HostError> + 'static,
    ) -> Result<(), String>
    where
        for<'a> Args: AbiArgs<'a>,
        Ret: AbiTypeOf + Into<AbiValue>,
    {
        let effect_id = module
            .external_effect_id(interface, method)
            .ok_or_else(|| {
                format!("external effect `{interface}.{method}` not declared in module")
            })?;
        let idx: usize = effect_id.0 as usize;
        let Some(decl) = module.external_effect(effect_id) else {
            return Err(format!("invalid EffectId {}", effect_id.0));
        };

        let expected_params = Args::abi_param_types();
        let expected_ret = Ret::abi_type();
        if decl.sig.params != expected_params || decl.sig.ret != expected_ret {
            return Err(format!(
                "external effect `{interface}.{method}` signature mismatch: module declares ({:?}) -> {:?}, handler expects ({:?}) -> {:?}",
                decl.sig.params, decl.sig.ret, expected_params, expected_ret
            ));
        }

        if idx >= self.handlers.len() {
            return Err(format!(
                "internal error: external effect table out of range (id {} / len {})",
                effect_id.0,
                self.handlers.len()
            ));
        }
        if self.handlers[idx].is_some() {
            return Err(format!(
                "external effect `{interface}.{method}` handler already registered"
            ));
        }

        self.handlers[idx] = Some(Box::new(move |args: &[AbiValue]| {
            // VM already validated ABI types for externalized effect args, but we still decode
            // into host-friendly Rust types here.
            let decoded = Args::decode(args)?;
            let ret = f(decoded)?;
            Ok(ret.into())
        }));
        Ok(())
    }

    /// Dispatches a single external effect request by `effect_id`.
    pub fn dispatch(
        &mut self,
        effect_id: EffectId,
        args: &[AbiValue],
    ) -> Result<AbiValue, HostError> {
        let idx: usize = effect_id.0 as usize;
        let Some(slot) = self.handlers.get_mut(idx) else {
            return Err(HostError {
                message: format!(
                    "external effect dispatch out of range: id {} (len {})",
                    effect_id.0,
                    self.handlers.len()
                ),
            });
        };
        let Some(handler) = slot.as_mut() else {
            return Err(HostError {
                message: format!(
                    "no handler registered for external effect id {}",
                    effect_id.0
                ),
            });
        };
        handler.call(args)
    }
}

/// Errors produced by [`vm_step_with_effects`].
#[derive(Debug)]
pub enum StepWithEffectsError {
    /// Dispatch failed for a `StepResult::Request`.
    Dispatch {
        effect_id: EffectId,
        args: Vec<AbiValue>,
        k: crate::ContinuationHandle,
        error: HostError,
    },
    /// VM resume failed after successfully dispatching an effect request.
    Resume {
        effect_id: EffectId,
        k: crate::ContinuationHandle,
        error: VmError,
    },
}

impl core::fmt::Display for StepWithEffectsError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            StepWithEffectsError::Dispatch {
                effect_id, error, ..
            } => write!(
                f,
                "external effect dispatch failed (id {}): {error}",
                effect_id.0
            ),
            StepWithEffectsError::Resume { error, .. } => write!(f, "vm resume failed: {error}"),
        }
    }
}

impl std::error::Error for StepWithEffectsError {}

/// Steps the VM while automatically handling externalized effects via an [`EffectDispatchTable`].
///
/// Semantics:
/// - Returns `Ok(Done/Trap/Yield)` when the VM reaches a non-request boundary.
/// - On `Request`, dispatches via `effects`, calls `vm_resume`, and continues stepping.
/// - If dispatch/resume fails, returns an error and leaves the VM suspended (the caller can
///   `vm_resume` or `vm_drop_continuation` using the provided continuation handle).
///
/// Fuel is applied per internal `vm_step` segment (between effect suspensions).
pub fn vm_step_with_effects(
    vm: &mut Vm,
    fuel: Option<u64>,
    effects: &mut EffectDispatchTable,
) -> Result<StepResult, StepWithEffectsError> {
    loop {
        let step = vm_step(vm, fuel);
        match step {
            StepResult::Done { .. } | StepResult::Trap { .. } | StepResult::Yield { .. } => {
                return Ok(step);
            }
            StepResult::Request { effect_id, args, k } => {
                let resume_value = effects.dispatch(effect_id, &args).map_err(|error| {
                    StepWithEffectsError::Dispatch {
                        effect_id,
                        args,
                        k: k.clone(),
                        error,
                    }
                })?;
                vm_resume(vm, k.clone(), resume_value).map_err(|error| {
                    StepWithEffectsError::Resume {
                        effect_id,
                        k,
                        error,
                    }
                })?;
                continue;
            }
        }
    }
}
