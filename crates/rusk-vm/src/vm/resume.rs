use super::*;

/// Resumes a suspended VM by providing the continuation result value.
///
/// Returns an error if the VM is not currently suspended or if `k` does not match the
/// currently suspended continuation handle.
pub fn vm_resume(vm: &mut Vm, k: ContinuationHandle, value: AbiValue) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }
    match &vm.state {
        VmState::Suspended {
            k: want,
            perform_dst,
        } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }

            let dst = *perform_dst;
            vm.state = VmState::Running;
            vm.continuation_generation = vm.continuation_generation.wrapping_add(1);

            if let Some(dst) = dst {
                let Some(frame) = vm.frames.last_mut() else {
                    return Err(VmError::InvalidState {
                        message: "resume with empty stack".to_string(),
                    });
                };
                let v = Value::from_abi(&mut vm.heap, &mut vm.gc_allocations_since_collect, &value)
                    .map_err(|message| VmError::InvalidState {
                        message: format!("resume value conversion failed: {message}"),
                    })?;
                write_value(frame, dst, v).map_err(|message| VmError::InvalidState {
                    message: format!("resume dst write failed: {message}"),
                })?;
            }

            Ok(())
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => {
            Err(VmError::InvalidState {
                message: "vm is not suspended".to_string(),
            })
        }
    }
}

/// Cancels a suspended VM by dropping the continuation handle.
///
/// This is equivalent to aborting the outstanding external effect request; the VM becomes
/// trapped with a cancellation message.
pub fn vm_drop_continuation(vm: &mut Vm, k: ContinuationHandle) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }

    match &vm.state {
        VmState::Suspended { k: want, .. } => {
            if want != &k {
                return Err(VmError::InvalidContinuation {
                    message: "continuation handle mismatch".to_string(),
                });
            }
            vm.continuation_generation = vm.continuation_generation.wrapping_add(1);
            vm.frames.clear();
            vm.handlers.clear();
            vm.state = VmState::Trapped {
                message: "cancelled".to_string(),
            };
            Ok(())
        }
        VmState::Running | VmState::Done { .. } | VmState::Trapped { .. } => {
            Err(VmError::InvalidState {
                message: "vm is not suspended".to_string(),
            })
        }
    }
}
