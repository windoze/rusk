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
                let v = Value::from_abi(
                    &mut vm.heap,
                    &mut vm.gc_allocations_since_collect,
                    &vm.pinned_continuations,
                    &value,
                )
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

/// Drops a host-pinned continuation handle exported via `AbiValue::Continuation`.
///
/// This releases the pinned continuation from the VM root set, allowing any captured state to be
/// collected normally (unless it is also reachable from the running VM state).
pub fn vm_drop_pinned_continuation(vm: &mut Vm, k: ContinuationHandle) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }

    vm.pinned_continuations.drop_pinned(k)
}

/// Tail-resumes a host-pinned continuation handle exported via `AbiValue::Continuation`.
///
/// This is a host-driven, non-interlaced resume operation:
///
/// - It consumes the one-shot continuation state associated with `k`.
/// - It schedules the captured continuation segment to run *on top of* the current VM stack
///   (as-if tail-resuming from a host handler frame that is not represented in the VM).
/// - The continuation's final return value is discarded (there is no "second half" host handler
///   frame to receive it). If the VM stack is empty, the resumed continuation becomes the new
///   entry computation and its return value becomes the program result as usual.
///
/// The host must subsequently call [`vm_step`] to drive execution.
pub fn vm_resume_pinned_continuation_tail(
    vm: &mut Vm,
    k: ContinuationHandle,
    value: AbiValue,
) -> Result<(), VmError> {
    if vm.in_host_call {
        return Err(VmError::InvalidState {
            message: "vm re-entered during host call".to_string(),
        });
    }

    if matches!(vm.state, VmState::Suspended { .. }) {
        return Err(VmError::InvalidState {
            message: "vm is suspended; call resume/drop first".to_string(),
        });
    }

    if matches!(vm.state, VmState::Trapped { .. }) {
        return Err(VmError::InvalidState {
            message: "vm is trapped".to_string(),
        });
    }

    let token = vm
        .pinned_continuations
        .resolve(k.clone())
        .map_err(|message| VmError::InvalidContinuation { message })?;

    // Convert the resume argument before consuming the one-shot continuation state so we don't
    // invalidate `k` on conversion failure.
    let v = Value::from_abi(
        &mut vm.heap,
        &mut vm.gc_allocations_since_collect,
        &vm.pinned_continuations,
        &value,
    )
    .map_err(|message| VmError::InvalidState {
        message: format!("resume value conversion failed: {message}"),
    })?;

    let Some(mut cont) = token.take_state() else {
        return Err(VmError::InvalidContinuation {
            message: "invalid pinned continuation handle: already consumed".to_string(),
        });
    };

    if let Some(perform_dst) = cont.perform_dst {
        let top_index = cont.frames.len().saturating_sub(1);
        let Some(top_frame) = cont.frames.get_mut(top_index) else {
            return Err(VmError::InvalidState {
                message: "invalid pinned continuation state: empty frame stack".to_string(),
            });
        };
        write_value(top_frame, perform_dst, v).map_err(|message| VmError::InvalidState {
            message: format!("resume inject failed: {message}"),
        })?;
    }

    let Some(bottom) = cont.frames.first_mut() else {
        return Err(VmError::InvalidState {
            message: "invalid pinned continuation state: empty frame stack".to_string(),
        });
    };
    bottom.return_dsts = ReturnDsts::None;

    let base_depth = vm.frames.len();
    for handler in &mut cont.handlers {
        handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
    }

    vm.frames.extend(cont.frames);
    let handlers_changed = !cont.handlers.is_empty();
    vm.handlers.extend(cont.handlers);
    vm.state = VmState::Running;

    if handlers_changed {
        vm.handler_stack_generation = vm.handler_stack_generation.wrapping_add(1);
        vm.handler_lookup_cache = None;
    }

    // Resuming consumes the continuation; release the pinned slot so the handle becomes invalid.
    vm.pinned_continuations.drop_pinned(k)?;

    Ok(())
}
