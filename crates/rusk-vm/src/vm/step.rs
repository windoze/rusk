use super::*;

/// Steps the VM until it yields, traps, requests an external effect, or completes.
///
/// When `fuel` is `Some(n)`, the VM executes at most `n` instructions and returns
/// [`StepResult::Yield`] if fuel is exhausted.
pub fn vm_step(vm: &mut Vm, fuel: Option<u64>) -> StepResult {
    if vm.in_host_call {
        return StepResult::Trap {
            message: "vm re-entered during host call".to_string(),
        };
    }

    match &vm.state {
        VmState::Running => {}
        VmState::Suspended { .. } => {
            return StepResult::Trap {
                message: "vm is suspended; call resume/drop first".to_string(),
            };
        }
        VmState::Done { value } => {
            return StepResult::Done {
                value: value.clone(),
            };
        }
        VmState::Trapped { message } => {
            return StepResult::Trap {
                message: message.clone(),
            };
        }
    }

    let mut remaining = fuel.unwrap_or(u64::MAX);

    loop {
        if fuel.is_some() && remaining == 0 {
            return StepResult::Yield { remaining_fuel: 0 };
        }

        if vm.gc_allocations_since_collect >= VM_GC_TRIGGER_ALLOCATIONS {
            vm.collect_garbage_now();
        }

        record_stack_maxima(vm);

        let frame_index = match vm.frames.len() {
            0 => {
                vm.state = VmState::Done {
                    value: AbiValue::Unit,
                };
                return StepResult::Done {
                    value: AbiValue::Unit,
                };
            }
            len => len - 1,
        };

        let frame = &mut vm.frames[frame_index];

        let Some(func) = vm.module.function(frame.func) else {
            let message = format!("invalid function id {}", frame.func.0);
            vm.state = VmState::Trapped {
                message: message.clone(),
            };
            return StepResult::Trap { message };
        };

        if frame.pc >= func.code.len() {
            let ret = Value::Unit;
            let return_dsts = frame.return_dsts.clone();
            vm.frames.pop();
            let popped_handlers = unwind_handlers_to_stack_len(&mut vm.handlers, vm.frames.len());
            if popped_handlers {
                handler_stack_changed(
                    &mut vm.handler_stack_generation,
                    &mut vm.handler_lookup_cache,
                );
            }
            if let Some(caller) = vm.frames.last_mut() {
                match return_dsts {
                    ReturnDsts::None => {}
                    ReturnDsts::One(dst) => {
                        let idx: usize = match dst.try_into() {
                            Ok(i) => i,
                            Err(_) => {
                                let message = format!("return dst reg {dst} overflow");
                                vm.state = VmState::Trapped {
                                    message: message.clone(),
                                };
                                return StepResult::Trap { message };
                            }
                        };
                        if idx >= caller.regs.len() {
                            let message = format!("return dst reg {dst} out of range");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        }
                        caller.regs[idx] = Some(ret);
                    }
                    ReturnDsts::Multi(dsts) => {
                        let message = format!(
                            "unexpected implicit return from multi-return frame (dsts={})",
                            dsts.len()
                        );
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    }
                }
                continue;
            }

            if !matches!(return_dsts, ReturnDsts::None) {
                return trap(
                    vm,
                    "non-empty return destination at entry return".to_string(),
                );
            }

            let ret = match ret.try_to_abi(&vm.heap, &mut vm.pinned_continuations) {
                Ok(Some(ret)) => ret,
                Ok(None) => return trap(vm, format!("non-ABI-safe return value ({})", ret.kind())),
                Err(msg) => return trap(vm, msg),
            };
            vm.state = VmState::Done { value: ret.clone() };
            return StepResult::Done { value: ret };
        }

        let instr = &func.code[frame.pc];
        frame.pc += 1;

        if fuel.is_some() {
            remaining = remaining.saturating_sub(1);
        }

        if vm.collect_metrics {
            vm.metrics.record(instr);
        }

        match instr {
            rusk_bytecode::Instruction::Const { dst, value } => {
                let idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let Some(slot) = frame.regs.get_mut(idx) else {
                    let message = format!("const dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let v = match value {
                    rusk_bytecode::ConstValue::Unit => Value::Unit,
                    rusk_bytecode::ConstValue::Bool(b) => Value::Bool(*b),
                    rusk_bytecode::ConstValue::Int(n) => Value::Int(*n),
                    rusk_bytecode::ConstValue::Float(x) => Value::Float(*x),
                    rusk_bytecode::ConstValue::String(s) => {
                        match alloc_string(
                            &mut vm.heap,
                            &mut vm.gc_allocations_since_collect,
                            s.clone(),
                        ) {
                            Ok(v) => v,
                            Err(msg) => return trap(vm, msg),
                        }
                    }
                    rusk_bytecode::ConstValue::Bytes(b) => {
                        match alloc_bytes(
                            &mut vm.heap,
                            &mut vm.gc_allocations_since_collect,
                            b.clone(),
                        ) {
                            Ok(v) => v,
                            Err(msg) => return trap(vm, msg),
                        }
                    }
                    rusk_bytecode::ConstValue::TypeRep(lit) => {
                        let ctor = match TypeReps::ctor_from_lit(&vm.module, lit) {
                            Ok(ctor) => ctor,
                            Err(msg) => return trap(vm, format!("const typerep: {msg}")),
                        };
                        let id = vm.type_reps.intern(TypeRepNode {
                            ctor,
                            args: Vec::new(),
                        });
                        Value::TypeRep(id)
                    }
                    rusk_bytecode::ConstValue::Function(id) => Value::Function(*id),
                };
                *slot = Some(v);
            }
            rusk_bytecode::Instruction::Copy { dst, src } => {
                let dst_idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let src_idx: usize = (*src).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned() else {
                    let message = format!("copy from uninitialized reg {src}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(dst_slot) = frame.regs.get_mut(dst_idx) else {
                    let message = format!("copy dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                *dst_slot = Some(v);
            }
            rusk_bytecode::Instruction::Move { dst, src } => {
                let dst_idx: usize = (*dst).try_into().unwrap_or(usize::MAX);
                let src_idx: usize = (*src).try_into().unwrap_or(usize::MAX);
                let Some(src_slot) = frame.regs.get_mut(src_idx) else {
                    let message = format!("move src reg {src} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(v) = src_slot.take() else {
                    let message = format!("move from uninitialized reg {src}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                let Some(dst_slot) = frame.regs.get_mut(dst_idx) else {
                    let message = format!("move dst reg {dst} out of range");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };
                *dst_slot = Some(v);
            }

            rusk_bytecode::Instruction::AsReadonly { dst, src } => {
                let v = match read_value(frame, *src) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("as_readonly src: {msg}")),
                };
                let v = v.into_readonly_view();
                if let Err(msg) = write_value(frame, *dst, v) {
                    return trap(vm, format!("as_readonly dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IsType { dst, value, ty } => {
                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("is_type value: {msg}")),
                };
                let target = match read_type_rep(frame, *ty) {
                    Ok(id) => id,
                    Err(msg) => return trap(vm, format!("is_type ty: {msg}")),
                };
                let ok = match type_test(
                    &vm.module,
                    &vm.type_reps,
                    &vm.heap,
                    &vm.primitive_type_ids,
                    &v,
                    target,
                ) {
                    Ok(ok) => ok,
                    Err(msg) => return trap(vm, msg),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(ok)) {
                    return trap(vm, format!("is_type dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::MakeTypeRep { dst, base, args } => {
                let mut arg_ids = Vec::with_capacity(args.len());
                for reg in args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_typerep arg: {msg}")),
                    };
                    arg_ids.push(id);
                }
                let ctor = match TypeReps::ctor_from_lit(&vm.module, base) {
                    Ok(ctor) => ctor,
                    Err(msg) => return trap(vm, format!("make_typerep base: {msg}")),
                };
                let id = vm.type_reps.intern(TypeRepNode {
                    ctor,
                    args: arg_ids,
                });
                if let Err(msg) = write_value(frame, *dst, Value::TypeRep(id)) {
                    return trap(vm, format!("make_typerep dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::MakeStruct {
                dst,
                type_id,
                type_args,
                fields,
            } => {
                let mut arg_ids = Vec::with_capacity(type_args.len());
                for reg in type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_struct type_args: {msg}")),
                    };
                    arg_ids.push(id);
                }

                let layout = match vm
                    .module
                    .struct_layouts
                    .get(type_id.0 as usize)
                    .and_then(|v| v.as_ref())
                {
                    Some(l) => l,
                    None => {
                        let ty = vm.module.type_name(*type_id).unwrap_or("<unknown>");
                        return trap(vm, format!("missing struct layout for `{ty}`"));
                    }
                };

                let mut out: Vec<Option<Value>> = vec![None; layout.len()];
                for (field_name, reg) in fields {
                    let value = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => {
                            return trap(vm, format!("make_struct field `{field_name}`: {msg}"));
                        }
                    };
                    let Some(idx) = layout.iter().position(|name| name == field_name) else {
                        return trap(vm, format!("missing field: {field_name}"));
                    };
                    if out[idx].is_some() {
                        let ty = vm.module.type_name(*type_id).unwrap_or("<unknown>");
                        return trap(
                            vm,
                            format!("duplicate field `{field_name}` in `{ty}` literal"),
                        );
                    }
                    out[idx] = Some(value);
                }

                let mut values = Vec::with_capacity(layout.len());
                for (idx, slot) in out.into_iter().enumerate() {
                    let Some(v) = slot else {
                        return trap(vm, format!("missing field: {}", layout[idx]));
                    };
                    values.push(v);
                }

                let obj = HeapValue::Struct {
                    type_id: *type_id,
                    type_args: arg_ids,
                    fields: values,
                };
                if let Err(msg) = write_value(
                    frame,
                    *dst,
                    alloc_ref(&mut vm.heap, &mut vm.gc_allocations_since_collect, obj),
                ) {
                    return trap(vm, format!("make_struct dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeArray { dst, items } => {
                let mut values = Vec::with_capacity(items.len());
                for reg in items {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_array item: {msg}")),
                    };
                    values.push(v);
                }
                if let Err(msg) = write_value(
                    frame,
                    *dst,
                    alloc_ref(
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        HeapValue::Array(values),
                    ),
                ) {
                    return trap(vm, format!("make_array dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeTuple { dst, items } => {
                if items.is_empty() {
                    if let Err(msg) = write_value(frame, *dst, Value::Unit) {
                        return trap(vm, format!("make_tuple dst: {msg}"));
                    }
                    continue;
                }
                let mut values = Vec::with_capacity(items.len());
                for reg in items {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_tuple item: {msg}")),
                    };
                    values.push(v);
                }
                if let Err(msg) = write_value(
                    frame,
                    *dst,
                    alloc_ref(
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        HeapValue::Tuple(values),
                    ),
                ) {
                    return trap(vm, format!("make_tuple dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::MakeEnum {
                dst,
                enum_type_id,
                type_args,
                variant,
                fields,
            } => {
                let mut arg_ids = Vec::with_capacity(type_args.len());
                for reg in type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("make_enum type_args: {msg}")),
                    };
                    arg_ids.push(id);
                }
                let mut values = Vec::with_capacity(fields.len());
                for reg in fields {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("make_enum field: {msg}")),
                    };
                    values.push(v);
                }
                let obj = HeapValue::Enum {
                    type_id: *enum_type_id,
                    type_args: arg_ids,
                    variant: variant.clone(),
                    fields: values,
                };
                if let Err(msg) = write_value(
                    frame,
                    *dst,
                    alloc_ref(&mut vm.heap, &mut vm.gc_allocations_since_collect, obj),
                ) {
                    return trap(vm, format!("make_enum dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::GetField { dst, obj, field } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("get_field obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in get_field: expected ref(struct/tuple), got {}",
                            obj_v.kind()
                        ),
                    );
                };

                let Some(obj) = vm.heap.get(r.handle) else {
                    return trap(vm, "get_field: dangling reference".to_string());
                };
                let value = match obj {
                    HeapValue::Struct {
                        type_id, fields, ..
                    } => {
                        let idx = match struct_field_index(&vm.module, *type_id, field.as_str()) {
                            Ok(i) => i,
                            Err(msg) => return trap(vm, msg),
                        };
                        let Some(v) = fields.get(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    HeapValue::Tuple(items) => {
                        let Some(idx) = tuple_field_index(field.as_str()) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        let Some(v) = items.get(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in get_field: expected struct/tuple, got ref".to_string(),
                        );
                    }
                };

                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("get_field dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::SetField { obj, field, value } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("set_field obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in set_field: expected ref(struct/tuple), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("set_field value: {msg}")),
                };

                let Some(obj) = vm.heap.get_mut(r.handle) else {
                    return trap(vm, "set_field: dangling reference".to_string());
                };

                match obj {
                    HeapValue::Struct {
                        type_id, fields, ..
                    } => {
                        let idx = match struct_field_index(&vm.module, *type_id, field.as_str()) {
                            Ok(i) => i,
                            Err(msg) => return trap(vm, msg),
                        };
                        let Some(slot) = fields.get_mut(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        *slot = val;
                    }
                    HeapValue::Tuple(items) => {
                        let Some(idx) = tuple_field_index(field.as_str()) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        let Some(slot) = items.get_mut(idx) else {
                            return trap(vm, format!("missing field: {field}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in set_field: expected struct/tuple, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::StructGet { dst, obj, idx } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_get obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in struct_get: expected ref(struct), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                let Some(obj) = vm.heap.get(r.handle) else {
                    return trap(vm, "struct_get: dangling reference".to_string());
                };
                let value = match obj {
                    HeapValue::Struct {
                        type_id, fields, ..
                    } => {
                        let Some(v) = fields.get(*idx) else {
                            let field_name = vm
                                .module
                                .struct_layouts
                                .get(type_id.0 as usize)
                                .and_then(|layout| layout.as_ref())
                                .and_then(|names| names.get(*idx))
                                .cloned()
                                .unwrap_or_else(|| format!("#{idx}"));
                            return trap(vm, format!("missing field: {field_name}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in struct_get: expected struct, got ref".to_string(),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("struct_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::StructSet { obj, idx, value } => {
                let obj_v = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_set obj: {msg}")),
                };
                let Value::Ref(r) = obj_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in struct_set: expected ref(struct), got {}",
                            obj_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("struct_set value: {msg}")),
                };
                let Some(obj) = vm.heap.get_mut(r.handle) else {
                    return trap(vm, "struct_set: dangling reference".to_string());
                };
                match obj {
                    HeapValue::Struct { fields, .. } => {
                        let Some(slot) = fields.get_mut(*idx) else {
                            return trap(vm, format!("missing field: #{idx}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in struct_set: expected struct, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::TupleGet { dst, tup, idx } => {
                let tup_v = match read_value(frame, *tup) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_get tup: {msg}")),
                };
                let Value::Ref(r) = tup_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in tuple_get: expected ref(tuple), got {}",
                            tup_v.kind()
                        ),
                    );
                };
                let Some(obj) = vm.heap.get(r.handle) else {
                    return trap(vm, "tuple_get: dangling reference".to_string());
                };
                let value = match obj {
                    HeapValue::Tuple(items) => {
                        let Some(v) = items.get(*idx) else {
                            return trap(vm, format!("missing field: .{idx}"));
                        };
                        if r.readonly {
                            v.clone().into_readonly_view()
                        } else {
                            v.clone()
                        }
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in tuple_get: expected tuple, got ref".to_string(),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, value) {
                    return trap(vm, format!("tuple_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::TupleSet { tup, idx, value } => {
                let tup_v = match read_value(frame, *tup) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_set tup: {msg}")),
                };
                let Value::Ref(r) = tup_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in tuple_set: expected ref(tuple), got {}",
                            tup_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("tuple_set value: {msg}")),
                };
                let Some(obj) = vm.heap.get_mut(r.handle) else {
                    return trap(vm, "tuple_set: dangling reference".to_string());
                };
                match obj {
                    HeapValue::Tuple(items) => {
                        let Some(slot) = items.get_mut(*idx) else {
                            return trap(vm, format!("missing field: .{idx}"));
                        };
                        *slot = val;
                    }
                    _ => {
                        return trap(
                            vm,
                            "type error in tuple_set: expected tuple, got ref".to_string(),
                        );
                    }
                }
            }

            rusk_bytecode::Instruction::IndexGet { dst, arr, idx } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_get arr: {msg}")),
                };
                let i = match read_int(frame, *idx) {
                    Ok(i) => i,
                    Err(msg) => {
                        return trap(
                            vm,
                            format!("type error in index_get: expected int index, got {msg}"),
                        );
                    }
                };

                let element = match arr_v {
                    Value::Ref(r) => {
                        let Some(obj) = vm.heap.get(r.handle) else {
                            return trap(vm, "index_get: dangling reference".to_string());
                        };
                        match obj {
                            HeapValue::Array(items) => {
                                let idx_usize: usize = match i.try_into() {
                                    Ok(u) => u,
                                    Err(_) => {
                                        return trap(
                                            vm,
                                            format!(
                                                "index out of bounds: index={i}, len={}",
                                                items.len()
                                            ),
                                        );
                                    }
                                };
                                let Some(v) = items.get(idx_usize) else {
                                    return trap(
                                        vm,
                                        format!(
                                            "index out of bounds: index={i}, len={}",
                                            items.len()
                                        ),
                                    );
                                };
                                if r.readonly {
                                    v.clone().into_readonly_view()
                                } else {
                                    v.clone()
                                }
                            }
                            _ => {
                                return trap(
                                    vm,
                                    "type error in index_get: expected array, got ref".to_string(),
                                );
                            }
                        }
                    }
                    Value::Bytes(b) => {
                        let len = b.len_usize();
                        let idx_usize: usize = match i.try_into() {
                            Ok(u) => u,
                            Err(_) => {
                                return trap(
                                    vm,
                                    format!("index out of bounds: index={i}, len={len}"),
                                );
                            }
                        };
                        let byte = match b.as_slice(&vm.heap) {
                            Ok(bytes) => bytes.get(idx_usize).copied(),
                            Err(msg) => return trap(vm, format!("index_get bytes: {msg}")),
                        };
                        let Some(byte) = byte else {
                            return trap(vm, format!("index out of bounds: index={i}, len={len}"));
                        };
                        Value::Byte(byte)
                    }
                    other => {
                        return trap(
                            vm,
                            format!(
                                "type error in index_get: expected array or bytes, got {}",
                                other.kind()
                            ),
                        );
                    }
                };
                if let Err(msg) = write_value(frame, *dst, element) {
                    return trap(vm, format!("index_get dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IndexSet { arr, idx, value } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_set arr: {msg}")),
                };
                let Value::Ref(r) = arr_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in index_set: expected ref(array), got {}",
                            arr_v.kind()
                        ),
                    );
                };
                if r.is_readonly() {
                    return trap(vm, "illegal write through readonly reference".to_string());
                }
                let i = match read_int(frame, *idx) {
                    Ok(i) => i,
                    Err(msg) => {
                        return trap(
                            vm,
                            format!("type error in index_set: expected int index, got {msg}"),
                        );
                    }
                };
                let val = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("index_set value: {msg}")),
                };

                let len = match vm.heap.get(r.handle) {
                    Some(HeapValue::Array(items)) => items.len(),
                    Some(_) => {
                        return trap(
                            vm,
                            "type error in index_set: expected array, got ref".to_string(),
                        );
                    }
                    None => return trap(vm, "index_set: dangling reference".to_string()),
                };

                let idx_usize: usize = match i.try_into() {
                    Ok(u) => u,
                    Err(_) => {
                        return trap(vm, format!("index out of bounds: index={i}, len={len}"));
                    }
                };
                if idx_usize >= len {
                    return trap(vm, format!("index out of bounds: index={i}, len={len}"));
                }

                let Some(obj) = vm.heap.get_mut(r.handle) else {
                    return trap(vm, "index_set: dangling reference".to_string());
                };
                let HeapValue::Array(items) = obj else {
                    return trap(
                        vm,
                        "type error in index_set: expected array, got ref".to_string(),
                    );
                };
                items[idx_usize] = val;
            }
            rusk_bytecode::Instruction::Len { dst, arr } => {
                let arr_v = match read_value(frame, *arr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("len arr: {msg}")),
                };
                let Value::Ref(r) = arr_v else {
                    return trap(
                        vm,
                        format!(
                            "type error in len: expected ref(array), got {}",
                            arr_v.kind()
                        ),
                    );
                };
                let Some(obj) = vm.heap.get(r.handle) else {
                    return trap(vm, "len: dangling reference".to_string());
                };
                let len = match obj {
                    HeapValue::Array(items) => items.len(),
                    _ => {
                        return trap(vm, "type error in len: expected array, got ref".to_string());
                    }
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(len as i64)) {
                    return trap(vm, format!("len dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::IntAdd { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_add a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_add b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_add(b))) {
                    return trap(vm, format!("int_add dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntSub { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_sub a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_sub b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_sub(b))) {
                    return trap(vm, format!("int_sub dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntMul { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mul a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mul b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_mul(b))) {
                    return trap(vm, format!("int_mul dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntDiv { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_div a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_div b: {msg}")),
                };
                if b == 0 {
                    return trap(vm, "int_div: division by zero".to_string());
                }
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_div(b))) {
                    return trap(vm, format!("int_div dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntMod { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mod a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_mod b: {msg}")),
                };
                if b == 0 {
                    return trap(vm, "int_mod: modulo by zero".to_string());
                }
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_rem(b))) {
                    return trap(vm, format!("int_mod dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::IntAnd { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_and a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_and b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a & b)) {
                    return trap(vm, format!("int_and dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntOr { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_or a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_or b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a | b)) {
                    return trap(vm, format!("int_or dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntXor { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_xor a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_xor b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a ^ b)) {
                    return trap(vm, format!("int_xor dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntShl { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_shl a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 64) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_shl b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_shl(sh))) {
                    return trap(vm, format!("int_shl dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntShr { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_shr a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 64) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_shr b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(a.wrapping_shr(sh))) {
                    return trap(vm, format!("int_shr dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntUShr { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ushr a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 64) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ushr b: {msg}")),
                };
                let out = ((a as u64) >> sh) as i64;
                if let Err(msg) = write_value(frame, *dst, Value::Int(out)) {
                    return trap(vm, format!("int_ushr dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntNot { dst, v } => {
                let v = match read_int(frame, *v) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_not v: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Int(!v)) {
                    return trap(vm, format!("int_not dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::IntLt { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_lt a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_lt b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a < b)) {
                    return trap(vm, format!("int_lt dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntLe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_le a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_le b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a <= b)) {
                    return trap(vm, format!("int_le dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntGt { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_gt a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_gt b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a > b)) {
                    return trap(vm, format!("int_gt dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntGe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ge a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ge b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a >= b)) {
                    return trap(vm, format!("int_ge dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntEq { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_eq a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_eq b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a == b)) {
                    return trap(vm, format!("int_eq dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::IntNe { dst, a, b } => {
                let a = match read_int(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ne a: {msg}")),
                };
                let b = match read_int(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("int_ne b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a != b)) {
                    return trap(vm, format!("int_ne dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::ByteAnd { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_and a: {msg}")),
                };
                let b = match read_byte(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_and b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a & b)) {
                    return trap(vm, format!("byte_and dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteOr { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_or a: {msg}")),
                };
                let b = match read_byte(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_or b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a | b)) {
                    return trap(vm, format!("byte_or dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteXor { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_xor a: {msg}")),
                };
                let b = match read_byte(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_xor b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a ^ b)) {
                    return trap(vm, format!("byte_xor dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteShl { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_shl a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 8) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_shl b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a.wrapping_shl(sh))) {
                    return trap(vm, format!("byte_shl dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteShr { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_shr a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 8) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_shr b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a.wrapping_shr(sh))) {
                    return trap(vm, format!("byte_shr dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteUShr { dst, a, b } => {
                let a = match read_byte(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_ushr a: {msg}")),
                };
                let sh = match read_shift_amount(frame, *b, 8) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_ushr b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(a.wrapping_shr(sh))) {
                    return trap(vm, format!("byte_ushr dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::ByteNot { dst, v } => {
                let v = match read_byte(frame, *v) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("byte_not v: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Byte(!v)) {
                    return trap(vm, format!("byte_not dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::BoolNot { dst, v } => {
                let v = match read_bool(frame, *v) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_not v: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(!v)) {
                    return trap(vm, format!("bool_not dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::BoolEq { dst, a, b } => {
                let a = match read_bool(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_eq a: {msg}")),
                };
                let b = match read_bool(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_eq b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a == b)) {
                    return trap(vm, format!("bool_eq dst: {msg}"));
                }
            }
            rusk_bytecode::Instruction::BoolNe { dst, a, b } => {
                let a = match read_bool(frame, *a) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_ne a: {msg}")),
                };
                let b = match read_bool(frame, *b) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("bool_ne b: {msg}")),
                };
                if let Err(msg) = write_value(frame, *dst, Value::Bool(a != b)) {
                    return trap(vm, format!("bool_ne dst: {msg}"));
                }
            }

            rusk_bytecode::Instruction::CallMulti { dsts, func, args } => match func {
                rusk_bytecode::CallTarget::Bc(fid) => {
                    let Some(callee) = vm.module.function(*fid) else {
                        let message = format!("invalid function id {}", fid.0);
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    };
                    if args.len() != callee.param_count as usize {
                        let message = format!(
                            "call arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            args.len()
                        );
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    }

                    let mut regs: Vec<Option<Value>> =
                        Vec::with_capacity(callee.reg_count as usize);
                    regs.resize(callee.reg_count as usize, None);
                    for (idx, arg_reg) in args.iter().enumerate() {
                        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                        let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned()
                        else {
                            let message = format!("call arg reg {arg_reg} is uninitialized");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        };
                        regs[idx] = Some(v);
                    }

                    vm.frames.push(Frame {
                        func: *fid,
                        pc: 0,
                        regs,
                        return_dsts: ReturnDsts::Multi(dsts.clone()),
                    });
                }
                _ => {
                    return trap(
                        vm,
                        "CallMulti target must be a bytecode function".to_string(),
                    );
                }
            },

            rusk_bytecode::Instruction::Call { dst, func, args } => match func {
                rusk_bytecode::CallTarget::Bc(fid) => {
                    let Some(callee) = vm.module.function(*fid) else {
                        let message = format!("invalid function id {}", fid.0);
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    };
                    if args.len() != callee.param_count as usize {
                        let message = format!(
                            "call arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            args.len()
                        );
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    }

                    let generic_params =
                        vm.module.function_generic_param_count(*fid).unwrap_or(0) as usize;
                    if generic_params > 0 && generic_params <= args.len() {
                        let mut type_args = Vec::with_capacity(generic_params);
                        let mut ok = true;
                        for arg_reg in args.iter().take(generic_params) {
                            let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                            let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()) else {
                                ok = false;
                                break;
                            };
                            let Value::TypeRep(id) = v else {
                                ok = false;
                                break;
                            };
                            type_args.push(*id);
                        }

                        if ok {
                            let key = GenericSpecializationKey {
                                func: *fid,
                                type_args,
                            };
                            if let Some(hid) = vm.generic_specializations.get(&key).copied() {
                                if let Err(msg) = call_host_import(
                                    &vm.module,
                                    &mut vm.heap,
                                    &mut vm.gc_allocations_since_collect,
                                    &mut vm.pinned_continuations,
                                    &mut vm.host_fns,
                                    &mut vm.in_host_call,
                                    frame,
                                    *dst,
                                    hid,
                                    &args[generic_params..],
                                ) {
                                    return trap(vm, msg);
                                }
                                continue;
                            }
                        }
                    }
                    let mut regs: Vec<Option<Value>> =
                        Vec::with_capacity(callee.reg_count as usize);
                    regs.resize(callee.reg_count as usize, None);
                    for (idx, arg_reg) in args.iter().enumerate() {
                        let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                        let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned()
                        else {
                            let message = format!("call arg reg {arg_reg} is uninitialized");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        };
                        regs[idx] = Some(v);
                    }

                    vm.frames.push(Frame {
                        func: *fid,
                        pc: 0,
                        regs,
                        return_dsts: ReturnDsts::from_option(*dst),
                    });
                }
                rusk_bytecode::CallTarget::Host(hid) => {
                    if let Err(msg) = call_host_import(
                        &vm.module,
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        &mut vm.pinned_continuations,
                        &mut vm.host_fns,
                        &mut vm.in_host_call,
                        frame,
                        *dst,
                        *hid,
                        args.as_slice(),
                    ) {
                        return trap(vm, msg);
                    }
                }
                rusk_bytecode::CallTarget::Intrinsic(intr) => {
                    let out = match eval_core_intrinsic(
                        &vm.module,
                        &mut vm.type_reps,
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        frame,
                        *intr,
                        args.as_slice(),
                    ) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, msg),
                    };
                    if let Some(dst) = dst
                        && let Err(msg) = write_value(frame, *dst, out)
                    {
                        return trap(vm, format!("intrinsic call dst: {msg}"));
                    }
                }
            },

            rusk_bytecode::Instruction::ICall { dst, fnptr, args } => {
                let fn_value = match read_value(frame, *fnptr) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("icall fnptr: {msg}")),
                };
                let Value::Function(id) = fn_value else {
                    return trap(
                        vm,
                        format!(
                            "type error in icall: expected fn reference, got {}",
                            fn_value.kind()
                        ),
                    );
                };

                let Some(callee) = vm.module.function(id) else {
                    return trap(vm, format!("invalid function id {}", id.0));
                };
                if args.len() != callee.param_count as usize {
                    return trap(
                        vm,
                        format!(
                            "icall arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            args.len()
                        ),
                    );
                }

                let mut regs: Vec<Option<Value>> = Vec::with_capacity(callee.reg_count as usize);
                regs.resize(callee.reg_count as usize, None);
                for (idx, arg_reg) in args.iter().enumerate() {
                    let src_idx: usize = (*arg_reg).try_into().unwrap_or(usize::MAX);
                    let Some(v) = frame.regs.get(src_idx).and_then(|v| v.as_ref()).cloned() else {
                        return trap(vm, format!("icall arg reg {arg_reg} is uninitialized"));
                    };
                    regs[idx] = Some(v);
                }

                vm.frames.push(Frame {
                    func: id,
                    pc: 0,
                    regs,
                    return_dsts: ReturnDsts::from_option(*dst),
                });
            }

            rusk_bytecode::Instruction::VCall {
                dst,
                obj,
                method,
                method_type_args,
                args,
            } => {
                let recv = match read_value(frame, *obj) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("vcall obj: {msg}")),
                };

                // Fast path for common core interface calls on primitives.
                //
                // This is primarily an optimization for generic/bounded code that uses `VCall`
                // even when the runtime receiver is a known primitive (e.g. `Hash::hash` on
                // `int` keys in `core::map::Map`).
                if method_type_args.is_empty() {
                    let fast_out: Option<Value> = if vm.vcall_fast_path_ids.hash == Some(*method) {
                        match (&recv, args.as_slice()) {
                            (Value::Unit, []) => Some(Value::Int(0)),
                            (Value::Bool(b), []) => {
                                Some(Value::Int(hash_int_fnv(if *b { 1 } else { 0 })))
                            }
                            (Value::Int(v), []) => Some(Value::Int(hash_int_fnv(*v))),
                            (Value::Byte(v), []) => Some(Value::Int(hash_int_fnv(*v as i64))),
                            (Value::Char(v), []) => {
                                Some(Value::Int(hash_int_fnv(*v as u32 as i64)))
                            }
                            (Value::String(s), []) => {
                                let bytes = match s.as_str(&vm.heap) {
                                    Ok(s) => s.as_bytes(),
                                    Err(msg) => {
                                        return trap(vm, format!("vcall hash_string: {msg}"));
                                    }
                                };
                                Some(Value::Int(fnv1a_hash(bytes.iter().copied())))
                            }
                            (Value::Bytes(b), []) => {
                                let bytes = match b.as_slice(&vm.heap) {
                                    Ok(b) => b,
                                    Err(msg) => {
                                        return trap(vm, format!("vcall hash_bytes: {msg}"));
                                    }
                                };
                                Some(Value::Int(fnv1a_hash(bytes.iter().copied())))
                            }
                            _ => None,
                        }
                    } else if vm.vcall_fast_path_ids.eq == Some(*method) {
                        match (&recv, args.as_slice()) {
                            (Value::Unit, [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                matches!(other, Value::Unit).then_some(Value::Bool(true))
                            }
                            (Value::Bool(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Bool(b) => Some(Value::Bool(*a == b)),
                                    _ => None,
                                }
                            }
                            (Value::Int(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Int(b) => Some(Value::Bool(*a == b)),
                                    _ => None,
                                }
                            }
                            (Value::Float(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Float(b) => Some(Value::Bool(*a == b)),
                                    _ => None,
                                }
                            }
                            (Value::Byte(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Byte(b) => Some(Value::Bool(*a == b)),
                                    _ => None,
                                }
                            }
                            (Value::Char(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Char(b) => Some(Value::Bool(*a == b)),
                                    _ => None,
                                }
                            }
                            (Value::String(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::String(b) => {
                                        let a_str = match a.as_str(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall string_eq: {msg}"));
                                            }
                                        };
                                        let b_str = match b.as_str(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall string_eq: {msg}"));
                                            }
                                        };
                                        Some(Value::Bool(a_str == b_str))
                                    }
                                    _ => None,
                                }
                            }
                            (Value::Bytes(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Bytes(b) => {
                                        let a_bytes = match a.as_slice(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall bytes_eq: {msg}"));
                                            }
                                        };
                                        let b_bytes = match b.as_slice(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall bytes_eq: {msg}"));
                                            }
                                        };
                                        Some(Value::Bool(a_bytes == b_bytes))
                                    }
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    } else if vm.vcall_fast_path_ids.ne == Some(*method) {
                        match (&recv, args.as_slice()) {
                            (Value::Unit, [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                matches!(other, Value::Unit).then_some(Value::Bool(false))
                            }
                            (Value::Bool(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Bool(b) => Some(Value::Bool(*a != b)),
                                    _ => None,
                                }
                            }
                            (Value::Int(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Int(b) => Some(Value::Bool(*a != b)),
                                    _ => None,
                                }
                            }
                            (Value::Float(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Float(b) => Some(Value::Bool(*a != b)),
                                    _ => None,
                                }
                            }
                            (Value::Byte(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Byte(b) => Some(Value::Bool(*a != b)),
                                    _ => None,
                                }
                            }
                            (Value::Char(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Char(b) => Some(Value::Bool(*a != b)),
                                    _ => None,
                                }
                            }
                            (Value::String(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::String(b) => {
                                        let a_str = match a.as_str(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall string_ne: {msg}"));
                                            }
                                        };
                                        let b_str = match b.as_str(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall string_ne: {msg}"));
                                            }
                                        };
                                        Some(Value::Bool(a_str != b_str))
                                    }
                                    _ => None,
                                }
                            }
                            (Value::Bytes(a), [other]) => {
                                let other = match read_value(frame, *other) {
                                    Ok(v) => v,
                                    Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                                };
                                match other {
                                    Value::Bytes(b) => {
                                        let a_bytes = match a.as_slice(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall bytes_ne: {msg}"));
                                            }
                                        };
                                        let b_bytes = match b.as_slice(&vm.heap) {
                                            Ok(s) => s,
                                            Err(msg) => {
                                                return trap(vm, format!("vcall bytes_ne: {msg}"));
                                            }
                                        };
                                        Some(Value::Bool(a_bytes != b_bytes))
                                    }
                                    _ => None,
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };

                    if let Some(out) = fast_out {
                        if vm.collect_metrics {
                            vm.metrics.vcall_fast_path_hits =
                                vm.metrics.vcall_fast_path_hits.saturating_add(1);
                        }
                        if let Some(dst) = dst
                            && let Err(msg) = write_value(frame, *dst, out)
                        {
                            return trap(vm, format!("vcall dst: {msg}"));
                        }
                        continue;
                    }
                }
                let (type_id, type_args) = match &recv {
                    Value::Unit => (vm.primitive_type_ids.unit, Vec::new()),
                    Value::Bool(_) => (vm.primitive_type_ids.bool, Vec::new()),
                    Value::Int(_) => (vm.primitive_type_ids.int, Vec::new()),
                    Value::Float(_) => (vm.primitive_type_ids.float, Vec::new()),
                    Value::Byte(_) => (vm.primitive_type_ids.byte, Vec::new()),
                    Value::Char(_) => (vm.primitive_type_ids.char, Vec::new()),
                    Value::String(_) => (vm.primitive_type_ids.string, Vec::new()),
                    Value::Bytes(_) => (vm.primitive_type_ids.bytes, Vec::new()),
                    Value::Ref(r) => {
                        let Some(obj) = vm.heap.get(r.handle) else {
                            return trap(vm, "vcall: dangling reference".to_string());
                        };
                        match obj {
                            HeapValue::Struct {
                                type_id, type_args, ..
                            } => (*type_id, type_args.clone()),
                            HeapValue::Enum {
                                type_id, type_args, ..
                            } => (*type_id, type_args.clone()),
                            HeapValue::Array(_)
                            | HeapValue::Tuple(_)
                            | HeapValue::BytesBuf { .. }
                            | HeapValue::StringBuf { .. } => {
                                return trap(
                                    vm,
                                    format!(
                                        "type error in vcall: expected struct|enum, got {}",
                                        recv.kind()
                                    ),
                                );
                            }
                        }
                    }
                    Value::TypeRep(_) | Value::Function(_) | Value::Continuation(_) => {
                        return trap(
                            vm,
                            format!(
                                "type error in vcall: expected struct|enum ref or primitive value, got {}",
                                recv.kind()
                            ),
                        );
                    }
                };

                let Some(fn_id) = vm.module.vcall_target(type_id, *method) else {
                    let type_name = vm.module.type_name(type_id).unwrap_or("<unknown>");
                    let method_name = vm.module.method_name(*method).unwrap_or("<unknown>");
                    return trap(
                        vm,
                        format!("unresolved vcall method: {method_name} on {type_name}"),
                    );
                };

                let Some(callee) = vm.module.function(fn_id) else {
                    return trap(vm, format!("invalid function id {}", fn_id.0));
                };

                let mut arg_values =
                    Vec::with_capacity(type_args.len() + method_type_args.len() + args.len() + 1);
                for id in type_args {
                    arg_values.push(Value::TypeRep(id));
                }
                for reg in method_type_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("vcall method_type_args: {msg}")),
                    };
                    arg_values.push(Value::TypeRep(id));
                }
                arg_values.push(recv);
                for reg in args {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("vcall arg: {msg}")),
                    };
                    arg_values.push(v);
                }

                if arg_values.len() != callee.param_count as usize {
                    return trap(
                        vm,
                        format!(
                            "vcall arity mismatch: expected {} args but got {}",
                            callee.param_count,
                            arg_values.len()
                        ),
                    );
                }

                let mut regs: Vec<Option<Value>> = Vec::with_capacity(callee.reg_count as usize);
                regs.resize(callee.reg_count as usize, None);
                for (idx, value) in arg_values.into_iter().enumerate() {
                    regs[idx] = Some(value);
                }

                vm.frames.push(Frame {
                    func: fn_id,
                    pc: 0,
                    regs,
                    return_dsts: ReturnDsts::from_option(*dst),
                });
            }

            rusk_bytecode::Instruction::PushHandler { clauses } => {
                let owner_depth = frame_index;
                let mut runtime_clauses = Vec::with_capacity(clauses.len());
                for clause in clauses {
                    let mut interface_args = Vec::with_capacity(clause.effect.interface_args.len());
                    for reg in &clause.effect.interface_args {
                        let id = match read_type_rep(frame, *reg) {
                            Ok(id) => id,
                            Err(msg) => {
                                return trap(vm, format!("push_handler interface args: {msg}"));
                            }
                        };
                        interface_args.push(id);
                    }
                    runtime_clauses.push(RuntimeHandlerClause {
                        effect: RuntimeEffectId {
                            interface: clause.effect.interface.clone(),
                            interface_args,
                            method: clause.effect.method.clone(),
                        },
                        arg_patterns: clause.arg_patterns.clone(),
                        target_pc: clause.target_pc,
                        param_regs: clause.param_regs.clone(),
                    });
                }
                vm.handlers.push(HandlerEntry {
                    owner_depth,
                    clauses: runtime_clauses,
                });
                handler_stack_changed(
                    &mut vm.handler_stack_generation,
                    &mut vm.handler_lookup_cache,
                );
            }
            rusk_bytecode::Instruction::PopHandler => {
                let Some(top) = vm.handlers.last() else {
                    return trap(vm, "mismatched pop_handler".to_string());
                };
                if top.owner_depth != frame_index {
                    return trap(vm, "mismatched pop_handler".to_string());
                }
                vm.handlers.pop();
                handler_stack_changed(
                    &mut vm.handler_stack_generation,
                    &mut vm.handler_lookup_cache,
                );
            }

            rusk_bytecode::Instruction::Perform { dst, effect, args } => {
                let mut interface_args = Vec::with_capacity(effect.interface_args.len());
                for reg in &effect.interface_args {
                    let id = match read_type_rep(frame, *reg) {
                        Ok(id) => id,
                        Err(msg) => return trap(vm, format!("perform interface args: {msg}")),
                    };
                    interface_args.push(id);
                }
                let effect_interface = effect.interface.as_str();
                let effect_method = effect.method.as_str();
                // Include interface type arguments in the cache key: handler selection can depend on
                // instantiated generic effect parameters.
                let effect_hash =
                    effect_hash_spec(effect_interface, effect_method, &interface_args);

                let mut arg_values = Vec::with_capacity(args.len());
                for reg in args {
                    let v = match read_value(frame, *reg) {
                        Ok(v) => v,
                        Err(msg) => return trap(vm, format!("perform arg: {msg}")),
                    };
                    arg_values.push(v);
                }

                let mut handled: Option<(usize, usize, Vec<Value>)> = None;

                // Fast path: try cached handler/clause under a stable handler stack.
                if let Some(cache) = vm.handler_lookup_cache
                    && cache.generation == vm.handler_stack_generation
                    && cache.handlers_len == vm.handlers.len()
                    && cache.effect_hash == effect_hash
                {
                    match match_cached_handler_for_effect(
                        &vm.module,
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        &vm.type_reps,
                        vm.handlers.as_slice(),
                        cache.handler_index,
                        cache.clause_index,
                        effect_interface,
                        &interface_args,
                        effect_method,
                        &arg_values,
                    ) {
                        Ok(Some(binds)) => {
                            if vm.collect_metrics {
                                vm.metrics.handler_cache_hits =
                                    vm.metrics.handler_cache_hits.saturating_add(1);
                            }
                            handled = Some((cache.handler_index, cache.clause_index, binds));
                        }
                        Ok(None) => {
                            if vm.collect_metrics {
                                vm.metrics.handler_cache_misses =
                                    vm.metrics.handler_cache_misses.saturating_add(1);
                            }
                        }
                        Err(msg) => return trap(vm, msg),
                    }
                }

                if handled.is_none() {
                    match find_handler_for_effect_spec(
                        &vm.module,
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        &vm.type_reps,
                        vm.handlers.as_slice(),
                        effect_interface,
                        &interface_args,
                        effect_method,
                        &arg_values,
                    ) {
                        Ok(Some((handler_index, clause_index, binds))) => {
                            vm.handler_lookup_cache = Some(HandlerLookupCacheEntry {
                                generation: vm.handler_stack_generation,
                                handlers_len: vm.handlers.len(),
                                effect_hash,
                                handler_index,
                                clause_index,
                            });
                            handled = Some((handler_index, clause_index, binds));
                        }
                        Ok(None) => {}
                        Err(msg) => return trap(vm, msg),
                    }
                }

                if let Some((handler_index, clause_index, binds)) = handled {
                    let handler_owner_depth = vm.handlers[handler_index].owner_depth;

                    let (clause_target_pc, clause_param_regs) = {
                        let Some(clause) = vm.handlers[handler_index].clauses.get(clause_index)
                        else {
                            return trap(vm, "invalid handler clause index".to_string());
                        };
                        (clause.target_pc, clause.param_regs.clone())
                    };

                    let expected_min = binds.len();
                    let expected_max = binds.len() + 1;
                    let wants_continuation = if clause_param_regs.len() == expected_min {
                        false
                    } else if clause_param_regs.len() == expected_max {
                        true
                    } else {
                        return trap(
                            vm,
                            format!(
                                "invalid handler params for {effect_interface}.{effect_method}: expected {expected_min} or {expected_max}, got {}",
                                clause_param_regs.len()
                            ),
                        );
                    };

                    if !wants_continuation {
                        if vm.collect_metrics {
                            vm.metrics.continuations_skipped_abortive =
                                vm.metrics.continuations_skipped_abortive.saturating_add(1);
                        }

                        // Abortive handler: unwind and jump to handler clause without capturing.
                        vm.frames.truncate(handler_owner_depth + 1);
                        let old_handlers_len = vm.handlers.len();
                        vm.handlers.truncate(handler_index + 1);
                        if vm.handlers.len() != old_handlers_len {
                            handler_stack_changed(
                                &mut vm.handler_stack_generation,
                                &mut vm.handler_lookup_cache,
                            );
                        }

                        let Some(handler_frame) = vm.frames.get_mut(handler_owner_depth) else {
                            return trap(vm, "invalid handler owner frame".to_string());
                        };

                        for (dst_reg, value) in
                            clause_param_regs.iter().copied().zip(binds.into_iter())
                        {
                            if let Err(msg) = write_value(handler_frame, dst_reg, value) {
                                return trap(vm, format!("handler bind dst: {msg}"));
                            }
                        }

                        handler_frame.pc = clause_target_pc as usize;
                        continue;
                    }

                    if vm.collect_metrics {
                        vm.metrics.continuations_captured =
                            vm.metrics.continuations_captured.saturating_add(1);
                    }

                    // Capture: snapshot the owning frame, move everything above it.
                    let Some(owner_snapshot) = vm.frames.get(handler_owner_depth).cloned() else {
                        return trap(vm, "invalid handler owner frame".to_string());
                    };
                    let moved_frames = vm.frames.split_off(handler_owner_depth + 1);
                    let mut captured_frames = Vec::with_capacity(1 + moved_frames.len());
                    captured_frames.push(owner_snapshot);
                    captured_frames.extend(moved_frames);

                    // Capture handler entries: clone entries that remain, move the unwound suffix.
                    let moved_handlers_above = vm.handlers.split_off(handler_index + 1);
                    if !moved_handlers_above.is_empty() {
                        handler_stack_changed(
                            &mut vm.handler_stack_generation,
                            &mut vm.handler_lookup_cache,
                        );
                    }

                    let mut captured_handlers = Vec::new();
                    for entry in &vm.handlers {
                        if entry.owner_depth < handler_owner_depth {
                            continue;
                        }
                        captured_handlers.push(HandlerEntry {
                            owner_depth: entry.owner_depth.saturating_sub(handler_owner_depth),
                            clauses: entry.clauses.clone(),
                        });
                    }
                    for mut entry in moved_handlers_above {
                        let Some(rebased) = entry.owner_depth.checked_sub(handler_owner_depth)
                        else {
                            return trap(vm, "invalid handler owner depth".to_string());
                        };
                        entry.owner_depth = rebased;
                        captured_handlers.push(entry);
                    }

                    // Ensure the destination is uninitialized in the captured state until resume injects it.
                    if let Some(dst_reg) = dst {
                        let Some(top) = captured_frames.last_mut() else {
                            return trap(
                                vm,
                                format!("unhandled effect: {effect_interface}.{effect_method}"),
                            );
                        };
                        let idx: usize = (*dst_reg).try_into().unwrap_or(usize::MAX);
                        let Some(slot) = top.regs.get_mut(idx) else {
                            return trap(vm, format!("perform dst reg {dst_reg} out of range"));
                        };
                        *slot = None;
                    }

                    let token = ContinuationToken::new(ContinuationState {
                        frames: captured_frames,
                        handlers: captured_handlers,
                        perform_dst: *dst,
                    });

                    let Some(handler_frame) = vm.frames.get_mut(handler_owner_depth) else {
                        return trap(vm, "invalid handler owner frame".to_string());
                    };

                    for (dst_reg, value) in clause_param_regs
                        .iter()
                        .copied()
                        .take(binds.len())
                        .zip(binds.into_iter())
                    {
                        if let Err(msg) = write_value(handler_frame, dst_reg, value) {
                            return trap(vm, format!("handler bind dst: {msg}"));
                        }
                    }

                    let k_reg = clause_param_regs[expected_max - 1];
                    if let Err(msg) = write_value(handler_frame, k_reg, Value::Continuation(token))
                    {
                        return trap(vm, format!("handler k dst: {msg}"));
                    }

                    handler_frame.pc = clause_target_pc as usize;
                    continue;
                }

                // No in-VM handler: attempt externalized effect request.
                let external_id = if interface_args.is_empty() {
                    vm.module
                        .external_effect_id(effect_interface, effect_method)
                } else {
                    None
                };

                let Some(effect_id_u32) = external_id else {
                    return trap(
                        vm,
                        format!("unhandled effect: {effect_interface}.{effect_method}"),
                    );
                };

                let Some(effect) = vm.module.external_effect(effect_id_u32) else {
                    return trap(vm, format!("invalid effect id {}", effect_id_u32.0));
                };
                if arg_values.len() != effect.sig.params.len() {
                    return trap(
                        vm,
                        format!(
                            "external effect `{}.{}` arity mismatch: expected {} args but got {}",
                            effect.interface,
                            effect.method,
                            effect.sig.params.len(),
                            arg_values.len()
                        ),
                    );
                }

                let mut abi_args = Vec::with_capacity(arg_values.len());
                for (v, expected) in arg_values.iter().zip(effect.sig.params.iter()) {
                    let abi = match v.try_to_abi(&vm.heap, &mut vm.pinned_continuations) {
                        Ok(Some(abi)) => abi,
                        Ok(None) => {
                            return trap(
                                vm,
                                format!(
                                    "external effect `{}.{}` arg type mismatch: expected {:?}, got {}",
                                    effect.interface,
                                    effect.method,
                                    expected,
                                    v.kind()
                                ),
                            );
                        }
                        Err(msg) => return trap(vm, msg),
                    };
                    if abi.ty() != *expected {
                        return trap(
                            vm,
                            format!(
                                "external effect `{}.{}` arg type mismatch: expected {:?}, got {:?}",
                                effect.interface,
                                effect.method,
                                expected,
                                abi.ty()
                            ),
                        );
                    }
                    abi_args.push(abi);
                }

                // Continuation handles are generational: any subsequent `resume`/`drop` bumps the
                // generation counter so stale handles from earlier suspensions are rejected.
                let k = ContinuationHandle {
                    index: 0,
                    generation: vm.continuation_generation,
                };
                vm.state = VmState::Suspended {
                    k: k.clone(),
                    perform_dst: *dst,
                };
                return StepResult::Request {
                    effect_id: effect_id_u32,
                    args: abi_args,
                    k,
                };
            }

            rusk_bytecode::Instruction::Resume { dst, k, value } => {
                let k_value = match read_value(frame, *k) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume k: {msg}")),
                };
                let Value::Continuation(token) = k_value else {
                    return trap(vm, "invalid resume".to_string());
                };

                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume value: {msg}")),
                };
                // `ContinuationToken` is single-shot: resuming consumes the captured state.
                let Some(mut cont) = token.take_state() else {
                    return trap(vm, "invalid resume".to_string());
                };

                if let Some(perform_dst) = cont.perform_dst {
                    let top_index = cont.frames.len().saturating_sub(1);
                    let Some(top_frame) = cont.frames.get_mut(top_index) else {
                        return trap(vm, "invalid resume".to_string());
                    };
                    if let Err(msg) = write_value(top_frame, perform_dst, v) {
                        return trap(vm, format!("resume inject: {msg}"));
                    }
                }

                let base_depth = vm.frames.len();
                let Some(bottom) = cont.frames.first_mut() else {
                    return trap(vm, "invalid resume".to_string());
                };
                bottom.return_dsts = ReturnDsts::from_option(*dst);

                for handler in &mut cont.handlers {
                    handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
                }

                vm.frames.extend(cont.frames);
                let handlers_changed = !cont.handlers.is_empty();
                vm.handlers.extend(cont.handlers);
                if handlers_changed {
                    handler_stack_changed(
                        &mut vm.handler_stack_generation,
                        &mut vm.handler_lookup_cache,
                    );
                }
            }

            rusk_bytecode::Instruction::ResumeTail { k, value } => {
                let k_value = match read_value(frame, *k) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume_tail k: {msg}")),
                };
                let Value::Continuation(token) = k_value else {
                    return trap(vm, "invalid resume".to_string());
                };

                let v = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("resume_tail value: {msg}")),
                };
                let Some(mut cont) = token.take_state() else {
                    return trap(vm, "invalid resume".to_string());
                };

                if let Some(perform_dst) = cont.perform_dst {
                    let top_index = cont.frames.len().saturating_sub(1);
                    let Some(top_frame) = cont.frames.get_mut(top_index) else {
                        return trap(vm, "invalid resume".to_string());
                    };
                    if let Err(msg) = write_value(top_frame, perform_dst, v) {
                        return trap(vm, format!("resume_tail inject: {msg}"));
                    }
                }

                // Tail call: replace the current frame with the captured continuation segment.
                let base_depth = frame_index;
                let return_dsts = frame.return_dsts.clone();

                let Some(bottom) = cont.frames.first_mut() else {
                    return trap(vm, "invalid resume".to_string());
                };
                bottom.return_dsts = return_dsts;

                for handler in &mut cont.handlers {
                    handler.owner_depth = handler.owner_depth.saturating_add(base_depth);
                }

                vm.frames.truncate(base_depth);
                let popped_handlers = unwind_handlers_to_stack_len(&mut vm.handlers, base_depth);

                vm.frames.extend(cont.frames);
                let handlers_changed = !cont.handlers.is_empty();
                vm.handlers.extend(cont.handlers);
                if popped_handlers || handlers_changed {
                    handler_stack_changed(
                        &mut vm.handler_stack_generation,
                        &mut vm.handler_lookup_cache,
                    );
                }
            }

            rusk_bytecode::Instruction::Jump { target_pc } => {
                frame.pc = (*target_pc) as usize;
            }
            rusk_bytecode::Instruction::JumpIf {
                cond,
                then_pc,
                else_pc,
            } => {
                let cond = match read_bool(frame, *cond) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("jump_if cond: {msg}")),
                };
                frame.pc = if cond { *then_pc } else { *else_pc } as usize;
            }
            rusk_bytecode::Instruction::Switch {
                value,
                cases,
                default_pc,
            } => {
                let scrutinee = match read_value(frame, *value) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, format!("switch value: {msg}")),
                };

                let mut matched = false;
                for case in cases {
                    let mut binds = Vec::new();
                    let ok = match match_pattern(
                        &vm.module,
                        &mut vm.heap,
                        &mut vm.gc_allocations_since_collect,
                        &vm.type_reps,
                        &case.pattern,
                        &scrutinee,
                        &mut binds,
                    ) {
                        Ok(ok) => ok,
                        Err(msg) => return trap(vm, msg),
                    };
                    if !ok {
                        continue;
                    }
                    if binds.len() != case.param_regs.len() {
                        return trap(
                            vm,
                            format!(
                                "invalid switch binds: expected {}, got {}",
                                case.param_regs.len(),
                                binds.len()
                            ),
                        );
                    }
                    for (dst, value) in case.param_regs.iter().copied().zip(binds.into_iter()) {
                        if let Err(msg) = write_value(frame, dst, value) {
                            return trap(vm, format!("switch bind dst: {msg}"));
                        }
                    }
                    frame.pc = case.target_pc as usize;
                    matched = true;
                    break;
                }

                if !matched {
                    frame.pc = (*default_pc) as usize;
                }
            }
            rusk_bytecode::Instruction::Return { value } => {
                let idx: usize = (*value).try_into().unwrap_or(usize::MAX);
                let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()).cloned() else {
                    let message = format!("return from uninitialized reg {value}");
                    vm.state = VmState::Trapped {
                        message: message.clone(),
                    };
                    return StepResult::Trap { message };
                };

                let return_dsts = frame.return_dsts.clone();
                vm.frames.pop();
                let popped_handlers =
                    unwind_handlers_to_stack_len(&mut vm.handlers, vm.frames.len());
                if popped_handlers {
                    handler_stack_changed(
                        &mut vm.handler_stack_generation,
                        &mut vm.handler_lookup_cache,
                    );
                }
                if let Some(caller) = vm.frames.last_mut() {
                    match return_dsts {
                        ReturnDsts::None => {}
                        ReturnDsts::One(dst) => {
                            let idx: usize = match dst.try_into() {
                                Ok(i) => i,
                                Err(_) => {
                                    let message = format!("return dst reg {dst} overflow");
                                    vm.state = VmState::Trapped {
                                        message: message.clone(),
                                    };
                                    return StepResult::Trap { message };
                                }
                            };
                            if idx >= caller.regs.len() {
                                let message = format!("return dst reg {dst} out of range");
                                vm.state = VmState::Trapped {
                                    message: message.clone(),
                                };
                                return StepResult::Trap { message };
                            }
                            caller.regs[idx] = Some(v);
                        }
                        ReturnDsts::Multi(dsts) => {
                            let message = format!(
                                "single-value return does not match multi-return destination (dsts={})",
                                dsts.len()
                            );
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        }
                    }
                    continue;
                }

                if !matches!(return_dsts, ReturnDsts::None) {
                    return trap(
                        vm,
                        "non-empty return destination at entry return".to_string(),
                    );
                }

                let v = match unwrap_control_token_at_host_boundary(vm, v) {
                    Ok(v) => v,
                    Err(msg) => return trap(vm, msg),
                };

                let ret = match v.try_to_abi(&vm.heap, &mut vm.pinned_continuations) {
                    Ok(Some(ret)) => ret,
                    Ok(None) => {
                        return trap(vm, format!("non-ABI-safe return value ({})", v.kind()));
                    }
                    Err(msg) => return trap(vm, msg),
                };
                vm.state = VmState::Done { value: ret.clone() };
                return StepResult::Done { value: ret };
            }
            rusk_bytecode::Instruction::ReturnMulti { values } => {
                let mut rets = Vec::with_capacity(values.len());
                for value_reg in values {
                    let idx: usize = (*value_reg).try_into().unwrap_or(usize::MAX);
                    let Some(v) = frame.regs.get(idx).and_then(|v| v.as_ref()).cloned() else {
                        let message = format!("return from uninitialized reg {value_reg}");
                        vm.state = VmState::Trapped {
                            message: message.clone(),
                        };
                        return StepResult::Trap { message };
                    };
                    rets.push(v);
                }

                let return_dsts = frame.return_dsts.clone();
                vm.frames.pop();
                let popped_handlers =
                    unwind_handlers_to_stack_len(&mut vm.handlers, vm.frames.len());
                if popped_handlers {
                    handler_stack_changed(
                        &mut vm.handler_stack_generation,
                        &mut vm.handler_lookup_cache,
                    );
                }

                if let Some(caller) = vm.frames.last_mut() {
                    let ReturnDsts::Multi(dsts) = return_dsts else {
                        return trap(
                            vm,
                            "multi-value return does not match caller destination".to_string(),
                        );
                    };
                    if dsts.len() != rets.len() {
                        return trap(
                            vm,
                            format!(
                                "return arity mismatch: expected {} values but got {}",
                                dsts.len(),
                                rets.len()
                            ),
                        );
                    }
                    for (dst, value) in dsts.into_iter().zip(rets.into_iter()) {
                        let idx: usize = match dst.try_into() {
                            Ok(i) => i,
                            Err(_) => {
                                let message = format!("return dst reg {dst} overflow");
                                vm.state = VmState::Trapped {
                                    message: message.clone(),
                                };
                                return StepResult::Trap { message };
                            }
                        };
                        if idx >= caller.regs.len() {
                            let message = format!("return dst reg {dst} out of range");
                            vm.state = VmState::Trapped {
                                message: message.clone(),
                            };
                            return StepResult::Trap { message };
                        }
                        caller.regs[idx] = Some(value);
                    }
                    continue;
                }

                return trap(
                    vm,
                    "multi-value return from entry frame is not supported".to_string(),
                );
            }
            rusk_bytecode::Instruction::Trap { message } => {
                vm.state = VmState::Trapped {
                    message: message.clone(),
                };
                return StepResult::Trap {
                    message: message.clone(),
                };
            }
        }
    }
}

fn trap(vm: &mut Vm, message: String) -> StepResult {
    record_stack_maxima(vm);
    vm.state = VmState::Trapped {
        message: message.clone(),
    };
    StepResult::Trap { message }
}

fn handler_stack_changed(
    handler_stack_generation: &mut u32,
    handler_lookup_cache: &mut Option<HandlerLookupCacheEntry>,
) {
    *handler_stack_generation = handler_stack_generation.wrapping_add(1);
    *handler_lookup_cache = None;
}

fn record_stack_maxima(vm: &mut Vm) {
    if !vm.collect_metrics {
        return;
    }
    vm.metrics.max_frames_len = vm.metrics.max_frames_len.max(vm.frames.len() as u64);
    vm.metrics.max_handlers_len = vm.metrics.max_handlers_len.max(vm.handlers.len() as u64);
}

fn unwind_handlers_to_stack_len(handlers: &mut Vec<HandlerEntry>, stack_len: usize) -> bool {
    let mut popped = false;
    while let Some(top) = handlers.last() {
        if top.owner_depth < stack_len {
            break;
        }
        handlers.pop();
        popped = true;
    }
    popped
}

fn unwrap_control_token_at_host_boundary(vm: &Vm, v: Value) -> Result<Value, String> {
    const INTERNAL_CONTROL_ENUM: &str = "$Control";
    const CONTROL_VARIANT_VALUE: &str = "Value";
    const CONTROL_VARIANT_RETURN: &str = "Return";
    const CONTROL_VARIANT_BREAK: &str = "Break";
    const CONTROL_VARIANT_CONTINUE: &str = "Continue";

    let Some(control_type_id) = vm.module.type_id(INTERNAL_CONTROL_ENUM) else {
        return Ok(v);
    };

    let Value::Ref(r) = &v else {
        return Ok(v);
    };

    let Some(obj) = vm.heap.get(r.handle) else {
        return Err("invalid control flow token".to_string());
    };

    let HeapValue::Enum {
        type_id,
        variant,
        fields,
        ..
    } = obj
    else {
        return Ok(v);
    };

    if *type_id != control_type_id {
        return Ok(v);
    }

    match variant.as_str() {
        CONTROL_VARIANT_VALUE | CONTROL_VARIANT_RETURN => fields
            .first()
            .cloned()
            .ok_or_else(|| "invalid control flow token".to_string()),
        CONTROL_VARIANT_BREAK => Err("`break` outside of a loop".to_string()),
        CONTROL_VARIANT_CONTINUE => Err("`continue` outside of a loop".to_string()),
        _ => Err("invalid control flow token".to_string()),
    }
}
