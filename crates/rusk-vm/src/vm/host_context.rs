use rusk_bytecode::{AbiSchema, AbiSchemaType, AbiType, TypeId};

use super::*;
use std::collections::{HashMap, HashSet};

/// A host-call context for inspecting and constructing composite ABI values.
///
/// This borrows VM state (heap + module) for the duration of a host import call or external
/// effect dispatch. Composite ABI values are opaque references into the VM heap, and must be
/// inspected/constructed through this context.
pub struct HostContext<'vm> {
    module: &'vm ExecutableModule,
    heap: &'vm mut ImmixHeap<HeapValue>,
    gc_allocations_since_collect: &'vm mut usize,
    pinned_continuations: &'vm mut PinnedContinuations,
    type_reps: &'vm mut TypeReps,
}

impl<'vm> HostContext<'vm> {
    pub(super) fn new(
        module: &'vm ExecutableModule,
        heap: &'vm mut ImmixHeap<HeapValue>,
        gc_allocations_since_collect: &'vm mut usize,
        pinned_continuations: &'vm mut PinnedContinuations,
        type_reps: &'vm mut TypeReps,
    ) -> Self {
        Self {
            module,
            heap,
            gc_allocations_since_collect,
            pinned_continuations,
            type_reps,
        }
    }

    pub fn module(&self) -> &ExecutableModule {
        self.module
    }

    pub fn array_len(&self, arr: &crate::AbiArrayRef) -> Result<usize, HostError> {
        let HeapValue::Array(items) = self.heap_obj(arr.handle())? else {
            return Err(HostError {
                message: "abi array ref: expected array object".to_string(),
            });
        };
        Ok(items.len())
    }

    pub fn array_get(
        &mut self,
        arr: &crate::AbiArrayRef,
        idx: usize,
    ) -> Result<AbiValue, HostError> {
        let expected = arr.elem_ty().clone();
        let v = {
            let HeapValue::Array(items) = self.heap_obj(arr.handle())? else {
                return Err(HostError {
                    message: "abi array ref: expected array object".to_string(),
                });
            };
            items.get(idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "abi array ref: index {idx} out of bounds (len {})",
                    items.len()
                ),
            })?
        };
        self.value_to_abi(&v, &expected)
            .map_err(|message| HostError { message })
    }

    pub fn array_set(
        &mut self,
        arr: &crate::AbiArrayRef,
        idx: usize,
        value: AbiValue,
    ) -> Result<(), HostError> {
        if arr.readonly() {
            return Err(HostError {
                message: "abi array ref is readonly".to_string(),
            });
        }
        let expected = arr.elem_ty().clone();
        let got = value.ty();
        if got != expected {
            return Err(HostError {
                message: format!(
                    "abi array ref set type mismatch: expected {:?}, got {:?}",
                    expected, got
                ),
            });
        }

        let v = self
            .value_from_abi(&value)
            .map_err(|message| HostError { message })?;

        let HeapValue::Array(items) = self.heap_obj_mut(arr.handle())? else {
            return Err(HostError {
                message: "abi array ref: expected array object".to_string(),
            });
        };
        let Some(slot) = items.get_mut(idx) else {
            return Err(HostError {
                message: format!(
                    "abi array ref: index {idx} out of bounds (len {})",
                    items.len()
                ),
            });
        };
        *slot = v;
        Ok(())
    }

    pub fn tuple_len(&self, tup: &crate::AbiTupleRef) -> Result<usize, HostError> {
        let HeapValue::Tuple(items) = self.heap_obj(tup.handle())? else {
            return Err(HostError {
                message: "abi tuple ref: expected tuple object".to_string(),
            });
        };
        Ok(items.len())
    }

    pub fn tuple_get(
        &mut self,
        tup: &crate::AbiTupleRef,
        idx: usize,
    ) -> Result<AbiValue, HostError> {
        let expected = tup.item_tys().get(idx).cloned().ok_or_else(|| HostError {
            message: format!(
                "abi tuple ref: index {idx} out of bounds (len {})",
                tup.item_tys().len()
            ),
        })?;
        let v = {
            let HeapValue::Tuple(items) = self.heap_obj(tup.handle())? else {
                return Err(HostError {
                    message: "abi tuple ref: expected tuple object".to_string(),
                });
            };
            items.get(idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "abi tuple ref: index {idx} out of bounds (len {})",
                    items.len()
                ),
            })?
        };
        self.value_to_abi(&v, &expected)
            .map_err(|message| HostError { message })
    }

    pub fn struct_type_name(&self, s: &crate::AbiStructRef) -> Result<&str, HostError> {
        let type_id = s.type_id();
        self.module.type_name(type_id).ok_or_else(|| HostError {
            message: format!("unknown struct type id {}", type_id.0),
        })
    }

    pub fn struct_get(
        &mut self,
        s: &crate::AbiStructRef,
        field: &str,
    ) -> Result<AbiValue, HostError> {
        let type_id = s.type_id();
        let schema = self.schema_for_type_id(type_id)?;
        let AbiSchema::Struct {
            type_params,
            fields: schema_fields,
        } = schema
        else {
            return Err(HostError {
                message: "abi struct ref: missing struct ABI schema".to_string(),
            });
        };

        if *type_params as usize != s.type_args().len() {
            return Err(HostError {
                message: format!(
                    "abi struct ref: type arg arity mismatch for `{}`: expected {type_params}, got {}",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    s.type_args().len()
                ),
            });
        }

        let (field_idx, expected_schema_ty) = schema_fields
            .iter()
            .enumerate()
            .find_map(|(idx, f)| (f.name == field).then_some((idx, f.ty.clone())))
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown field `{field}` for struct `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;

        let (got_type_args, v) = {
            let HeapValue::Struct {
                type_id: got_type_id,
                type_args,
                fields,
            } = self.heap_obj(s.handle())?
            else {
                return Err(HostError {
                    message: "abi struct ref: expected struct object".to_string(),
                });
            };
            if *got_type_id != type_id {
                return Err(HostError {
                    message: "abi struct ref: type id mismatch".to_string(),
                });
            }
            let got_type_args = type_args.clone();
            let v = fields.get(field_idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "struct `{}` field index {} out of range (len {})",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    field_idx,
                    fields.len()
                ),
            })?
            ;
            (got_type_args, v)
        };

        self.expect_type_args_match(
            &got_type_args,
            s.type_args(),
            "struct",
            self.module.type_name(type_id).unwrap_or("<unknown>"),
        )?;

        let expected_ty = self.instantiate_schema_type(&expected_schema_ty, s.type_args())?;
        self.value_to_abi(&v, &expected_ty)
            .map_err(|message| HostError { message })
    }

    pub fn struct_set(
        &mut self,
        s: &crate::AbiStructRef,
        field: &str,
        value: AbiValue,
    ) -> Result<(), HostError> {
        if s.readonly() {
            return Err(HostError {
                message: "abi struct ref is readonly".to_string(),
            });
        }
        let type_id = s.type_id();
        let schema = self.schema_for_type_id(type_id)?;
        let AbiSchema::Struct {
            type_params,
            fields: schema_fields,
        } = schema
        else {
            return Err(HostError {
                message: "abi struct ref: missing struct ABI schema".to_string(),
            });
        };

        if *type_params as usize != s.type_args().len() {
            return Err(HostError {
                message: format!(
                    "abi struct ref: type arg arity mismatch for `{}`: expected {type_params}, got {}",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    s.type_args().len()
                ),
            });
        }

        let (field_idx, expected_schema_ty) = schema_fields
            .iter()
            .enumerate()
            .find_map(|(idx, f)| (f.name == field).then_some((idx, f.ty.clone())))
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown field `{field}` for struct `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;

        let expected_ty = self.instantiate_schema_type(&expected_schema_ty, s.type_args())?;
        let got = value.ty();
        if got != expected_ty {
            return Err(HostError {
                message: format!(
                    "struct `{}` field `{field}` type mismatch: expected {:?}, got {:?}",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    expected_ty,
                    got
                ),
            });
        }

        let v = self
            .value_from_abi(&value)
            .map_err(|message| HostError { message })?;

        let got_type_args = {
            let HeapValue::Struct {
                type_id: got_type_id,
                type_args,
                ..
            } = self.heap_obj(s.handle())?
            else {
                return Err(HostError {
                    message: "abi struct ref: expected struct object".to_string(),
                });
            };
            if *got_type_id != type_id {
                return Err(HostError {
                    message: "abi struct ref: type id mismatch".to_string(),
                });
            }
            type_args.clone()
        };
        self.expect_type_args_match(
            &got_type_args,
            s.type_args(),
            "struct",
            self.module.type_name(type_id).unwrap_or("<unknown>"),
        )?;

        let HeapValue::Struct {
            type_id: got_type_id,
            fields,
            ..
        } = self.heap_obj_mut(s.handle())?
        else {
            return Err(HostError {
                message: "abi struct ref: expected struct object".to_string(),
            });
        };
        if *got_type_id != type_id {
            return Err(HostError {
                message: "abi struct ref: type id mismatch".to_string(),
            });
        }
        let Some(slot) = fields.get_mut(field_idx) else {
            return Err(HostError {
                message: "struct field index out of range".to_string(),
            });
        };
        *slot = v;
        Ok(())
    }

    pub fn enum_type_name(&self, e: &crate::AbiEnumRef) -> Result<&str, HostError> {
        let type_id = e.type_id();
        self.module.type_name(type_id).ok_or_else(|| HostError {
            message: format!("unknown enum type id {}", type_id.0),
        })
    }

    pub fn enum_variant(&self, e: &crate::AbiEnumRef) -> Result<(&str, usize), HostError> {
        let HeapValue::Enum {
            variant, fields, ..
        } = self.heap_obj(e.handle())?
        else {
            return Err(HostError {
                message: "abi enum ref: expected enum object".to_string(),
            });
        };
        Ok((variant.as_str(), fields.len()))
    }

    pub fn enum_get(&mut self, e: &crate::AbiEnumRef, idx: usize) -> Result<AbiValue, HostError> {
        let type_id = e.type_id();
        let (got_type_args, variant_name, v) = {
            let HeapValue::Enum {
                type_id: got_type_id,
                type_args,
                variant,
                fields,
            } = self.heap_obj(e.handle())?
            else {
                return Err(HostError {
                    message: "abi enum ref: expected enum object".to_string(),
                });
            };
            if *got_type_id != type_id {
                return Err(HostError {
                    message: "abi enum ref: type id mismatch".to_string(),
                });
            }
            let v = fields.get(idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "abi enum ref: field index {idx} out of range (len {})",
                    fields.len()
                ),
            })?;
            (type_args.clone(), variant.clone(), v)
        };

        let (type_params, variants) = match self.schema_for_type_id(type_id)? {
            AbiSchema::Enum {
                type_params,
                variants,
            } => (*type_params, variants.clone()),
            _ => {
                return Err(HostError {
                    message: "abi enum ref: missing enum ABI schema".to_string(),
                });
            }
        };

        if type_params as usize != e.type_args().len() {
            return Err(HostError {
                message: format!(
                    "abi enum ref: type arg arity mismatch for `{}`: expected {type_params}, got {}",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    e.type_args().len()
                ),
            });
        }

        self.expect_type_args_match(
            &got_type_args,
            e.type_args(),
            "enum",
            self.module.type_name(type_id).unwrap_or("<unknown>"),
        )?;

        let schema_variant = variants
            .iter()
            .find(|v| v.name == variant_name)
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown enum variant `{variant_name}` for enum `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;
        let expected_schema_ty = schema_variant
            .fields
            .get(idx)
            .cloned()
            .ok_or_else(|| HostError {
                message: format!(
                    "abi enum ref: index {idx} out of bounds for variant `{}` (arity {})",
                    schema_variant.name,
                    schema_variant.fields.len()
                ),
            })?;

        let expected = self.instantiate_schema_type(&expected_schema_ty, e.type_args())?;
        self.value_to_abi(&v, &expected)
            .map_err(|message| HostError { message })
    }

    pub fn alloc_array(&mut self, items: Vec<AbiValue>) -> Result<AbiValue, HostError> {
        let Some(first) = items.first() else {
            return Err(HostError {
                message: "cannot infer element type for empty array; use `alloc_array_typed`"
                    .to_string(),
            });
        };
        let elem_ty = first.ty();
        self.alloc_array_typed(elem_ty, items)
    }

    pub fn alloc_array_typed(
        &mut self,
        elem_ty: AbiType,
        items: Vec<AbiValue>,
    ) -> Result<AbiValue, HostError> {
        let mut values = Vec::with_capacity(items.len());
        for (idx, item) in items.into_iter().enumerate() {
            if item.ty() != elem_ty {
                return Err(HostError {
                    message: format!(
                        "alloc_array_typed: element {idx} type mismatch: expected {:?}, got {:?}",
                        elem_ty,
                        item.ty()
                    ),
                });
            }
            values.push(
                self.value_from_abi(&item)
                    .map_err(|message| HostError { message })?,
            );
        }

        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Array(values),
        );
        let Value::Ref(r) = v else {
            return Err(HostError {
                message: "internal error: array allocation did not produce ref".to_string(),
            });
        };
        Ok(AbiValue::Array(crate::AbiArrayRef::new(
            r.handle, r.readonly, elem_ty,
        )))
    }

    pub fn alloc_tuple(&mut self, items: Vec<AbiValue>) -> Result<AbiValue, HostError> {
        if items.is_empty() {
            return Ok(AbiValue::Unit);
        }
        let mut item_tys = Vec::with_capacity(items.len());
        let mut values = Vec::with_capacity(items.len());
        for item in items {
            item_tys.push(item.ty());
            values.push(
                self.value_from_abi(&item)
                    .map_err(|message| HostError { message })?,
            );
        }
        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Tuple(values),
        );
        let Value::Ref(r) = v else {
            return Err(HostError {
                message: "internal error: tuple allocation did not produce ref".to_string(),
            });
        };
        Ok(AbiValue::Tuple(crate::AbiTupleRef::new(
            r.handle, r.readonly, item_tys,
        )))
    }

    pub fn alloc_struct(
        &mut self,
        type_name: &str,
        fields: Vec<(&str, AbiValue)>,
    ) -> Result<AbiValue, HostError> {
        self.alloc_struct_typed(type_name, Vec::new(), fields)
    }

    pub fn alloc_struct_typed(
        &mut self,
        type_name: &str,
        type_args: Vec<AbiType>,
        fields: Vec<(&str, AbiValue)>,
    ) -> Result<AbiValue, HostError> {
        let type_id = self.module.type_id(type_name).ok_or_else(|| HostError {
            message: format!("unknown struct type `{type_name}`"),
        })?;
        let (type_params, schema_fields) = match self.schema_for_type_id(type_id)? {
            AbiSchema::Struct {
                type_params,
                fields,
            } => (*type_params, fields.clone()),
            _ => {
                return Err(HostError {
                    message: format!("type `{type_name}` is not a struct ABI schema"),
                });
            }
        };
        if type_args.len() != type_params as usize {
            return Err(HostError {
                message: format!(
                    "struct `{type_name}` type arg arity mismatch: expected {type_params}, got {}",
                    type_args.len()
                ),
            });
        }

        let mut provided: HashMap<&str, AbiValue> = HashMap::new();
        for (name, value) in fields {
            if provided.insert(name, value).is_some() {
                return Err(HostError {
                    message: format!("duplicate struct field `{name}`"),
                });
            }
        }

        let mut values = Vec::with_capacity(schema_fields.len());
        for field in schema_fields {
            let value = provided
                .remove(field.name.as_str())
                .ok_or_else(|| HostError {
                    message: format!("missing struct field `{}`", field.name),
                })?;
            let expected = self.instantiate_schema_type(&field.ty, &type_args)?;
            let got = value.ty();
            if got != expected {
                return Err(HostError {
                    message: format!(
                        "struct `{type_name}` field `{}` type mismatch: expected {:?}, got {:?}",
                        field.name, expected, got
                    ),
                });
            }
            values.push(
                self.value_from_abi(&value)
                    .map_err(|message| HostError { message })?,
            );
        }

        if let Some((extra, _)) = provided.into_iter().next() {
            return Err(HostError {
                message: format!("unknown struct field `{extra}` for `{type_name}`"),
            });
        }

        let mut type_arg_reps = Vec::with_capacity(type_args.len());
        for arg in &type_args {
            type_arg_reps.push(
                self.type_rep_for_abi_type(arg)
                    .map_err(|message| HostError { message })?,
            );
        }

        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Struct {
                type_id,
                type_args: type_arg_reps,
                fields: values,
            },
        );
        let Value::Ref(r) = v else {
            return Err(HostError {
                message: "internal error: struct allocation did not produce ref".to_string(),
            });
        };

        Ok(AbiValue::Struct(crate::AbiStructRef::new(
            r.handle, r.readonly, type_id, type_args,
        )))
    }

    pub fn alloc_enum(
        &mut self,
        type_name: &str,
        variant: &str,
        fields: Vec<AbiValue>,
    ) -> Result<AbiValue, HostError> {
        self.alloc_enum_typed(type_name, Vec::new(), variant, fields)
    }

    pub fn alloc_enum_typed(
        &mut self,
        type_name: &str,
        type_args: Vec<AbiType>,
        variant: &str,
        fields: Vec<AbiValue>,
    ) -> Result<AbiValue, HostError> {
        let type_id = self.module.type_id(type_name).ok_or_else(|| HostError {
            message: format!("unknown enum type `{type_name}`"),
        })?;
        let (type_params, variants) = match self.schema_for_type_id(type_id)? {
            AbiSchema::Enum {
                type_params,
                variants,
            } => (*type_params, variants.clone()),
            _ => {
                return Err(HostError {
                    message: format!("type `{type_name}` is not an enum ABI schema"),
                });
            }
        };
        if type_args.len() != type_params as usize {
            return Err(HostError {
                message: format!(
                    "enum `{type_name}` type arg arity mismatch: expected {type_params}, got {}",
                    type_args.len()
                ),
            });
        }

        let Some(schema_variant) = variants.iter().find(|v| v.name == variant) else {
            return Err(HostError {
                message: format!("unknown enum variant `{variant}` for `{type_name}`"),
            });
        };
        if schema_variant.fields.len() != fields.len() {
            return Err(HostError {
                message: format!(
                    "enum `{type_name}` variant `{variant}` arity mismatch: expected {}, got {}",
                    schema_variant.fields.len(),
                    fields.len()
                ),
            });
        }
        let mut values = Vec::with_capacity(fields.len());
        for (idx, (schema_field_ty, value)) in schema_variant
            .fields
            .iter()
            .cloned()
            .zip(fields.into_iter())
            .enumerate()
        {
            let expected = self.instantiate_schema_type(&schema_field_ty, &type_args)?;
            let got = value.ty();
            if got != expected {
                return Err(HostError {
                    message: format!(
                        "enum `{type_name}` variant `{variant}` field {idx} type mismatch: expected {:?}, got {:?}",
                        expected, got
                    ),
                });
            }
            values.push(
                self.value_from_abi(&value)
                    .map_err(|message| HostError { message })?,
            );
        }

        let mut type_arg_reps = Vec::with_capacity(type_args.len());
        for arg in &type_args {
            type_arg_reps.push(
                self.type_rep_for_abi_type(arg)
                    .map_err(|message| HostError { message })?,
            );
        }
        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Enum {
                type_id,
                type_args: type_arg_reps,
                variant: variant.to_string(),
                fields: values,
            },
        );
        let Value::Ref(r) = v else {
            return Err(HostError {
                message: "internal error: enum allocation did not produce ref".to_string(),
            });
        };
        Ok(AbiValue::Enum(crate::AbiEnumRef::new(
            r.handle, r.readonly, type_id, type_args,
        )))
    }

    /// Converts a VM-internal value into an ABI value by inferring a corresponding ABI type.
    ///
    /// This is used at "untyped" host boundaries like `StepResult::Done`, where there is no
    /// declared ABI signature to validate against.
    pub(super) fn value_to_abi_infer(&mut self, v: &Value) -> Result<AbiValue, String> {
        match v {
            Value::Unit => Ok(AbiValue::Unit),
            Value::Bool(b) => Ok(AbiValue::Bool(*b)),
            Value::Int(n) => Ok(AbiValue::Int(*n)),
            Value::Float(x) => Ok(AbiValue::Float(*x)),
            Value::Byte(b) => Ok(AbiValue::Byte(*b)),
            Value::Char(c) => Ok(AbiValue::Char(*c)),
            Value::String(s) => Ok(AbiValue::String(s.as_str(self.heap)?.to_string())),
            Value::Bytes(b) => Ok(AbiValue::Bytes(b.as_slice(self.heap)?.to_vec())),
            Value::Continuation(k) => Ok(AbiValue::Continuation(
                self.pinned_continuations.pin(k.clone())?,
            )),
            Value::TypeRep(_) => Err("non-ABI-safe value (typerep)".to_string()),
            Value::Function(_) => Err("non-ABI-safe value (function)".to_string()),
            Value::Ref(r) => {
                let Some(obj) = self.heap.get(r.handle) else {
                    return Err("dangling abi object handle".to_string());
                };
                match obj {
                    HeapValue::Array(items) => {
                        let mut stack = HashSet::new();
                        stack.insert(r.handle);
                        let elem_ty = Self::infer_array_elem_ty(self, items, &mut stack)?;
                        Ok(AbiValue::Array(crate::AbiArrayRef::new(
                            r.handle, r.readonly, elem_ty,
                        )))
                    }
                    HeapValue::Tuple(items) => {
                        let mut stack = HashSet::new();
                        stack.insert(r.handle);
                        let mut item_tys = Vec::with_capacity(items.len());
                        for item in items {
                            item_tys.push(self.infer_abi_type_for_value(item, &mut stack)?);
                        }
                        Ok(AbiValue::Tuple(crate::AbiTupleRef::new(
                            r.handle, r.readonly, item_tys,
                        )))
                    }
                    HeapValue::Struct {
                        type_id, type_args, ..
                    } => {
                        let mut args = Vec::with_capacity(type_args.len());
                        for arg in type_args {
                            args.push(self.abi_type_for_type_rep(*arg)?);
                        }
                        Ok(AbiValue::Struct(crate::AbiStructRef::new(
                            r.handle, r.readonly, *type_id, args,
                        )))
                    }
                    HeapValue::Enum {
                        type_id, type_args, ..
                    } => {
                        let mut args = Vec::with_capacity(type_args.len());
                        for arg in type_args {
                            args.push(self.abi_type_for_type_rep(*arg)?);
                        }
                        Ok(AbiValue::Enum(crate::AbiEnumRef::new(
                            r.handle, r.readonly, *type_id, args,
                        )))
                    }
                    HeapValue::BytesBuf { .. } | HeapValue::StringBuf { .. } => {
                        Err("non-ABI-safe value (buffer)".to_string())
                    }
                }
            }
        }
    }

    fn infer_array_elem_ty(
        &self,
        items: &[Value],
        stack: &mut HashSet<GcRef>,
    ) -> Result<AbiType, String> {
        let Some(first) = items.first() else {
            return Err("cannot infer ABI element type for empty array".to_string());
        };
        let elem_ty = self.infer_abi_type_for_value(first, stack)?;
        for (idx, item) in items.iter().enumerate().skip(1) {
            let got = self.infer_abi_type_for_value(item, stack)?;
            if got != elem_ty {
                return Err(format!(
                    "cannot infer ABI element type for array: element {idx} has type {:?}, expected {:?}",
                    got, elem_ty
                ));
            }
        }
        Ok(elem_ty)
    }

    fn infer_abi_type_for_value(
        &self,
        v: &Value,
        stack: &mut HashSet<GcRef>,
    ) -> Result<AbiType, String> {
        match v {
            Value::Unit => Ok(AbiType::Unit),
            Value::Bool(_) => Ok(AbiType::Bool),
            Value::Int(_) => Ok(AbiType::Int),
            Value::Float(_) => Ok(AbiType::Float),
            Value::Byte(_) => Ok(AbiType::Byte),
            Value::Char(_) => Ok(AbiType::Char),
            Value::String(_) => Ok(AbiType::String),
            Value::Bytes(_) => Ok(AbiType::Bytes),
            Value::Continuation(_) => Ok(AbiType::Continuation),
            Value::TypeRep(_) => Err("non-ABI-safe value (typerep)".to_string()),
            Value::Function(_) => Err("non-ABI-safe value (function)".to_string()),
            Value::Ref(r) => {
                if !stack.insert(r.handle) {
                    return Err(
                        "cyclic object graph is not supported for ABI type inference".to_string(),
                    );
                }
                let Some(obj) = self.heap.get(r.handle) else {
                    stack.remove(&r.handle);
                    return Err("dangling abi object handle".to_string());
                };
                let ty = match obj {
                    HeapValue::Array(items) => {
                        AbiType::Array(Box::new(self.infer_array_elem_ty(items, stack)?))
                    }
                    HeapValue::Tuple(items) => {
                        let mut item_tys = Vec::with_capacity(items.len());
                        for item in items {
                            item_tys.push(self.infer_abi_type_for_value(item, stack)?);
                        }
                        AbiType::Tuple(item_tys)
                    }
                    HeapValue::Struct {
                        type_id, type_args, ..
                    } => {
                        let mut args = Vec::with_capacity(type_args.len());
                        for arg in type_args {
                            let abi = match self.abi_type_for_type_rep(*arg) {
                                Ok(t) => t,
                                Err(e) => {
                                    stack.remove(&r.handle);
                                    return Err(e);
                                }
                            };
                            args.push(abi);
                        }
                        AbiType::Struct {
                            type_id: *type_id,
                            args,
                        }
                    }
                    HeapValue::Enum {
                        type_id, type_args, ..
                    } => {
                        let mut args = Vec::with_capacity(type_args.len());
                        for arg in type_args {
                            let abi = match self.abi_type_for_type_rep(*arg) {
                                Ok(t) => t,
                                Err(e) => {
                                    stack.remove(&r.handle);
                                    return Err(e);
                                }
                            };
                            args.push(abi);
                        }
                        AbiType::Enum {
                            type_id: *type_id,
                            args,
                        }
                    }
                    HeapValue::BytesBuf { .. } | HeapValue::StringBuf { .. } => {
                        stack.remove(&r.handle);
                        return Err("non-ABI-safe value (buffer)".to_string());
                    }
                };
                stack.remove(&r.handle);
                Ok(ty)
            }
        }
    }

    pub(super) fn value_to_abi(
        &mut self,
        v: &Value,
        expected: &AbiType,
    ) -> Result<AbiValue, String> {
        match (expected, v) {
            (AbiType::Unit, Value::Unit) => Ok(AbiValue::Unit),
            (AbiType::Bool, Value::Bool(b)) => Ok(AbiValue::Bool(*b)),
            (AbiType::Int, Value::Int(n)) => Ok(AbiValue::Int(*n)),
            (AbiType::Float, Value::Float(x)) => Ok(AbiValue::Float(*x)),
            (AbiType::Byte, Value::Byte(b)) => Ok(AbiValue::Byte(*b)),
            (AbiType::Char, Value::Char(c)) => Ok(AbiValue::Char(*c)),
            (AbiType::String, Value::String(s)) => {
                Ok(AbiValue::String(s.as_str(self.heap)?.to_string()))
            }
            (AbiType::Bytes, Value::Bytes(b)) => {
                Ok(AbiValue::Bytes(b.as_slice(self.heap)?.to_vec()))
            }
            (AbiType::Continuation, Value::Continuation(k)) => Ok(AbiValue::Continuation(
                self.pinned_continuations.pin(k.clone())?,
            )),
            (AbiType::Array(elem_ty), Value::Ref(r)) => {
                let HeapValue::Array(_items) = self.heap_obj(r.handle).map_err(|e| e.message)?
                else {
                    return Err(format!(
                        "abi type mismatch: expected array, got {}",
                        self.heap_kind(r.handle)?
                    ));
                };
                Ok(AbiValue::Array(crate::AbiArrayRef::new(
                    r.handle,
                    r.readonly,
                    elem_ty.as_ref().clone(),
                )))
            }
            (AbiType::Tuple(item_tys), Value::Ref(r)) => {
                let HeapValue::Tuple(items) = self.heap_obj(r.handle).map_err(|e| e.message)?
                else {
                    return Err(format!(
                        "abi type mismatch: expected tuple, got {}",
                        self.heap_kind(r.handle)?
                    ));
                };
                if items.len() != item_tys.len() {
                    return Err(format!(
                        "abi tuple length mismatch: expected {}, got {}",
                        item_tys.len(),
                        items.len()
                    ));
                }
                Ok(AbiValue::Tuple(crate::AbiTupleRef::new(
                    r.handle,
                    r.readonly,
                    item_tys.clone(),
                )))
            }
            (AbiType::Struct { type_id, args }, Value::Ref(r)) => {
                let (got_type_id, got_type_args) = {
                    let HeapValue::Struct {
                        type_id: got_type_id,
                        type_args,
                        ..
                    } = self.heap_obj(r.handle).map_err(|e| e.message)?
                    else {
                        return Err(format!(
                            "abi type mismatch: expected struct, got {}",
                            self.heap_kind(r.handle)?
                        ));
                    };
                    (*got_type_id, type_args.clone())
                };
                if got_type_id != *type_id {
                    return Err(format!(
                        "abi struct type mismatch: expected {:?}, got {:?}",
                        type_id, got_type_id
                    ));
                }
                self.expect_type_args_match(
                    &got_type_args,
                    args,
                    "struct",
                    self.module.type_name(*type_id).unwrap_or("<unknown>"),
                )
                .map_err(|e| e.message)?;
                Ok(AbiValue::Struct(crate::AbiStructRef::new(
                    r.handle,
                    r.readonly,
                    *type_id,
                    args.clone(),
                )))
            }
            (AbiType::Enum { type_id, args }, Value::Ref(r)) => {
                let (got_type_id, got_type_args) = {
                    let HeapValue::Enum {
                        type_id: got_type_id,
                        type_args,
                        ..
                    } = self.heap_obj(r.handle).map_err(|e| e.message)?
                    else {
                        return Err(format!(
                            "abi type mismatch: expected enum, got {}",
                            self.heap_kind(r.handle)?
                        ));
                    };
                    (*got_type_id, type_args.clone())
                };
                if got_type_id != *type_id {
                    return Err(format!(
                        "abi enum type mismatch: expected {:?}, got {:?}",
                        type_id, got_type_id
                    ));
                }
                self.expect_type_args_match(
                    &got_type_args,
                    args,
                    "enum",
                    self.module.type_name(*type_id).unwrap_or("<unknown>"),
                )
                .map_err(|e| e.message)?;
                Ok(AbiValue::Enum(crate::AbiEnumRef::new(
                    r.handle,
                    r.readonly,
                    *type_id,
                    args.clone(),
                )))
            }
            (expected, other) => Err(format!(
                "abi type mismatch: expected {:?}, got {}",
                expected,
                other.kind()
            )),
        }
    }

    pub(super) fn value_from_abi(&mut self, v: &AbiValue) -> Result<Value, String> {
        Ok(match v {
            AbiValue::Unit => Value::Unit,
            AbiValue::Bool(b) => Value::Bool(*b),
            AbiValue::Int(n) => Value::Int(*n),
            AbiValue::Float(x) => Value::Float(*x),
            AbiValue::Byte(b) => Value::Byte(*b),
            AbiValue::Char(c) => Value::Char(*c),
            AbiValue::String(s) => {
                alloc_string(self.heap, self.gc_allocations_since_collect, s.clone())?
            }
            AbiValue::Bytes(b) => {
                alloc_bytes(self.heap, self.gc_allocations_since_collect, b.clone())?
            }
            AbiValue::Continuation(h) => {
                Value::Continuation(self.pinned_continuations.resolve(h.clone())?)
            }
            AbiValue::Array(r) => {
                self.expect_heap_kind(r.handle(), "array")?;
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
            AbiValue::Tuple(r) => {
                let HeapValue::Tuple(items) = self
                    .heap
                    .get(r.handle())
                    .ok_or_else(|| "dangling abi object handle".to_string())?
                else {
                    return Err(format!(
                        "abi ref kind mismatch: expected tuple, got {}",
                        self.heap_kind(r.handle())?
                    ));
                };
                if items.len() != r.item_tys().len() {
                    return Err(format!(
                        "abi tuple length mismatch: expected {}, got {}",
                        r.item_tys().len(),
                        items.len()
                    ));
                }
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
            AbiValue::Struct(r) => {
                let (type_id, type_args) = {
                    let HeapValue::Struct {
                        type_id, type_args, ..
                    } = self
                        .heap
                        .get(r.handle())
                        .ok_or_else(|| "dangling abi object handle".to_string())?
                    else {
                        return Err(format!(
                            "abi ref kind mismatch: expected struct, got {}",
                            self.heap_kind(r.handle())?
                        ));
                    };
                    (*type_id, type_args.clone())
                };
                if type_id != r.type_id() {
                    return Err("abi struct ref: type id mismatch".to_string());
                }
                self.expect_type_args_match(
                    &type_args,
                    r.type_args(),
                    "struct",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                )
                .map_err(|e| e.message)?;
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
            AbiValue::Enum(r) => {
                let (type_id, type_args) = {
                    let HeapValue::Enum {
                        type_id, type_args, ..
                    } = self
                        .heap
                        .get(r.handle())
                        .ok_or_else(|| "dangling abi object handle".to_string())?
                    else {
                        return Err(format!(
                            "abi ref kind mismatch: expected enum, got {}",
                            self.heap_kind(r.handle())?
                        ));
                    };
                    (*type_id, type_args.clone())
                };
                if type_id != r.type_id() {
                    return Err("abi enum ref: type id mismatch".to_string());
                }
                self.expect_type_args_match(
                    &type_args,
                    r.type_args(),
                    "enum",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                )
                .map_err(|e| e.message)?;
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
        })
    }

    fn instantiate_schema_type(
        &self,
        ty: &AbiSchemaType,
        type_args: &[AbiType],
    ) -> Result<AbiType, HostError> {
        Ok(match ty {
            AbiSchemaType::Unit => AbiType::Unit,
            AbiSchemaType::Bool => AbiType::Bool,
            AbiSchemaType::Int => AbiType::Int,
            AbiSchemaType::Float => AbiType::Float,
            AbiSchemaType::Byte => AbiType::Byte,
            AbiSchemaType::Char => AbiType::Char,
            AbiSchemaType::String => AbiType::String,
            AbiSchemaType::Bytes => AbiType::Bytes,
            AbiSchemaType::Continuation => AbiType::Continuation,
            AbiSchemaType::Array(elem) => AbiType::Array(Box::new(
                self.instantiate_schema_type(elem, type_args)?,
            )),
            AbiSchemaType::Tuple(items) => AbiType::Tuple(
                items
                    .iter()
                    .map(|t| self.instantiate_schema_type(t, type_args))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            AbiSchemaType::Struct { type_id, args } => AbiType::Struct {
                type_id: *type_id,
                args: args
                    .iter()
                    .map(|t| self.instantiate_schema_type(t, type_args))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            AbiSchemaType::Enum { type_id, args } => AbiType::Enum {
                type_id: *type_id,
                args: args
                    .iter()
                    .map(|t| self.instantiate_schema_type(t, type_args))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            AbiSchemaType::TypeParam(idx) => type_args
                .get(*idx as usize)
                .cloned()
                .ok_or_else(|| HostError {
                    message: format!(
                        "abi schema type param index {} out of bounds (type_args={})",
                        idx,
                        type_args.len()
                    ),
                })?,
        })
    }

    fn type_rep_for_abi_type(&mut self, ty: &AbiType) -> Result<TypeRepId, String> {
        let (ctor, args) = match ty {
            AbiType::Unit => (TypeCtor::Unit, Vec::new()),
            AbiType::Bool => (TypeCtor::Bool, Vec::new()),
            AbiType::Int => (TypeCtor::Int, Vec::new()),
            AbiType::Float => (TypeCtor::Float, Vec::new()),
            AbiType::Byte => (TypeCtor::Byte, Vec::new()),
            AbiType::Char => (TypeCtor::Char, Vec::new()),
            AbiType::String => (TypeCtor::String, Vec::new()),
            AbiType::Bytes => (TypeCtor::Bytes, Vec::new()),
            AbiType::Continuation => (TypeCtor::Cont, Vec::new()),
            AbiType::Array(elem) => (
                TypeCtor::Array,
                vec![self.type_rep_for_abi_type(elem.as_ref())?],
            ),
            AbiType::Tuple(items) => {
                let mut args = Vec::with_capacity(items.len());
                for item in items {
                    args.push(self.type_rep_for_abi_type(item)?);
                }
                (TypeCtor::Tuple(items.len()), args)
            }
            AbiType::Struct { type_id, args } => {
                let mut out_args = Vec::with_capacity(args.len());
                for arg in args {
                    out_args.push(self.type_rep_for_abi_type(arg)?);
                }
                (TypeCtor::Struct(*type_id), out_args)
            }
            AbiType::Enum { type_id, args } => {
                let mut out_args = Vec::with_capacity(args.len());
                for arg in args {
                    out_args.push(self.type_rep_for_abi_type(arg)?);
                }
                (TypeCtor::Enum(*type_id), out_args)
            }
        };
        Ok(self.type_reps.intern(TypeRepNode { ctor, args }))
    }

    fn abi_type_for_type_rep(&self, id: TypeRepId) -> Result<AbiType, String> {
        let node = self.type_reps.node(id).ok_or_else(|| {
            format!(
                "internal error: unknown typerep id {} (type rep table len {})",
                id.0,
                self.type_reps.nodes.len()
            )
        })?;
        Ok(match &node.ctor {
            TypeCtor::Unit => AbiType::Unit,
            TypeCtor::Bool => AbiType::Bool,
            TypeCtor::Int => AbiType::Int,
            TypeCtor::Float => AbiType::Float,
            TypeCtor::Byte => AbiType::Byte,
            TypeCtor::Char => AbiType::Char,
            TypeCtor::String => AbiType::String,
            TypeCtor::Bytes => AbiType::Bytes,
            TypeCtor::Cont => AbiType::Continuation,
            TypeCtor::Array => {
                let [elem] = node.args.as_slice() else {
                    return Err("typerep array node has invalid arity".to_string());
                };
                AbiType::Array(Box::new(self.abi_type_for_type_rep(*elem)?))
            }
            TypeCtor::Tuple(arity) => {
                if node.args.len() != *arity {
                    return Err("typerep tuple node has invalid arity".to_string());
                }
                let mut items = Vec::with_capacity(node.args.len());
                for arg in &node.args {
                    items.push(self.abi_type_for_type_rep(*arg)?);
                }
                AbiType::Tuple(items)
            }
            TypeCtor::Struct(type_id) => {
                let mut args = Vec::with_capacity(node.args.len());
                for arg in &node.args {
                    args.push(self.abi_type_for_type_rep(*arg)?);
                }
                AbiType::Struct {
                    type_id: *type_id,
                    args,
                }
            }
            TypeCtor::Enum(type_id) => {
                let mut args = Vec::with_capacity(node.args.len());
                for arg in &node.args {
                    args.push(self.abi_type_for_type_rep(*arg)?);
                }
                AbiType::Enum {
                    type_id: *type_id,
                    args,
                }
            }
            TypeCtor::Never => return Err("non-ABI-safe typerep (`!`)".to_string()),
            TypeCtor::Interface(_) => return Err("non-ABI-safe typerep (interface)".to_string()),
            TypeCtor::Fn => return Err("non-ABI-safe typerep (fn)".to_string()),
        })
    }

    fn expect_type_args_match(
        &mut self,
        got: &[TypeRepId],
        expected: &[AbiType],
        kind: &str,
        type_name: &str,
    ) -> Result<(), HostError> {
        if got.len() != expected.len() {
            return Err(HostError {
                message: format!(
                    "abi {kind} `{type_name}` type arg arity mismatch: expected {}, got {}",
                    expected.len(),
                    got.len()
                ),
            });
        }
        for (idx, (got_id, expected_ty)) in got.iter().copied().zip(expected.iter()).enumerate() {
            let expected_id = self
                .type_rep_for_abi_type(expected_ty)
                .map_err(|message| HostError { message })?;
            if got_id != expected_id {
                let got_desc = match self.abi_type_for_type_rep(got_id) {
                    Ok(t) => format!("{t:?}"),
                    Err(_) => format!("typerep#{}", got_id.0),
                };
                return Err(HostError {
                    message: format!(
                        "abi {kind} `{type_name}` type arg {idx} mismatch: expected {:?}, got {got_desc}",
                        expected_ty
                    ),
                });
            }
        }
        Ok(())
    }

    fn expect_heap_kind(&self, handle: GcRef, want: &str) -> Result<(), String> {
        let got = self.heap_kind(handle)?;
        if got != want {
            return Err(format!("abi ref kind mismatch: expected {want}, got {got}"));
        }
        Ok(())
    }

    fn heap_kind(&self, handle: GcRef) -> Result<&'static str, String> {
        let obj = self
            .heap
            .get(handle)
            .ok_or_else(|| "dangling abi object handle".to_string())?;
        Ok(match obj {
            HeapValue::BytesBuf { .. } | HeapValue::StringBuf { .. } => "buffer",
            HeapValue::Struct { .. } => "struct",
            HeapValue::Array(_) => "array",
            HeapValue::Tuple(_) => "tuple",
            HeapValue::Enum { .. } => "enum",
        })
    }

    fn heap_obj(&self, handle: GcRef) -> Result<&HeapValue, HostError> {
        self.heap.get(handle).ok_or_else(|| HostError {
            message: "dangling abi object handle".to_string(),
        })
    }

    fn heap_obj_mut(&mut self, handle: GcRef) -> Result<&mut HeapValue, HostError> {
        self.heap.get_mut(handle).ok_or_else(|| HostError {
            message: "dangling abi object handle".to_string(),
        })
    }

    fn schema_for_type_id(&self, type_id: TypeId) -> Result<&AbiSchema, HostError> {
        self.module
            .abi_schemas
            .get(type_id.0 as usize)
            .and_then(|s| s.as_ref())
            .ok_or_else(|| HostError {
                message: format!(
                    "no ABI schema available for type id {} (`{}`)",
                    type_id.0,
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })
    }
}
