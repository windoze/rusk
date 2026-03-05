use rusk_bytecode::{AbiSchema, AbiType, TypeId};

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
}

impl<'vm> HostContext<'vm> {
    pub(super) fn new(
        module: &'vm ExecutableModule,
        heap: &'vm mut ImmixHeap<HeapValue>,
        gc_allocations_since_collect: &'vm mut usize,
        pinned_continuations: &'vm mut PinnedContinuations,
    ) -> Self {
        Self {
            module,
            heap,
            gc_allocations_since_collect,
            pinned_continuations,
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
            fields: schema_fields,
        } = schema
        else {
            return Err(HostError {
                message: "abi struct ref: missing struct ABI schema".to_string(),
            });
        };

        let (field_idx, expected_ty) = schema_fields
            .iter()
            .enumerate()
            .find_map(|(idx, f)| (f.name == field).then_some((idx, f.ty.clone())))
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown field `{field}` for struct `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;

        let v = {
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
            if !type_args.is_empty() {
                return Err(HostError {
                    message: "generic struct values are not ABI-eligible in v1".to_string(),
                });
            }
            fields.get(field_idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "struct `{}` field index {} out of range (len {})",
                    self.module.type_name(type_id).unwrap_or("<unknown>"),
                    field_idx,
                    fields.len()
                ),
            })?
        };

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
            fields: schema_fields,
        } = schema
        else {
            return Err(HostError {
                message: "abi struct ref: missing struct ABI schema".to_string(),
            });
        };

        let (field_idx, expected_ty) = schema_fields
            .iter()
            .enumerate()
            .find_map(|(idx, f)| (f.name == field).then_some((idx, f.ty.clone())))
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown field `{field}` for struct `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;

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

        let HeapValue::Struct {
            type_id: got_type_id,
            type_args,
            fields,
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
        if !type_args.is_empty() {
            return Err(HostError {
                message: "generic struct values are not ABI-eligible in v1".to_string(),
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
        let (variant_name, v) = {
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
            if !type_args.is_empty() {
                return Err(HostError {
                    message: "generic enum values are not ABI-eligible in v1".to_string(),
                });
            }
            let v = fields.get(idx).cloned().ok_or_else(|| HostError {
                message: format!(
                    "abi enum ref: field index {idx} out of range (len {})",
                    fields.len()
                ),
            })?;
            (variant.clone(), v)
        };

        let schema = self.schema_for_type_id(type_id)?;
        let AbiSchema::Enum { variants } = schema else {
            return Err(HostError {
                message: "abi enum ref: missing enum ABI schema".to_string(),
            });
        };
        let schema_variant = variants
            .iter()
            .find(|v| v.name == variant_name)
            .ok_or_else(|| HostError {
                message: format!(
                    "unknown enum variant `{variant_name}` for enum `{}`",
                    self.module.type_name(type_id).unwrap_or("<unknown>")
                ),
            })?;
        let expected = schema_variant
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
        let type_id = self.module.type_id(type_name).ok_or_else(|| HostError {
            message: format!("unknown struct type `{type_name}`"),
        })?;
        let schema_fields = match self.schema_for_type_id(type_id)? {
            AbiSchema::Struct { fields } => fields.clone(),
            _ => {
                return Err(HostError {
                    message: format!("type `{type_name}` is not a struct ABI schema"),
                });
            }
        };

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
            let expected = field.ty.clone();
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

        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Struct {
                type_id,
                type_args: Vec::new(),
                fields: values,
            },
        );
        let Value::Ref(r) = v else {
            return Err(HostError {
                message: "internal error: struct allocation did not produce ref".to_string(),
            });
        };

        Ok(AbiValue::Struct(crate::AbiStructRef::new(
            r.handle, r.readonly, type_id,
        )))
    }

    pub fn alloc_enum(
        &mut self,
        type_name: &str,
        variant: &str,
        fields: Vec<AbiValue>,
    ) -> Result<AbiValue, HostError> {
        let type_id = self.module.type_id(type_name).ok_or_else(|| HostError {
            message: format!("unknown enum type `{type_name}`"),
        })?;
        let variants = match self.schema_for_type_id(type_id)? {
            AbiSchema::Enum { variants } => variants.clone(),
            _ => {
                return Err(HostError {
                    message: format!("type `{type_name}` is not an enum ABI schema"),
                });
            }
        };
        let Some(schema_variant) = variants.into_iter().find(|v| v.name == variant) else {
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
        let schema_fields = schema_variant.fields;
        for (idx, (expected, value)) in schema_fields
            .into_iter()
            .zip(fields.into_iter())
            .enumerate()
        {
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
        let v = alloc_ref(
            self.heap,
            self.gc_allocations_since_collect,
            HeapValue::Enum {
                type_id,
                type_args: Vec::new(),
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
            r.handle, r.readonly, type_id,
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
                        if !type_args.is_empty() {
                            return Err(
                                "generic struct values are not ABI-eligible in v1".to_string()
                            );
                        }
                        Ok(AbiValue::Struct(crate::AbiStructRef::new(
                            r.handle, r.readonly, *type_id,
                        )))
                    }
                    HeapValue::Enum {
                        type_id, type_args, ..
                    } => {
                        if !type_args.is_empty() {
                            return Err(
                                "generic enum values are not ABI-eligible in v1".to_string()
                            );
                        }
                        Ok(AbiValue::Enum(crate::AbiEnumRef::new(
                            r.handle, r.readonly, *type_id,
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
                        if !type_args.is_empty() {
                            stack.remove(&r.handle);
                            return Err(
                                "generic struct values are not ABI-eligible in v1".to_string()
                            );
                        }
                        AbiType::Struct(*type_id)
                    }
                    HeapValue::Enum {
                        type_id, type_args, ..
                    } => {
                        if !type_args.is_empty() {
                            stack.remove(&r.handle);
                            return Err(
                                "generic enum values are not ABI-eligible in v1".to_string()
                            );
                        }
                        AbiType::Enum(*type_id)
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
            (AbiType::Struct(type_id), Value::Ref(r)) => {
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
                if got_type_id != type_id {
                    return Err(format!(
                        "abi struct type mismatch: expected {:?}, got {:?}",
                        type_id, got_type_id
                    ));
                }
                if !type_args.is_empty() {
                    return Err("generic struct values are not ABI-eligible in v1".to_string());
                }
                Ok(AbiValue::Struct(crate::AbiStructRef::new(
                    r.handle, r.readonly, *type_id,
                )))
            }
            (AbiType::Enum(type_id), Value::Ref(r)) => {
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
                if got_type_id != type_id {
                    return Err(format!(
                        "abi enum type mismatch: expected {:?}, got {:?}",
                        type_id, got_type_id
                    ));
                }
                if !type_args.is_empty() {
                    return Err("generic enum values are not ABI-eligible in v1".to_string());
                }
                Ok(AbiValue::Enum(crate::AbiEnumRef::new(
                    r.handle, r.readonly, *type_id,
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
                if *type_id != r.type_id() {
                    return Err("abi struct ref: type id mismatch".to_string());
                }
                if !type_args.is_empty() {
                    return Err("generic struct values are not ABI-eligible in v1".to_string());
                }
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
            AbiValue::Enum(r) => {
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
                if *type_id != r.type_id() {
                    return Err("abi enum ref: type id mismatch".to_string());
                }
                if !type_args.is_empty() {
                    return Err("generic enum values are not ABI-eligible in v1".to_string());
                }
                Value::Ref(RefValue {
                    readonly: r.readonly(),
                    handle: r.handle(),
                })
            }
        })
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
