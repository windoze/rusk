extern crate alloc;

use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::ToString;
use alloc::vec;
use alloc::vec::Vec;

use crate::interpreter::{HeapValue, Interpreter, RuntimeError, Value};

const ARRAY_ITER_TYPE: &str = "core::intrinsics::ArrayIter";
const ARRAY_ITER_FIELD_ARRAY: &str = "arr";
const ARRAY_ITER_FIELD_INDEX: &str = "idx";

/// Registers the required `core::intrinsics::*` host functions for executing code produced by the compiler.
///
/// The Rusk compiler lowers operators, formatted strings, and `for` loops into calls to these
/// functions. They form the core-library surface of this reference implementation.
pub fn register_core_host_fns(interp: &mut Interpreter) {
    register_string_fns(interp);
    register_to_string(interp);
    register_panic(interp);
    register_bool_fns(interp);
    register_int_fns(interp);
    register_float_fns(interp);
    register_bytes_string_unit_eq(interp);
    register_array_fns(interp);
    register_iterator_fns(interp);
}

fn register_string_fns(interp: &mut Interpreter) {
    interp.register_host_fn(
        "core::intrinsics::string_concat",
        |_interp, args| match args {
            [Value::String(a), Value::String(b)] => Ok(Value::String(format!("{a}{b}"))),
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::string_concat: bad args: {other:?}"),
            }),
        },
    );
}

fn register_to_string(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::to_string", |_interp, args| match args {
        [Value::TypeRep(_), Value::Unit] => Ok(Value::String("()".to_string())),
        [Value::TypeRep(_), Value::Bool(v)] => Ok(Value::String(v.to_string())),
        [Value::TypeRep(_), Value::Int(v)] => Ok(Value::String(v.to_string())),
        [Value::TypeRep(_), Value::Float(v)] => Ok(Value::String(v.to_string())),
        [Value::TypeRep(_), Value::String(v)] => Ok(Value::String(v.clone())),
        [Value::TypeRep(_), Value::Bytes(v)] => {
            Ok(Value::String(format!("bytes(len={})", v.len())))
        }
        [Value::TypeRep(_), Value::Ref(r)] => {
            Ok(Value::String(format!("{:?}", Value::Ref(r.clone()))))
        }
        [Value::TypeRep(_), Value::Function(id)] => {
            let name = _interp.function_name(*id).unwrap_or("<unknown function>");
            Ok(Value::String(format!("fn({name})")))
        }
        [Value::TypeRep(_), Value::Continuation(_)] => {
            Ok(Value::String("continuation(..)".to_string()))
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::to_string: bad args: {other:?}"),
        }),
    });
}

fn register_bool_fns(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::bool_not", |_interp, args| match args {
        [Value::Bool(v)] => Ok(Value::Bool(!v)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::bool_not: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::bool_eq", |_interp, args| match args {
        [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::bool_eq: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::bool_ne", |_interp, args| match args {
        [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::bool_ne: bad args: {other:?}"),
        }),
    });
}

fn register_int_fns(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::int_add", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_add: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_sub", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a - b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_sub: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_mul", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a * b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_mul: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_div", |_interp, args| match args {
        [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
            message: "core::intrinsics::int_div: division by zero".to_string(),
        }),
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a / b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_div: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_mod", |_interp, args| match args {
        [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
            message: "core::intrinsics::int_mod: modulo by zero".to_string(),
        }),
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a % b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_mod: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::int_eq", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_ne", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_ne: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_lt", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a < b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_lt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_le", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a <= b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_le: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_gt", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a > b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_gt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::int_ge", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a >= b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::int_ge: bad args: {other:?}"),
        }),
    });
}

fn register_float_fns(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::float_add", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_add: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_sub", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a - b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_sub: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_mul", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a * b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_mul: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_div", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a / b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_div: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_mod", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a % b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_mod: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::float_eq", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_ne", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_ne: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_lt", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a < b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_lt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_le", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a <= b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_le: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_gt", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a > b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_gt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::float_ge", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a >= b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::float_ge: bad args: {other:?}"),
        }),
    });
}

fn register_bytes_string_unit_eq(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::string_eq", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::string_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::string_ne", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::string_ne: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::bytes_eq", |_interp, args| match args {
        [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::bytes_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::bytes_ne", |_interp, args| match args {
        [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::bytes_ne: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::unit_eq", |_interp, args| match args {
        [Value::Unit, Value::Unit] => Ok(Value::Bool(true)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::unit_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("core::intrinsics::unit_ne", |_interp, args| match args {
        [Value::Unit, Value::Unit] => Ok(Value::Bool(false)),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::unit_ne: bad args: {other:?}"),
        }),
    });
}

fn register_iterator_fns(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::into_iter", |interp, args| match args {
        [Value::TypeRep(elem_rep), Value::Ref(arr)] => {
            let HeapValue::Array(_) = interp.heap_value(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::into_iter: expected an array".to_string(),
                });
            };

            let mut fields = BTreeMap::new();
            fields.insert(ARRAY_ITER_FIELD_ARRAY.to_string(), Value::Ref(arr.clone()));
            fields.insert(ARRAY_ITER_FIELD_INDEX.to_string(), Value::Int(0));
            Ok(interp.alloc_struct_typed(ARRAY_ITER_TYPE, vec![*elem_rep], fields))
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::into_iter: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::next", |interp, args| match args {
        [Value::TypeRep(elem_rep), Value::Ref(iter)] => {
            if iter.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }

            let HeapValue::Struct { type_name, .. } = interp.heap_value(iter)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: expected iterator struct".to_string(),
                });
            };
            if type_name != ARRAY_ITER_TYPE {
                return Err(RuntimeError::Trap {
                    message: format!(
                        "core::intrinsics::next: expected `{ARRAY_ITER_TYPE}`, got `{type_name}`"
                    ),
                });
            }

            let Value::Ref(arr_ref) = interp.read_struct_field(iter, ARRAY_ITER_FIELD_ARRAY)?
            else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator missing `arr` field".to_string(),
                });
            };
            let Value::Int(idx) = interp.read_struct_field(iter, ARRAY_ITER_FIELD_INDEX)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator missing `idx` field".to_string(),
                });
            };
            let idx: i64 = idx;

            let HeapValue::Array(items) = interp.heap_value(&arr_ref)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator `arr` is not an array".to_string(),
                });
            };

            if idx < 0 {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: negative iterator index".to_string(),
                });
            }
            let idx_usize = idx as usize;

            // Compute result first, then mutate iterator state.
            let out = if idx_usize < items.len() {
                let mut item = items[idx_usize].clone();
                if arr_ref.is_readonly() {
                    item = item.into_readonly_view();
                }
                interp.alloc_enum_typed("Option", vec![*elem_rep], "Some", vec![item])
            } else {
                interp.alloc_enum_typed("Option", vec![*elem_rep], "None", vec![])
            };

            interp.write_struct_field(iter, ARRAY_ITER_FIELD_INDEX, Value::Int(idx + 1))?;
            Ok(out)
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::next: bad args: {other:?}"),
        }),
    });
}

fn register_panic(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::panic", |_interp, args| match args {
        [Value::TypeRep(_), Value::String(msg)] => Err(RuntimeError::Trap {
            message: format!("panic: {msg}"),
        }),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::panic: bad args: {other:?}"),
        }),
    });
}

fn register_array_fns(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::array_len", |interp, args| match args {
        [Value::TypeRep(_), Value::Ref(arr)] => {
            let HeapValue::Array(items) = interp.heap_value(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_len: expected an array".to_string(),
                });
            };
            Ok(Value::Int(items.len() as i64))
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::array_len: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn(
        "core::intrinsics::array_len_ro",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(arr)] => {
                let HeapValue::Array(items) = interp.heap_value(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_len_ro: expected an array".to_string(),
                    });
                };
                Ok(Value::Int(items.len() as i64))
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_len_ro: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn("core::intrinsics::array_push", |interp, args| match args {
        [Value::TypeRep(_), Value::Ref(arr), value] => {
            if arr.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }
            let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_push: expected an array".to_string(),
                });
            };
            items.push(value.clone());
            Ok(Value::Unit)
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::array_push: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::array_pop", |interp, args| match args {
        [Value::TypeRep(elem_rep), Value::Ref(arr)] => {
            if arr.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }
            let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_pop: expected an array".to_string(),
                });
            };

            match items.pop() {
                Some(v) => Ok(interp.alloc_enum_typed("Option", vec![*elem_rep], "Some", vec![v])),
                None => Ok(interp.alloc_enum_typed("Option", vec![*elem_rep], "None", vec![])),
            }
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::array_pop: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::array_clear", |interp, args| match args {
        [Value::TypeRep(_), Value::Ref(arr)] => {
            if arr.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }
            let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_clear: expected an array".to_string(),
                });
            };
            items.clear();
            Ok(Value::Unit)
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::array_clear: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn(
        "core::intrinsics::array_resize",
        |interp, args| match args {
            [
                Value::TypeRep(_),
                Value::Ref(arr),
                Value::Int(new_len),
                fill,
            ] => {
                if arr.is_readonly() {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                if *new_len < 0 {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_resize: new_len must be >= 0".to_string(),
                    });
                }
                let new_len_usize: usize = (*new_len) as usize;

                let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_resize: expected an array".to_string(),
                    });
                };

                if new_len_usize <= items.len() {
                    items.truncate(new_len_usize);
                    return Ok(Value::Unit);
                }

                let extra = new_len_usize - items.len();
                items.reserve(extra);
                for _ in 0..extra {
                    items.push(fill.clone());
                }
                Ok(Value::Unit)
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_resize: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn(
        "core::intrinsics::array_insert",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(arr), Value::Int(idx), value] => {
                if arr.is_readonly() {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_insert: expected an array".to_string(),
                    });
                };
                let idx_usize: usize =
                    (*idx)
                        .try_into()
                        .map_err(|_| RuntimeError::IndexOutOfBounds {
                            index: *idx,
                            len: items.len(),
                        })?;
                if idx_usize > items.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        len: items.len(),
                    });
                }
                items.insert(idx_usize, value.clone());
                Ok(Value::Unit)
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_insert: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn(
        "core::intrinsics::array_remove",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(arr), Value::Int(idx)] => {
                if arr.is_readonly() {
                    return Err(RuntimeError::ReadonlyWrite);
                }
                let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_remove: expected an array".to_string(),
                    });
                };
                let idx_usize: usize =
                    (*idx)
                        .try_into()
                        .map_err(|_| RuntimeError::IndexOutOfBounds {
                            index: *idx,
                            len: items.len(),
                        })?;
                if idx_usize >= items.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *idx,
                        len: items.len(),
                    });
                }
                Ok(items.remove(idx_usize))
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_remove: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn(
        "core::intrinsics::array_extend",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(arr), Value::Ref(other)] => {
                if arr.is_readonly() {
                    return Err(RuntimeError::ReadonlyWrite);
                }

                let other_items = {
                    let HeapValue::Array(items) = interp.heap_value(other)? else {
                        return Err(RuntimeError::Trap {
                            message:
                                "core::intrinsics::array_extend: expected an array for `other`"
                                    .to_string(),
                        });
                    };
                    items.clone()
                };

                let HeapValue::Array(items) = interp.heap_value_mut(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_extend: expected an array for `arr`"
                            .to_string(),
                    });
                };
                items.extend(other_items);
                Ok(Value::Unit)
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_extend: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn(
        "core::intrinsics::array_concat",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(a), Value::Ref(b)] => {
                let HeapValue::Array(a_items) = interp.heap_value(a)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_concat: expected an array for `a`"
                            .to_string(),
                    });
                };
                let HeapValue::Array(b_items) = interp.heap_value(b)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_concat: expected an array for `b`"
                            .to_string(),
                    });
                };

                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items.iter().cloned());
                items.extend(b_items.iter().cloned());
                Ok(interp.alloc_array(items))
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_concat: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn(
        "core::intrinsics::array_concat_ro",
        |interp, args| match args {
            [Value::TypeRep(_), Value::Ref(a), Value::Ref(b)] => {
                let HeapValue::Array(a_items) = interp.heap_value(a)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_concat_ro: expected an array for `a`"
                            .to_string(),
                    });
                };
                let HeapValue::Array(b_items) = interp.heap_value(b)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_concat_ro: expected an array for `b`"
                            .to_string(),
                    });
                };

                let mut items = Vec::with_capacity(a_items.len() + b_items.len());
                items.extend(a_items.iter().cloned().map(Value::into_readonly_view));
                items.extend(b_items.iter().cloned().map(Value::into_readonly_view));
                Ok(interp.alloc_array(items))
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_concat_ro: bad args: {other:?}"),
            }),
        },
    );

    interp.register_host_fn("core::intrinsics::array_slice", |interp, args| match args {
        [
            Value::TypeRep(_),
            Value::Ref(arr),
            Value::Int(start),
            Value::Int(end),
        ] => {
            let HeapValue::Array(items) = interp.heap_value(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_slice: expected an array".to_string(),
                });
            };

            let start_usize: usize =
                (*start)
                    .try_into()
                    .map_err(|_| RuntimeError::IndexOutOfBounds {
                        index: *start,
                        len: items.len(),
                    })?;
            let end_usize: usize =
                (*end)
                    .try_into()
                    .map_err(|_| RuntimeError::IndexOutOfBounds {
                        index: *end,
                        len: items.len(),
                    })?;
            if start_usize > end_usize {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::array_slice: start must be <= end".to_string(),
                });
            }
            if end_usize > items.len() {
                return Err(RuntimeError::IndexOutOfBounds {
                    index: *end,
                    len: items.len(),
                });
            }

            Ok(interp.alloc_array(items[start_usize..end_usize].to_vec()))
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::array_slice: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn(
        "core::intrinsics::array_slice_ro",
        |interp, args| match args {
            [
                Value::TypeRep(_),
                Value::Ref(arr),
                Value::Int(start),
                Value::Int(end),
            ] => {
                let HeapValue::Array(items) = interp.heap_value(arr)? else {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_slice_ro: expected an array".to_string(),
                    });
                };

                let start_usize: usize =
                    (*start)
                        .try_into()
                        .map_err(|_| RuntimeError::IndexOutOfBounds {
                            index: *start,
                            len: items.len(),
                        })?;
                let end_usize: usize =
                    (*end)
                        .try_into()
                        .map_err(|_| RuntimeError::IndexOutOfBounds {
                            index: *end,
                            len: items.len(),
                        })?;
                if start_usize > end_usize {
                    return Err(RuntimeError::Trap {
                        message: "core::intrinsics::array_slice_ro: start must be <= end"
                            .to_string(),
                    });
                }
                if end_usize > items.len() {
                    return Err(RuntimeError::IndexOutOfBounds {
                        index: *end,
                        len: items.len(),
                    });
                }

                Ok(interp.alloc_array(
                    items[start_usize..end_usize]
                        .iter()
                        .cloned()
                        .map(Value::into_readonly_view)
                        .collect(),
                ))
            }
            other => Err(RuntimeError::Trap {
                message: format!("core::intrinsics::array_slice_ro: bad args: {other:?}"),
            }),
        },
    );
}
