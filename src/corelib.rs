use crate::interpreter::{HeapValue, Interpreter, RuntimeError, Value};
use std::collections::BTreeMap;

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
        [Value::Unit] => Ok(Value::String("()".to_string())),
        [Value::Bool(v)] => Ok(Value::String(v.to_string())),
        [Value::Int(v)] => Ok(Value::String(v.to_string())),
        [Value::Float(v)] => Ok(Value::String(v.to_string())),
        [Value::String(v)] => Ok(Value::String(v.clone())),
        [Value::Bytes(v)] => Ok(Value::String(format!("bytes(len={})", v.len()))),
        [Value::Ref(r)] => Ok(Value::String(format!("{:?}", Value::Ref(r.clone())))),
        [Value::Function(name)] => Ok(Value::String(format!("fn({name})"))),
        [Value::Continuation(_)] => Ok(Value::String("continuation(..)".to_string())),
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
        [Value::Ref(arr)] => {
            let HeapValue::Array(_) = interp.heap_value(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::into_iter: expected an array".to_string(),
                });
            };

            let mut fields = BTreeMap::new();
            fields.insert(ARRAY_ITER_FIELD_ARRAY.to_string(), Value::Ref(arr.clone()));
            fields.insert(ARRAY_ITER_FIELD_INDEX.to_string(), Value::Int(0));
            Ok(interp.alloc_struct(ARRAY_ITER_TYPE, fields))
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::into_iter: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("core::intrinsics::next", |interp, args| match args {
        [Value::Ref(iter)] => {
            if iter.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }

            let HeapValue::Struct { type_name, fields } = interp.heap_value(iter)? else {
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

            let Some(Value::Ref(arr_ref)) = fields.get(ARRAY_ITER_FIELD_ARRAY) else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator missing `arr` field".to_string(),
                });
            };
            let Some(Value::Int(idx)) = fields.get(ARRAY_ITER_FIELD_INDEX) else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator missing `idx` field".to_string(),
                });
            };
            let idx: i64 = *idx;

            let HeapValue::Array(items) = interp.heap_value(arr_ref)? else {
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
                interp.alloc_enum("Option", "Some", vec![item])
            } else {
                interp.alloc_enum("Option", "None", vec![Value::Unit])
            };

            let HeapValue::Struct { fields, .. } = interp.heap_value_mut(iter)? else {
                return Err(RuntimeError::Trap {
                    message: "core::intrinsics::next: iterator mutated into non-struct".to_string(),
                });
            };
            fields.insert(ARRAY_ITER_FIELD_INDEX.to_string(), Value::Int(idx + 1));
            Ok(out)
        }
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::next: bad args: {other:?}"),
        }),
    });
}

fn register_panic(interp: &mut Interpreter) {
    interp.register_host_fn("core::intrinsics::panic", |_interp, args| match args {
        [Value::String(msg)] => Err(RuntimeError::Trap {
            message: format!("panic: {msg}"),
        }),
        other => Err(RuntimeError::Trap {
            message: format!("core::intrinsics::panic: bad args: {other:?}"),
        }),
    });
}
