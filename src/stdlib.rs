use crate::interpreter::{HeapValue, Interpreter, RuntimeError, Value};
use std::collections::BTreeMap;

const ARRAY_ITER_TYPE: &str = "std::ArrayIter";
const ARRAY_ITER_FIELD_ARRAY: &str = "arr";
const ARRAY_ITER_FIELD_INDEX: &str = "idx";

/// Registers the required `std::*` host functions for executing code produced by the compiler.
///
/// The Rusk compiler lowers operators, formatted strings, and `for` loops into calls to these
/// functions. They form the standard-library surface of this reference implementation.
pub fn register_std_host_fns(interp: &mut Interpreter) {
    register_string_fns(interp);
    register_to_string(interp);
    register_bool_fns(interp);
    register_int_fns(interp);
    register_float_fns(interp);
    register_bytes_string_unit_eq(interp);
    register_iterator_fns(interp);
}

fn register_string_fns(interp: &mut Interpreter) {
    interp.register_host_fn("std::string_concat", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::String(format!("{a}{b}"))),
        other => Err(RuntimeError::Trap {
            message: format!("std::string_concat: bad args: {other:?}"),
        }),
    });
}

fn register_to_string(interp: &mut Interpreter) {
    interp.register_host_fn("std::to_string", |_interp, args| match args {
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
            message: format!("std::to_string: bad args: {other:?}"),
        }),
    });
}

fn register_bool_fns(interp: &mut Interpreter) {
    interp.register_host_fn("std::bool_not", |_interp, args| match args {
        [Value::Bool(v)] => Ok(Value::Bool(!v)),
        other => Err(RuntimeError::Trap {
            message: format!("std::bool_not: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::bool_eq", |_interp, args| match args {
        [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::bool_eq: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::bool_ne", |_interp, args| match args {
        [Value::Bool(a), Value::Bool(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::bool_ne: bad args: {other:?}"),
        }),
    });
}

fn register_int_fns(interp: &mut Interpreter) {
    interp.register_host_fn("std::int_add", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_add: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_sub", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a - b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_sub: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_mul", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a * b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_mul: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_div", |_interp, args| match args {
        [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
            message: "std::int_div: division by zero".to_string(),
        }),
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a / b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_div: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_mod", |_interp, args| match args {
        [Value::Int(_), Value::Int(0)] => Err(RuntimeError::Trap {
            message: "std::int_mod: modulo by zero".to_string(),
        }),
        [Value::Int(a), Value::Int(b)] => Ok(Value::Int(a % b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_mod: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::int_eq", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_ne", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_ne: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_lt", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a < b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_lt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_le", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a <= b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_le: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_gt", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a > b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_gt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::int_ge", |_interp, args| match args {
        [Value::Int(a), Value::Int(b)] => Ok(Value::Bool(a >= b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::int_ge: bad args: {other:?}"),
        }),
    });
}

fn register_float_fns(interp: &mut Interpreter) {
    interp.register_host_fn("std::float_add", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a + b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_add: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_sub", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a - b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_sub: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_mul", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a * b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_mul: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_div", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a / b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_div: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_mod", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Float(a % b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_mod: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::float_eq", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_ne", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_ne: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_lt", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a < b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_lt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_le", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a <= b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_le: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_gt", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a > b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_gt: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::float_ge", |_interp, args| match args {
        [Value::Float(a), Value::Float(b)] => Ok(Value::Bool(a >= b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::float_ge: bad args: {other:?}"),
        }),
    });
}

fn register_bytes_string_unit_eq(interp: &mut Interpreter) {
    interp.register_host_fn("std::string_eq", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::string_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::string_ne", |_interp, args| match args {
        [Value::String(a), Value::String(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::string_ne: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::bytes_eq", |_interp, args| match args {
        [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a == b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::bytes_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::bytes_ne", |_interp, args| match args {
        [Value::Bytes(a), Value::Bytes(b)] => Ok(Value::Bool(a != b)),
        other => Err(RuntimeError::Trap {
            message: format!("std::bytes_ne: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::unit_eq", |_interp, args| match args {
        [Value::Unit, Value::Unit] => Ok(Value::Bool(true)),
        other => Err(RuntimeError::Trap {
            message: format!("std::unit_eq: bad args: {other:?}"),
        }),
    });
    interp.register_host_fn("std::unit_ne", |_interp, args| match args {
        [Value::Unit, Value::Unit] => Ok(Value::Bool(false)),
        other => Err(RuntimeError::Trap {
            message: format!("std::unit_ne: bad args: {other:?}"),
        }),
    });
}

fn register_iterator_fns(interp: &mut Interpreter) {
    interp.register_host_fn("std::into_iter", |interp, args| match args {
        [Value::Ref(arr)] => {
            let HeapValue::Array(_) = interp.heap_value(arr)? else {
                return Err(RuntimeError::Trap {
                    message: "std::into_iter: expected an array".to_string(),
                });
            };

            let mut fields = BTreeMap::new();
            fields.insert(ARRAY_ITER_FIELD_ARRAY.to_string(), Value::Ref(arr.clone()));
            fields.insert(ARRAY_ITER_FIELD_INDEX.to_string(), Value::Int(0));
            Ok(interp.alloc_struct(ARRAY_ITER_TYPE, fields))
        }
        other => Err(RuntimeError::Trap {
            message: format!("std::into_iter: bad args: {other:?}"),
        }),
    });

    interp.register_host_fn("std::next", |interp, args| match args {
        [Value::Ref(iter)] => {
            if iter.is_readonly() {
                return Err(RuntimeError::ReadonlyWrite);
            }

            let HeapValue::Struct { type_name, fields } = interp.heap_value(iter)? else {
                return Err(RuntimeError::Trap {
                    message: "std::next: expected iterator struct".to_string(),
                });
            };
            if type_name != ARRAY_ITER_TYPE {
                return Err(RuntimeError::Trap {
                    message: format!("std::next: expected `{ARRAY_ITER_TYPE}`, got `{type_name}`"),
                });
            }

            let Some(Value::Ref(arr_ref)) = fields.get(ARRAY_ITER_FIELD_ARRAY) else {
                return Err(RuntimeError::Trap {
                    message: "std::next: iterator missing `arr` field".to_string(),
                });
            };
            let Some(Value::Int(idx)) = fields.get(ARRAY_ITER_FIELD_INDEX) else {
                return Err(RuntimeError::Trap {
                    message: "std::next: iterator missing `idx` field".to_string(),
                });
            };
            let idx: i64 = *idx;

            let HeapValue::Array(items) = interp.heap_value(arr_ref)? else {
                return Err(RuntimeError::Trap {
                    message: "std::next: iterator `arr` is not an array".to_string(),
                });
            };

            if idx < 0 {
                return Err(RuntimeError::Trap {
                    message: "std::next: negative iterator index".to_string(),
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
                    message: "std::next: iterator mutated into non-struct".to_string(),
                });
            };
            fields.insert(ARRAY_ITER_FIELD_INDEX.to_string(), Value::Int(idx + 1));
            Ok(out)
        }
        other => Err(RuntimeError::Trap {
            message: format!("std::next: bad args: {other:?}"),
        }),
    });
}
