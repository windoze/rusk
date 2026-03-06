use rusk_compiler::{CompileOptions, HostFnSig, HostType};
use rusk_vm::{AbiValue, ContinuationHandle, HostContext, HostError, HostFn, Vm};

use rusk_bytecode::AbiType;

#[allow(unused)]
pub fn register_test_external_effects(options: &mut CompileOptions) {
    options
        .register_external_effect(
            "TestFfi",
            "add",
            HostFnSig {
                params: vec![HostType::Int, HostType::Int],
                ret: HostType::Int,
            },
        )
        .expect("register TestFfi.add");

    options
        .register_external_effect(
            "TestFfi",
            "echo",
            HostFnSig {
                params: vec![HostType::String],
                ret: HostType::String,
            },
        )
        .expect("register TestFfi.echo");

    options
        .register_external_effect(
            "TestFfi",
            "echo_bytes",
            HostFnSig {
                params: vec![HostType::Bytes],
                ret: HostType::Bytes,
            },
        )
        .expect("register TestFfi.echo_bytes");
}

#[allow(unused)]
pub fn install_test_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    let stored_cont: std::rc::Rc<std::cell::RefCell<Option<ContinuationHandle>>> =
        std::rc::Rc::new(std::cell::RefCell::new(None));

    if let Some(id) = module.host_import_id("test::add_int") {
        vm.register_host_import_typed(id, |(a, b): (i64, i64)| -> Result<i64, HostError> {
            Ok(a + b)
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::concat_string") {
        vm.register_host_import_typed(
            id,
            |(a, b): (String, String)| -> Result<String, HostError> { Ok(format!("{a}{b}")) },
        )
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bool_not") {
        vm.register_host_import_typed(id, |(v,): (bool,)| -> Result<bool, HostError> { Ok(!v) })
            .unwrap();
    }

    if let Some(id) = module.host_import_id("test::float_mul") {
        vm.register_host_import_typed(id, |(a, b): (f64, f64)| -> Result<f64, HostError> {
            Ok(a * b)
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::float_eq") {
        vm.register_host_import_typed(id, |(a, b): (f64, f64)| -> Result<bool, HostError> {
            Ok(a == b)
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::unit") {
        vm.register_host_import_typed(id, |()| -> Result<(), HostError> { Ok(()) })
            .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bytes_echo") {
        vm.register_host_import_typed(id, |(b,): (Vec<u8>,)| -> Result<Vec<u8>, HostError> {
            Ok(b)
        })
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::bytes_eq") {
        vm.register_host_import_typed(
            id,
            |(a, b): (Vec<u8>, Vec<u8>)| -> Result<bool, HostError> { Ok(a == b) },
        )
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::store_cont") {
        let stored_cont = std::rc::Rc::clone(&stored_cont);
        vm.register_host_import_typed(
            id,
            move |(k,): (ContinuationHandle,)| -> Result<(), HostError> {
                *stored_cont.borrow_mut() = Some(k);
                Ok(())
            },
        )
        .unwrap();
    }

    if let Some(id) = module.host_import_id("test::take_cont") {
        let stored_cont = std::rc::Rc::clone(&stored_cont);
        vm.register_host_import_typed(id, move |()| -> Result<ContinuationHandle, HostError> {
            stored_cont.borrow().clone().ok_or_else(|| HostError {
                message: "test::take_cont: missing stored continuation".to_string(),
            })
        })
        .unwrap();
    }
}

#[allow(unused)]
pub fn install_core_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    // Strings
    if let Some(id) = module.host_import_id("core::intrinsics::string_concat") {
        vm.register_host_import_typed(
            id,
            |(a, b): (String, String)| -> Result<String, HostError> { Ok(format!("{a}{b}")) },
        )
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::string_eq") {
        vm.register_host_import_typed(id, |(a, b): (String, String)| -> Result<bool, HostError> {
            Ok(a == b)
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::string_ne") {
        vm.register_host_import_typed(id, |(a, b): (String, String)| -> Result<bool, HostError> {
            Ok(a != b)
        })
        .unwrap();
    }

    // Bytes
    if let Some(id) = module.host_import_id("core::intrinsics::bytes_eq") {
        vm.register_host_import_typed(
            id,
            |(a, b): (Vec<u8>, Vec<u8>)| -> Result<bool, HostError> { Ok(a == b) },
        )
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::bytes_ne") {
        vm.register_host_import_typed(
            id,
            |(a, b): (Vec<u8>, Vec<u8>)| -> Result<bool, HostError> { Ok(a != b) },
        )
        .unwrap();
    }

    // Unit
    if let Some(id) = module.host_import_id("core::intrinsics::unit_eq") {
        vm.register_host_import_typed(id, |(_a, _b): ((), ())| -> Result<bool, HostError> {
            Ok(true)
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::unit_ne") {
        vm.register_host_import_typed(id, |(_a, _b): ((), ())| -> Result<bool, HostError> {
            Ok(false)
        })
        .unwrap();
    }

    // Floats
    if let Some(id) = module.host_import_id("core::intrinsics::float_lt") {
        vm.register_host_import_typed(id, |(a, b): (f64, f64)| -> Result<bool, HostError> {
            Ok(a < b)
        })
        .unwrap();
    }
    if let Some(id) = module.host_import_id("core::intrinsics::float_ge") {
        vm.register_host_import_typed(id, |(a, b): (f64, f64)| -> Result<bool, HostError> {
            Ok(a >= b)
        })
        .unwrap();
    }
}

struct AbiSumPoint;

impl HostFn for AbiSumPoint {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Struct(p)] = args else {
            return Err(HostError {
                message: format!("abi::sum_point: bad args: {args:?}"),
            });
        };
        let AbiValue::Int(x) = cx.struct_get(p, "x")? else {
            return Err(HostError {
                message: "abi::sum_point: Point.x is not an int".to_string(),
            });
        };
        let AbiValue::Int(y) = cx.struct_get(p, "y")? else {
            return Err(HostError {
                message: "abi::sum_point: Point.y is not an int".to_string(),
            });
        };
        Ok(AbiValue::Int(x + y))
    }
}

struct AbiMakePoint;

impl HostFn for AbiMakePoint {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Int(x), AbiValue::Int(y)] = args else {
            return Err(HostError {
                message: format!("abi::make_point: bad args: {args:?}"),
            });
        };
        cx.alloc_struct(
            "Point",
            vec![("x", AbiValue::Int(*x)), ("y", AbiValue::Int(*y))],
        )
    }
}

struct AbiSumPair;

impl HostFn for AbiSumPair {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Tuple(t)] = args else {
            return Err(HostError {
                message: format!("abi::sum_pair: bad args: {args:?}"),
            });
        };
        let AbiValue::Int(a) = cx.tuple_get(t, 0)? else {
            return Err(HostError {
                message: "abi::sum_pair: expected int at index 0".to_string(),
            });
        };
        let AbiValue::Int(b) = cx.tuple_get(t, 1)? else {
            return Err(HostError {
                message: "abi::sum_pair: expected int at index 1".to_string(),
            });
        };
        Ok(AbiValue::Int(a + b))
    }
}

struct AbiMakePair;

impl HostFn for AbiMakePair {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Int(a), AbiValue::Int(b)] = args else {
            return Err(HostError {
                message: format!("abi::make_pair: bad args: {args:?}"),
            });
        };
        cx.alloc_tuple(vec![AbiValue::Int(*a), AbiValue::Int(*b)])
    }
}

struct AbiSumInts;

impl HostFn for AbiSumInts {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Array(xs)] = args else {
            return Err(HostError {
                message: format!("abi::sum_ints: bad args: {args:?}"),
            });
        };
        let len = cx.array_len(xs)?;
        let mut sum: i64 = 0;
        for i in 0..len {
            let AbiValue::Int(n) = cx.array_get(xs, i)? else {
                return Err(HostError {
                    message: format!("abi::sum_ints: expected int at index {i}"),
                });
            };
            sum += n;
        }
        Ok(AbiValue::Int(sum))
    }
}

struct AbiMakeInts;

impl HostFn for AbiMakeInts {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::make_ints: expected no args, got {args:?}"),
            });
        }
        cx.alloc_array_typed(
            AbiType::Int,
            vec![AbiValue::Int(1), AbiValue::Int(2), AbiValue::Int(3)],
        )
    }
}

struct AbiSumPoints;

impl HostFn for AbiSumPoints {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Array(ps)] = args else {
            return Err(HostError {
                message: format!("abi::sum_points: bad args: {args:?}"),
            });
        };
        let len = cx.array_len(ps)?;
        let mut sum: i64 = 0;
        for i in 0..len {
            let AbiValue::Struct(p) = cx.array_get(ps, i)? else {
                return Err(HostError {
                    message: format!("abi::sum_points: expected Point at index {i}"),
                });
            };
            let AbiValue::Int(x) = cx.struct_get(&p, "x")? else {
                return Err(HostError {
                    message: "abi::sum_points: Point.x is not an int".to_string(),
                });
            };
            let AbiValue::Int(y) = cx.struct_get(&p, "y")? else {
                return Err(HostError {
                    message: "abi::sum_points: Point.y is not an int".to_string(),
                });
            };
            sum += x + y;
        }
        Ok(AbiValue::Int(sum))
    }
}

struct AbiMakeRect;

impl HostFn for AbiMakeRect {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Int(w), AbiValue::Int(h)] = args else {
            return Err(HostError {
                message: format!("abi::make_rect: bad args: {args:?}"),
            });
        };
        cx.alloc_enum("Shape", "Rect", vec![AbiValue::Int(*w), AbiValue::Int(*h)])
    }
}

struct AbiShapeSum;

impl HostFn for AbiShapeSum {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Enum(e)] = args else {
            return Err(HostError {
                message: format!("abi::shape_sum: bad args: {args:?}"),
            });
        };
        let (variant, _arity) = cx.enum_variant(e)?;
        match variant {
            "Rect" => {
                let AbiValue::Int(w) = cx.enum_get(e, 0)? else {
                    return Err(HostError {
                        message: "abi::shape_sum: Rect field 0 is not an int".to_string(),
                    });
                };
                let AbiValue::Int(h) = cx.enum_get(e, 1)? else {
                    return Err(HostError {
                        message: "abi::shape_sum: Rect field 1 is not an int".to_string(),
                    });
                };
                Ok(AbiValue::Int(w + h))
            }
            "Unit" => Ok(AbiValue::Int(0)),
            other => Err(HostError {
                message: format!("abi::shape_sum: unknown variant `{other}`"),
            }),
        }
    }
}

fn find_type_name(
    module: &rusk_bytecode::ExecutableModule,
    candidates: &[&str],
    suffix: &str,
) -> Option<String> {
    for &candidate in candidates {
        if module.type_id(candidate).is_some() {
            return Some(candidate.to_string());
        }
    }
    let suffix = format!("::{suffix}");
    module
        .type_names
        .iter()
        .find(|name| name.as_str() == suffix.trim_start_matches("::") || name.ends_with(&suffix))
        .cloned()
}

struct AbiMakeSomeBytes {
    option_type: String,
}

impl HostFn for AbiMakeSomeBytes {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::make_some_bytes: expected no args, got {args:?}"),
            });
        }
        cx.alloc_enum_typed(
            &self.option_type,
            vec![AbiType::Bytes],
            "Some",
            vec![AbiValue::Bytes(b"hi".to_vec())],
        )
    }
}

struct AbiMakeNoneBytes {
    option_type: String,
}

impl HostFn for AbiMakeNoneBytes {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::make_none_bytes: expected no args, got {args:?}"),
            });
        }
        cx.alloc_enum_typed(&self.option_type, vec![AbiType::Bytes], "None", Vec::new())
    }
}

struct AbiOptionBytesLen;

impl HostFn for AbiOptionBytesLen {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Enum(o)] = args else {
            return Err(HostError {
                message: format!("abi::option_bytes_len: bad args: {args:?}"),
            });
        };

        let expected_args = [AbiType::Bytes];
        if o.type_args() != expected_args.as_slice() {
            return Err(HostError {
                message: format!(
                    "abi::option_bytes_len: expected Option<bytes>, got type args {:?}",
                    o.type_args()
                ),
            });
        }

        let (variant, arity) = cx.enum_variant(o)?;
        match variant {
            "None" => Ok(AbiValue::Int(0)),
            "Some" => {
                if arity != 1 {
                    return Err(HostError {
                        message: format!(
                            "abi::option_bytes_len: Some arity mismatch: expected 1, got {arity}"
                        ),
                    });
                }
                let AbiValue::Bytes(b) = cx.enum_get(o, 0)? else {
                    return Err(HostError {
                        message: "abi::option_bytes_len: Some field 0 is not bytes".to_string(),
                    });
                };
                Ok(AbiValue::Int(b.len() as i64))
            }
            other => Err(HostError {
                message: format!("abi::option_bytes_len: unknown variant `{other}`"),
            }),
        }
    }
}

struct AbiMakeOkString {
    result_type: String,
}

impl HostFn for AbiMakeOkString {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::make_ok_string: expected no args, got {args:?}"),
            });
        }
        cx.alloc_enum_typed(
            &self.result_type,
            vec![AbiType::String, AbiType::Int],
            "Ok",
            vec![AbiValue::String("ok".to_string())],
        )
    }
}

struct AbiMakeErrInt {
    result_type: String,
}

impl HostFn for AbiMakeErrInt {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::make_err_int: expected no args, got {args:?}"),
            });
        }
        cx.alloc_enum_typed(
            &self.result_type,
            vec![AbiType::String, AbiType::Int],
            "Err",
            vec![AbiValue::Int(5)],
        )
    }
}

struct AbiResultCode;

impl HostFn for AbiResultCode {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        let [AbiValue::Enum(r)] = args else {
            return Err(HostError {
                message: format!("abi::result_code: bad args: {args:?}"),
            });
        };

        let expected_args = [AbiType::String, AbiType::Int];
        if r.type_args() != expected_args.as_slice() {
            return Err(HostError {
                message: format!(
                    "abi::result_code: expected Result<string, int>, got type args {:?}",
                    r.type_args()
                ),
            });
        }

        let (variant, arity) = cx.enum_variant(r)?;
        match variant {
            "Ok" => {
                if arity != 1 {
                    return Err(HostError {
                        message: format!(
                            "abi::result_code: Ok arity mismatch: expected 1, got {arity}"
                        ),
                    });
                }
                let AbiValue::String(s) = cx.enum_get(r, 0)? else {
                    return Err(HostError {
                        message: "abi::result_code: Ok field 0 is not string".to_string(),
                    });
                };
                if s != "ok" {
                    return Err(HostError {
                        message: format!("abi::result_code: unexpected Ok value {s:?}"),
                    });
                }
                Ok(AbiValue::Int(10))
            }
            "Err" => {
                if arity != 1 {
                    return Err(HostError {
                        message: format!(
                            "abi::result_code: Err arity mismatch: expected 1, got {arity}"
                        ),
                    });
                }
                let AbiValue::Int(e) = cx.enum_get(r, 0)? else {
                    return Err(HostError {
                        message: "abi::result_code: Err field 0 is not int".to_string(),
                    });
                };
                if e != 5 {
                    return Err(HostError {
                        message: format!("abi::result_code: unexpected Err value {e}"),
                    });
                }
                Ok(AbiValue::Int(20))
            }
            other => Err(HostError {
                message: format!("abi::result_code: unknown variant `{other}`"),
            }),
        }
    }
}

struct AbiBadResult {
    result_type: String,
}

impl HostFn for AbiBadResult {
    fn call(&mut self, cx: &mut HostContext<'_>, args: &[AbiValue]) -> Result<AbiValue, HostError> {
        if !args.is_empty() {
            return Err(HostError {
                message: format!("abi::bad_result: expected no args, got {args:?}"),
            });
        }
        // Intentionally return `Result<int, string>` (swapped type args) to trigger VM validation.
        cx.alloc_enum_typed(
            &self.result_type,
            vec![AbiType::Int, AbiType::String],
            "Ok",
            vec![AbiValue::Int(1)],
        )
    }
}

#[allow(unused)]
pub fn install_complex_abi_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
    let option_type_name = if module.host_import_id("abi::make_some_bytes").is_some()
        || module.host_import_id("abi::make_none_bytes").is_some()
    {
        Some(
            find_type_name(module, &["core::option::Option", "Option"], "Option").unwrap_or_else(
                || panic!("failed to find `Option` type name in module type_names"),
            ),
        )
    } else {
        None
    };

    let result_type_name = if module.host_import_id("abi::make_ok_string").is_some()
        || module.host_import_id("abi::make_err_int").is_some()
        || module.host_import_id("abi::bad_result").is_some()
    {
        Some(
            find_type_name(module, &["core::result::Result", "Result"], "Result").unwrap_or_else(
                || panic!("failed to find `Result` type name in module type_names"),
            ),
        )
    } else {
        None
    };

    if let Some(id) = module.host_import_id("abi::sum_point") {
        vm.register_host_import(id, AbiSumPoint)
            .expect("register abi::sum_point");
    }
    if let Some(id) = module.host_import_id("abi::make_point") {
        vm.register_host_import(id, AbiMakePoint)
            .expect("register abi::make_point");
    }
    if let Some(id) = module.host_import_id("abi::sum_pair") {
        vm.register_host_import(id, AbiSumPair)
            .expect("register abi::sum_pair");
    }
    if let Some(id) = module.host_import_id("abi::make_pair") {
        vm.register_host_import(id, AbiMakePair)
            .expect("register abi::make_pair");
    }
    if let Some(id) = module.host_import_id("abi::sum_ints") {
        vm.register_host_import(id, AbiSumInts)
            .expect("register abi::sum_ints");
    }
    if let Some(id) = module.host_import_id("abi::make_ints") {
        vm.register_host_import(id, AbiMakeInts)
            .expect("register abi::make_ints");
    }
    if let Some(id) = module.host_import_id("abi::sum_points") {
        vm.register_host_import(id, AbiSumPoints)
            .expect("register abi::sum_points");
    }
    if let Some(id) = module.host_import_id("abi::make_rect") {
        vm.register_host_import(id, AbiMakeRect)
            .expect("register abi::make_rect");
    }
    if let Some(id) = module.host_import_id("abi::shape_sum") {
        vm.register_host_import(id, AbiShapeSum)
            .expect("register abi::shape_sum");
    }

    if let Some(id) = module.host_import_id("abi::make_some_bytes") {
        let option_type = option_type_name
            .clone()
            .expect("Option type name for abi::make_some_bytes");
        vm.register_host_import(id, AbiMakeSomeBytes { option_type })
            .expect("register abi::make_some_bytes");
    }
    if let Some(id) = module.host_import_id("abi::make_none_bytes") {
        let option_type = option_type_name
            .clone()
            .expect("Option type name for abi::make_none_bytes");
        vm.register_host_import(id, AbiMakeNoneBytes { option_type })
            .expect("register abi::make_none_bytes");
    }
    if let Some(id) = module.host_import_id("abi::option_bytes_len") {
        vm.register_host_import(id, AbiOptionBytesLen)
            .expect("register abi::option_bytes_len");
    }

    if let Some(id) = module.host_import_id("abi::make_ok_string") {
        let result_type = result_type_name
            .clone()
            .expect("Result type name for abi::make_ok_string");
        vm.register_host_import(id, AbiMakeOkString { result_type })
            .expect("register abi::make_ok_string");
    }
    if let Some(id) = module.host_import_id("abi::make_err_int") {
        let result_type = result_type_name
            .clone()
            .expect("Result type name for abi::make_err_int");
        vm.register_host_import(id, AbiMakeErrInt { result_type })
            .expect("register abi::make_err_int");
    }
    if let Some(id) = module.host_import_id("abi::result_code") {
        vm.register_host_import(id, AbiResultCode)
            .expect("register abi::result_code");
    }
    if let Some(id) = module.host_import_id("abi::bad_result") {
        let result_type = result_type_name
            .clone()
            .expect("Result type name for abi::bad_result");
        vm.register_host_import(id, AbiBadResult { result_type })
            .expect("register abi::bad_result");
    }

    if let Some(id) = module.host_import_id("abi::xor_byte") {
        vm.register_host_import_typed(id, |(a, b): (u8, u8)| -> Result<u8, HostError> {
            Ok(a ^ b)
        })
        .expect("register abi::xor_byte");
    }

    if let Some(id) = module.host_import_id("abi::next_char") {
        vm.register_host_import_typed(id, |(c,): (char,)| -> Result<char, HostError> {
            let next = (c as u32)
                .checked_add(1)
                .and_then(char::from_u32)
                .ok_or_else(|| HostError {
                    message: "abi::next_char: overflow".to_string(),
                })?;
            Ok(next)
        })
        .expect("register abi::next_char");
    }
}
