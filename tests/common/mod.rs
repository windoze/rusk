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

#[allow(unused)]
pub fn install_complex_abi_host_fns_vm(module: &rusk_bytecode::ExecutableModule, vm: &mut Vm) {
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
