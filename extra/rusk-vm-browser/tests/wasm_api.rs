use js_sys::{Array, BigInt, Function, Reflect, Uint8Array};
use wasm_bindgen::JsCast;
use wasm_bindgen::JsValue;
use wasm_bindgen::closure::Closure;
use wasm_bindgen_test::*;

use rusk_vm_browser::Vm;

fn step_tag(step: &JsValue) -> String {
    Reflect::get(step, &JsValue::from_str("tag"))
        .expect("tag")
        .as_string()
        .unwrap_or_default()
}

fn done_value(step: &JsValue) -> JsValue {
    Reflect::get(step, &JsValue::from_str("value")).expect("value")
}

fn trap_message(step: &JsValue) -> String {
    Reflect::get(step, &JsValue::from_str("message"))
        .expect("message")
        .as_string()
        .unwrap_or_default()
}

fn request_k(step: &JsValue) -> (u32, u32) {
    let k = Reflect::get(step, &JsValue::from_str("k")).expect("k");
    let index = Reflect::get(&k, &JsValue::from_str("index"))
        .expect("k.index")
        .as_f64()
        .unwrap_or(0.0) as u32;
    let generation = Reflect::get(&k, &JsValue::from_str("generation"))
        .expect("k.generation")
        .as_f64()
        .unwrap_or(0.0) as u32;
    (index, generation)
}

fn request_args(step: &JsValue) -> Array {
    let args = Reflect::get(step, &JsValue::from_str("args")).expect("args");
    args.dyn_into::<Array>().expect("args is Array")
}

fn request_effect_id(step: &JsValue) -> u32 {
    Reflect::get(step, &JsValue::from_str("effectId"))
        .expect("effectId")
        .as_f64()
        .unwrap_or(0.0) as u32
}

fn find_external_effect_id(vm: &Vm, interface: &str, method: &str) -> u32 {
    let effects = vm.list_external_effects();
    for eff in effects.iter() {
        let iface = Reflect::get(&eff, &JsValue::from_str("interface"))
            .unwrap()
            .as_string()
            .unwrap();
        let m = Reflect::get(&eff, &JsValue::from_str("method"))
            .unwrap()
            .as_string()
            .unwrap();
        if iface == interface && m == method {
            return Reflect::get(&eff, &JsValue::from_str("id"))
                .unwrap()
                .as_f64()
                .unwrap() as u32;
        }
    }
    panic!("missing external effect {interface}.{method}");
}

#[wasm_bindgen_test]
fn host_import_add_int_smoke() {
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/call_host_add_int.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let host_imports = vm.list_host_imports();
    assert!(host_imports.length() > 0);

    // Find the id for `test::add_int`.
    let mut add_id = None;
    for imp in host_imports.iter() {
        let name = Reflect::get(&imp, &JsValue::from_str("name"))
            .unwrap()
            .as_string()
            .unwrap();
        if name == "test::add_int" {
            add_id = Some(
                Reflect::get(&imp, &JsValue::from_str("id"))
                    .unwrap()
                    .as_f64()
                    .unwrap() as u32,
            );
        }
    }
    let add_id = add_id.expect("found test::add_int import");

    let add_impl = Closure::wrap(Box::new(move |args: JsValue| -> JsValue {
        let args: Array = args.dyn_into().expect("args array");
        let a: BigInt = args.get(0).dyn_into().expect("a bigint");
        let b: BigInt = args.get(1).dyn_into().expect("b bigint");
        let a_i = i64::try_from(a).expect("a i64");
        let b_i = i64::try_from(b).expect("b i64");
        JsValue::from(BigInt::from(a_i + b_i))
    }) as Box<dyn FnMut(JsValue) -> JsValue>);

    let f: Function = add_impl.as_ref().unchecked_ref::<Function>().clone();
    vm.register_host_import(add_id, f)
        .expect("register host import");
    add_impl.forget();

    let r = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r), "done");
    let v = done_value(&r);
    let v: BigInt = v.dyn_into().expect("done value bigint");
    assert_eq!(i64::try_from(v).unwrap(), 3);
}

#[wasm_bindgen_test]
fn external_effect_request_resume_add_int() {
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/effect_add_int.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let want_id = find_external_effect_id(&vm, "TestFfi", "add");

    let r1 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r1), "request");
    assert_eq!(request_effect_id(&r1), want_id);

    let args = request_args(&r1);
    let a: BigInt = args.get(0).dyn_into().expect("a bigint");
    let b: BigInt = args.get(1).dyn_into().expect("b bigint");
    assert_eq!(i64::try_from(a).unwrap(), 1);
    assert_eq!(i64::try_from(b).unwrap(), 2);

    let (k_index, k_generation) = request_k(&r1);
    vm.resume(k_index, k_generation, JsValue::from(BigInt::from(3)))
        .expect("resume");

    let r2 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r2), "done");
    let v: BigInt = done_value(&r2).dyn_into().expect("done bigint");
    assert_eq!(i64::try_from(v).unwrap(), 3);
}

#[wasm_bindgen_test]
fn external_effect_cancel_traps() {
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/effect_add_int.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let r1 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r1), "request");
    let (k_index, k_generation) = request_k(&r1);

    vm.drop_continuation(k_index, k_generation)
        .expect("drop continuation");

    let r2 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r2), "trap");
    assert_eq!(trap_message(&r2), "cancelled");
}

#[wasm_bindgen_test]
fn bytes_external_effect_and_bytes_eq_host_import() {
    let rbc: &[u8] = include_bytes!(concat!(
        env!("OUT_DIR"),
        "/effect_echo_bytes_and_bytes_eq.rbc"
    ));
    let mut vm = Vm::new(rbc).expect("vm init");

    let want_id = find_external_effect_id(&vm, "TestFfi", "echo_bytes");

    // Register `test::bytes_eq` for the fixture.
    let host_imports = vm.list_host_imports();
    let mut bytes_eq_id = None;
    for imp in host_imports.iter() {
        let name = Reflect::get(&imp, &JsValue::from_str("name"))
            .unwrap()
            .as_string()
            .unwrap();
        if name == "test::bytes_eq" {
            bytes_eq_id = Some(
                Reflect::get(&imp, &JsValue::from_str("id"))
                    .unwrap()
                    .as_f64()
                    .unwrap() as u32,
            );
        }
    }
    let bytes_eq_id = bytes_eq_id.expect("found test::bytes_eq import");

    let bytes_eq_impl = Closure::wrap(Box::new(move |args: JsValue| -> JsValue {
        let args: Array = args.dyn_into().expect("args array");
        let a: Uint8Array = args.get(0).dyn_into().expect("a bytes");
        let b: Uint8Array = args.get(1).dyn_into().expect("b bytes");
        let ok = a.to_vec() == b.to_vec();
        JsValue::from_bool(ok)
    }) as Box<dyn FnMut(JsValue) -> JsValue>);

    let f: Function = bytes_eq_impl.as_ref().unchecked_ref::<Function>().clone();
    vm.register_host_import(bytes_eq_id, f)
        .expect("register bytes_eq");
    bytes_eq_impl.forget();

    let r1 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r1), "request");
    assert_eq!(request_effect_id(&r1), want_id);

    let args = request_args(&r1);
    let bytes: Uint8Array = args.get(0).dyn_into().expect("bytes arg");
    assert_eq!(bytes.to_vec(), b"hi");

    let (k_index, k_generation) = request_k(&r1);
    vm.resume(
        k_index,
        k_generation,
        Uint8Array::from(b"hi".as_slice()).into(),
    )
    .expect("resume");

    let r2 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r2), "done");
    assert!(done_value(&r2).as_bool().unwrap_or(false));
}

#[wasm_bindgen_test]
fn external_effect_request_resume_echo_string() {
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/effect_echo_string.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let want_id = find_external_effect_id(&vm, "TestFfi", "echo");

    let r1 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r1), "request");
    assert_eq!(request_effect_id(&r1), want_id);

    let args = request_args(&r1);
    let s = args.get(0).as_string().expect("string arg");
    assert_eq!(s, "hi");

    let (k_index, k_generation) = request_k(&r1);
    vm.resume(k_index, k_generation, JsValue::from_str("hi"))
        .expect("resume");

    let r2 = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r2), "done");
    assert_eq!(done_value(&r2).as_string().unwrap(), "hi");
}

#[wasm_bindgen_test]
fn host_import_return_type_mismatch_traps() {
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/call_host_add_int.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let host_imports = vm.list_host_imports();
    let mut add_id = None;
    for imp in host_imports.iter() {
        let name = Reflect::get(&imp, &JsValue::from_str("name"))
            .unwrap()
            .as_string()
            .unwrap();
        if name == "test::add_int" {
            add_id = Some(
                Reflect::get(&imp, &JsValue::from_str("id"))
                    .unwrap()
                    .as_f64()
                    .unwrap() as u32,
            );
        }
    }
    let add_id = add_id.expect("found test::add_int import");

    let bad_impl =
        Closure::wrap(
            Box::new(move |_args: JsValue| -> JsValue { JsValue::from_str("not an int") })
                as Box<dyn FnMut(JsValue) -> JsValue>,
        );

    let f: Function = bad_impl.as_ref().unchecked_ref::<Function>().clone();
    vm.register_host_import(add_id, f)
        .expect("register host import");
    bad_impl.forget();

    let r = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r), "trap");
    assert!(
        trap_message(&r).contains("return type mismatch"),
        "trap message should mention return type mismatch: {}",
        trap_message(&r)
    );
}

#[wasm_bindgen_test]
fn panic_traps_and_never_typerep_is_decodable() {
    // This fixture uses a `TypeRep` literal for `!` (never) and then panics at runtime.
    //
    // The primary goal is to ensure the browser VM stays in sync with the bytecode `TypeRepLit`
    // tags and the `panic(msg: string) -> !` intrinsic signature.
    let rbc: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/panic_never_typerep.rbc"));
    let mut vm = Vm::new(rbc).expect("vm init");

    let r = vm.step(None).expect("step ok");
    assert_eq!(step_tag(&r), "trap");
    assert_eq!(trap_message(&r), "panic: boom");
}
