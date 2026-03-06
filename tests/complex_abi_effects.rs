use rusk_compiler::{CompileOptions, HostFnSig, HostType, compile_to_bytecode_with_options};
use rusk_vm::{AbiValue, HostError, StepResult, Vm, vm_resume, vm_step};

#[test]
fn external_effect_can_roundtrip_struct_values() {
    let src = r#"
struct Point { x: int, y: int }

interface HostFx { fn swap(p: Point) -> Point; }

fn main() -> int {
    let p = Point { x: 1, y: 2 };
    let q = @HostFx.swap(p);
    q.x * 10 + q.y
}
"#;

    let mut options = CompileOptions {
        load_std: false,
        ..Default::default()
    };
    options
        .register_external_effect(
            "HostFx",
            "swap",
            HostFnSig {
                params: vec![HostType::Struct {
                    name: "Point".to_string(),
                    args: Vec::new(),
                }],
                ret: HostType::Struct {
                    name: "Point".to_string(),
                    args: Vec::new(),
                },
            },
        )
        .expect("register HostFx.swap");

    let module = compile_to_bytecode_with_options(src, &options).expect("compile");
    let point_id = module.type_id("Point").expect("Point type id");
    let swap_id = module
        .external_effect_id("HostFx", "swap")
        .expect("HostFx.swap effect id");

    let mut vm = Vm::new(module).expect("vm init");
    let got = vm_step(&mut vm, None);
    let StepResult::Request {
        effect_id, args, k, ..
    } = got
    else {
        panic!("expected Request, got {got:?}");
    };
    assert_eq!(effect_id, swap_id);
    assert_eq!(args.len(), 1);
    assert_eq!(
        args[0].ty(),
        rusk_bytecode::AbiType::Struct {
            type_id: point_id,
            args: Vec::new(),
        }
    );

    let resume_value = vm
        .with_host_context(|cx| -> Result<AbiValue, HostError> {
            let [AbiValue::Struct(p)] = args.as_slice() else {
                return Err(HostError {
                    message: format!("HostFx.swap: bad args: {args:?}"),
                });
            };
            let AbiValue::Int(x) = cx.struct_get(p, "x")? else {
                return Err(HostError {
                    message: "HostFx.swap: Point.x is not an int".to_string(),
                });
            };
            let AbiValue::Int(y) = cx.struct_get(p, "y")? else {
                return Err(HostError {
                    message: "HostFx.swap: Point.y is not an int".to_string(),
                });
            };
            cx.alloc_struct(
                "Point",
                vec![("x", AbiValue::Int(y)), ("y", AbiValue::Int(x))],
            )
        })
        .expect("dispatch");

    vm_resume(&mut vm, k, resume_value).expect("resume");

    loop {
        match vm_step(&mut vm, None) {
            StepResult::Done { value } => {
                assert_eq!(value, AbiValue::Int(21));
                break;
            }
            StepResult::Trap { message } => panic!("trap: {message}"),
            StepResult::Yield { .. } => continue,
            StepResult::Request { .. } => panic!("unexpected external effect request"),
        }
    }
}
