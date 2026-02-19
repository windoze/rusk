use rusk_compiler::compile_to_bytecode;
use rusk_vm::{AbiValue, StepResult, Vm, vm_step};

#[test]
fn for_loop_iterates_using_iterator_protocol() {
    let src = r#"
        fn sum() -> int {
            let total = 0;
            let xs = [1, 2, 3];
            for x in xs {
                total = total + x;
            };
            total
        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(6)
        }
    );
}

#[test]
fn for_loop_over_readonly_array_is_allowed() {
    let src = r#"
        fn sum() -> int {
            let total = 0;
            readonly xs = [1, 2, 3];
            for x in xs {
                total = total + x;
            };
            total
        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(6)
        }
    );
}

#[test]
fn for_loop_continue_advances_for_builtin_arrays() {
    let src = r#"
        fn sum_skip_three() -> int {
            let total = 0;
            let xs = [1, 2, 3, 4, 5];
            for x in xs {
                if x == 3 { continue; };
                total = total + x;
            };
            total
        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum_skip_three").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(1 + 2 + 4 + 5)
        }
    );
}

#[test]
fn for_loop_over_string_iterates_unicode_scalars() {
    let src = r#"
		        fn sum() -> int {
		            let total = 0;
	            for c in "é" {
	                total = total + c.to_int();
	            };
	            total
	        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(233)
        }
    );
}

#[test]
fn for_loop_over_bytes_iterates_byte_values() {
    let src = r#"
	        fn sum() -> int {
	            let total = 0;
	            readonly bs = b"ABC";
	            for b in bs {
	                total = total + b.to_int();
	            };
	            total
	        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(198)
        }
    );
}

#[test]
fn for_loop_over_custom_iterator_type_is_supported() {
    let src = r#"
        use core::iter::Iterator;

        struct Range { cur: int, end: int }

        impl Iterator for Range {
          type Item = int;
          fn next() -> Option<int> {
            if self.cur < self.end {
              let out = self.cur;
              self.cur = self.cur + 1;
              Option::Some(out)
            } else {
              Option::None
            }
          }
        }

        fn sum() -> int {
            let total = 0;
            let r = Range { cur: 0, end: 5 };
            for x in r {
                total = total + x;
            };
            total
        }

        fn main() -> unit { () }
    "#;

    let mut module = compile_to_bytecode(src).expect("compile");
    module.entry = module.function_id("sum").expect("sum fn id");

    let mut vm = Vm::new(module).expect("vm init");
    let out = vm_step(&mut vm, None);
    assert_eq!(
        out,
        StepResult::Done {
            value: AbiValue::Int(10)
        }
    );
}

#[test]
fn for_loop_over_readonly_iterator_value_is_compile_error() {
    let src = r#"
        use core::iter::Iterator;

        struct Range { cur: int, end: int }

        impl Iterator for Range {
          type Item = int;
          fn next() -> Option<int> {
            if self.cur < self.end {
              let out = self.cur;
              self.cur = self.cur + 1;
              Option::Some(out)
            } else {
              Option::None
            }
          }
        }

        fn sum() -> int {
            let total = 0;
            readonly r = Range { cur: 0, end: 3 };
            for x in r {
                total = total + x;
            };
            total
        }

        fn main() -> unit { () }
    "#;

    let err = compile_to_bytecode(src).expect_err("compile error");
    assert!(
        err.message
            .contains("cannot iterate over a readonly iterator (`Iterator::next` is mutable)"),
        "{}",
        err.message
    );
}
