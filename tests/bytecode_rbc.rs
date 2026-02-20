use rusk_bytecode::{from_bytes, to_bytes};
use rusk_compiler::compile_to_bytecode;

fn sample_module_bytes() -> Vec<u8> {
    // Include effects + pattern matching to exercise a larger subset of the bytecode format.
    // Also include int/byte bitwise + shift ops to exercise newer instruction tags.
    let src = r#"
struct Pair { x: int, y: int }
interface Tick { fn tick(n: int) -> int; }

fn sum_pair(Pair { x, y }: Pair) -> int { x + y }

fn maybe_add_one(x: int) -> Option<int> {
    if x > 0 { Option::Some(x + 1) } else { Option::None }
}

fn main() -> int {
    let p = Pair { x: 1, y: 2 };
    let base = sum_pair(p) + [1, 2, 3][0];

    // Bitwise ops on runtime values (should not be constant-folded away).
    let int_bits = (!base) ^ (base & 7) | (base << 1) | (base >> 2) | (base >>> 3);

    let byte_a = core::intrinsics::int_to_byte(base);
    let byte_b = core::intrinsics::int_to_byte(15);
    let byte_bits =
        core::intrinsics::byte_to_int((byte_a & byte_b) ^ (!byte_b) ^ (byte_a << 1) ^ (byte_a >>> 1));

    let maybe = match maybe_add_one(base) {
        Option::Some(v) => v,
        Option::None => base,
    };
    match @Tick.tick(maybe) {
        @Tick.tick(n) => resume(n * 10 + int_bits + byte_bits)
        v => v
    }
}
"#;

    let module = compile_to_bytecode(src).expect("compile");
    to_bytes(&module).expect("encode")
}

#[test]
fn rbc_roundtrip_is_byte_identical() {
    let bytes1 = sample_module_bytes();
    let module2 = from_bytes(&bytes1).expect("decode");
    let bytes2 = to_bytes(&module2).expect("re-encode");
    assert_eq!(bytes1, bytes2);
}

#[test]
fn rbc_truncated_inputs_fail_gracefully() {
    let bytes = sample_module_bytes();
    for n in 0..bytes.len() {
        let prefix = &bytes[..n];
        assert!(
            from_bytes(prefix).is_err(),
            "prefix length {n} unexpectedly decoded successfully"
        );
    }
}

#[test]
fn rbc_version_mismatch_is_error() {
    let mut bytes = sample_module_bytes();
    // Header layout:
    //   [0..8]  magic
    //   [8..10] major (u16 LE)
    //   [10..12] minor (u16 LE)
    bytes[8] = 0x01;
    bytes[9] = 0x00;

    let err = from_bytes(&bytes).expect_err("expected version mismatch error");
    assert!(err.message.contains("unsupported rbc version"), "{err}");
}

#[test]
fn rbc_rejects_trailing_bytes() {
    let mut bytes = sample_module_bytes();
    bytes.extend_from_slice(&[0, 1, 2, 3]);

    let err = from_bytes(&bytes).expect_err("expected trailing bytes error");
    assert!(err.message.contains("trailing bytes"), "{err}");
}
