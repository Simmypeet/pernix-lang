//! Tests for the binary serializer implementation.

use super::*;
use crate::ser::Serialize;

/// Helper function to create a serializer and run a test with it
fn with_serializer<F>(mut f: F) -> Vec<u8>
where
    F: FnMut(&mut BinarySerializer<Vec<u8>>) -> Result<(), std::io::Error>,
{
    let mut serializer = BinarySerializer::new(Vec::new());
    f(&mut serializer).unwrap();
    serializer.into_inner()
}

#[test]
fn primitives() {
    // Test integers
    let buf = with_serializer(|s| 42u8.serialize(s, &mut ()));
    assert_eq!(buf, [42]); // u8 stored as-is

    // Test u16 - should be varint encoded
    let buf = with_serializer(|s| 1000u16.serialize(s, &mut ()));
    // 1000 = 0x3E8 = 0b1111101000
    // varint: 0xE8 (low 7 bits + continuation) | 0x07 (high bits, no
    // continuation)
    assert_eq!(buf, [0xE8, 0x07]);

    // Test i32 - should be zigzag + varint encoded
    let buf = with_serializer(|s| (-42i32).serialize(s, &mut ()));
    // -42 zigzag encoded: ((−42 << 1) ^ (−42 >> 31)) = (−84 ^ −1) = 83
    // 83 = 0x53 = 0b1010011 -> single byte
    assert_eq!(buf, [83]);

    // Test boolean
    let buf = with_serializer(|s| true.serialize(s, &mut ()));
    assert_eq!(buf, [1]);

    let buf = with_serializer(|s| false.serialize(s, &mut ()));
    assert_eq!(buf, [0]);

    // Test string
    let buf = with_serializer(|s| "hello".serialize(s, &mut ()));
    assert_eq!(buf[0], 5); // length
    assert_eq!(&buf[1..], b"hello");
}

#[test]
fn varint() {
    // Test various varint values
    let buf = with_serializer(|s| s.write_varint(0));
    assert_eq!(buf, [0]);

    let buf = with_serializer(|s| s.write_varint(127));
    assert_eq!(buf, [127]);

    let buf = with_serializer(|s| s.write_varint(128));
    assert_eq!(buf, [0x80, 0x01]);

    let buf = with_serializer(|s| s.write_varint(16383));
    assert_eq!(buf, [0xFF, 0x7F]);

    let buf = with_serializer(|s| s.write_varint(16384));
    assert_eq!(buf, [0x80, 0x80, 0x01]);
}

#[test]
fn sequences() {
    // Test Vec<u8>
    let buf = with_serializer(|s| vec![1u8, 2, 3, 4, 5].serialize(s, &mut ()));
    // Should start with length varint (5), then the bytes
    assert_eq!(buf[0], 5); // length
    assert_eq!(&buf[1..], &[1, 2, 3, 4, 5]);

    // Test Vec<u32> - each u32 should be varint encoded
    let buf = with_serializer(|s| vec![100u32, 200, 300].serialize(s, &mut ()));

    assert_eq!(buf[0], 3); // length
                           // 100 = 0x64 (single byte, since 100 < 128)
                           // 200 = 0xC8 -> varint: 0xC8 (200 & 0x7F | 0x80), 0x01 (200 >> 7)
                           // 300 = 0x12C -> varint: 0xAC ((300 & 0x7F) | 0x80), 0x02 (300 >> 7)
    assert_eq!(&buf[1..], &[100, 200, 1, 172, 2]);

    // Test empty Vec
    let buf = with_serializer(|s| Vec::<u8>::new().serialize(s, &mut ()));
    assert_eq!(buf, [0]); // just the length

    // Test Vec<String>
    let buf = with_serializer(|s| {
        vec!["hello".to_string(), "world".to_string()].serialize(s, &mut ())
    });
    assert_eq!(buf[0], 2); // length of vec
                           // First string: length (5) + "hello"
                           // Second string: length (5) + "world"
    assert_eq!(buf[1], 5); // length of "hello"
    assert_eq!(&buf[2..7], b"hello");
    assert_eq!(buf[7], 5); // length of "world"
    assert_eq!(&buf[8..13], b"world");
}

#[test]
fn maps() {
    use std::collections::HashMap;

    // Test HashMap<String, u32>
    let mut map = HashMap::new();
    map.insert("key1".to_string(), 100u32);
    map.insert("key2".to_string(), 200u32);

    let buf = with_serializer(|s| map.serialize(s, &mut ()));
    assert_eq!(buf[0], 2); // length of map

    // Note: HashMap iteration order is not guaranteed, so we need to check
    // that both key-value pairs are present without assuming order
    let buf_str = String::from_utf8_lossy(&buf);
    assert!(buf_str.contains("key1"));
    assert!(buf_str.contains("key2"));
    // The exact layout depends on HashMap iteration order

    // Test empty HashMap
    let empty_map: HashMap<String, u32> = HashMap::new();
    let buf = with_serializer(|s| empty_map.serialize(s, &mut ()));
    assert_eq!(buf, [0]); // just the length
}

#[test]
fn structs() {
    // Test tuple struct
    let tuple_struct = (42u32, "hello".to_string(), true);
    let buf = with_serializer(|s| tuple_struct.serialize(s, &mut ()));

    // Should serialize as: u32 varint (42 = single byte) + string length +
    // string bytes + bool (1 byte)
    assert_eq!(buf[0], 42); // u32 as varint
    assert_eq!(buf[1], 5); // string length
    assert_eq!(&buf[2..7], b"hello");
    assert_eq!(buf[7], 1); // true as byte

    // Test nested tuple with different sized integers
    let nested = ((1u8, 2u8), (300u16, 4u16)); // 300 needs varint encoding
    let buf = with_serializer(|s| nested.serialize(s, &mut ()));

    // Should serialize as: u8 + u8 + u16 varint + u16
    assert_eq!(buf[0], 1); // u8 as-is
    assert_eq!(buf[1], 2); // u8 as-is
                           // 300 = 0x12C -> varint: 0xAC (low 7 bits + continuation) | 0x02 (high
                           // bits)
    assert_eq!(buf[2], 0xAC); // 300 as varint (first byte)
    assert_eq!(buf[3], 0x02); // 300 as varint (second byte)
    assert_eq!(buf[4], 4); // 4 as varint (single byte)
}

#[test]
fn options() {
    // Test Some variant
    let some_value = Some(42u32);
    let buf = with_serializer(|s| some_value.serialize(s, &mut ()));
    // Should serialize as: variant index (1 for Some) + value as varint
    assert_eq!(buf[0], 1); // Some variant index
    assert_eq!(buf[1], 42); // 42 as varint (single byte)

    // Test None variant
    let none_value: Option<u32> = None;
    let buf = with_serializer(|s| none_value.serialize(s, &mut ()));
    // Should serialize as: variant index (0 for None)
    assert_eq!(buf, [0]); // None variant index only

    // Test nested Option
    let nested_some = Some(Some(123u16));
    let buf = with_serializer(|s| nested_some.serialize(s, &mut ()));
    assert_eq!(buf[0], 1); // outer Some
    assert_eq!(buf[1], 1); // inner Some
    assert_eq!(buf[2], 123); // 123 as varint (single byte)

    // Test Option<String>
    let some_string = Some("test".to_string());
    let buf = with_serializer(|s| some_string.serialize(s, &mut ()));
    assert_eq!(buf[0], 1); // Some variant
    assert_eq!(buf[1], 4); // string length
    assert_eq!(&buf[2..6], b"test");
}

#[test]
fn results() {
    // Test Ok variant
    let ok_value: Result<u32, String> = Ok(42);
    let buf = with_serializer(|s| ok_value.serialize(s, &mut ()));
    assert_eq!(buf[0], 0); // Ok variant index
    assert_eq!(buf[1], 42); // 42 as varint (single byte)

    // Test Err variant
    let err_value: Result<u32, String> = Err("error".to_string());
    let buf = with_serializer(|s| err_value.serialize(s, &mut ()));
    assert_eq!(buf[0], 1); // Err variant index
    assert_eq!(buf[1], 5); // string length
    assert_eq!(&buf[2..7], b"error");
}

#[test]
fn complex_nested() {
    // Test complex nested structure
    let complex = vec![
        Some(("key1".to_string(), vec![1u8, 2, 3])),
        None,
        Some(("key2".to_string(), vec![4u8, 5])),
    ];

    let buf = with_serializer(|s| complex.serialize(s, &mut ()));

    // Should start with vec length
    assert_eq!(buf[0], 3); // 3 elements in vec

    // First element: Some variant
    assert_eq!(buf[1], 1); // Some variant index
                           // Then tuple: string + vec
    assert_eq!(buf[2], 4); // "key1" length
    assert_eq!(&buf[3..7], b"key1");
    assert_eq!(buf[7], 3); // vec length
    assert_eq!(&buf[8..11], &[1, 2, 3]);

    // Second element: None variant
    assert_eq!(buf[11], 0); // None variant index

    // Third element: Some variant
    assert_eq!(buf[12], 1); // Some variant index
    assert_eq!(buf[13], 4); // "key2" length
    assert_eq!(&buf[14..18], b"key2");
    assert_eq!(buf[18], 2); // vec length
    assert_eq!(&buf[19..21], &[4, 5]);
}

#[test]
fn floating_point() {
    // Test f32
    let f32_val = std::f32::consts::PI;
    let buf = with_serializer(|s| f32_val.serialize(s, &mut ()));
    assert_eq!(buf, f32_val.to_le_bytes());

    // Test f64
    let f64_val = std::f64::consts::E;
    let buf = with_serializer(|s| f64_val.serialize(s, &mut ()));
    assert_eq!(buf, f64_val.to_le_bytes());

    // Test special values
    let nan = f32::NAN;
    let buf = with_serializer(|s| nan.serialize(s, &mut ()));
    assert_eq!(buf, nan.to_le_bytes());

    let infinity = f32::INFINITY;
    let buf = with_serializer(|s| infinity.serialize(s, &mut ()));
    assert_eq!(buf, infinity.to_le_bytes());

    let neg_infinity = f32::NEG_INFINITY;
    let buf = with_serializer(|s| neg_infinity.serialize(s, &mut ()));
    assert_eq!(buf, neg_infinity.to_le_bytes());
}

#[test]
fn large_varints() {
    // Test larger varint encodings to ensure they work correctly
    let buf = with_serializer(|s| s.write_varint(0x80)); // 128
    assert_eq!(buf, [0x80, 0x01]);

    let buf = with_serializer(|s| s.write_varint(0x4000)); // 16384
    assert_eq!(buf, [0x80, 0x80, 0x01]);

    let buf = with_serializer(|s| s.write_varint(0x200000)); // 2097152
    assert_eq!(buf, [0x80, 0x80, 0x80, 0x01]);

    let buf = with_serializer(|s| s.write_varint(0x10000000)); // 268435456
    assert_eq!(buf, [0x80, 0x80, 0x80, 0x80, 0x01]);

    // Test maximum value
    let buf = with_serializer(|s| s.write_varint(u64::MAX));
    // u64::MAX should encode as 10 bytes (9 bytes with high bit set + 1 final
    // byte)
    assert_eq!(buf.len(), 10);
    assert_eq!(buf[9], 0x01); // final byte
}

#[test]
fn integers_128_bit() {
    // Test u128 serialization
    let buf = with_serializer(|s| 42u128.serialize(s, &mut ()));
    assert_eq!(buf, [42]); // Small values fit in single byte

    let buf = with_serializer(|s| 128u128.serialize(s, &mut ()));
    assert_eq!(buf, [0x80, 0x01]); // 128 requires two bytes

    // Test large u128 value
    let buf = with_serializer(|s| u128::from(u64::MAX).serialize(s, &mut ()));
    // Should be same as u64::MAX encoded as varint
    assert_eq!(buf.len(), 10);

    // Test i128 with positive value
    let buf = with_serializer(|s| 42i128.serialize(s, &mut ()));
    // 42 zigzag encoded: ((42 << 1) ^ (42 >> 127)) = (84 ^ 0) = 84
    assert_eq!(buf, [84]);

    // Test i128 with negative value
    let buf = with_serializer(|s| (-42i128).serialize(s, &mut ()));
    // -42 zigzag encoded: ((-42 << 1) ^ (-42 >> 127)) = (-84 ^ -1) = 83
    assert_eq!(buf, [83]);

    // Test edge cases
    let buf = with_serializer(|s| 0i128.serialize(s, &mut ()));
    assert_eq!(buf, [0]); // 0 zigzag encoded is 0

    let buf = with_serializer(|s| (-1i128).serialize(s, &mut ()));
    assert_eq!(buf, [1]); // -1 zigzag encoded is 1

    // Test large positive i128
    let buf = with_serializer(|s| i128::MAX.serialize(s, &mut ()));
    // i128::MAX zigzag encoded should be large but finite
    assert!(!buf.is_empty());

    // Test large negative i128
    let buf = with_serializer(|s| i128::MIN.serialize(s, &mut ()));
    // i128::MIN zigzag encoded should be large but finite
    assert!(!buf.is_empty());
}

#[test]
fn varint_128_bit() {
    // Test 128-bit varint directly
    let buf = with_serializer(|s| s.write_varint_u128(0));
    assert_eq!(buf, [0]);

    let buf = with_serializer(|s| s.write_varint_u128(127));
    assert_eq!(buf, [127]);

    let buf = with_serializer(|s| s.write_varint_u128(128));
    assert_eq!(buf, [0x80, 0x01]);

    // Test large 128-bit value that exceeds u64
    let large_value = u128::from(u64::MAX) + 1;
    let buf = with_serializer(|s| s.write_varint_u128(large_value));
    // u64::MAX + 1 = 0x10000000000000000, which is 1 followed by 64 zeros
    // This actually encodes as 10 bytes (same as u64::MAX in terms of varint length)
    assert_eq!(buf.len(), 10);

    // Test maximum u128 value
    let buf = with_serializer(|s| s.write_varint_u128(u128::MAX));
    // u128::MAX should encode as 19 bytes maximum (18 bytes with high bit set + 1 final byte)
    assert!(buf.len() <= 19);
    assert!(!buf.is_empty());
}
