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
    assert_eq!(buf, [42]);

    let buf = with_serializer(|s| 1000u16.serialize(s, &mut ()));
    assert_eq!(buf, 1000u16.to_le_bytes());

    let buf = with_serializer(|s| (-42i32).serialize(s, &mut ()));
    assert_eq!(buf, (-42i32).to_le_bytes());

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

    // Test Vec<u32>
    let buf = with_serializer(|s| vec![100u32, 200, 300].serialize(s, &mut ()));
    assert_eq!(buf[0], 3); // length
                           // Each u32 is 4 bytes in little-endian
    let expected: Vec<u8> =
        vec![100u32.to_le_bytes(), 200u32.to_le_bytes(), 300u32.to_le_bytes()]
            .into_iter()
            .flatten()
            .collect();
    assert_eq!(&buf[1..], &expected);

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

    // Should serialize as: u32 (4 bytes) + string length + string bytes + bool
    // (1 byte)
    assert_eq!(&buf[0..4], &42u32.to_le_bytes());
    assert_eq!(buf[4], 5); // string length
    assert_eq!(&buf[5..10], b"hello");
    assert_eq!(buf[10], 1); // true as byte

    // Test nested tuple
    let nested = ((1u8, 2u8), (3u16, 4u16));
    let buf = with_serializer(|s| nested.serialize(s, &mut ()));

    // Should serialize as: u8 + u8 + u16 + u16
    assert_eq!(buf[0], 1);
    assert_eq!(buf[1], 2);
    assert_eq!(&buf[2..4], &3u16.to_le_bytes());
    assert_eq!(&buf[4..6], &4u16.to_le_bytes());
}

#[test]
fn options() {
    // Test Some variant
    let some_value = Some(42u32);
    let buf = with_serializer(|s| some_value.serialize(s, &mut ()));
    // Should serialize as: variant index (1 for Some) + value
    assert_eq!(buf[0], 1); // Some variant index
    assert_eq!(&buf[1..5], &42u32.to_le_bytes());

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
    assert_eq!(&buf[2..4], &123u16.to_le_bytes());

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
    assert_eq!(&buf[1..5], &42u32.to_le_bytes());

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
