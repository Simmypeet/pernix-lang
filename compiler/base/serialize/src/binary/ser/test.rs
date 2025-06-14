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
    let buf = with_serializer(|s| 42u8.serialize(s));
    assert_eq!(buf, vec![42]);

    let buf = with_serializer(|s| 1000u16.serialize(s));
    assert_eq!(buf, 1000u16.to_le_bytes());

    let buf = with_serializer(|s| (-42i32).serialize(s));
    assert_eq!(buf, (-42i32).to_le_bytes());

    // Test boolean
    let buf = with_serializer(|s| true.serialize(s));
    assert_eq!(buf, vec![1]);

    let buf = with_serializer(|s| false.serialize(s));
    assert_eq!(buf, vec![0]);

    // Test string
    let buf = with_serializer(|s| "hello".serialize(s));
    assert_eq!(buf[0], 5); // length
    assert_eq!(&buf[1..], b"hello");
}

#[test]
fn varint() {
    // Test various varint values
    let buf = with_serializer(|s| s.write_varint(0));
    assert_eq!(buf, vec![0]);

    let buf = with_serializer(|s| s.write_varint(127));
    assert_eq!(buf, vec![127]);

    let buf = with_serializer(|s| s.write_varint(128));
    assert_eq!(buf, vec![0x80, 0x01]);

    let buf = with_serializer(|s| s.write_varint(16383));
    assert_eq!(buf, vec![0xFF, 0x7F]);

    let buf = with_serializer(|s| s.write_varint(16384));
    assert_eq!(buf, vec![0x80, 0x80, 0x01]);
}
