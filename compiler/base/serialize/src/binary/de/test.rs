//! Tests for the binary deserializer.

use std::collections::HashMap;

use super::*;
use crate::{binary::ser::BinarySerializer, de::Deserialize, ser::Serialize};

/// Helper function to serialize and deserialize a value, ensuring round-trip
/// compatibility.
fn round_trip<T>(value: &T) -> Result<T, Box<dyn std::error::Error>>
where
    T: Serialize<BinarySerializer<Vec<u8>, ()>>
        + Deserialize<BinaryDeserializer<std::io::Cursor<Vec<u8>>, ()>>
        + std::fmt::Debug
        + PartialEq,
{
    // Serialize
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::new(buffer);
    value.serialize(&mut serializer)?;
    let buffer = serializer.into_inner();

    // Deserialize
    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = BinaryDeserializer::new(cursor);
    let result = T::deserialize(&mut deserializer)?;

    Ok(result)
}

#[test]
fn test_primitives() {
    // Test various primitive types
    assert_eq!(round_trip(&42u8).unwrap(), 42u8);
    assert_eq!(round_trip(&42u16).unwrap(), 42u16);
    assert_eq!(round_trip(&42u32).unwrap(), 42u32);
    assert_eq!(round_trip(&42u64).unwrap(), 42u64);
    assert_eq!(round_trip(&42usize).unwrap(), 42usize);

    assert_eq!(round_trip(&-42i8).unwrap(), -42i8);
    assert_eq!(round_trip(&-42i16).unwrap(), -42i16);
    assert_eq!(round_trip(&-42i32).unwrap(), -42i32);
    assert_eq!(round_trip(&-42i64).unwrap(), -42i64);
    assert_eq!(round_trip(&-42isize).unwrap(), -42isize);

    assert_eq!(round_trip(&2.5f32).unwrap(), 2.5f32);
    assert_eq!(round_trip(&1.23456789f64).unwrap(), 1.23456789f64);

    assert!(round_trip(&true).unwrap());
    assert!(!round_trip(&false).unwrap());
}

#[test]
fn test_char() {
    assert_eq!(round_trip(&'a').unwrap(), 'a');
    assert_eq!(round_trip(&'🦀').unwrap(), '🦀');
    assert_eq!(round_trip(&'\0').unwrap(), '\0');
}

#[test]
fn test_strings() {
    assert_eq!(round_trip(&"hello".to_string()).unwrap(), "hello".to_string());
    assert_eq!(round_trip(&String::new()).unwrap(), String::new());
    assert_eq!(round_trip(&"🦀🔥".to_string()).unwrap(), "🦀🔥".to_string());

    // Test string with null bytes
    assert_eq!(
        round_trip(&"hello\0world".to_string()).unwrap(),
        "hello\0world".to_string()
    );
}

#[test]
fn test_option() {
    assert_eq!(round_trip(&Some(42u32)).unwrap(), Some(42u32));
    assert_eq!(round_trip(&None::<u32>).unwrap(), None::<u32>);
    assert_eq!(
        round_trip(&Some("hello".to_string())).unwrap(),
        Some("hello".to_string())
    );
}

#[test]
fn test_result() {
    assert_eq!(
        round_trip(&Ok::<u32, String>(42)).unwrap(),
        Ok::<u32, String>(42)
    );
    assert_eq!(
        round_trip(&Err::<u32, String>("error".to_string())).unwrap(),
        Err::<u32, String>("error".to_string())
    );
}

#[test]
fn test_sequences() {
    // Test Vec
    assert_eq!(round_trip(&vec![1, 2, 3, 4]).unwrap(), vec![1, 2, 3, 4]);
    assert_eq!(round_trip(&Vec::<u32>::new()).unwrap(), Vec::<u32>::new());

    // Test arrays
    assert_eq!(round_trip(&[1, 2, 3]).unwrap(), [1, 2, 3]);
    assert_eq!(round_trip(&[0u8; 10]).unwrap(), [0u8; 10]);

    // Test tuples
    assert_eq!(round_trip(&(1, 2)).unwrap(), (1, 2));
    assert_eq!(round_trip(&(1, "hello".to_string(), 2.5)).unwrap(), (1, "hello".to_string(), 2.5));
}

#[test]
fn test_maps() {
    let mut map = HashMap::new();
    map.insert("key1".to_string(), 42u32);
    map.insert("key2".to_string(), 84u32);

    let result = round_trip(&map).unwrap();
    assert_eq!(result, map);

    // Test empty map
    let empty_map: HashMap<String, u32> = HashMap::new();
    assert_eq!(round_trip(&empty_map).unwrap(), empty_map);
}

#[test]
fn test_varint_encoding() {
    // Test values that require different varint sizes
    assert_eq!(round_trip(&0u64).unwrap(), 0u64);
    assert_eq!(round_trip(&127u64).unwrap(), 127u64);
    assert_eq!(round_trip(&128u64).unwrap(), 128u64);
    assert_eq!(round_trip(&16383u64).unwrap(), 16383u64);
    assert_eq!(round_trip(&16384u64).unwrap(), 16384u64);
    assert_eq!(round_trip(&u64::MAX).unwrap(), u64::MAX);

    // Test signed varint
    assert_eq!(round_trip(&0i64).unwrap(), 0i64);
    assert_eq!(round_trip(&-1i64).unwrap(), -1i64);
    assert_eq!(round_trip(&i64::MIN).unwrap(), i64::MIN);
    assert_eq!(round_trip(&i64::MAX).unwrap(), i64::MAX);
}

#[test]
fn test_floating_point_special_values() {
    // Test special floating point values
    assert_eq!(round_trip(&f32::INFINITY).unwrap(), f32::INFINITY);
    assert_eq!(round_trip(&f32::NEG_INFINITY).unwrap(), f32::NEG_INFINITY);
    assert!(round_trip(&f32::NAN).unwrap().is_nan());

    assert_eq!(round_trip(&f64::INFINITY).unwrap(), f64::INFINITY);
    assert_eq!(round_trip(&f64::NEG_INFINITY).unwrap(), f64::NEG_INFINITY);
    assert!(round_trip(&f64::NAN).unwrap().is_nan());

    // Test zero variants
    assert_eq!(round_trip(&0.0f32).unwrap(), 0.0f32);
    assert_eq!(round_trip(&-0.0f32).unwrap(), -0.0f32);
    assert_eq!(round_trip(&0.0f64).unwrap(), 0.0f64);
    assert_eq!(round_trip(&-0.0f64).unwrap(), -0.0f64);
}

#[test]
fn test_complex_nested_structures() {
    // Test deeply nested structures
    let complex = vec![
        Some(HashMap::from([
            ("key1".to_string(), Ok::<Vec<u32>, String>(vec![1, 2, 3])),
            ("key2".to_string(), Err("error".to_string())),
        ])),
        None,
        Some(HashMap::from([("key3".to_string(), Ok(vec![4, 5, 6]))])),
    ];

    assert_eq!(round_trip(&complex).unwrap(), complex);
}

#[test]
fn test_large_sequences() {
    // Test large sequences to ensure varint length encoding works
    let large_vec: Vec<u32> = (0..10000).collect();
    assert_eq!(round_trip(&large_vec).unwrap(), large_vec);

    // Test sequence with large strings
    let string_vec: Vec<String> = (0..100)
        .map(|i| format!("This is a longer string number {}", i))
        .collect();
    assert_eq!(round_trip(&string_vec).unwrap(), string_vec);
}

#[test]
fn test_empty_collections() {
    // Test empty collections
    assert_eq!(round_trip(&Vec::<u32>::new()).unwrap(), Vec::<u32>::new());
    assert_eq!(
        round_trip(&HashMap::<String, u32>::new()).unwrap(),
        HashMap::<String, u32>::new()
    );
    assert_eq!(round_trip(&String::new()).unwrap(), String::new());
}

#[test]
fn test_extension_field_access() {
    let buffer = Vec::new();
    let mut deserializer = BinaryDeserializer::with_extension(std::io::Cursor::new(buffer), 42u32);
    
    // Test getter through trait method
    use crate::de::Deserializer;
    assert_eq!(*deserializer.extension(), 42u32);
    
    // Test mutable access through trait method
    *deserializer.extension() = 200u32;
    assert_eq!(*deserializer.extension(), 200u32);
}

#[test]
fn test_reader_access() {
    let data = vec![1, 2, 3, 4];
    let cursor = std::io::Cursor::new(data);
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    // Read directly from the reader to test access
    let mut byte = [0u8; 1];
    deserializer.reader_mut().read_exact(&mut byte).unwrap();
    assert_eq!(byte[0], 1);
}

#[test]
fn test_error_handling() {
    // Test deserializing from empty buffer
    let cursor = std::io::Cursor::new(Vec::new());
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    let result = u32::deserialize(&mut deserializer);
    assert!(result.is_err());
    
    // Test deserializing incomplete data
    let cursor = std::io::Cursor::new(vec![1, 2]); // Only 2 bytes for a u32
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    let result = u32::deserialize(&mut deserializer);
    assert!(result.is_err());
}

#[test]
fn test_bool_invalid_values() {
    // Test that invalid bool values are rejected
    let cursor = std::io::Cursor::new(vec![2u8]); // Invalid bool value
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    let result = bool::deserialize(&mut deserializer);
    assert!(result.is_err());
}

#[test]
fn test_string_with_invalid_utf8() {
    // Create a buffer with invalid UTF-8
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::new(buffer);
    
    // Serialize a valid string first to get the length encoding
    "hello".serialize(&mut serializer).unwrap();
    
    // Now manually corrupt the buffer to have invalid UTF-8
    let mut buffer = serializer.into_inner();
    let len = buffer.len();
    buffer[len - 1] = 0xFF; // Invalid UTF-8 byte
    
    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    let result = String::deserialize(&mut deserializer);
    assert!(result.is_err());
}

#[test]
fn test_char_invalid_unicode() {
    // Test that invalid Unicode code points are rejected
    let cursor = std::io::Cursor::new(vec![0xFFu8, 0xFFu8, 0xFFu8, 0xFFu8]); // Invalid Unicode
    let mut deserializer = BinaryDeserializer::new(cursor);
    
    let result = char::deserialize(&mut deserializer);
    assert!(result.is_err());
}
