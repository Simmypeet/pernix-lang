//! Tests for the binary deserializer.

use std::collections::HashMap;

use super::*;
use crate::{binary::ser::BinarySerializer, de::Deserialize, ser::Serialize};

/// Helper function to serialize and deserialize a value, ensuring round-trip
/// compatibility.
fn round_trip<T>(value: &T) -> Result<T, Box<dyn std::error::Error>>
where
    T: Serialize<BinarySerializer<Vec<u8>>, ()>
        + Deserialize<BinaryDeserializer<std::io::Cursor<Vec<u8>>>, ()>
        + std::fmt::Debug
        + PartialEq,
{
    // Serialize
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::new(buffer);
    value.serialize(&mut serializer, &())?;
    let buffer = serializer.into_inner();

    // Deserialize
    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = BinaryDeserializer::new(cursor);
    let result = T::deserialize(&mut deserializer, &())?;

    Ok(result)
}

#[test]
fn primitives() {
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
fn char() {
    assert_eq!(round_trip(&'a').unwrap(), 'a');
    assert_eq!(round_trip(&'🦀').unwrap(), '🦀');
    assert_eq!(round_trip(&'\0').unwrap(), '\0');
}

#[test]
fn strings() {
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
fn option() {
    assert_eq!(round_trip(&Some(42u32)).unwrap(), Some(42u32));
    assert_eq!(round_trip(&None::<u32>).unwrap(), None::<u32>);
    assert_eq!(
        round_trip(&Some("hello".to_string())).unwrap(),
        Some("hello".to_string())
    );
}

#[test]
fn result() {
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
fn sequences() {
    // Test Vec
    assert_eq!(round_trip(&vec![1, 2, 3, 4]).unwrap(), vec![1, 2, 3, 4]);
    assert_eq!(round_trip(&Vec::<u32>::new()).unwrap(), Vec::<u32>::new());

    // Test arrays
    assert_eq!(round_trip(&[1, 2, 3]).unwrap(), [1, 2, 3]);
    assert_eq!(round_trip(&[0u8; 10]).unwrap(), [0u8; 10]);

    // Test tuples
    assert_eq!(round_trip(&(1, 2)).unwrap(), (1, 2));
    assert_eq!(
        round_trip(&(1, "hello".to_string(), 2.5)).unwrap(),
        (1, "hello".to_string(), 2.5)
    );
}

#[test]
fn maps() {
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
fn varint_encoding() {
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
fn floating_point_special_values() {
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
fn complex_nested_structures() {
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
fn large_sequences() {
    // Test large sequences to ensure varint length encoding works
    let large_vec: Vec<u32> = (0..10000).collect();
    assert_eq!(round_trip(&large_vec).unwrap(), large_vec);

    // Test sequence with large strings
    let string_vec: Vec<String> = (0..100)
        .map(|i| format!("This is a longer string number {i}"))
        .collect();
    assert_eq!(round_trip(&string_vec).unwrap(), string_vec);
}

#[test]
fn empty_collections() {
    // Test empty collections
    assert_eq!(round_trip(&Vec::<u32>::new()).unwrap(), Vec::<u32>::new());
    assert_eq!(
        round_trip(&HashMap::<String, u32>::new()).unwrap(),
        HashMap::<String, u32>::new()
    );
    assert_eq!(round_trip(&String::new()).unwrap(), String::new());
}

#[test]
fn reader_access() {
    let data = vec![1, 2, 3, 4];
    let cursor = std::io::Cursor::new(data);
    let mut deserializer = BinaryDeserializer::new(cursor);

    // Read directly from the reader to test access
    let mut byte = [0u8; 1];
    deserializer.reader_mut().read_exact(&mut byte).unwrap();
    assert_eq!(byte[0], 1);
}

#[test]
fn error_handling() {
    // Test deserializing from empty buffer
    let cursor = std::io::Cursor::new(Vec::new());
    let mut deserializer = BinaryDeserializer::new(cursor);

    let result = u32::deserialize(&mut deserializer, &());
    assert!(result.is_err());

    // Test deserializing incomplete data
    let cursor = std::io::Cursor::new(vec![0x80]); // Varint with continuation bit but no follow-up byte
    let mut deserializer = BinaryDeserializer::new(cursor);

    let result = u32::deserialize(&mut deserializer, &());
    assert!(result.is_err());
}

#[test]
fn bool_invalid_values() {
    // Test that invalid bool values are rejected
    let cursor = std::io::Cursor::new(vec![2u8]); // Invalid bool value
    let mut deserializer = BinaryDeserializer::new(cursor);

    let result = bool::deserialize(&mut deserializer, &());
    assert!(result.is_err());
}

#[test]
fn string_with_invalid_utf8() {
    // Create a buffer with invalid UTF-8
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::new(buffer);

    // Serialize a valid string first to get the length encoding
    "hello".serialize(&mut serializer, &()).unwrap();

    // Now manually corrupt the buffer to have invalid UTF-8
    let mut buffer = serializer.into_inner();
    let len = buffer.len();
    buffer[len - 1] = 0xFF; // Invalid UTF-8 byte

    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = BinaryDeserializer::new(cursor);

    let result = String::deserialize(&mut deserializer, &());
    assert!(result.is_err());
}

#[test]
fn char_invalid_unicode() {
    // Test that invalid Unicode code points are rejected
    let cursor = std::io::Cursor::new(vec![0xFFu8, 0xFFu8, 0xFFu8, 0xFFu8]); // Invalid Unicode
    let mut deserializer = BinaryDeserializer::new(cursor);

    let result = char::deserialize(&mut deserializer, &());
    assert!(result.is_err());
}

#[test]
fn array_error_handling() {
    // Test array deserialization with insufficient elements
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::new(buffer);

    // Serialize a vector with only 2 elements
    vec![1u32, 2u32].serialize(&mut serializer, &()).unwrap();
    let buffer = serializer.into_inner();

    let cursor = std::io::Cursor::new(buffer);
    let mut deserializer = BinaryDeserializer::new(cursor);

    // Try to deserialize as an array of 3 elements - should panic
    let result = <[u32; 3]>::deserialize(&mut deserializer, &());

    // Should have returned an error due to insufficient elements
    assert!(result.is_err());
}

#[test]
fn array_drop_behavior_with_shared_counter() {
    use std::sync::atomic::{AtomicUsize, Ordering};

    // Global drop counter that all instances share
    static GLOBAL_DROP_COUNT: AtomicUsize = AtomicUsize::new(0);

    // Custom type that tracks drops globally
    #[derive(Debug, Clone, PartialEq)]
    struct GlobalDropCounter {
        id: usize,
    }

    impl GlobalDropCounter {
        fn new(id: usize) -> Self { Self { id } }
    }

    impl Drop for GlobalDropCounter {
        fn drop(&mut self) { GLOBAL_DROP_COUNT.fetch_add(1, Ordering::SeqCst); }
    }

    // Implement Serialize for GlobalDropCounter
    impl Serialize<BinarySerializer<Vec<u8>>, ()> for GlobalDropCounter {
        fn serialize(
            &self,
            serializer: &mut BinarySerializer<Vec<u8>>,
            extension: &(),
        ) -> Result<
            (),
            <BinarySerializer<Vec<u8>> as crate::ser::Serializer<()>>::Error,
        > {
            self.id.serialize(serializer, extension)
        }
    }

    // Implement Deserialize for GlobalDropCounter
    impl Deserialize<BinaryDeserializer<std::io::Cursor<Vec<u8>>>, ()>
        for GlobalDropCounter
    {
        fn deserialize(deserializer: &mut BinaryDeserializer<std::io::Cursor<Vec<u8>>>, extension: & ()) -> Result<Self, <BinaryDeserializer<std::io::Cursor<Vec<u8>>> as crate::de::Deserializer<()>>::Error>{
            let id = usize::deserialize(deserializer, extension)?;
            Ok(GlobalDropCounter::new(id))
        }
    }

    // Reset global counter
    GLOBAL_DROP_COUNT.store(0, Ordering::SeqCst);

    // Test 1: Happy path
    {
        let initial_count = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);

        // Create and serialize an array
        {
            let original_array = [
                GlobalDropCounter::new(0),
                GlobalDropCounter::new(1),
                GlobalDropCounter::new(2),
            ];

            let buffer = Vec::new();
            let mut serializer = BinarySerializer::new(buffer);
            original_array.serialize(&mut serializer, &()).unwrap();
            let buffer = serializer.into_inner();

            // Deserialize the array
            let cursor = std::io::Cursor::new(buffer);
            let mut deserializer = BinaryDeserializer::new(cursor);
            let _ =
                <[GlobalDropCounter; 3]>::deserialize(&mut deserializer, &())
                    .unwrap();

            // Arrays automatically drop when going out of scope
        } // original_array and deserialized_array drop here

        let final_count = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            final_count - initial_count,
            6,
            "Total should be 6 drops (3 original + 3 deserialized)"
        );
    }

    // Test 2: Error path with proper cleanup
    {
        let initial_count = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);

        // Create a vector with only 2 elements
        let partial_vec =
            vec![GlobalDropCounter::new(10), GlobalDropCounter::new(11)];

        let buffer = Vec::new();
        let mut serializer = BinarySerializer::new(buffer);
        partial_vec.serialize(&mut serializer, &()).unwrap();
        let buffer = serializer.into_inner();

        // Try to deserialize as an array of 3 elements
        let cursor = std::io::Cursor::new(buffer);
        let mut deserializer = BinaryDeserializer::new(cursor);

        let before_error = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);

        let result =
            <[GlobalDropCounter; 3]>::deserialize(&mut deserializer, &());

        assert!(
            result.is_err(),
            "Should return error due to insufficient elements"
        );

        let after_error = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);

        // The 2 successfully created elements during deserialization should
        // have been dropped during the error cleanup
        assert_eq!(
            after_error - before_error,
            2,
            "Should have 2 drops from cleanup during error"
        );

        // Drop the original partial vector
        drop(partial_vec);
        let after_original_drop = GLOBAL_DROP_COUNT.load(Ordering::SeqCst);
        assert_eq!(
            after_original_drop - after_error,
            2,
            "Original partial vec should cause 2 more drops"
        );

        assert_eq!(
            after_original_drop - initial_count,
            4,
            "Total should be 4 drops (2 from cleanup + 2 from original)"
        );
    }
}

#[test]
fn integers_128_bit() {
    // Test u128 round-trip
    assert_eq!(round_trip(&42u128).unwrap(), 42u128);
    assert_eq!(round_trip(&0u128).unwrap(), 0u128);
    assert_eq!(round_trip(&128u128).unwrap(), 128u128);
    assert_eq!(round_trip(&u128::MAX).unwrap(), u128::MAX);

    // Test some large values
    let large_u128 = u128::from(u64::MAX) + 1000;
    assert_eq!(round_trip(&large_u128).unwrap(), large_u128);

    // Test i128 round-trip
    assert_eq!(round_trip(&42i128).unwrap(), 42i128);
    assert_eq!(round_trip(&-42i128).unwrap(), -42i128);
    assert_eq!(round_trip(&0i128).unwrap(), 0i128);
    assert_eq!(round_trip(&i128::MAX).unwrap(), i128::MAX);
    assert_eq!(round_trip(&i128::MIN).unwrap(), i128::MIN);

    // Test edge cases around zero
    assert_eq!(round_trip(&1i128).unwrap(), 1i128);
    assert_eq!(round_trip(&-1i128).unwrap(), -1i128);

    // Test some large positive and negative values
    let large_positive = i128::from(i64::MAX) + 1000;
    let large_negative = i128::from(i64::MIN) - 1000;
    assert_eq!(round_trip(&large_positive).unwrap(), large_positive);
    assert_eq!(round_trip(&large_negative).unwrap(), large_negative);
}

#[test]
fn varint_128_bit() {
    // Test varint functionality through the public serialization interface
    let test_values = [
        0u128,
        1u128,
        127u128,
        128u128,
        255u128,
        256u128,
        u128::from(u64::MAX),
        u128::from(u64::MAX) + 1,
        u128::MAX,
    ];

    for &value in &test_values {
        // Test using the public round_trip function
        assert_eq!(
            round_trip(&value).unwrap(),
            value,
            "Failed for u128 value: {value}"
        );
    }
}

#[test]
fn zigzag_128_bit() {
    // Test zigzag functionality through the public serialization interface
    let test_values = [
        0i128,
        1i128,
        -1i128,
        42i128,
        -42i128,
        i128::MAX,
        i128::MIN,
        i128::from(i64::MAX),
        i128::from(i64::MIN),
    ];

    for &value in &test_values {
        // Test using the public round_trip function
        assert_eq!(
            round_trip(&value).unwrap(),
            value,
            "Failed for i128 value: {value}"
        );
    }
}
