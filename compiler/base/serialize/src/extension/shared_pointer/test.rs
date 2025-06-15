//! Tests for shared pointer extension functionality.

use super::*;

#[test]
fn shared_pointer_tracker() {
    let mut tracker = SharedPointerTracker::new();

    let arc1 = Arc::new(42);
    let arc2 = arc1.clone();
    let arc3 = Arc::new(42); // Different instance, same value

    // First registration should return true (first time seen)
    assert!(tracker.register_arc(&arc1));
    // Same Arc should return false (already seen)
    assert!(!tracker.register_arc(&arc2));
    // Different Arc should return true (first time seen)
    assert!(tracker.register_arc(&arc3));
}

#[test]
fn shared_pointer_store() {
    let mut store = SharedPointerStore::new();

    let arc = Arc::new(String::from("test"));
    let pointer = 0x1234;

    // Store and retrieve
    store.store_arc(pointer, arc.clone());
    let retrieved = store.get_arc::<String>(pointer).unwrap();
    assert_eq!(*retrieved, "test");

    // Non-existent pointer
    assert!(store.get_arc::<String>(0x5678).is_none());
}

#[test]
fn arc_shared_serialization() {
    use std::io::Cursor;

    use crate::binary::{de::BinaryDeserializer, ser::BinarySerializer};

    // Create a shared pointer extension
    let tracker = SharedPointerTracker::new();
    let store = SharedPointerStore::new();

    // Create two Arc instances that share the same data
    let arc1 = Arc::new(String::from("shared_data"));
    let arc2 = arc1.clone(); // This should serialize as reference
    let arc3 = Arc::new(String::from("other_data")); // This should serialize as owned

    let data = vec![arc1, arc2, arc3];

    // Serialize
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::with_extension(buffer, tracker);
    data.serialize(&mut serializer).unwrap();
    let (buffer, _tracker) = serializer.into_parts();

    // Deserialize
    let cursor = Cursor::new(buffer);
    let mut deserializer =
        BinaryDeserializer::with_extension(cursor, store);
    let deserialized: Vec<Arc<String>> =
        Vec::deserialize(&mut deserializer).unwrap();

    // Verify the data is correct
    assert_eq!(deserialized.len(), 3);
    assert_eq!(*deserialized[0], "shared_data");
    assert_eq!(*deserialized[1], "shared_data");
    assert_eq!(*deserialized[2], "other_data");

    // Verify that the first two Arcs actually share the same allocation
    assert!(Arc::ptr_eq(&deserialized[0], &deserialized[1]));
    // And that the third one doesn't
    assert!(!Arc::ptr_eq(&deserialized[0], &deserialized[2]));
}

#[test]
fn rc_shared_serialization() {
    use std::io::Cursor;

    use crate::binary::{de::BinaryDeserializer, ser::BinarySerializer};

    // Create a shared pointer extension
    let tracker = SharedPointerTracker::new();
    let store = SharedPointerStore::new();

    // Create two Rc instances that share the same data
    let rc1 = Rc::new(42u32);
    let rc2 = rc1.clone(); // This should serialize as reference
    let rc3 = Rc::new(100u32); // This should serialize as owned

    let data = vec![rc1, rc2, rc3];

    // Serialize
    let buffer = Vec::new();
    let mut serializer = BinarySerializer::with_extension(buffer, tracker);
    data.serialize(&mut serializer).unwrap();
    let (buffer, _tracker) = serializer.into_parts();

    // Deserialize
    let cursor = Cursor::new(buffer);
    let mut deserializer =
        BinaryDeserializer::with_extension(cursor, store);
    let deserialized: Vec<Rc<u32>> =
        Vec::deserialize(&mut deserializer).unwrap();

    // Verify the data is correct
    assert_eq!(deserialized.len(), 3);
    assert_eq!(*deserialized[0], 42);
    assert_eq!(*deserialized[1], 42);
    assert_eq!(*deserialized[2], 100);

    // Verify that the first two Rcs actually share the same allocation
    assert!(Rc::ptr_eq(&deserialized[0], &deserialized[1]));
    // And that the third one doesn't
    assert!(!Rc::ptr_eq(&deserialized[0], &deserialized[2]));
}
