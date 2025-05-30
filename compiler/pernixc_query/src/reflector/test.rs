use insta::{assert_ron_snapshot, Settings};
use serde::{de::DeserializeSeed, Deserialize, Serialize};
use smallbox::smallbox;

use crate::{call_graph::DynamicBox, Database, Key};

// Basic equality-based merge (default behavior)
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct Variable(String);

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct NegateVariable(String);

// Custom merge function - additive
#[allow(clippy::unnecessary_wraps)]
fn additive_merge(old: &mut i32, new: i32) -> Result<(), String> {
    *old += new;
    Ok(())
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[merge(additive_merge)]
pub struct AdditiveKey(String);

// Custom merge function that can fail
fn conditional_merge(old: &mut i32, new: i32) -> Result<(), String> {
    if new > 100 {
        Err("Value too large for merge".to_string())
    } else if new == *old {
        Ok(()) // Allow same values
    } else if new > *old {
        *old = new; // Accept larger values
        Ok(())
    } else {
        Err(format!(
            "Cannot merge {} into {} (new value must be >= old value)",
            new, *old
        ))
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
#[merge(conditional_merge)]
pub struct ConditionalMergeKey(String);

#[test]
fn seriaizable() {
    let mut db = crate::Database::default();

    db.register_reflector::<Variable>();
    db.register_reflector::<NegateVariable>();

    db.set_input(&Variable("x".to_string()), 32);
    db.set_input(&NegateVariable("x".to_string()), -32);
    db.set_input(&Variable("y".to_string()), 64);
    db.set_input(&NegateVariable("y".to_string()), -64);

    let map = db.map.serializable(&db.reflector);

    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.bind(|| assert_ron_snapshot!(map));
}

#[test]
fn deserializable() {
    // Create a new database with the same reflector setup
    let mut db = crate::Database::default();
    db.register_reflector::<Variable>();
    db.register_reflector::<NegateVariable>();

    // Set up the original data
    db.set_input(&Variable("x".to_string()), 32);
    db.set_input(&NegateVariable("x".to_string()), -32);
    db.set_input(&Variable("y".to_string()), 64);
    db.set_input(&NegateVariable("y".to_string()), -64);

    // Serialize the map
    let serializable_map = db.map.serializable(&db.reflector);
    let ron_string =
        ron::to_string(&serializable_map).expect("Failed to serialize to RON");

    // Create a new empty database for deserialization
    let mut target_db = crate::Database::default();
    target_db.register_reflector::<Variable>();
    target_db.register_reflector::<NegateVariable>();

    // Deserialize back into the target map
    let deserializable_map = target_db.map.deserializable(&target_db.reflector);
    let mut deserializer = ron::Deserializer::from_str(&ron_string)
        .expect("Failed to create deserializer");

    deserializable_map
        .deserialize(&mut deserializer)
        .expect("Failed to deserialize");

    // Verify the deserialized data matches the original
    assert_eq!(target_db.map.get(&Variable("x".to_string())), Some(32));
    assert_eq!(target_db.map.get(&NegateVariable("x".to_string())), Some(-32));
    assert_eq!(target_db.map.get(&Variable("y".to_string())), Some(64));
    assert_eq!(target_db.map.get(&NegateVariable("y".to_string())), Some(-64));
}

#[test]
fn merge_value_basic_equality() {
    // Test basic case: default merge_value implementation with equality
    let mut db = crate::Database::default();
    db.register_reflector::<Variable>();

    // Set initial value
    db.set_input(&Variable("test".to_string()), 42);

    // Test merging with the same value (should succeed)
    {
        // Create a separate database with the same value to serialize from
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<Variable>();
        source_db.set_input(&Variable("test".to_string()), 42); // Same value

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        // Deserialize into the target database - should succeed with same value
        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully merge identical values");

        // Value should remain unchanged
        assert_eq!(db.map.get(&Variable("test".to_string())), Some(42));
    }

    // Test merging with a different value (should fail with default equality
    // merge)
    {
        // Create a separate database with a different value to serialize from
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<Variable>();
        source_db.set_input(&Variable("test".to_string()), 100); // Different value

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        // Deserialize into the target database - should fail with different
        // value
        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(
            result.is_err(),
            "Should fail to merge different values with default equality merge"
        );

        // Original value should be preserved
        assert_eq!(db.map.get(&Variable("test".to_string())), Some(42));
    }
}

#[test]
fn merge_value_custom_additive() {
    // Test alternative case: custom implementation (additive merge)
    let mut db = crate::Database::default();
    db.register_reflector::<AdditiveKey>();

    // Set initial value
    db.set_input(&AdditiveKey("counter".to_string()), 10);

    // Test first addition
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<AdditiveKey>();
        source_db.set_input(&AdditiveKey("counter".to_string()), 5);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully perform additive merge");

        // Value should be added: 10 + 5 = 15
        assert_eq!(db.map.get(&AdditiveKey("counter".to_string())), Some(15));
    }

    // Test second addition
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<AdditiveKey>();
        source_db.set_input(&AdditiveKey("counter".to_string()), 25);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(
            result.is_ok(),
            "Should successfully perform second additive merge"
        );

        // Value should be added: 15 + 25 = 40
        assert_eq!(db.map.get(&AdditiveKey("counter".to_string())), Some(40));
    }

    // Test with new key (should work as normal insertion)
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<AdditiveKey>();
        source_db.set_input(&AdditiveKey("new_counter".to_string()), 100);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully insert new key");

        // New key should have its value
        assert_eq!(
            db.map.get(&AdditiveKey("new_counter".to_string())),
            Some(100)
        );
        // Original key should be unchanged
        assert_eq!(db.map.get(&AdditiveKey("counter".to_string())), Some(40));
    }
}

#[test]
fn merge_value_conditional_errors() {
    // Test exceptional case: Err(...) from merge function
    let mut db = crate::Database::default();
    db.register_reflector::<ConditionalMergeKey>();

    // Set initial value
    db.set_input(&ConditionalMergeKey("test".to_string()), 50);

    // Test 1: Value too large (> 100) - should fail
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<ConditionalMergeKey>();
        source_db.set_input(&ConditionalMergeKey("test".to_string()), 150);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_err(), "Should fail with value too large error");

        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("Value too large for merge"),
            "Error should mention value too large"
        );

        // Original value should be preserved
        assert_eq!(
            db.map.get(&ConditionalMergeKey("test".to_string())),
            Some(50)
        );
    }

    // Test 2: Smaller value (< existing) - should fail
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<ConditionalMergeKey>();
        source_db.set_input(&ConditionalMergeKey("test".to_string()), 30);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_err(), "Should fail with smaller value error");

        let error_msg = result.unwrap_err().to_string();
        assert!(
            error_msg.contains("Cannot merge 30 into 50"),
            "Error should mention merge conflict"
        );

        // Original value should be preserved
        assert_eq!(
            db.map.get(&ConditionalMergeKey("test".to_string())),
            Some(50)
        );
    }

    // Test 3: Same value - should succeed
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<ConditionalMergeKey>();
        source_db.set_input(&ConditionalMergeKey("test".to_string()), 50);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with same value");

        // Value should remain unchanged
        assert_eq!(
            db.map.get(&ConditionalMergeKey("test".to_string())),
            Some(50)
        );
    }

    // Test 4: Larger value (but <= 100) - should succeed and update
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<ConditionalMergeKey>();
        source_db.set_input(&ConditionalMergeKey("test".to_string()), 75);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with larger acceptable value");

        // Value should be updated to the larger value
        assert_eq!(
            db.map.get(&ConditionalMergeKey("test".to_string())),
            Some(75)
        );
    }

    // Test 5: Edge case - exactly 100 should succeed
    {
        let mut source_db = crate::Database::default();
        source_db.register_reflector::<ConditionalMergeKey>();
        source_db.set_input(&ConditionalMergeKey("test".to_string()), 100);

        let serializable_map = source_db.map.serializable(&source_db.reflector);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = db.map.deserializable(&db.reflector);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with value exactly 100");

        // Value should be updated to 100
        assert_eq!(
            db.map.get(&ConditionalMergeKey("test".to_string())),
            Some(100)
        );
    }
}

#[test]
fn serialize_key() {
    let key = DynamicBox(smallbox!(Variable("test".to_string())));

    let mut database = Database::default();
    database.register_reflector::<Variable>();

    super::set_reflector(&mut database.reflector, || {
        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.bind(|| assert_ron_snapshot!(key));
    });
}
