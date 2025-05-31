use insta::{assert_ron_snapshot, Settings};
use serde::{de::DeserializeSeed, Deserialize, Serialize};
use smallbox::smallbox;

use crate::{key::DynamicBox, map, Key};

// Basic equality-based merge (default behavior)
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
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
    Default,
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
    Default,
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

// Generic key type for testing serialization with type parameters
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
#[value(String)] // Use String as value type instead of T to avoid constraints
pub struct GenericKey<T>(String, std::marker::PhantomData<T>)
where
    T: Clone
        + std::fmt::Debug
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash
        + serde::Serialize
        + for<'a> serde::Deserialize<'a>
        + Default;

impl<T> GenericKey<T>
where
    T: Clone
        + std::fmt::Debug
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + std::hash::Hash
        + serde::Serialize
        + for<'a> serde::Deserialize<'a>
        + Default,
{
    fn new(name: String) -> Self { Self(name, std::marker::PhantomData) }
}

#[test]
fn serializable() {
    let mut registry = super::Registry::default();
    let map = map::Map::default();

    registry.register_reflector::<Variable>();
    registry.register_reflector::<NegateVariable>();

    map.insert(Variable("x".to_string()), 32);
    map.insert(NegateVariable("x".to_string()), -32);
    map.insert(Variable("y".to_string()), 64);
    map.insert(NegateVariable("y".to_string()), -64);

    let map = map.serializable(&registry);

    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.bind(|| assert_ron_snapshot!(map));
}

#[test]
fn deserializable() {
    // Create a new registry and map with the same reflector setup
    let mut registry = super::Registry::default();
    let map = map::Map::default();

    registry.register_reflector::<Variable>();
    registry.register_reflector::<NegateVariable>();

    // Set up the original data
    map.insert(Variable("x".to_string()), 32);
    map.insert(NegateVariable("x".to_string()), -32);
    map.insert(Variable("y".to_string()), 64);
    map.insert(NegateVariable("y".to_string()), -64);

    // Serialize the map
    let serializable_map = map.serializable(&registry);
    let ron_string =
        ron::to_string(&serializable_map).expect("Failed to serialize to RON");

    // Create a new empty registry and map for deserialization
    let mut target_registry = super::Registry::default();
    let target_map = map::Map::default();
    target_registry.register_reflector::<Variable>();
    target_registry.register_reflector::<NegateVariable>();

    // Deserialize back into the target map
    let deserializable_map = target_map.deserializable(&target_registry);
    let mut deserializer = ron::Deserializer::from_str(&ron_string)
        .expect("Failed to create deserializer");

    deserializable_map
        .deserialize(&mut deserializer)
        .expect("Failed to deserialize");

    // Verify the deserialized data matches the original
    assert_eq!(target_map.get(&Variable("x".to_string())), Some(32));
    assert_eq!(target_map.get(&NegateVariable("x".to_string())), Some(-32));
    assert_eq!(target_map.get(&Variable("y".to_string())), Some(64));
    assert_eq!(target_map.get(&NegateVariable("y".to_string())), Some(-64));
}

#[test]
fn merge_value_basic_equality() {
    // Test basic case: default merge_value implementation with equality
    let mut registry = super::Registry::default();
    let map = map::Map::default();

    registry.register_reflector::<Variable>();

    // Set initial value
    map.insert(Variable("test".to_string()), 42);

    // Test merging with the same value (should succeed)
    {
        // Create a separate registry and map with the same value to serialize
        // from
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<Variable>();
        source_map.insert(Variable("test".to_string()), 42); // Same value

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        // Deserialize into the target map - should succeed with same value
        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully merge identical values");

        // Value should remain unchanged
        assert_eq!(map.get(&Variable("test".to_string())), Some(42));
    }

    // Test merging with a different value (should fail with default equality
    // merge)
    {
        // Create a separate registry and map with a different value to
        // serialize from
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<Variable>();
        source_map.insert(Variable("test".to_string()), 100); // Different value

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        // Deserialize into the target map - should fail with different
        // value
        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(
            result.is_err(),
            "Should fail to merge different values with default equality merge"
        );

        // Original value should be preserved
        assert_eq!(map.get(&Variable("test".to_string())), Some(42));
    }
}

#[test]
fn merge_value_custom_additive() {
    // Test alternative case: custom implementation (additive merge)
    let mut registry = super::Registry::default();
    let map = map::Map::default();

    registry.register_reflector::<AdditiveKey>();

    // Set initial value
    map.insert(AdditiveKey("counter".to_string()), 10);

    // Test first addition
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<AdditiveKey>();
        source_map.insert(AdditiveKey("counter".to_string()), 5);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully perform additive merge");

        // Value should be added: 10 + 5 = 15
        assert_eq!(map.get(&AdditiveKey("counter".to_string())), Some(15));
    }

    // Test second addition
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<AdditiveKey>();
        source_map.insert(AdditiveKey("counter".to_string()), 25);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(
            result.is_ok(),
            "Should successfully perform second additive merge"
        );

        // Value should be added: 15 + 25 = 40
        assert_eq!(map.get(&AdditiveKey("counter".to_string())), Some(40));
    }

    // Test with new key (should work as normal insertion)
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<AdditiveKey>();
        source_map.insert(AdditiveKey("new_counter".to_string()), 100);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should successfully insert new key");

        // New key should have its value
        assert_eq!(map.get(&AdditiveKey("new_counter".to_string())), Some(100));
        // Original key should be unchanged
        assert_eq!(map.get(&AdditiveKey("counter".to_string())), Some(40));
    }
}

#[test]
fn merge_value_conditional_errors() {
    // Test exceptional case: Err(...) from merge function
    let mut registry = super::Registry::default();
    let map = map::Map::default();

    registry.register_reflector::<ConditionalMergeKey>();

    // Set initial value
    map.insert(ConditionalMergeKey("test".to_string()), 50);

    // Test 1: Value too large (> 100) - should fail
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<ConditionalMergeKey>();
        source_map.insert(ConditionalMergeKey("test".to_string()), 150);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
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
        assert_eq!(map.get(&ConditionalMergeKey("test".to_string())), Some(50));
    }

    // Test 2: Smaller value (< existing) - should fail
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<ConditionalMergeKey>();
        source_map.insert(ConditionalMergeKey("test".to_string()), 30);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
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
        assert_eq!(map.get(&ConditionalMergeKey("test".to_string())), Some(50));
    }

    // Test 3: Same value - should succeed
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<ConditionalMergeKey>();
        source_map.insert(ConditionalMergeKey("test".to_string()), 50);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with same value");

        // Value should remain unchanged
        assert_eq!(map.get(&ConditionalMergeKey("test".to_string())), Some(50));
    }

    // Test 4: Larger value (but <= 100) - should succeed and update
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<ConditionalMergeKey>();
        source_map.insert(ConditionalMergeKey("test".to_string()), 75);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with larger acceptable value");

        // Value should be updated to the larger value
        assert_eq!(map.get(&ConditionalMergeKey("test".to_string())), Some(75));
    }

    // Test 5: Edge case - exactly 100 should succeed
    {
        let mut source_registry = super::Registry::default();
        let source_map = map::Map::default();
        source_registry.register_reflector::<ConditionalMergeKey>();
        source_map.insert(ConditionalMergeKey("test".to_string()), 100);

        let serializable_map = source_map.serializable(&source_registry);
        let ron_string =
            ron::to_string(&serializable_map).expect("Failed to serialize");

        let deserializable_map = map.deserializable(&registry);
        let mut deserializer = ron::Deserializer::from_str(&ron_string)
            .expect("Failed to create deserializer");

        let result = deserializable_map.deserialize(&mut deserializer);
        assert!(result.is_ok(), "Should succeed with value exactly 100");

        // Value should be updated to 100
        assert_eq!(
            map.get(&ConditionalMergeKey("test".to_string())),
            Some(100)
        );
    }
}

#[test]
fn serialize_key() {
    let key = DynamicBox(smallbox!(Variable("test".to_string())));

    let mut registry = super::Registry::default();
    registry.register_reflector::<Variable>();

    super::set_reflector(&mut registry, || {
        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.bind(|| assert_ron_snapshot!(key));
    });
}

#[test]
fn deserialize_dynamic_box() {
    let original_key = DynamicBox(smallbox!(Variable("test".to_string())));

    let mut registry = super::Registry::default();
    registry.register_reflector::<Variable>();

    super::set_reflector(&mut registry, || {
        // Serialize the DynamicBox to RON format
        let ron_string = ron::to_string(&original_key)
            .expect("Failed to serialize DynamicBox");

        // Deserialize the RON string back to a DynamicBox
        let deserialized_key: DynamicBox = ron::from_str(&ron_string)
            .expect("Failed to deserialize DynamicBox");

        // Verify that the deserialized key equals the original
        assert_eq!(original_key, deserialized_key);

        // Verify that the inner value is correct by downcasting
        let original_variable = original_key
            .0
            .any()
            .downcast_ref::<Variable>()
            .expect("Original should be Variable");
        let deserialized_variable = deserialized_key
            .0
            .any()
            .downcast_ref::<Variable>()
            .expect("Deserialized should be Variable");

        assert_eq!(original_variable, deserialized_variable);
        assert_eq!(original_variable.0, "test");
        assert_eq!(deserialized_variable.0, "test");
    });
}

#[test]
fn deserialize_dynamic_box_different_types() {
    let variable_key = DynamicBox(smallbox!(Variable("test_var".to_string())));
    let negate_key =
        DynamicBox(smallbox!(NegateVariable("test_negate".to_string())));

    let mut registry = super::Registry::default();
    registry.register_reflector::<Variable>();
    registry.register_reflector::<NegateVariable>();

    super::set_reflector(&mut registry, || {
        // Test Variable deserialization
        let variable_ron = ron::to_string(&variable_key)
            .expect("Failed to serialize Variable");
        let deserialized_variable: DynamicBox = ron::from_str(&variable_ron)
            .expect("Failed to deserialize Variable");
        assert_eq!(variable_key, deserialized_variable);

        // Test NegateVariable deserialization
        let negate_ron = ron::to_string(&negate_key)
            .expect("Failed to serialize NegateVariable");
        let deserialized_negate: DynamicBox = ron::from_str(&negate_ron)
            .expect("Failed to deserialize NegateVariable");
        assert_eq!(negate_key, deserialized_negate);

        // Verify the types are correctly preserved
        assert!(variable_key.0.any().is::<Variable>());
        assert!(deserialized_variable.0.any().is::<Variable>());
        assert!(negate_key.0.any().is::<NegateVariable>());
        assert!(deserialized_negate.0.any().is::<NegateVariable>());
    });
}

#[test]
fn deserialize_dynamic_box_roundtrip_multiple() {
    // Test with multiple different key types in sequence
    let keys = [
        DynamicBox(smallbox!(Variable("var1".to_string()))),
        DynamicBox(smallbox!(NegateVariable("neg1".to_string()))),
        DynamicBox(smallbox!(AdditiveKey("add1".to_string()))),
        DynamicBox(smallbox!(ConditionalMergeKey("cond1".to_string()))),
    ];

    let mut registry = super::Registry::default();
    registry.register_reflector::<Variable>();
    registry.register_reflector::<NegateVariable>();
    registry.register_reflector::<AdditiveKey>();
    registry.register_reflector::<ConditionalMergeKey>();

    super::set_reflector(&mut registry, || {
        for (i, original_key) in keys.iter().enumerate() {
            let ron_string = ron::to_string(original_key)
                .unwrap_or_else(|_| panic!("Failed to serialize key {i}"));

            let deserialized_key: DynamicBox = ron::from_str(&ron_string)
                .unwrap_or_else(|_| panic!("Failed to deserialize key {i}"));

            assert_eq!(*original_key, deserialized_key, "Mismatch at key {i}");
        }
    });
}

#[test]
fn serialize_generic_key() {
    // Test serialization of GenericKey with Variable type parameter
    let variable_key = DynamicBox(smallbox!(GenericKey::<Variable>::new(
        "test_variable".to_string()
    )));

    let mut registry = super::Registry::default();
    registry.register_reflector::<GenericKey<Variable>>();

    super::set_reflector(&mut registry, || {
        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.bind(|| assert_ron_snapshot!(variable_key));
    });

    // Test serialization of GenericKey with AdditiveKey type parameter
    let additive_key = DynamicBox(smallbox!(GenericKey::<AdditiveKey>::new(
        "test_additive".to_string()
    )));

    let mut registry2 = super::Registry::default();
    registry2.register_reflector::<GenericKey<AdditiveKey>>();

    super::set_reflector(&mut registry2, || {
        let mut settings = Settings::clone_current();
        settings.set_sort_maps(true);
        settings.bind(|| assert_ron_snapshot!(additive_key));
    });
}

#[test]
fn deserialize_generic_key() {
    // Test with GenericKey<Variable>
    let original_variable_key = DynamicBox(smallbox!(
        GenericKey::<Variable>::new("test_variable".to_string())
    ));

    let mut registry = super::Registry::default();
    registry.register_reflector::<GenericKey<Variable>>();

    super::set_reflector(&mut registry, || {
        // Serialize the DynamicBox to RON format
        let ron_string = ron::to_string(&original_variable_key)
            .expect("Failed to serialize GenericKey<Variable>");

        // Deserialize the RON string back to a DynamicBox
        let deserialized_key: DynamicBox = ron::from_str(&ron_string)
            .expect("Failed to deserialize GenericKey<Variable>");

        // Verify that the deserialized key equals the original
        assert_eq!(original_variable_key, deserialized_key);

        // Verify that the inner value is correct by downcasting
        let original_generic = original_variable_key
            .0
            .any()
            .downcast_ref::<GenericKey<Variable>>()
            .expect("Original should be GenericKey<Variable>");
        let deserialized_generic = deserialized_key
            .0
            .any()
            .downcast_ref::<GenericKey<Variable>>()
            .expect("Deserialized should be GenericKey<Variable>");

        assert_eq!(original_generic, deserialized_generic);
        assert_eq!(original_generic.0, "test_variable");
        assert_eq!(deserialized_generic.0, "test_variable");
    });

    // Test with GenericKey<AdditiveKey>
    let original_additive_key = DynamicBox(smallbox!(
        GenericKey::<AdditiveKey>::new("test_additive".to_string())
    ));

    let mut registry2 = super::Registry::default();
    registry2.register_reflector::<GenericKey<AdditiveKey>>();

    super::set_reflector(&mut registry2, || {
        // Serialize the DynamicBox to RON format
        let ron_string = ron::to_string(&original_additive_key)
            .expect("Failed to serialize GenericKey<AdditiveKey>");

        // Deserialize the RON string back to a DynamicBox
        let deserialized_key: DynamicBox = ron::from_str(&ron_string)
            .expect("Failed to deserialize GenericKey<AdditiveKey>");

        // Verify that the deserialized key equals the original
        assert_eq!(original_additive_key, deserialized_key);

        // Verify that the inner value is correct by downcasting
        let original_generic = original_additive_key
            .0
            .any()
            .downcast_ref::<GenericKey<AdditiveKey>>()
            .expect("Original should be GenericKey<AdditiveKey>");
        let deserialized_generic = deserialized_key
            .0
            .any()
            .downcast_ref::<GenericKey<AdditiveKey>>()
            .expect("Deserialized should be GenericKey<AdditiveKey>");

        assert_eq!(original_generic, deserialized_generic);
        assert_eq!(original_generic.0, "test_additive");
        assert_eq!(deserialized_generic.0, "test_additive");
    });
}
