use std::any::TypeId;

use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;

use crate::{
    database::map::Map,
    runtime::persistence::serde::{DynamicRegistry, SelfRegistry},
    Key,
};

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
    StableHash,
)]
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
    StableHash,
)]
#[value(i32)]
pub struct Constant(String);

/// Additional types with generic parameters that implement Identifiable
/// Wrapper for String type implementing Identifiable
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
    StableHash,
)]
#[value(String)]
pub struct StringWrapper(pub String);

/// Wrapper for i32 type implementing Identifiable
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
    StableHash,
)]
#[value(i32)]
pub struct I32Wrapper(pub i32);

/// Wrapper for bool type implementing Identifiable
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
    StableHash,
)]
#[value(bool)]
pub struct BoolWrapper(pub bool);

/// Generic key with type parameter T that implements Identifiable
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
    StableHash,
)]
#[value(StringWrapper)]
pub struct GenericKey<
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + StableHash,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default
            + StableHash,
    > Default for GenericKey<T>
{
    fn default() -> Self { Self(T::default()) }
}

/// Generic key with bool value type
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
    StableHash,
)]
#[value(BoolWrapper)]
pub struct GenericBoolKey<
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + StableHash,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default
            + StableHash,
    > Default for GenericBoolKey<T>
{
    fn default() -> Self { Self(T::default()) }
}

/// Generic key with i32 value type
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
    StableHash,
)]
#[value(I32Wrapper)]
pub struct GenericI32Key<
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + StableHash,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default
            + StableHash,
    > Default for GenericI32Key<T>
{
    fn default() -> Self { Self(T::default()) }
}

/// Multi-parameter generic key
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
    StableHash,
)]
#[value(StringWrapper)]
pub struct MultiGenericKey<T, U>(T, U)
where
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + StableHash,
    U: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + StableHash;

impl<T, U> Default for MultiGenericKey<T, U>
where
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + Default
        + StableHash,
    U: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + Default
        + StableHash,
{
    fn default() -> Self { Self(T::default(), U::default()) }
}

#[test]
fn map_basic_round_trip() {
    let mut serde_registry = SelfRegistry::default();
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();

    let dynamic_map = Map::default();
    dynamic_map.insert(Variable("x".to_string()), 1);
    dynamic_map.insert(Constant("c".to_string()), 42);

    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    dynamic_map.serialize(&mut binary_serializer, &serde_registry).unwrap();

    buffer = dbg!(binary_serializer.into_inner());

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));

    let deserialized_map: Map =
        Map::deserialize(&mut deserializer, &serde_registry).unwrap();

    assert_eq!(deserialized_map.get(&Variable("x".to_string())), Some(1));
    assert_eq!(deserialized_map.get(&Constant("c".to_string())), Some(42));

    assert_eq!(deserialized_map.type_lens(), 2);

    assert!(deserialized_map.has_type_id(TypeId::of::<Variable>()));
    assert!(deserialized_map.has_type_id(TypeId::of::<Constant>()));
}

#[test]
fn dynamic_box_basic_round_trip() {
    use smallbox::smallbox;

    use crate::key::DynamicKey;

    let mut serde_registry = SelfRegistry::default();
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();

    // Test with Variable
    let variable_key = Variable("test_var".to_string());
    let variable_dynamic_box = DynamicKey(smallbox!(variable_key.clone()));

    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    variable_dynamic_box
        .serialize(&mut binary_serializer, &serde_registry)
        .unwrap();

    buffer = binary_serializer.into_inner();

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));
    let deserialized_variable_box: DynamicKey =
        DynamicKey::deserialize(&mut deserializer, &serde_registry).unwrap();

    // Verify the deserialized dynamic box contains the correct value
    assert_eq!(
        deserialized_variable_box.stable_type_id(),
        variable_dynamic_box.stable_type_id()
    );
    let deserialized_variable =
        deserialized_variable_box.any().downcast_ref::<Variable>().unwrap();
    assert_eq!(deserialized_variable, &variable_key);

    // Test with Constant
    let constant_key = Constant("test_const".to_string());
    let constant_dynamic_box = DynamicKey(smallbox!(constant_key.clone()));

    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    constant_dynamic_box
        .serialize(&mut binary_serializer, &serde_registry)
        .unwrap();

    buffer = binary_serializer.into_inner();

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));
    let deserialized_constant_box: DynamicKey =
        DynamicKey::deserialize(&mut deserializer, &serde_registry).unwrap();

    // Verify the deserialized dynamic box contains the correct value
    assert_eq!(
        deserialized_constant_box.stable_type_id(),
        constant_dynamic_box.stable_type_id()
    );
    let deserialized_constant =
        deserialized_constant_box.any().downcast_ref::<Constant>().unwrap();
    assert_eq!(deserialized_constant, &constant_key);
}

#[test]
fn dynamic_box_multiple_types_round_trip() {
    use smallbox::smallbox;

    use crate::key::DynamicKey;

    let mut serde_registry = SelfRegistry::default();
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();

    // Create multiple dynamic boxes with different types
    let variable_box = DynamicKey(smallbox!(Variable("var1".to_string())));
    let constant_box = DynamicKey(smallbox!(Constant("const1".to_string())));
    let another_variable_box =
        DynamicKey(smallbox!(Variable("var2".to_string())));

    let dynamic_boxes =
        vec![&variable_box, &constant_box, &another_variable_box];
    let mut deserialized_boxes = Vec::new();

    // Serialize and deserialize each box
    for dynamic_box in &dynamic_boxes {
        let mut buffer = Vec::new();
        let mut binary_serializer = BinarySerializer::new(buffer);
        dynamic_box.serialize(&mut binary_serializer, &serde_registry).unwrap();

        buffer = binary_serializer.into_inner();

        let mut deserializer =
            BinaryDeserializer::new(std::io::Cursor::new(buffer));
        let deserialized_box: DynamicKey =
            DynamicKey::deserialize(&mut deserializer, &serde_registry)
                .unwrap();

        deserialized_boxes.push(deserialized_box);
    }

    // Verify all boxes were correctly deserialized
    assert_eq!(deserialized_boxes.len(), 3);

    // Check first box (Variable)
    assert_eq!(
        deserialized_boxes[0].stable_type_id(),
        variable_box.stable_type_id()
    );
    let var1 = deserialized_boxes[0].any().downcast_ref::<Variable>().unwrap();
    assert_eq!(var1, &Variable("var1".to_string()));

    // Check second box (Constant)
    assert_eq!(
        deserialized_boxes[1].stable_type_id(),
        constant_box.stable_type_id()
    );
    let const1 =
        deserialized_boxes[1].any().downcast_ref::<Constant>().unwrap();
    assert_eq!(const1, &Constant("const1".to_string()));

    // Check third box (Variable)
    assert_eq!(
        deserialized_boxes[2].stable_type_id(),
        another_variable_box.stable_type_id()
    );
    let var2 = deserialized_boxes[2].any().downcast_ref::<Variable>().unwrap();
    assert_eq!(var2, &Variable("var2".to_string()));
}

#[test]
fn map_generic_types_round_trip() {
    let mut serde_registry = SelfRegistry::default();

    // Register concrete instances of generic types
    serde_registry.register::<GenericKey<StringWrapper>>();
    serde_registry.register::<GenericBoolKey<I32Wrapper>>();
    serde_registry.register::<GenericI32Key<BoolWrapper>>();
    serde_registry.register::<MultiGenericKey<StringWrapper, I32Wrapper>>();

    let dynamic_map = Map::default();

    // Insert values with generic keys
    let generic_key1 = GenericKey(StringWrapper("test1".to_string()));
    let generic_bool_key = GenericBoolKey(I32Wrapper(42));
    let generic_i32_key = GenericI32Key(BoolWrapper(true));
    let multi_generic_key =
        MultiGenericKey(StringWrapper("multi".to_string()), I32Wrapper(100));

    dynamic_map
        .insert(generic_key1.clone(), StringWrapper("value1".to_string()));
    dynamic_map.insert(generic_bool_key.clone(), BoolWrapper(false));
    dynamic_map.insert(generic_i32_key.clone(), I32Wrapper(999));
    dynamic_map.insert(
        multi_generic_key.clone(),
        StringWrapper("multi_value".to_string()),
    );

    // Serialize the map
    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    dynamic_map.serialize(&mut binary_serializer, &serde_registry).unwrap();

    buffer = binary_serializer.into_inner();

    // Deserialize the map
    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));

    let deserialized_map: Map =
        Map::deserialize(&mut deserializer, &serde_registry).unwrap();

    // Verify all generic key-value pairs were correctly deserialized
    assert_eq!(
        deserialized_map.get(&generic_key1),
        Some(StringWrapper("value1".to_string()))
    );
    assert_eq!(
        deserialized_map.get(&generic_bool_key),
        Some(BoolWrapper(false))
    );
    assert_eq!(deserialized_map.get(&generic_i32_key), Some(I32Wrapper(999)));
    assert_eq!(
        deserialized_map.get(&multi_generic_key),
        Some(StringWrapper("multi_value".to_string()))
    );

    // Verify we have all 4 types in the map
    assert_eq!(deserialized_map.type_lens(), 4);

    // Verify type IDs are preserved
    assert!(
        deserialized_map.has_type_id(TypeId::of::<GenericKey<StringWrapper>>())
    );
    assert!(deserialized_map
        .has_type_id(TypeId::of::<GenericBoolKey<I32Wrapper>>()));
    assert!(deserialized_map
        .has_type_id(TypeId::of::<GenericI32Key<BoolWrapper>>()));
    assert!(deserialized_map.has_type_id(TypeId::of::<
        MultiGenericKey<StringWrapper, I32Wrapper>,
    >()));
}

#[test]
fn dynamic_box_generic_types_round_trip() {
    use smallbox::smallbox;

    use crate::key::DynamicKey;

    let mut serde_registry = SelfRegistry::default();

    // Register concrete instances of generic types
    serde_registry.register::<GenericKey<StringWrapper>>();
    serde_registry.register::<GenericBoolKey<I32Wrapper>>();
    serde_registry.register::<GenericI32Key<BoolWrapper>>();
    serde_registry.register::<MultiGenericKey<StringWrapper, I32Wrapper>>();

    // Create generic keys
    let generic_key1 = GenericKey(StringWrapper("test_generic1".to_string()));
    let generic_bool_key = GenericBoolKey(I32Wrapper(123));
    let generic_i32_key = GenericI32Key(BoolWrapper(false));
    let multi_generic_key = MultiGenericKey(
        StringWrapper("multi_test".to_string()),
        I32Wrapper(456),
    );

    // Create dynamic boxes with generic types
    let generic_box1 = DynamicKey(smallbox!(generic_key1.clone()));
    let generic_bool_box = DynamicKey(smallbox!(generic_bool_key.clone()));
    let generic_i32_box = DynamicKey(smallbox!(generic_i32_key.clone()));
    let multi_generic_box = DynamicKey(smallbox!(multi_generic_key.clone()));

    let dynamic_boxes = vec![
        &generic_box1,
        &generic_bool_box,
        &generic_i32_box,
        &multi_generic_box,
    ];
    let mut deserialized_boxes = Vec::new();

    // Serialize and deserialize each box
    for dynamic_box in &dynamic_boxes {
        let mut buffer = Vec::new();
        let mut binary_serializer = BinarySerializer::new(buffer);
        dynamic_box.serialize(&mut binary_serializer, &serde_registry).unwrap();

        buffer = binary_serializer.into_inner();

        let mut deserializer =
            BinaryDeserializer::new(std::io::Cursor::new(buffer));
        let deserialized_box: DynamicKey =
            DynamicKey::deserialize(&mut deserializer, &serde_registry)
                .unwrap();

        deserialized_boxes.push(deserialized_box);
    }

    // Verify all boxes were correctly deserialized
    assert_eq!(deserialized_boxes.len(), 4);

    // Check first box (GenericKey<StringWrapper>)
    assert_eq!(
        deserialized_boxes[0].stable_type_id(),
        generic_box1.stable_type_id()
    );
    let deserialized_generic1 = deserialized_boxes[0]
        .any()
        .downcast_ref::<GenericKey<StringWrapper>>()
        .unwrap();
    assert_eq!(deserialized_generic1, &generic_key1);

    // Check second box (GenericBoolKey<I32Wrapper>)
    assert_eq!(
        deserialized_boxes[1].stable_type_id(),
        generic_bool_box.stable_type_id()
    );
    let deserialized_generic_bool = deserialized_boxes[1]
        .any()
        .downcast_ref::<GenericBoolKey<I32Wrapper>>()
        .unwrap();
    assert_eq!(deserialized_generic_bool, &generic_bool_key);

    // Check third box (GenericI32Key<BoolWrapper>)
    assert_eq!(
        deserialized_boxes[2].stable_type_id(),
        generic_i32_box.stable_type_id()
    );
    let deserialized_generic_i32 = deserialized_boxes[2]
        .any()
        .downcast_ref::<GenericI32Key<BoolWrapper>>()
        .unwrap();
    assert_eq!(deserialized_generic_i32, &generic_i32_key);

    // Check fourth box (MultiGenericKey<StringWrapper, I32Wrapper>)
    assert_eq!(
        deserialized_boxes[3].stable_type_id(),
        multi_generic_box.stable_type_id()
    );
    let deserialized_multi_generic = deserialized_boxes[3]
        .any()
        .downcast_ref::<MultiGenericKey<StringWrapper, I32Wrapper>>()
        .unwrap();
    assert_eq!(deserialized_multi_generic, &multi_generic_key);
}

#[test]
fn dynamic_box_mixed_generic_and_simple_types_round_trip() {
    use smallbox::smallbox;

    use crate::key::DynamicKey;

    let mut serde_registry = SelfRegistry::default();

    // Register both simple and generic types
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();
    serde_registry.register::<GenericKey<StringWrapper>>();
    serde_registry.register::<MultiGenericKey<I32Wrapper, BoolWrapper>>();

    // Create mixed types
    let variable = Variable("mixed_test".to_string());
    let constant = Constant("mixed_const".to_string());
    let generic_key = GenericKey(StringWrapper("mixed_generic".to_string()));
    let multi_generic_key = MultiGenericKey(I32Wrapper(789), BoolWrapper(true));

    // Create dynamic boxes with mixed types
    let variable_box = DynamicKey(smallbox!(variable.clone()));
    let constant_box = DynamicKey(smallbox!(constant.clone()));
    let generic_box = DynamicKey(smallbox!(generic_key.clone()));
    let multi_generic_box = DynamicKey(smallbox!(multi_generic_key.clone()));

    let dynamic_boxes =
        vec![&variable_box, &constant_box, &generic_box, &multi_generic_box];
    let mut deserialized_boxes = Vec::new();

    // Serialize and deserialize each box
    for dynamic_box in &dynamic_boxes {
        let mut buffer = Vec::new();
        let mut binary_serializer = BinarySerializer::new(buffer);
        dynamic_box.serialize(&mut binary_serializer, &serde_registry).unwrap();

        buffer = binary_serializer.into_inner();

        let mut deserializer =
            BinaryDeserializer::new(std::io::Cursor::new(buffer));
        let deserialized_box: DynamicKey =
            DynamicKey::deserialize(&mut deserializer, &serde_registry)
                .unwrap();

        deserialized_boxes.push(deserialized_box);
    }

    // Verify all boxes were correctly deserialized
    assert_eq!(deserialized_boxes.len(), 4);

    // Check simple types first
    let deserialized_var =
        deserialized_boxes[0].any().downcast_ref::<Variable>().unwrap();
    assert_eq!(deserialized_var, &variable);

    let deserialized_const =
        deserialized_boxes[1].any().downcast_ref::<Constant>().unwrap();
    assert_eq!(deserialized_const, &constant);

    // Check generic types
    let deserialized_generic = deserialized_boxes[2]
        .any()
        .downcast_ref::<GenericKey<StringWrapper>>()
        .unwrap();
    assert_eq!(deserialized_generic, &generic_key);

    let deserialized_multi_generic = deserialized_boxes[3]
        .any()
        .downcast_ref::<MultiGenericKey<I32Wrapper, BoolWrapper>>()
        .unwrap();
    assert_eq!(deserialized_multi_generic, &multi_generic_key);

    // Verify stable type IDs are preserved correctly
    assert_eq!(
        deserialized_boxes[0].stable_type_id(),
        variable_box.stable_type_id()
    );
    assert_eq!(
        deserialized_boxes[1].stable_type_id(),
        constant_box.stable_type_id()
    );
    assert_eq!(
        deserialized_boxes[2].stable_type_id(),
        generic_box.stable_type_id()
    );
    assert_eq!(
        deserialized_boxes[3].stable_type_id(),
        multi_generic_box.stable_type_id()
    );
}

#[test]
fn map_different_generic_instantiations_round_trip() {
    let mut serde_registry = SelfRegistry::default();

    // Register different instantiations of the same generic type
    serde_registry.register::<GenericKey<StringWrapper>>();
    serde_registry.register::<GenericKey<I32Wrapper>>();
    serde_registry.register::<GenericKey<BoolWrapper>>();

    let dynamic_map = Map::default();

    // Insert values with different generic instantiations
    let generic_string_key =
        GenericKey(StringWrapper("string_key".to_string()));
    let generic_i32_key = GenericKey(I32Wrapper(42));
    let generic_bool_key = GenericKey(BoolWrapper(true));

    dynamic_map.insert(
        generic_string_key.clone(),
        StringWrapper("result1".to_string()),
    );
    dynamic_map
        .insert(generic_i32_key.clone(), StringWrapper("result2".to_string()));
    dynamic_map
        .insert(generic_bool_key.clone(), StringWrapper("result3".to_string()));

    // Serialize the map
    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    dynamic_map.serialize(&mut binary_serializer, &serde_registry).unwrap();

    buffer = binary_serializer.into_inner();

    // Deserialize the map
    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));

    let deserialized_map: Map =
        Map::deserialize(&mut deserializer, &serde_registry).unwrap();

    // Verify all different generic instantiations were preserved
    assert_eq!(
        deserialized_map.get(&generic_string_key),
        Some(StringWrapper("result1".to_string()))
    );
    assert_eq!(
        deserialized_map.get(&generic_i32_key),
        Some(StringWrapper("result2".to_string()))
    );
    assert_eq!(
        deserialized_map.get(&generic_bool_key),
        Some(StringWrapper("result3".to_string()))
    );

    // Verify we have 3 different types in the map (different generic
    // instantiations)
    assert_eq!(deserialized_map.type_lens(), 3);

    // Verify each generic instantiation has its own type ID
    assert!(
        deserialized_map.has_type_id(TypeId::of::<GenericKey<StringWrapper>>())
    );
    assert!(
        deserialized_map.has_type_id(TypeId::of::<GenericKey<I32Wrapper>>())
    );
    assert!(
        deserialized_map.has_type_id(TypeId::of::<GenericKey<BoolWrapper>>())
    );

    // Verify they are indeed different type IDs
    assert_ne!(
        TypeId::of::<GenericKey<StringWrapper>>(),
        TypeId::of::<GenericKey<I32Wrapper>>()
    );
    assert_ne!(
        TypeId::of::<GenericKey<I32Wrapper>>(),
        TypeId::of::<GenericKey<BoolWrapper>>()
    );
    assert_ne!(
        TypeId::of::<GenericKey<StringWrapper>>(),
        TypeId::of::<GenericKey<BoolWrapper>>()
    );
}
