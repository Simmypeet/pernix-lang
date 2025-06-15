use std::any::TypeId;

use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};
use pernixc_stable_type_id::Identifiable;

use crate::{
    database::map::Map,
    serde::{DynamicRegistry, SelfRegistry},
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
)]
#[value(StringWrapper)]
pub struct GenericKey<
    T: Identifiable + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default,
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
)]
#[value(BoolWrapper)]
pub struct GenericBoolKey<
    T: Identifiable + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default,
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
)]
#[value(I32Wrapper)]
pub struct GenericI32Key<
    T: Identifiable + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
>(T);

impl<
        T: Identifiable
            + Clone
            + Eq
            + std::hash::Hash
            + Send
            + Sync
            + 'static
            + Default,
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
)]
#[value(StringWrapper)]
pub struct MultiGenericKey<T, U>(T, U)
where
    T: Identifiable + Clone + Eq + std::hash::Hash + Send + Sync + 'static,
    U: Identifiable + Clone + Eq + std::hash::Hash + Send + Sync + 'static;

impl<T, U> Default for MultiGenericKey<T, U>
where
    T: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + Default,
    U: Identifiable
        + Clone
        + Eq
        + std::hash::Hash
        + Send
        + Sync
        + 'static
        + Default,
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
    dynamic_map.serialize(&mut binary_serializer, &mut serde_registry).unwrap();

    buffer = dbg!(binary_serializer.into_inner());

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));

    let deserialized_map: Map =
        Map::deserialize(&mut deserializer, &mut serde_registry).unwrap();

    assert_eq!(deserialized_map.get(&Variable("x".to_string())), Some(1));
    assert_eq!(deserialized_map.get(&Constant("c".to_string())), Some(42));

    assert_eq!(deserialized_map.type_lens(), 2);

    assert!(deserialized_map.has_type_id(TypeId::of::<Variable>()));
    assert!(deserialized_map.has_type_id(TypeId::of::<Constant>()));
}

#[test]
fn dynamic_box_basic_round_trip() {
    use smallbox::smallbox;

    use crate::key::DynamicBox;

    let mut serde_registry = SelfRegistry::default();
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();

    // Test with Variable
    let variable_key = Variable("test_var".to_string());
    let variable_dynamic_box = DynamicBox(smallbox!(variable_key.clone()));

    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    variable_dynamic_box
        .serialize(&mut binary_serializer, &mut serde_registry)
        .unwrap();

    buffer = binary_serializer.into_inner();

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));
    let deserialized_variable_box: DynamicBox =
        DynamicBox::deserialize(&mut deserializer, &mut serde_registry)
            .unwrap();

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
    let constant_dynamic_box = DynamicBox(smallbox!(constant_key.clone()));

    let mut buffer = Vec::new();
    let mut binary_serializer = BinarySerializer::new(buffer);
    constant_dynamic_box
        .serialize(&mut binary_serializer, &mut serde_registry)
        .unwrap();

    buffer = binary_serializer.into_inner();

    let mut deserializer =
        BinaryDeserializer::new(std::io::Cursor::new(buffer));
    let deserialized_constant_box: DynamicBox =
        DynamicBox::deserialize(&mut deserializer, &mut serde_registry)
            .unwrap();

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

    use crate::key::DynamicBox;

    let mut serde_registry = SelfRegistry::default();
    serde_registry.register::<Variable>();
    serde_registry.register::<Constant>();

    // Create multiple dynamic boxes with different types
    let variable_box = DynamicBox(smallbox!(Variable("var1".to_string())));
    let constant_box = DynamicBox(smallbox!(Constant("const1".to_string())));
    let another_variable_box =
        DynamicBox(smallbox!(Variable("var2".to_string())));

    let dynamic_boxes =
        vec![&variable_box, &constant_box, &another_variable_box];
    let mut deserialized_boxes = Vec::new();

    // Serialize and deserialize each box
    for dynamic_box in &dynamic_boxes {
        let mut buffer = Vec::new();
        let mut binary_serializer = BinarySerializer::new(buffer);
        dynamic_box
            .serialize(&mut binary_serializer, &mut serde_registry)
            .unwrap();

        buffer = binary_serializer.into_inner();

        let mut deserializer =
            BinaryDeserializer::new(std::io::Cursor::new(buffer));
        let deserialized_box: DynamicBox =
            DynamicBox::deserialize(&mut deserializer, &mut serde_registry)
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
