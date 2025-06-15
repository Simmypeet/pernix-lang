use std::any::TypeId;

use pernixc_serialize::{
    binary::{de::BinaryDeserializer, ser::BinarySerializer},
    Deserialize, Serialize,
};

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

#[test]
fn basic_round_trip() {
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
