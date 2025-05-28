use insta::{assert_ron_snapshot, Settings};
use serde::{de::DeserializeSeed, Deserialize, Serialize};

use crate::Key;

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
