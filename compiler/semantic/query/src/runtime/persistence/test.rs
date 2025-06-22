use std::{process::Command, sync::Arc};

use pernixc_query_derive::Key;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    database::map::Map,
    fingerprint,
    runtime::{
        persistence::Persistence,
        serde::{DynamicRegistry, SelfRegistry},
    },
};

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Key,
    Serialize,
    Deserialize,
)]
#[value(String)]
pub struct Variable(pub String);

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Key,
    Serialize,
    Deserialize,
)]
#[value(String)]
pub struct Constant(pub String);

#[test]
fn save_and_load_value() {
    let mut serde_config = SelfRegistry::default();
    serde_config.register::<Variable>();

    let initial_string_value = "some not so secret value anothe sa".to_string();
    let initial_fingerprint = fingerprint::fingerprint(&initial_string_value);

    let tmp_dir = tempfile::tempdir().unwrap();

    let persistence =
        Persistence::new(tmp_dir.path().to_path_buf(), Arc::new(serde_config));

    persistence.save::<Variable>(&initial_string_value).unwrap();

    let loaded_string_value =
        persistence.try_load::<Variable>(initial_fingerprint).unwrap().unwrap();

    assert_eq!(initial_string_value, loaded_string_value);

    Command::new("tree")
        .arg(tmp_dir.path())
        .status()
        .expect("Failed to run tree command");
}

#[test]
#[allow(clippy::similar_names)]
fn save_and_load_entire_map() {
    let mut serde_config = SelfRegistry::default();
    serde_config.register::<Variable>();
    serde_config.register::<Constant>();

    let initial_variable_a_value = "12".to_string();
    let initial_variable_b_value = "34".to_string();

    let initial_constant_pi_value = "3.14".to_string();
    let initial_constant_e_value = "2.718".to_string();

    let a_hash = fingerprint::fingerprint(&initial_variable_a_value);
    let b_hash = fingerprint::fingerprint(&initial_variable_b_value);

    let pi_hash = fingerprint::fingerprint(&initial_constant_pi_value);
    let e_hash = fingerprint::fingerprint(&initial_constant_e_value);

    let map = Map::default();

    map.insert(Variable("a".to_string()), initial_variable_a_value.clone());
    map.insert(Variable("b".to_string()), initial_variable_b_value.clone());

    map.insert(Constant("pi".to_string()), initial_constant_pi_value.clone());
    map.insert(Constant("e".to_string()), initial_constant_e_value.clone());

    let tmp_dir = tempfile::tempdir().unwrap();

    let persistence =
        Persistence::new(tmp_dir.path().to_path_buf(), Arc::new(serde_config));

    persistence.serialize_map(&map).unwrap();

    let loaded_a = persistence.try_load::<Variable>(a_hash).unwrap().unwrap();
    let loaded_b = persistence.try_load::<Variable>(b_hash).unwrap().unwrap();

    let loaded_pi = persistence.try_load::<Constant>(pi_hash).unwrap().unwrap();
    let loaded_e = persistence.try_load::<Constant>(e_hash).unwrap().unwrap();

    assert_eq!(initial_variable_a_value, loaded_a);
    assert_eq!(initial_variable_b_value, loaded_b);
    assert_eq!(initial_constant_pi_value, loaded_pi);
    assert_eq!(initial_constant_e_value, loaded_e);

    Command::new("tree")
        .arg(tmp_dir.path())
        .status()
        .expect("Failed to run tree command");
}
