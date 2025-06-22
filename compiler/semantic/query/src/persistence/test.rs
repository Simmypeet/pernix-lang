use std::sync::Arc;

use pernixc_query_derive::Key;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    fingerprint,
    persistence::Persistence,
    runtime::serde::{DynamicRegistry, SelfRegistry},
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
pub struct Key(pub String);

#[test]
fn save_and_load_value() {
    let mut serde_config = SelfRegistry::default();
    serde_config.register::<Key>();

    let initial_string_value = "some not so secret value anothe sa".to_string();
    let initial_fingerprint = fingerprint::fingerprint(&initial_string_value);

    let tmp_dir = tempfile::tempdir().unwrap();

    let persistence =
        Persistence::new(tmp_dir.path().to_path_buf(), Arc::new(serde_config));

    persistence.save::<Key>(&initial_string_value).unwrap();

    let loaded_string_value =
        persistence.try_load::<Key>(initial_fingerprint).unwrap().unwrap();

    assert_eq!(initial_string_value, loaded_string_value);
}
