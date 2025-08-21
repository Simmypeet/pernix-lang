use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    fingerprint::fingerprint,
    runtime::persistence::{
        self, backend,
        serde::{DynamicRegistry, SelfRegistry},
    },
};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    crate::Key,
)]
#[value(String)]
pub struct Key(usize);

#[test]
fn basic() {
    let tempdir = tempfile::tempdir().unwrap();

    let mut serde_registry = SelfRegistry::new();
    serde_registry.register::<Key>();
    let serde_registry = Arc::new(serde_registry);

    let target_string = "can you load this".to_string();
    let initial_seed = 12345;

    {
        let mut persistence =
            persistence::Persistence::<backend::redb::RedbBackend>::new(
                tempdir.path().to_owned(),
                serde_registry.clone(),
            )
            .unwrap();

        persistence.save_value::<Key>(
            fingerprint(initial_seed, &target_string),
            target_string.clone(),
        );

        persistence.commit();
    }

    // try load again
    {
        let persistence =
            persistence::Persistence::<backend::redb::RedbBackend>::new(
                tempdir.path().to_owned(),
                serde_registry,
            )
            .unwrap();

        let loaded_value: String = persistence
            .load_value::<Key>(fingerprint(initial_seed, &target_string))
            .unwrap()
            .unwrap();

        assert_eq!(loaded_value, target_string);
    }
}
