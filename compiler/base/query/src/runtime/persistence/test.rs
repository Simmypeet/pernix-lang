use std::sync::Arc;

use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    database::InputMetadata,
    fingerprint::fingerprint,
    runtime::persistence::{
        self,
        backend::Backend,
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

fn basic_template<B: Backend>() {
    const COUNT: usize = 10;

    let tempdir = tempfile::tempdir().unwrap();

    let mut serde_registry = SelfRegistry::new();
    serde_registry.register::<Key>();

    let serde_registry = Arc::new(serde_registry);
    let initial_seed = 42;

    {
        let mut persistence = persistence::Persistence::<B>::new(
            tempdir.path().to_owned(),
            serde_registry.clone(),
        )
        .unwrap();

        for i in 0..COUNT {
            let string = format!("can you load this {i}");
            let value_fingerprint = fingerprint(initial_seed, &string);

            persistence.save_value::<Key>(value_fingerprint, string.clone());

            let key = Key(i);

            persistence.save_value_metadata::<Key>(
                fingerprint(initial_seed, &key),
                crate::database::ValueMetadata::Input(InputMetadata {
                    fingerprint: value_fingerprint,
                    updated_at: i as u64,
                }),
            );
        }

        persistence.commit();
    }

    // try load again
    {
        let persistence = persistence::Persistence::<B>::new(
            tempdir.path().to_owned(),
            serde_registry,
        )
        .unwrap();

        for i in 0..COUNT {
            let string = format!("can you load this {i}");
            let value_fingerprint = fingerprint(initial_seed, &string);

            let loaded_value: String = persistence
                .load_value::<Key>(value_fingerprint)
                .unwrap()
                .unwrap();

            assert_eq!(loaded_value, string);

            let loaded_metadata: InputMetadata = persistence
                .load_value_metadata::<Key>(fingerprint(initial_seed, &Key(i)))
                .unwrap()
                .unwrap()
                .into_input()
                .unwrap();

            assert_eq!(loaded_metadata.fingerprint, value_fingerprint);
            assert_eq!(loaded_metadata.updated_at, i as u64);
        }
    }
}

#[test]
fn basic_redb() { basic_template::<backend::redb::RedbBackend>(); }

#[test]
fn basic_fjall() { basic_template::<backend::fjall::FjallBackend>(); }
