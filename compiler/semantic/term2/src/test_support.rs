use std::sync::Arc;

use pernixc_qbice::{Engine, InMemoryFactory};
use qbice::{serialize::Plugin, stable_hash::SeededStableHasherBuilder};

pub(crate) async fn create_test_engine() -> Arc<Engine> {
    Arc::new(
        Engine::new_with(
            Plugin::default(),
            InMemoryFactory,
            SeededStableHasherBuilder::new(0),
        )
        .await
        .unwrap(),
    )
}
