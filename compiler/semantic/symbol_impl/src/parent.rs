use std::sync::Arc;

use linkme::distributed_slice;
use pernixc_hash::HashMap;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{ID, get_target_root_module_id, parent::Key};
use pernixc_target::{Global, TargetID};
use pernixc_tokio::{chunk::chunk_for_tasks, join_set::JoinSet};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
};

use crate::table::{self, MapKey, TableKey};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Option<Option<pernixc_symbol::ID>>)]
pub struct ProjectionKey {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config, style = qbice::ExecutionStyle::Projection)]
async fn projection_executor(
    key: &ProjectionKey,
    engine: &TrackedEngine,
) -> Option<Option<pernixc_symbol::ID>> {
    let intermediate =
        engine.query(&IntermediateKey(key.symbol_id.target_id)).await;

    intermediate.get(&key.symbol_id.id).copied().map(Some)
}

#[distributed_slice(PERNIX_PROGRAM)]
static PROJECTION_EXECUTOR: Registration<Config> =
    Registration::new::<ProjectionKey, ProjectionExecutor>();

/// The executor for the [`Parent`] component.
#[executor(config = Config)]
async fn parent_executor(key: &Key, engine: &TrackedEngine) -> Option<ID> {
    let symbol_id = key.symbol_id;
    if symbol_id.id
        == engine.get_target_root_module_id(symbol_id.target_id).await
    {
        return None;
    }

    engine.query(&ProjectionKey { symbol_id: key.symbol_id }).await.unwrap()
}

#[distributed_slice(PERNIX_PROGRAM)]
static PARENT_EXECUTOR: Registration<Config> =
    Registration::new::<Key, ParentExecutor>();

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Encode,
    Decode,
    StableHash,
    Query,
)]
#[value(Arc<HashMap<ID, ID>>)]
pub struct IntermediateKey(pub TargetID);

#[executor(config = Config, style = qbice::ExecutionStyle::Firewall)]
async fn intermediate_executor(
    key: &IntermediateKey,
    engine: &TrackedEngine,
) -> Arc<HashMap<ID, ID>> {
    let target_id = key.0;
    let map = engine.query(&MapKey(target_id)).await;

    let symbols = map.keys_by_symbol_id.keys().copied().collect::<Vec<_>>();
    let mut handles = JoinSet::default();

    for chunk in symbols.chunk_for_tasks().map(<[pernixc_symbol::ID]>::to_vec) {
        let target_id = key.0;
        let map = map.clone();
        let engine = engine.clone();

        handles.spawn(async move {
            let mut key_and_members = Vec::new();
            for symbol in chunk {
                let table_key = map
                    .keys_by_symbol_id
                    .get(&symbol)
                    .unwrap()
                    .as_ref()
                    .map_or_else(
                        || table::Key::Root(target_id),
                        |x| table::Key::Submodule {
                            external_submodule: x.clone(),
                            target_id,
                        },
                    );

                let table = engine.query(&TableKey(table_key)).await;

                key_and_members.push(table.members.get(&symbol).map(
                    |members| {
                        (
                            symbol,
                            members
                                .member_ids_by_name
                                .values()
                                .copied()
                                .chain(members.unnameds.iter().copied())
                                .collect::<Vec<_>>(),
                        )
                    },
                ));
            }

            key_and_members
        });
    }

    let mut parent_map = HashMap::default();

    while let Some(key_and_members) = handles.next().await {
        for (symbol, members) in key_and_members.into_iter().flatten() {
            for member in members {
                assert!(parent_map.insert(member, symbol).is_none());
            }
        }
    }

    Arc::new(parent_map)
}

#[distributed_slice(PERNIX_PROGRAM)]
static INTERMEDIATE_EXECUTOR: Registration<Config> =
    Registration::new::<IntermediateKey, IntermediateExecutor>();
