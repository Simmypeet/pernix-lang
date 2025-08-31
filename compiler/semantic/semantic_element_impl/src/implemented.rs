use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::implements::get_implements;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;
use pernixc_target::{get_all_target_ids, Global, TargetID};
use pernixc_tokio::scoped;

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
    Identifiable,
)]
pub struct ImplementationFilter;

impl pernixc_symbol::kind::Filter for ImplementationFilter {
    async fn filter(&self, kind: pernixc_symbol::kind::Kind) -> bool {
        kind.is_implementation()
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Key,
)]
#[value(Arc<HashSet<Global<pernixc_symbol::ID>>>)]
pub struct InTargetKey {
    pub implementable_id: Global<pernixc_symbol::ID>,
    pub target_id: TargetID,
}

#[pernixc_query::executor(key(InTargetKey), name(InTargetExecutor))]
pub async fn implemented_in_target_executor(
    key: &InTargetKey,
    engine: &TrackedEngine,
) -> Result<
    Arc<HashSet<Global<pernixc_symbol::ID>>>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let implementations = engine
        .query(&pernixc_symbol::kind::FilterKey {
            target_id: key.target_id,
            filter: ImplementationFilter,
        })
        .await?;

    // SAFETY: invokes implements calculation in parallel
    unsafe {
        engine.start_parallel();
    }

    let result: Result<_, executor::CyclicError> =
        scoped!(|scoped| async move {
            let mut results = HashSet::default();

            for implementation in
                implementations.iter().map(|x| key.target_id.make_global(*x))
            {
                let engine = engine.clone();

                scoped.spawn(async move {
                    engine.get_implements(implementation).await
                });
            }

            while let Some(result) = scoped.next().await {
                let Some(result) = result? else {
                    continue;
                };

                if result == key.implementable_id {
                    results.insert(result);
                }
            }

            Ok(Arc::new(results))
        });

    // SAFETY: invokes implements calculation in parallel
    unsafe {
        engine.end_parallel();
    }

    result
}

#[pernixc_query::executor(
    key(pernixc_semantic_element::implemented::Key),
    name(Executor)
)]
pub async fn implemented_executor(
    key: &pernixc_semantic_element::implemented::Key,
    engine: &TrackedEngine,
) -> Result<
    Arc<HashSet<Global<pernixc_symbol::ID>>>,
    pernixc_query::runtime::executor::CyclicError,
> {
    let target_ids = engine.get_all_target_ids().await;

    // SAFETY: invokes implements calculation in parallel
    unsafe {
        engine.start_parallel();
    }

    let result: Result<_, executor::CyclicError> =
        scoped!(|scoped| async move {
            let mut results = HashSet::default();

            for target_id in target_ids.iter() {
                let engine = engine.clone();
                let key = InTargetKey {
                    implementable_id: key.0,
                    target_id: *target_id,
                };

                scoped.spawn(async move { engine.query(&key).await });
            }

            while let Some(result) = scoped.next().await {
                let result = result?;

                results.extend(result.iter().copied());
            }

            Ok(Arc::new(results))
        });

    // SAFETY: invokes implements calculation in parallel
    unsafe {
        engine.end_parallel();
    }

    result
}
