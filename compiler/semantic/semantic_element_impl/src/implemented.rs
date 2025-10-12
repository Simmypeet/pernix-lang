use std::sync::Arc;

use pernixc_hash::HashSet;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_semantic_element::{
    implemented::InTargetKey, implements::get_implements,
};
use pernixc_symbol::get_all_implements_ids;
use pernixc_target::{get_all_target_ids, Global, TargetID};
use pernixc_tokio::scoped;

#[pernixc_query::executor(key(InTargetKey), name(InTargetExecutor))]
pub async fn implemented_in_target_executor(
    key: &InTargetKey,
    engine: &TrackedEngine,
) -> Result<
    Arc<HashSet<Global<pernixc_symbol::ID>>>,
    pernixc_query::runtime::executor::CyclicError,
> {
    if key.target_id == TargetID::CORE {
        assert_ne!(
            key.implementable_id.target_id,
            TargetID::CORE,
            "core library should've explicitly specified implementations for \
             its own traits and markers "
        );

        // core library will never implement traits or markers from other
        // targets
        return Ok(Arc::new(HashSet::default()));
    }

    let implementations = engine.get_all_implements_ids(key.target_id).await;

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
                    engine
                        .get_implements(implementation)
                        .await
                        .map(|x| (implementation, x))
                });
            }

            while let Some(result) = scoped.next().await {
                let (implementation_id, Some(implemented_id)) = result? else {
                    continue;
                };

                if implemented_id == key.implementable_id {
                    results.insert(implementation_id);
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

pernixc_register::register!(InTargetKey, InTargetExecutor);

pernixc_register::register!(
    pernixc_semantic_element::implemented::Key,
    Executor
);

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
