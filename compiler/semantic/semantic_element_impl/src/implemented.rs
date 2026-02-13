use linkme::distributed_slice;
use pernixc_hash::HashSet;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    implemented::InTargetKey, implements::get_implements,
};
use pernixc_symbol::get_all_implements_ids;
use pernixc_target::{Global, TargetID, get_all_target_ids};
use pernixc_tokio::{chunk::chunk_for_tasks, scoped};
use qbice::{executor, program::Registration, storage::intern::Interned};

#[executor(config = Config)]
pub async fn in_target_executor(
    key: &InTargetKey,
    engine: &TrackedEngine,
) -> Interned<HashSet<Global<pernixc_symbol::ID>>> {
    if key.target_id == TargetID::CORE {
        assert_ne!(
            key.implementable_id.target_id,
            TargetID::CORE,
            "core library should've explicitly specified implementations for \
             its own traits and markers "
        );

        // core library will never implement traits or markers from other
        // targets
        return engine.intern(HashSet::default());
    }

    let implementations = engine.get_all_implements_ids(key.target_id).await;

    let result = scoped!(|scoped| async move {
        let mut results = HashSet::default();

        // PARALLEL: invokes implements calculation in parallel
        unsafe {
            engine.start_unordered_callee_group();
        }

        for implementation in implementations.chunk_for_tasks().map(|x| {
            x.iter().map(|x| key.target_id.make_global(*x)).collect::<Vec<_>>()
        }) {
            let engine = engine.clone();

            scoped.spawn(async move {
                let mut results = Vec::new();

                for implementation in implementation {
                    let Some(implemented_id) = engine
                        .get_implements(implementation)
                        .await
                        .map(|x| (implementation, x))
                    else {
                        continue;
                    };

                    results.push(implemented_id);
                }

                results
            });
        }

        while let Some(result) = scoped.next().await {
            for (implementation_id, implemented_id) in result {
                if implemented_id == key.implementable_id {
                    results.insert(implementation_id);
                }
            }
        }

        // End of parallel section
        unsafe {
            engine.end_unordered_callee_group();
        }

        engine.intern_unsized(results)
    });

    // PARALLEL: invokes implements calculation in parallel

    result
}

#[distributed_slice(PERNIX_PROGRAM)]
static IN_TARGET_EXECUTOR: Registration<Config> =
    Registration::new::<InTargetKey, InTargetExecutor>();

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLEMENTED_EXECUTOR: Registration<Config> = Registration::new::<
    pernixc_semantic_element::implemented::Key,
    ImplementedExecutor,
>();

#[executor(config = Config)]
pub async fn implemented_executor(
    key: &pernixc_semantic_element::implemented::Key,
    engine: &TrackedEngine,
) -> Interned<HashSet<Global<pernixc_symbol::ID>>> {
    let target_ids = engine.get_all_target_ids().await;

    let result = scoped!(|scoped| async move {
        let mut results = HashSet::default();

        // PARALLEL: invokes implements calculation in parallel
        unsafe {
            engine.start_unordered_callee_group();
        }

        for target_id in target_ids.iter() {
            let engine = engine.clone();
            let key = InTargetKey {
                implementable_id: key.symbol_id,
                target_id: *target_id,
            };

            scoped.spawn(async move { engine.query(&key).await });
        }

        while let Some(result) = scoped.next().await {
            results.extend(result.iter().copied());
        }

        unsafe {
            engine.end_unordered_callee_group();
        }

        engine.intern(results)
    });

    result
}
