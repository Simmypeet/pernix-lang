use linkme::distributed_slice;
use pernixc_hash::FxHashSet;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::InTargetKey, trait_ref::get_trait_ref_of_instance_symbol,
};
use pernixc_symbol::{
    external_instance::is_external_instance, get_all_instance_ids,
};
use pernixc_target::{Global, TargetID, get_linked_targets};
use pernixc_tokio::{chunk::chunk_for_tasks, join_set::JoinSet};
use qbice::{executor, program::Registration, storage::intern::Interned};

#[executor(config = Config)]
pub async fn in_target_executor(
    key: &InTargetKey,
    engine: &TrackedEngine,
) -> Interned<FxHashSet<Global<pernixc_symbol::SymbolID>>> {
    if key.target_id == TargetID::CORE {
        assert_ne!(
            key.trait_id.target_id,
            TargetID::CORE,
            "core library should've explicitly specified implementations for \
             its own traits and markers "
        );

        // core library will never implement traits or markers from other
        // targets
        return engine.intern(FxHashSet::default());
    }

    let instances = engine.get_all_instance_ids(key.target_id).await;

    let mut scoped = JoinSet::new();
    let mut results = FxHashSet::default();

    // PARALLEL: invokes implements calculation in parallel
    unsafe {
        engine.start_unordered_callee_group();
    }

    let expected_trait_id = key.trait_id;

    for implementation in instances.chunk_for_tasks().map(|x| {
        x.iter()
            .copied()
            .map(|x| key.target_id.make_global(x))
            .collect::<Vec<_>>()
    }) {
        let engine = engine.clone();

        scoped.spawn(async move {
            let mut results = Vec::new();

            for implementation in implementation {
                let Some(trait_ref) = engine
                    .get_trait_ref_of_instance_symbol(implementation)
                    .await
                else {
                    continue;
                };

                if engine.is_external_instance(implementation).await {
                    continue;
                }

                if trait_ref.trait_id() == expected_trait_id {
                    results.push(implementation);
                }
            }

            results
        });
    }

    while let Some(result) = scoped.next().await {
        results.extend(result);
    }

    // End of parallel section
    unsafe {
        engine.end_unordered_callee_group();
    }

    engine.intern_unsized(results)
}

#[distributed_slice(PERNIX_PROGRAM)]
static IN_TARGET_EXECUTOR: Registration<Config> =
    Registration::new::<InTargetKey, InTargetExecutor>();

#[distributed_slice(PERNIX_PROGRAM)]
static GLOBAL_INSTANCES_EXECUTOR: Registration<Config> = Registration::new::<
    pernixc_semantic_element::global_instances::Key,
    GlobalInstancesExecutor,
>();

#[executor(config = Config)]
pub async fn global_instances_executor(
    key: &pernixc_semantic_element::global_instances::Key,
    engine: &TrackedEngine,
) -> Interned<FxHashSet<Global<pernixc_symbol::SymbolID>>> {
    let mut scoped = JoinSet::new();
    let mut results = FxHashSet::default();

    let depedencies = engine.get_linked_targets(key.target_id).await;

    // PARALLEL: invokes implements calculation in parallel
    unsafe {
        engine.start_unordered_callee_group();
    }

    for target_id in depedencies.iter() {
        let engine = engine.clone();
        let key = pernixc_semantic_element::global_instances::Key {
            trait_id: key.trait_id,
            target_id: *target_id,
        };

        scoped.spawn(async move { engine.query(&key).await });
    }

    {
        let engine = engine.clone();
        let key =
            InTargetKey { trait_id: key.trait_id, target_id: key.target_id };

        scoped.spawn(async move { engine.query(&key).await });
    }

    while let Some(result) = scoped.next().await {
        results.extend(result.iter().copied());
    }

    unsafe {
        engine.end_unordered_callee_group();
    }

    engine.intern(results)
}
