use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::get_global_instances_of, import::get_import_map,
};
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
    parent::get_closest_module_id,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

/// A key for querying the inherent instances available in a certain scope.
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
    Decode,
    Encode,
    Query,
)]
#[value(Interned<[GlobalSymbolID]>)]
#[extend(name = get_global_instance_candidates, by_val)]
pub struct GlobalInstanceCandidatesKey {
    /// The site where the global instance candidates are being queried.
    pub current_site: GlobalSymbolID,

    /// The trait ID in which instance candidates must implement.
    pub trait_id: GlobalSymbolID,
}

#[executor(config = Config)]
async fn global_instance_candidates_executor(
    key: &GlobalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<[GlobalSymbolID]> {
    let mut candidates = Vec::default();

    let available_instances = engine
        .get_global_instances_of(key.trait_id, key.current_site.target_id)
        .await;

    candidates.extend(available_instances.iter().copied());

    // look at the current module and see for any imports
    let current_module_id = key
        .current_site
        .target_id
        .make_global(engine.get_closest_module_id(key.current_site).await);

    for symbol_id in
        engine.get_import_map(current_module_id).await.values().map(|x| x.id)
    {
        // if the imported symbol is an instance, add it to the candidates
        if engine.get_kind(symbol_id).await == Kind::Instance {
            candidates.push(symbol_id);
        }
    }

    candidates.sort();
    candidates.dedup();

    engine.intern_unsized(candidates)
}

#[distributed_slice(PERNIX_PROGRAM)]
static GLOBAL_INSTANCE_CANDIDATES_EXECUTOR: Registration<Config> =
    Registration::new::<
        GlobalInstanceCandidatesKey,
        GlobalInstanceCandidatesExecutor,
    >();
