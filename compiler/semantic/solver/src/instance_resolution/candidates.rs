use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::get_global_instances_of, import::get_import_map,
};
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
    member::get_members,
    parent::{get_closest_module_id, scope_walker},
};
use pernixc_type::generic_parameters::{
    GenericParameterID, get_generic_parameters2,
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
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
    Identifiable,
    Encode,
    Decode,
)]
pub enum LexicalInstanceCandidate {
    InstanceScope(GlobalSymbolID),
    Associated(GlobalSymbolID),
    InstanceParameter(GenericParameterID),
}

#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Identifiable, Encode, Decode,
)]
pub struct LexicalInstanceCandidates {
    candidates: Interned<[LexicalInstanceCandidate]>,
}

impl LexicalInstanceCandidates {
    #[must_use]
    pub fn new(
        candidates: impl IntoIterator<Item = LexicalInstanceCandidate>,
        engine: &TrackedEngine,
    ) -> Self {
        Self {
            candidates: engine
                .intern_unsized(candidates.into_iter().collect::<Vec<_>>()),
        }
    }

    #[cfg(test)]
    pub(crate) fn new_duplicating(
        candidates: impl IntoIterator<Item = LexicalInstanceCandidate>,
    ) -> Self {
        Self {
            candidates: Interned::new_duplicating_unsized(
                candidates.into_iter().collect::<Vec<_>>(),
            ),
        }
    }

    pub(crate) fn iter(
        &self,
    ) -> impl Iterator<Item = LexicalInstanceCandidate> + '_ {
        self.candidates.iter().copied()
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
    StableHash,
    Encode,
    Decode,
    Query,
)]
#[value(Interned<LexicalInstanceCandidates>)]
pub struct LexicalInstanceCandidatesKey {
    pub current_site: GlobalSymbolID,
}

#[executor(config = Config)]
async fn lexical_instance_candidates_executor(
    key: &LexicalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<LexicalInstanceCandidates> {
    let mut walker = engine.scope_walker(key.current_site);
    let mut candidates = Vec::new();

    while let Some(id) = walker.next().await {
        let scope = key.current_site.target_id.make_global(id);
        let kind = engine.get_kind(scope).await;
        if kind == Kind::Instance {
            candidates.push(LexicalInstanceCandidate::InstanceScope(scope));
        }
        if kind.has_generic_parameters() {
            let parameters = engine.get_generic_parameters2(scope).await;
            candidates.extend(parameters.instance_parameters().map(
                |(parameter, _)| {
                    LexicalInstanceCandidate::InstanceParameter(
                        GenericParameterID::new(scope, parameter),
                    )
                },
            ));
        }
        if matches!(kind, Kind::Trait | Kind::Instance) {
            for member in engine.get_members(scope).await.all_ids() {
                let member = scope.target_id.make_global(member);
                if matches!(
                    engine.get_kind(member).await,
                    Kind::TraitAssociatedInstance
                        | Kind::InstanceAssociatedInstance
                ) {
                    candidates
                        .push(LexicalInstanceCandidate::Associated(member));
                }
            }
        }
    }

    engine.intern(LexicalInstanceCandidates::new(candidates, engine))
}

#[distributed_slice(PERNIX_PROGRAM)]
static LEXICAL_INSTANCE_CANDIDATES_EXECUTOR: Registration<Config> =
    Registration::new::<
        LexicalInstanceCandidatesKey,
        LexicalInstanceCandidatesExecutor,
    >();

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
    Encode,
    Decode,
    Query,
)]
#[value(Interned<[GlobalSymbolID]>)]
#[extend(name = get_global_instance_candidates2, by_val)]
pub struct GlobalInstanceCandidatesKey {
    pub current_site: GlobalSymbolID,
    pub trait_id: GlobalSymbolID,
}

#[executor(config = Config)]
async fn global_instance_candidates_executor(
    key: &GlobalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<[GlobalSymbolID]> {
    let mut candidates = engine
        .get_global_instances_of(key.trait_id, key.current_site.target_id)
        .await
        .iter()
        .copied()
        .collect::<Vec<_>>();

    let module = key
        .current_site
        .target_id
        .make_global(engine.get_closest_module_id(key.current_site).await);
    for imported in engine.get_import_map(module).await.values() {
        if engine.get_kind(imported.id).await == Kind::Instance {
            candidates.push(imported.id);
        }
    }

    candidates.sort_unstable();
    candidates.dedup();
    engine.intern_unsized(candidates)
}

#[distributed_slice(PERNIX_PROGRAM)]
static GLOBAL_INSTANCE_CANDIDATES_EXECUTOR: Registration<Config> =
    Registration::new::<
        GlobalInstanceCandidatesKey,
        GlobalInstanceCandidatesExecutor,
    >();
