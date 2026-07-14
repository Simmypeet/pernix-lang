use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_symbol::{
    GlobalSymbolID,
    kind::{Kind, get_kind},
    member::get_members,
    parent::scope_walker,
};
use pernixc_type::{
    generic_parameters::{GenericParameterID, get_generic_parameters2},
    r#type::kind::TyKind,
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

/// An instance available in a certain scope.
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
)]
pub enum LexicalInstance {
    /// The query site is already inside an instance symbol.
    InInstance(GlobalSymbolID),

    /// The query site is currently in a trait or instance, making its
    /// associated instances available.
    FromAssociatedInstance(GlobalSymbolID),

    /// An instance generic parameter visible at the query site.
    FromInstanceParameter(GenericParameterID),
}

/// Contains the instances available in a certain scope.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Decode,
    Encode,
    Identifiable,
    StableHash,
)]
pub struct LexicalInstanceCandidates {
    available_instances: Vec<LexicalInstance>,
}

impl LexicalInstanceCandidates {
    /// Returns the instances available in the queried scope.
    #[must_use]
    pub fn available_instances(&self) -> &[LexicalInstance] {
        &self.available_instances
    }
}

/// A key for querying the lexical instances available in a certain scope.
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
#[value(Interned<LexicalInstanceCandidates>)]
pub struct LexicalInstanceCandidatesKey {
    /// The site where the lexical instances are being queried.
    pub current_site: GlobalSymbolID,
}

#[executor(config = Config)]
async fn lexical_instance_candidates_executor(
    key: &LexicalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<LexicalInstanceCandidates> {
    let mut scope_walker = engine.scope_walker(key.current_site);
    let mut available_instances = Vec::new();

    while let Some(scope_id) = scope_walker.next().await {
        let scope_id = key.current_site.target_id.make_global(scope_id);
        let kind = engine.get_kind(scope_id).await;

        if kind == Kind::Instance {
            available_instances.push(LexicalInstance::InInstance(scope_id));
        }

        if kind.has_generic_parameters() {
            let generic_parameters =
                engine.get_generic_parameters2(scope_id).await;

            for (parameter_id, parameter) in generic_parameters.iter() {
                if parameter.kind() == TyKind::Instance {
                    available_instances.push(
                        LexicalInstance::FromInstanceParameter(
                            GenericParameterID::new(scope_id, parameter_id),
                        ),
                    );
                }
            }
        }

        if matches!(kind, Kind::Trait | Kind::Instance) {
            let members = engine.get_members(scope_id).await;

            for member in
                members.all_ids().map(|id| scope_id.target_id.make_global(id))
            {
                if matches!(
                    engine.get_kind(member).await,
                    Kind::InstanceAssociatedInstance
                        | Kind::TraitAssociatedInstance
                ) {
                    available_instances
                        .push(LexicalInstance::FromAssociatedInstance(member));
                }
            }
        }
    }

    engine.intern(LexicalInstanceCandidates { available_instances })
}

#[distributed_slice(PERNIX_PROGRAM)]
static LEXICAL_INSTANCE_CANDIDATES_EXECUTOR: Registration<Config> =
    Registration::new::<
        LexicalInstanceCandidatesKey,
        LexicalInstanceCandidatesExecutor,
    >();
