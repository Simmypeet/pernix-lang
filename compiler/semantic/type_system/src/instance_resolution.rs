//! Contains the logic for resolving `instance`.
//!
//! The module for resolving an `instance` when the `instance` term is left
//! out for inference.

use std::sync::Arc;

use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::get_global_instances_of, import::get_import_map,
    trait_ref::get_trait_ref,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::{get_closest_module_id, scope_walker},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::{InstanceParameterID, get_generic_parameters},
    instance::{Instance, TraitRef},
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

use crate::{
    OverflowError,
    environment::{self, Environment},
    normalizer::Normalizer,
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
    InInstance(Global<pernixc_symbol::ID>),

    /// The query site is currently in the `trait` or `instance`, the associated
    /// instances of the said `trait` or `instance` are also considered.
    FromAssociatedInstance(Global<pernixc_symbol::ID>),

    /// The instance is available from because the instance parameter can be
    /// seen in the current scope.
    FromInstanceParameter(InstanceParameterID),
}

/// Contains a list of instances available in a certain scope.
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
    pub current_site: Global<pernixc_symbol::ID>,
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

        // if the current scope itself is an insace, add it to the list
        if kind == Kind::Instance {
            available_instances.push(LexicalInstance::InInstance(scope_id));
        }

        // if this kind of symbol can have generic parameters, extract the
        // instance parameters
        if kind.has_generic_parameters() {
            let generic_params = engine.get_generic_parameters(scope_id).await;

            for instance_param in generic_params.instance_parameter_order() {
                available_instances.push(
                    LexicalInstance::FromInstanceParameter(
                        InstanceParameterID::new(scope_id, instance_param),
                    ),
                );
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
#[value(Interned<[Global<pernixc_symbol::ID>]>)]
pub struct GlobalInstanceCandidatesKey {
    /// The site where the global instance candidates are being queried.
    pub current_site: Global<pernixc_symbol::ID>,

    /// The trait ID in which instance candidates must implement.
    pub trait_id: Global<pernixc_symbol::ID>,
}

#[executor(config = Config)]
async fn global_instance_candidates_executor(
    key: &GlobalInstanceCandidatesKey,
    engine: &TrackedEngine,
) -> Interned<[Global<pernixc_symbol::ID>]> {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(unused)]
struct ResolveInstance {
    trait_ref: TraitRef,
}

/// List of error that can occur during instance resolution.
#[derive(Debug, Clone)]
pub enum ResolveError {
    /// Couldn't find any instance that matches the expected trait reference.
    NotFound,

    /// The instance ended requiring itself, causing a cycle in the resolution
    /// process.
    Cyclic,

    /// There are multiple instances that matches the expected trait reference,
    /// so it's ambiguous
    Ambiguous(Arc<[ResolvedInstance]>),
}

impl environment::Query for ResolveInstance {
    type InProgress = ();
    type Result = Result<Arc<ResolvedInstance>, ResolveError>;

    fn query<'x, N: crate::normalizer::Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> environment::BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            let lexical_instances = resolve_from_lexical_instances(
                environment.premise().query_site,
                &self.trait_ref,
                environment,
            )
            .await?;

            // ambiguous if more than 1 instance found
            if lexical_instances.len() > 1 {
                return Ok(Err(ResolveError::Ambiguous(
                    lexical_instances.into(),
                )));
            }

            if lexical_instances.len() == 1 {
                return Ok(Ok(Arc::new(
                    lexical_instances.into_iter().next().unwrap(),
                )));
            }

            Ok(Err(ResolveError::NotFound))
        })
    }

    fn on_cyclic(
        &self,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[environment::Call<environment::DynArc, environment::DynArc>],
    ) -> Self::Result {
        Err(ResolveError::Cyclic)
    }
}

/// The source of the resolved instance, used for error reporting.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InstanceSource {
    /// Comes from the instance parameters surrounding the query site.
    FromInstanceParameterID(InstanceParameterID),

    /// The query site is made inside an instance, so the instance itself is
    /// available.
    FromInstanceScope(Global<pernixc_symbol::ID>),
}

/// Represents an instance that has been choosen as the result of instance
/// resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedInstance {
    instance: Instance,
    source: InstanceSource,
    is_not_general_enough: bool,
}

async fn resolve_from_lexical_instances<N: Normalizer>(
    current_site: Global<pernixc_symbol::ID>,
    expected_trait_ref: &TraitRef,
    environment: &Environment<'_, N>,
) -> Result<Vec<ResolvedInstance>, OverflowError> {
    let inherent_instance_scope = environment
        .tracked_engine()
        .query(&LexicalInstanceCandidatesKey { current_site })
        .await;

    let mut instances = Vec::new();

    for inherent_instance in &inherent_instance_scope.available_instances {
        match inherent_instance {
            LexicalInstance::InInstance(global) => {
                // malformed
                let Some(trait_ref) =
                    environment.tracked_engine().get_trait_ref(*global).await
                else {
                    continue;
                };

                // if different trait, skip
                if trait_ref.trait_id() != expected_trait_ref.trait_id() {
                    continue;
                }

                let Some(_) = environment
                    .subtypes_generic_arguments(
                        expected_trait_ref.generic_arguments(),
                        trait_ref.generic_arguments(),
                    )
                    .await?
                else {
                    continue;
                };

                let Some(result) = environment
                    .deduce(
                        expected_trait_ref.generic_arguments(),
                        trait_ref.generic_arguments(),
                    )
                    .await?
                else {
                    continue;
                };

                let generic_parameters = environment
                    .tracked_engine()
                    .get_generic_parameters(*global)
                    .await;

                let generic_arguments = result
                    .result
                    .instantiation
                    .create_generic_arguments(*global, &generic_parameters);

                instances.push(ResolvedInstance {
                    instance: Instance::Symbol(Symbol::new(
                        *global,
                        generic_arguments,
                    )),
                    source: InstanceSource::FromInstanceScope(*global),
                    is_not_general_enough: result.result.is_not_general_enough,
                });
            }

            LexicalInstance::FromInstanceParameter(member_id) => {
                let generic_params = environment
                    .tracked_engine()
                    .get_generic_parameters(member_id.parent_id())
                    .await;

                let Some(trait_ref) = generic_params
                    .get_instance_parameter(member_id.id())
                    .trait_ref()
                    .cloned()
                else {
                    continue;
                };

                // if different trait, skip
                if trait_ref.trait_id() != expected_trait_ref.trait_id() {
                    continue;
                }

                let Some(inst) = environment
                    .subtypes_generic_arguments(
                        expected_trait_ref.generic_arguments(),
                        trait_ref.generic_arguments(),
                    )
                    .await?
                else {
                    continue;
                };

                instances.push(ResolvedInstance {
                    instance: Instance::Parameter(*member_id),
                    source: InstanceSource::FromInstanceParameterID(*member_id),
                    is_not_general_enough: !inst
                        .result
                        .forall_lifetime_errors
                        .is_empty(),
                });
            }

            LexicalInstance::FromAssociatedInstance(_) => todo!(),
        }
    }

    Ok(instances)
}
