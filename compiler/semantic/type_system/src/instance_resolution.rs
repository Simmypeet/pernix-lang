//! Contains the logic for resolving `instance`.
//!
//! The module for resolving an `instance` when the `instance` term is left
//! out for inference.

use std::{
    ops::{Deref, Not},
    sync::Arc,
};

use derive_more::From;
use linkme::distributed_slice;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_semantic_element::{
    global_instances::get_global_instances_of, import::get_import_map,
    instance_associated_value::get_instance_associated_value,
    trait_ref::get_trait_ref_of_instance_symbol,
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
    parent::{get_closest_module_id, get_parent_global, scope_walker},
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        GenericParameters, InstanceParameter, InstanceParameterID,
        get_generic_parameters,
    },
    instance::{Instance, TraitRef},
    instantiation::{
        Instantiation, create_generic_arguments_from_instantiation,
    },
    lifetime::Lifetime,
    r#type::Type,
};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, executor,
    program::Registration, storage::intern::Interned,
};

use crate::{
    OverflowError,
    environment::{self, Environment},
    normalizer::Normalizer,
    order::{Order, get_instance_order},
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

        // we're in the scope of a `trait` or `instance`, so the associated
        // instances are also available.
        if matches!(kind, Kind::Trait | Kind::Instance) {
            let members = engine.get_members(scope_id).await;

            for member in
                members.all_ids().map(|x| scope_id.target_id.make_global(x))
            {
                let member_kind = engine.get_kind(member).await;

                if matches!(
                    member_kind,
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
#[extend(name = get_global_instance_candidates, by_val)]
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
    Ambiguous(Arc<[InstanceSource]>),

    /// The instance resolution ended up requiring another instance resolution,
    /// which has failed with the given error.
    Recursive(Arc<RecursiveError>),
}

/// The instance resolution ended up requiring another instance resolution,
/// which has failed with the given error.
#[derive(Debug, Clone)]
#[allow(unused)]
pub struct RecursiveError {
    current_trait_ref: TraitRef,
    resolving_symbol: Global<pernixc_symbol::ID>,

    recurse_instance_parameter_id: pernixc_arena::ID<InstanceParameter>,
    recurse_error: ResolveError,
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
                    lexical_instances.into_iter().map(|x| x.source).collect(),
                )));
            }

            if lexical_instances.len() == 1 {
                return Ok(Ok(Arc::new(
                    lexical_instances.into_iter().next().unwrap(),
                )));
            }

            // if no lexical instance found, we look for global candidates
            environment.handle_global_resolution(&self.trait_ref).await
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

    /// Comes from the associated instances of the `trait` or `instance` in
    /// which the query site is in.
    FromAssociatedInstance(Global<pernixc_symbol::ID>),

    /// Comes from global instance candidates.
    FromGlobalInstance(Global<pernixc_symbol::ID>),

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
}

#[allow(clippy::too_many_lines)]
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
                let Some(trait_ref) = environment
                    .tracked_engine()
                    .get_trait_ref_of_instance_symbol(*global)
                    .await
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

                if result.result.is_not_general_enough {
                    continue;
                }

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

                if inst.result.forall_lifetime_errors.is_empty().not() {
                    continue;
                }

                instances.push(ResolvedInstance {
                    instance: Instance::Parameter(*member_id),
                    source: InstanceSource::FromInstanceParameterID(*member_id),
                });
            }

            LexicalInstance::FromAssociatedInstance(global_id) => {
                if let Some(resolved) = environment
                    .handle_lexical_associated_instance(
                        *global_id,
                        expected_trait_ref,
                    )
                    .await?
                {
                    instances.push(resolved);
                }
            }
        }
    }

    Ok(instances)
}

#[derive(Debug, Clone, From)]
enum ResolveSymbolError {
    Incompatible,
    Recursive(Arc<RecursiveError>),
    OverflowError(OverflowError),
}

enum ResolvedCandidate {
    Perfect(Global<pernixc_symbol::ID>, Instantiation),
    WithError(Arc<RecursiveError>),
}

impl ResolvedCandidate {
    fn symbol_id(&self) -> Global<pernixc_symbol::ID> {
        match self {
            Self::Perfect(symbol_id, _) => *symbol_id,
            Self::WithError(err) => err.resolving_symbol,
        }
    }
}

struct CurrentCandidate {
    head: ResolvedCandidate,
    amb: Vec<ResolvedCandidate>,
}

impl<N: Normalizer> Environment<'_, N> {
    /// Resolves an instance with the given expected trait reference.
    pub async fn resolve_instance(
        &self,
        expected_trait_ref: &TraitRef,
    ) -> Result<Result<Arc<ResolvedInstance>, ResolveError>, OverflowError>
    {
        self.query(&ResolveInstance { trait_ref: expected_trait_ref.clone() })
            .await
    }
}

impl<N: Normalizer> Environment<'_, N> {
    async fn infer_instance(
        &self,
        expected_trait_ref: &GenericArguments,
        source_trait_ref: &GenericArguments,
    ) -> Result<Option<Instantiation>, OverflowError> {
        let deduction =
            self.deduce(source_trait_ref, expected_trait_ref).await?;

        let Some(deduction) = deduction else {
            return Ok(None);
        };

        if deduction.result.is_not_general_enough {
            return Ok(None);
        }

        Ok(Some(deduction.result.instantiation))
    }

    /// In case of [`LexicalInstance::FromAssociatedInstance`], we deduce the
    /// `trait_ref` of the associated instance with the expected `trait_ref`.
    /// It's possible that it includes the generic parameters of the parent
    /// symbol (from `trait` or `instance`). If that's the case, it's illegal
    /// because we can only manipulate the generic parameters of the instance
    /// itself.
    fn check_parent_generic_parameters(
        parent_symbol_id: Global<pernixc_symbol::ID>,
        generic_parameters: &GenericParameters,
        deduction: &mut Instantiation,
    ) -> bool {
        for lt in generic_parameters.lifetime_parameter_order() {
            let lt = Lifetime::new_parameter(parent_symbol_id, lt);

            let Some(_) = deduction.remove_lifetime_mapping(&lt) else {
                continue;
            };
        }

        for ty in generic_parameters.type_parameter_order() {
            let ty = Type::new_parameter(parent_symbol_id, ty);
            let Some(removed) = deduction.remove_type_mapping(&ty) else {
                continue;
            };

            if removed != ty {
                return false;
            }
        }

        for const_ in generic_parameters.constant_parameter_order() {
            let const_ = Constant::new_parameter(parent_symbol_id, const_);
            let Some(removed) = deduction.remove_constant_mapping(&const_)
            else {
                continue;
            };

            if removed != const_ {
                return false;
            }
        }

        for inst in generic_parameters.instance_parameter_order() {
            let inst = Instance::new_parameter(parent_symbol_id, inst);
            let Some(removed) = deduction.remove_instance_mapping(&inst) else {
                continue;
            };

            if removed != inst {
                return false;
            }
        }

        true
    }

    fn verify_instance_deduction(
        symbol_id: Global<pernixc_symbol::ID>,
        generic_parameters: &GenericParameters,
        deduction: &Instantiation,
        require_inst_all_deduced: bool,
    ) -> bool {
        for lt in generic_parameters.lifetime_parameter_order() {
            if deduction
                .get_lifetime_mapping(&Lifetime::new_parameter(symbol_id, lt))
                .is_none()
            {
                return false;
            }
        }

        for ty in generic_parameters.type_parameter_order() {
            if deduction
                .get_type_mapping(&Type::new_parameter(symbol_id, ty))
                .is_none()
            {
                return false;
            }
        }

        for const_ in generic_parameters.constant_parameter_order() {
            if deduction
                .get_constant_mapping(&Constant::new_parameter(
                    symbol_id, const_,
                ))
                .is_none()
            {
                return false;
            }
        }

        if require_inst_all_deduced {
            for inst in generic_parameters.instance_parameter_order() {
                if deduction
                    .get_instance_mapping(&Instance::new_parameter(
                        symbol_id, inst,
                    ))
                    .is_none()
                {
                    return false;
                }
            }
        }

        true
    }

    async fn recursive_resolve_instance(
        &self,
        symbol_id: Global<pernixc_symbol::ID>,
        symbol_generic_params: &GenericParameters,
        current_expected_trait_ref: &TraitRef,
        mut deduced: Instantiation,
    ) -> Result<Instantiation, ResolveSymbolError> {
        // NOTE: we iterate through the instance parameters **in order**, so
        // that the dependency between instance parameters is respected.
        for (instance_param_id, instance_param) in
            symbol_generic_params.instance_parameters_as_order()
        {
            let instance_param_term =
                Instance::new_parameter(symbol_id, instance_param_id);

            // if has already deduced, skip
            if deduced.get_instance_mapping(&instance_param_term).is_some() {
                continue;
            }

            // extract the expected `trait_ref` for the missing instance
            // parameter.
            let Some(mut trait_ref) =
                instance_param.trait_ref().map(|x| x.deref().clone())
            else {
                // `None` means malformed. we provide error term to the
                // deduction result.
                deduced.insert_instance_mapping(
                    instance_param_term,
                    Instance::new_error(),
                );

                continue;
            };

            // IMPORTANT: instantiate trait_ref to the latest known deduction.
            trait_ref.instantiate(&deduced);

            let resolved =
                match self.query(&ResolveInstance { trait_ref }).await? {
                    Ok(resolved) => resolved,
                    Err(err) => {
                        return Err(ResolveSymbolError::Recursive(Arc::new(
                            RecursiveError {
                                current_trait_ref: current_expected_trait_ref
                                    .clone(),
                                resolving_symbol: symbol_id,

                                recurse_instance_parameter_id:
                                    instance_param_id,
                                recurse_error: err,
                            },
                        )));
                    }
                };

            // Add the resolved instance to the deduction result.
            deduced.insert_instance_mapping(
                instance_param_term,
                resolved.instance.clone(),
            );
        }

        Ok(deduced)
    }

    async fn deduce_instance_symbol(
        &self,
        symbol_id: Global<pernixc_symbol::ID>,
        expected_trait_ref: &TraitRef,
    ) -> Result<Instantiation, ResolveSymbolError> {
        let trait_ref = self
            .tracked_engine()
            .get_trait_ref_of_instance_symbol(symbol_id)
            .await;

        let Some(trait_ref) = trait_ref else {
            return Err(ResolveSymbolError::Incompatible);
        };

        let Some(inst) = self
            .infer_instance(
                expected_trait_ref.generic_arguments(),
                trait_ref.generic_arguments(),
            )
            .await?
        else {
            return Err(ResolveSymbolError::Incompatible);
        };

        let symbol_generic_parameters =
            self.tracked_engine().get_generic_parameters(symbol_id).await;

        // not all generic parameters of the instance symbol are deduced, so
        // it's not a valid candidate
        if !Self::verify_instance_deduction(
            symbol_id,
            &symbol_generic_parameters,
            &inst,
            false,
        ) {
            return Err(ResolveSymbolError::Incompatible);
        }

        // the resolution might require some additional instance parameters that
        // are not immediately deducible from the `expected_trait_ref`, we
        // recursively call instance resolution to deduce those instance
        // parameters
        let deduced = self
            .recursive_resolve_instance(
                symbol_id,
                &symbol_generic_parameters,
                expected_trait_ref,
                inst,
            )
            .await?;

        Ok(deduced)
    }

    async fn deduce_associated_instance_symbol(
        &self,
        symbol_id: Global<pernixc_symbol::ID>,
        expected_trait_ref: &TraitRef,
    ) -> Result<Option<Instantiation>, OverflowError> {
        let trait_ref = self
            .tracked_engine()
            .get_trait_ref_of_instance_symbol(symbol_id)
            .await;

        let Some(trait_ref) = trait_ref else { return Ok(None) };

        let Some(mut inst) = self
            .infer_instance(
                expected_trait_ref.generic_arguments(),
                trait_ref.generic_arguments(),
            )
            .await?
        else {
            return Ok(None);
        };

        let symbol_generic_parameters =
            self.tracked_engine().get_generic_parameters(symbol_id).await;

        // not all generic parameters of the instance symbol are deduced, so
        // it's not a valid candidate
        if !Self::verify_instance_deduction(
            symbol_id,
            &symbol_generic_parameters,
            &inst,
            true,
        ) {
            return Ok(None);
        }

        // check if the parent `trait/instance` generic parameters are not
        // involved in the deduction
        let parent_symbol_id = symbol_id.target_id.make_global(
            self.tracked_engine().get_closest_module_id(symbol_id).await,
        );

        if !Self::check_parent_generic_parameters(
            parent_symbol_id,
            &symbol_generic_parameters,
            &mut inst,
        ) {
            return Ok(None);
        }

        Ok(Some(inst))
    }

    async fn handle_lexical_associated_instance(
        &self,
        associated_instance_id: Global<pernixc_symbol::ID>,
        expected_trait_ref: &TraitRef,
    ) -> Result<Option<ResolvedInstance>, OverflowError> {
        let Some(inst) = self
            .deduce_associated_instance_symbol(
                associated_instance_id,
                expected_trait_ref,
            )
            .await?
        else {
            return Ok(None);
        };

        let parent_id = self
            .tracked_engine()
            .get_parent_global(associated_instance_id)
            .await
            .unwrap();

        let kind = self.tracked_engine().get_kind(associated_instance_id).await;

        match kind {
            Kind::TraitAssociatedInstance => {
                let generic_args = self
                    .tracked_engine()
                    .create_generic_arguments_from_instantiation(
                        associated_instance_id,
                        inst,
                    )
                    .await;

                Ok(Some(ResolvedInstance {
                    instance: Instance::new_instance_associated(
                        Box::new(Instance::new_anonymous_trait(parent_id)),
                        associated_instance_id,
                        generic_args,
                    ),
                    source: InstanceSource::FromAssociatedInstance(
                        associated_instance_id,
                    ),
                }))
            }

            Kind::InstanceAssociatedInstance => {
                let mut associated_instance_value = self
                    .tracked_engine()
                    .get_instance_associated_value(associated_instance_id)
                    .await
                    .deref()
                    .clone();

                inst.instantiate(&mut associated_instance_value);

                Ok(Some(ResolvedInstance {
                    instance: associated_instance_value,
                    source: InstanceSource::FromAssociatedInstance(
                        associated_instance_id,
                    ),
                }))
            }

            _ => unreachable!(),
        }
    }

    async fn handle_global_resolution(
        &self,
        expected_trait_ref: &TraitRef,
    ) -> Result<Result<Arc<ResolvedInstance>, ResolveError>, OverflowError>
    {
        let global_instance_ids = self
            .tracked_engine()
            .get_global_instance_candidates(
                self.premise().query_site,
                expected_trait_ref.trait_id(),
            )
            .await;

        let mut candidate: Option<CurrentCandidate> = None;

        for instance_id in global_instance_ids.iter().copied() {
            let resolved_candidate = match self
                .deduce_instance_symbol(instance_id, expected_trait_ref)
                .await
            {
                Ok(inst) => ResolvedCandidate::Perfect(instance_id, inst),

                Err(ResolveSymbolError::Incompatible) => {
                    continue;
                }

                Err(ResolveSymbolError::Recursive(err)) => {
                    ResolvedCandidate::WithError(err)
                }

                Err(ResolveSymbolError::OverflowError(err)) => return Err(err),
            };

            match &mut candidate {
                Some(current) => {
                    let order = self
                        .tracked_engine()
                        .get_instance_order(
                            current.head.symbol_id(),
                            resolved_candidate.symbol_id(),
                        )
                        .await?
                        .expect("shouldn't be malformed");

                    match order {
                        Order::Incompatible => {
                            panic!("should've atleast be compatible")
                        }

                        Order::MoreGeneral => {
                            // if current is more general, we replace current
                            // with the new candidate.
                            *current = CurrentCandidate {
                                head: resolved_candidate,
                                amb: vec![],
                            };
                        }

                        // current is more specific, no need to do anything
                        Order::MoreSpecific => {}

                        Order::Ambiguous => {
                            // add the new candidate to the ambiguity list
                            current.amb.push(resolved_candidate);
                        }
                    }
                }

                None => {
                    candidate = Some(CurrentCandidate {
                        head: resolved_candidate,
                        amb: Vec::new(),
                    });
                }
            }
        }

        let Some(candidate) = candidate else {
            return Ok(Err(ResolveError::NotFound));
        };

        if candidate.amb.is_empty().not() {
            return Ok(Err(ResolveError::Ambiguous(
                std::iter::once(candidate.head)
                    .chain(candidate.amb.into_iter())
                    .map(|x| InstanceSource::FromGlobalInstance(x.symbol_id()))
                    .collect(),
            )));
        }

        match candidate.head {
            ResolvedCandidate::Perfect(global, instantiation) => {
                let generic_args = self
                    .tracked_engine()
                    .create_generic_arguments_from_instantiation(
                        global,
                        instantiation,
                    )
                    .await;

                Ok(Ok(Arc::new(ResolvedInstance {
                    instance: Instance::new_symbol(global, generic_args),
                    source: InstanceSource::FromGlobalInstance(global),
                })))
            }

            ResolvedCandidate::WithError(recursive_error) => {
                Ok(Err(ResolveError::Recursive(recursive_error)))
            }
        }
    }
}
