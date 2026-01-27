use std::{borrow::Cow, ops::Deref};

use linkme::distributed_slice;
use pernixc_arena::{Arena, ID};
use pernixc_handler::Storage;
use pernixc_hash::HashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::{Config, PERNIX_PROGRAM, TrackedEngine};
use pernixc_resolution::{
    Config as ResolutionConfig, ElidedTermProvider, ExtraNamespace,
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type,
};
use pernixc_semantic_element::{
    elided_lifetime,
    implied_predicate::{self, ImpliedPredicate},
    late_bound_lifetime,
    parameter::{self, Parameter, Parameters},
    return_type,
    where_clause::get_where_clause,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    parent::get_parent, syntax::get_function_signature_syntax,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{LifetimeParameter, get_generic_parameters},
    lifetime::{ElidedLifetime, ElidedLifetimeID, Lifetime},
    predicate::{Outlives, Predicate},
    tuple,
    r#type::Type,
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    environment::{Environment, get_active_premise},
    normalizer,
};
use qbice::{
    Decode, Encode, Query, StableHash, executor, program::Registration,
    storage::intern::Interned,
};

use crate::{
    Build,
    build::{self, Output},
    function_signature::diagnostic::Diagnostic,
    occurrences::Occurrences,
};

pub mod diagnostic;

#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct FunctionSignature {
    pub parameters: Interned<Parameters>,
    pub return_type: Interned<Type>,
    pub implied_predicates: Interned<HashSet<ImpliedPredicate>>,
    pub elided_lifetimes: Interned<Arena<ElidedLifetime>>,
    pub late_bound_lifetime_parameters:
        Interned<HashSet<ID<LifetimeParameter>>>,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, StableHash, Encode, Decode, Query,
)]
#[value(FunctionSignature)]
pub struct Key {
    pub symbol_id: Global<pernixc_symbol::ID>,
}

async fn create_parameters<
    T: Iterator<Item = pernixc_syntax::item::function::Parameter>,
>(
    engine: &TrackedEngine,
    id: Global<pernixc_symbol::ID>,
    extra_namespace: &ExtraNamespace,
    elided_lifetimes_provider: &mut ParametersElidedLifetimeProvider<'_>,
    occurrences: &mut Occurrences,
    storage: &Storage<Diagnostic>,
    parameters: T,
) -> Vec<(Type, RelativeSpan)> {
    let mut result = Vec::new();
    for parameter in parameters {
        let ty = match parameter.r#type() {
            Some(ty) => {
                engine
                    .resolve_type(
                        &ty,
                        ResolutionConfig::builder()
                            .referring_site(id)
                            .extra_namespace(extra_namespace)
                            .elided_lifetime_provider(elided_lifetimes_provider)
                            .observer(occurrences)
                            .build(),
                        storage,
                    )
                    .await
            }
            None => Type::Error(pernixc_term::error::Error),
        };

        result.push((ty, parameter.span()));
    }

    result
}

impl Build for Key {
    type Diagnostic = Diagnostic;

    #[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
    async fn execute(engine: &TrackedEngine, key: &Self) -> Output<Self> {
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;
        let mut elided_lifetimes = Arena::default();
        let mut occurrences = Occurrences::default();
        let mut elided_lifetimes_provider = ParametersElidedLifetimeProvider {
            global_id: key.symbol_id,
            elided_lifetimes: &mut elided_lifetimes,
            counter: 0,
        };
        let storage = Storage::<Diagnostic>::default();

        let (parameters_syn, return_type_syn) =
            engine.get_function_signature_syntax(key.symbol_id).await;

        let parameters = if let Some(parameters) = parameters_syn {
            create_parameters(
                engine,
                key.symbol_id,
                &extra_namespace,
                &mut elided_lifetimes_provider,
                &mut occurrences,
                &storage,
                parameters.parameters().filter_map(|x| x.into_regular().ok()),
            )
            .await
        } else {
            Vec::new()
        };

        let mut return_elided_lifetime_provider = (elided_lifetimes.len() == 1)
            .then(|| ReturnElidedLifetimeProvider {
                lifetime: Lifetime::Elided(ElidedLifetimeID {
                    parent_id: key.symbol_id,
                    id: elided_lifetimes.ids().next().unwrap(),
                }),
            });

        let return_type =
            if let Some(ty_syn) = return_type_syn.and_then(|x| x.r#type()) {
                let ty = engine
                    .resolve_type(
                        &ty_syn,
                        ResolutionConfig::builder()
                            .referring_site(key.symbol_id)
                            .maybe_elided_lifetime_provider(
                                return_elided_lifetime_provider
                                    .as_mut()
                                    .map(|x| x as _),
                            )
                            .extra_namespace(&extra_namespace)
                            .observer(&mut occurrences)
                            .build(),
                        &storage,
                    )
                    .await;

                Some((ty, ty_syn.span()))
            } else {
                None
            };

        let mut active_premise =
            engine
                .get_active_premise(key.symbol_id.target_id.make_global(
                    engine.get_parent(key.symbol_id).await.unwrap(),
                ))
                .await
                .deref()
                .clone();

        let where_clause = engine.get_where_clause(key.symbol_id).await;

        for predicate in where_clause.as_ref() {
            active_premise.predicates.insert(predicate.predicate.clone());
        }

        let mut implied_predicate_candidates =
            Vec::<(ImpliedPredicate, Option<RelativeSpan>, RelativeSpan)>::new(
            );

        for (ty, span) in parameters
            .iter()
            .map(|x| (&x.0, &x.1))
            .chain(return_type.as_ref().map(|x| (&x.0, &x.1)))
        {
            for ty in RecursiveIterator::new(ty) {
                let Some(ty) = ty.0.into_type().ok() else { continue };

                match ty {
                    Type::Symbol(symbol) => {
                        let where_clause =
                            engine.get_where_clause(symbol.id).await;

                        implied_predicate_candidates.extend(
                            where_clause.as_ref().iter().filter_map(
                                |x| match &x.predicate {
                                    Predicate::LifetimeOutlives(outlives) => {
                                        Some((
                                            ImpliedPredicate::LifetimeOutlives(
                                                outlives.clone(),
                                            ),
                                            x.span,
                                            *span,
                                        ))
                                    }
                                    Predicate::TypeOutlives(outlives) => {
                                        Some((
                                            ImpliedPredicate::TypeOutlives(
                                                outlives.clone(),
                                            ),
                                            x.span,
                                            *span,
                                        ))
                                    }
                                    _ => None,
                                },
                            ),
                        );
                    }
                    Type::Reference(reference) => {
                        implied_predicate_candidates.push((
                            ImpliedPredicate::TypeOutlives(Outlives::new(
                                (*reference.pointee).clone(),
                                reference.lifetime.clone(),
                            )),
                            None,
                            *span,
                        ));
                    }
                    _ => {}
                }
            }
        }

        let mut implied_predicates = HashSet::default();
        for (implied_predicate, declared_span, inst_span) in
            implied_predicate_candidates
        {
            let environment = Environment::new(
                Cow::Borrowed(&active_premise),
                Cow::Borrowed(engine),
                normalizer::NO_OP,
            );

            let result = match &implied_predicate {
                ImpliedPredicate::LifetimeOutlives(outlives) => {
                    environment.query(outlives).await
                }
                ImpliedPredicate::TypeOutlives(outlives) => {
                    environment.query(outlives).await
                }
            };

            match result {
                Ok(None) => {
                    implied_predicates.insert(implied_predicate);
                }
                Err(pernixc_type_system::Error::Overflow(error)) => {
                    error.report_as_undecidable_predicate(
                        implied_predicate.into(),
                        declared_span,
                        inst_span,
                        &storage,
                    );
                }

                Ok(Some(_)) => { /* already satisfied */ }
            }
        }

        let (parameters, return_type) = {
            for implied_predicate in &implied_predicates {
                active_premise
                    .predicates
                    .insert(implied_predicate.clone().into());
            }
            let environment = Environment::new(
                Cow::Borrowed(&active_premise),
                Cow::Borrowed(engine),
                normalizer::NO_OP,
            );

            let mut parameter_arena = Arena::default();
            let mut parameter_orders = Vec::new();

            for (r#type, span) in parameters {
                parameter_orders.push(
                    parameter_arena.insert(Parameter {
                        r#type: environment
                            .simplify_and_check_lifetime_constraints(
                                &r#type, &span, &storage,
                            )
                            .await,
                        span: Some(span),
                    }),
                );
            }

            (
                Parameters {
                    parameter_order: parameter_orders,
                    parameters: parameter_arena,
                },
                if let Some((return_ty, span)) = return_type {
                    environment
                        .simplify_and_check_lifetime_constraints(
                            &return_ty, &span, &storage,
                        )
                        .await
                } else {
                    Type::Tuple(tuple::Tuple::default())
                },
            )
        };

        let late_bound = {
            let generic_params =
                engine.get_generic_parameters(key.symbol_id).await;
            let where_clause = engine.get_where_clause(key.symbol_id).await;

            let mut all_lifetime_parameters = AllLifetimeParameters {
                lifetimes: generic_params.lifetimes().ids().collect(),
                current_function_id: key.symbol_id,
            };

            for predicate in where_clause.as_ref() {
                all_lifetime_parameters
                    .exclude_late_bound(&predicate.predicate);
            }

            all_lifetime_parameters.lifetimes
        };

        Output {
            item: FunctionSignature {
                parameters: engine.intern(parameters),
                return_type: engine.intern(return_type),
                implied_predicates: engine.intern(implied_predicates),
                elided_lifetimes: engine.intern(elided_lifetimes),
                late_bound_lifetime_parameters: engine.intern(late_bound),
            },
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(occurrences),
        }
    }
}

build::register_build!(Key);

struct ParametersElidedLifetimeProvider<'a> {
    global_id: Global<pernixc_symbol::ID>,
    elided_lifetimes: &'a mut Arena<ElidedLifetime>,
    counter: usize,
}

impl ElidedTermProvider<Lifetime> for ParametersElidedLifetimeProvider<'_> {
    fn create(&mut self) -> Lifetime {
        let current = self.counter;
        self.counter += 1;
        let id =
            self.elided_lifetimes.insert(ElidedLifetime { order: current });

        Lifetime::Elided(ElidedLifetimeID { parent_id: self.global_id, id })
    }
}

struct ReturnElidedLifetimeProvider {
    lifetime: Lifetime,
}

impl ElidedTermProvider<Lifetime> for ReturnElidedLifetimeProvider {
    fn create(&mut self) -> Lifetime { self.lifetime.clone() }
}

struct AllLifetimeParameters {
    // list of lifetimes that will be checked if they are late bound
    lifetimes: HashSet<ID<LifetimeParameter>>,
    current_function_id: Global<pernixc_symbol::ID>,
}

impl AllLifetimeParameters {
    // if the lifetime appears in the where clause, it is not late bound
    fn exclude_late_bound(&mut self, predicate: &Predicate) {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => {
                self.exclude_late_bound_in_generic_arguments(
                    &compatible.lhs.member_generic_arguments,
                );
                self.exclude_late_bound_in_generic_arguments(
                    &compatible.lhs.parent_generic_arguments,
                );

                self.exclude_late_bound_in_type(&compatible.rhs);
            }
            Predicate::ConstantType(constant_type) => {
                self.exclude_late_bound_in_type(&constant_type.0);
            }
            Predicate::LifetimeOutlives(outlives) => {
                self.exclude_late_bound_in_lifetime(&outlives.bound);
                self.exclude_late_bound_in_lifetime(&outlives.operand);
            }
            Predicate::TypeOutlives(outlives) => {
                self.exclude_late_bound_in_lifetime(&outlives.bound);
                self.exclude_late_bound_in_type(&outlives.operand);
            }
            Predicate::TupleType(tuple) => {
                self.exclude_late_bound_in_type(&tuple.0);
            }
            Predicate::PositiveTrait(positive) => {
                self.exclude_late_bound_in_generic_arguments(
                    &positive.generic_arguments,
                );
            }
            Predicate::NegativeTrait(negative) => {
                self.exclude_late_bound_in_generic_arguments(
                    &negative.generic_arguments,
                );
            }
            Predicate::PositiveMarker(positive) => {
                self.exclude_late_bound_in_generic_arguments(
                    &positive.generic_arguments,
                );
            }
            Predicate::NegativeMarker(negative) => {
                self.exclude_late_bound_in_generic_arguments(
                    &negative.generic_arguments,
                );
            }
        }
    }

    fn exclude_late_bound_in_lifetime(&mut self, lifetime: &Lifetime) {
        if let Lifetime::Parameter(lifetime) = lifetime
            && lifetime.parent_id == self.current_function_id
        {
            self.lifetimes.remove(&lifetime.id);
        }
    }

    fn exclude_late_bound_in_type(&mut self, ty: &Type) {
        for lt in RecursiveIterator::new(ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent_id == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }
    }

    fn exclude_late_bound_in_generic_arguments(
        &mut self,
        generic_arguments: &GenericArguments,
    ) {
        for lt in generic_arguments
            .lifetimes
            .iter()
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent_id == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }

        for lt in generic_arguments
            .types
            .iter()
            .flat_map(|x| {
                RecursiveIterator::new(x)
                    .filter_map(|x| x.0.into_lifetime().ok())
            })
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent_id == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }

        // Constant should not have lifetime parameters (at most 'static).
    }
}

#[executor(config = Config)]
pub async fn parameters_executor(
    &parameter::Key { symbol_id }: &parameter::Key,
    engine: &TrackedEngine,
) -> Interned<Parameters> {
    let signature = engine.query(&Key { symbol_id }).await;
    signature.parameters
}

#[distributed_slice(PERNIX_PROGRAM)]
static PARAMETERS_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<parameter::Key, ParametersExecutor>();

#[executor(config = Config)]
pub async fn return_type_executor(
    &return_type::Key { symbol_id }: &return_type::Key,
    engine: &TrackedEngine,
) -> Interned<Type> {
    let signature = engine.query(&Key { symbol_id }).await;
    signature.return_type
}

#[distributed_slice(PERNIX_PROGRAM)]
static RETURN_TYPE_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<return_type::Key, ReturnTypeExecutor>();

#[executor(config = Config)]
pub async fn implied_predicates_executor(
    &implied_predicate::Key { symbol_id }: &implied_predicate::Key,
    engine: &TrackedEngine,
) -> Interned<HashSet<ImpliedPredicate>> {
    let signature = engine.query(&Key { symbol_id }).await;
    signature.implied_predicates
}

#[distributed_slice(PERNIX_PROGRAM)]
static IMPLIED_PREDICATES_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<
        implied_predicate::Key,
        ImpliedPredicatesExecutor,
    >();

#[executor(config = Config)]
pub async fn elided_lifetimes_executor(
    &elided_lifetime::Key { symbol_id }: &elided_lifetime::Key,
    engine: &TrackedEngine,
) -> Interned<Arena<ElidedLifetime>> {
    let signature = engine.query(&Key { symbol_id }).await;
    signature.elided_lifetimes
}

#[distributed_slice(PERNIX_PROGRAM)]
static ELIDED_LIFETIMES_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<elided_lifetime::Key, ElidedLifetimesExecutor>(
    );

#[executor(config = Config)]
pub async fn late_bound_lifetimes_executor(
    &late_bound_lifetime::Key { symbol_id }: &late_bound_lifetime::Key,
    engine: &TrackedEngine,
) -> Interned<HashSet<ID<LifetimeParameter>>> {
    let signature = engine.query(&Key { symbol_id }).await;
    signature.late_bound_lifetime_parameters
}

#[distributed_slice(PERNIX_PROGRAM)]
static LATE_BOUND_LIFETIMES_EXECUTOR: Registration<Config> =
    Registration::<Config>::new::<
        late_bound_lifetime::Key,
        LateBoundLifetimesExecutor,
    >();
