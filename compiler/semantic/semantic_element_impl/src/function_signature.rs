use std::{borrow::Cow, sync::Arc};

use pernixc_arena::{Arena, ID};
use pernixc_handler::Storage;
use pernixc_hash::HashSet;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_resolution::{
    generic_parameter_namespace::get_generic_parameter_namespace,
    term::resolve_type, Config, ElidedTermProvider, ExtraNamespace,
};
use pernixc_semantic_element::{
    elided_lifetime,
    implied_predicate::{self, ImpliedPredicate},
    late_bound_lifetime,
    parameter::{self, Parameter, Parameters},
    return_type,
    where_clause::get_where_clause,
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_source_file::SourceElement;
use pernixc_stable_hash::StableHash;
use pernixc_symbol::{
    parent::get_parent, syntax::get_function_signature_syntax,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::{get_generic_parameters, LifetimeParameter},
    lifetime::{ElidedLifetime, ElidedLifetimeID, Lifetime},
    predicate::{Outlives, Predicate},
    r#type::Type,
    tuple,
    visitor::RecursiveIterator,
};
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
};

use crate::{
    build::{self, Output},
    function_signature::diagnostic::Diagnostic,
    occurrences::Occurrences,
    Build,
};

pub mod diagnostic;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Serialize,
    Deserialize,
    pernixc_query::Value,
)]
#[id(Global<pernixc_symbol::ID>)]
pub struct FunctionSignature {
    pub parameters: Arc<Parameters>,
    pub return_type: Arc<Type>,
    pub implied_predicates: Arc<HashSet<ImpliedPredicate>>,
    pub elided_lifetimes: Arc<Arena<ElidedLifetime>>,
    pub late_bound_lifetime_parameters: Arc<HashSet<ID<LifetimeParameter>>>,
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
) -> Result<Vec<(Type, RelativeSpan)>, executor::CyclicError> {
    let mut result = Vec::new();
    for parameter in parameters {
        let ty = match parameter.r#type() {
            Some(ty) => {
                engine
                    .resolve_type(
                        &ty,
                        Config::builder()
                            .referring_site(id)
                            .extra_namespace(extra_namespace)
                            .elided_lifetime_provider(elided_lifetimes_provider)
                            .observer(occurrences)
                            .build(),
                        storage,
                    )
                    .await?
            }
            None => Type::Error(pernixc_term::error::Error),
        };

        result.push((ty, parameter.span()));
    }

    Ok(result)
}

impl Build for Key {
    type Diagnostic = Diagnostic;

    #[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
    async fn execute(
        engine: &TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, executor::CyclicError> {
        let extra_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;
        let mut elided_lifetimes = Arena::default();
        let mut occurrences = Occurrences::default();
        let mut elided_lifetimes_provider = ParametersElidedLifetimeProvider {
            global_id: key.0,
            elided_lifetimes: &mut elided_lifetimes,
            counter: 0,
        };
        let storage = Storage::<Diagnostic>::default();

        let (parameters_syn, return_type_syn) =
            engine.get_function_signature_syntax(key.0).await;

        let parameters = if let Some(parameters) = parameters_syn {
            create_parameters(
                engine,
                key.0,
                &extra_namespace,
                &mut elided_lifetimes_provider,
                &mut occurrences,
                &storage,
                parameters.parameters().filter_map(|x| x.into_regular().ok()),
            )
            .await?
        } else {
            Vec::new()
        };

        let mut return_elided_lifetime_provider = (elided_lifetimes.len() == 1)
            .then(|| ReturnElidedLifetimeProvider {
                lifetime: Lifetime::Elided(ElidedLifetimeID {
                    parent_id: key.0,
                    id: elided_lifetimes.ids().next().unwrap(),
                }),
            });

        let return_type =
            if let Some(ty_syn) = return_type_syn.and_then(|x| x.r#type()) {
                let ty = engine
                    .resolve_type(
                        &ty_syn,
                        Config::builder()
                            .referring_site(key.0)
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
                    .await?;

                Some((ty, ty_syn.span()))
            } else {
                None
            };

        let mut active_premise = engine
            .get_active_premise(
                key.0
                    .target_id
                    .make_global(engine.get_parent(key.0).await.unwrap()),
            )
            .await?;

        let where_clause = engine.get_where_clause(key.0).await?;

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
                            engine.get_where_clause(symbol.id).await?;

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
                                reference.lifetime,
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
                Err(pernixc_type_system::Error::CyclicDependency(err)) => {
                    return Err(err);
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
                            .await?,
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
                        .await?
                } else {
                    Type::Tuple(tuple::Tuple::default())
                },
            )
        };

        let late_bound = {
            let generic_params = engine.get_generic_parameters(key.0).await?;
            let where_clause = engine.get_where_clause(key.0).await?;

            let mut all_lifetime_parameters = AllLifetimeParameters {
                lifetimes: generic_params.lifetimes().ids().collect(),
                current_function_id: key.0,
            };

            for predicate in where_clause.as_ref() {
                all_lifetime_parameters
                    .exclude_late_bound(&predicate.predicate);
            }

            all_lifetime_parameters.lifetimes
        };

        Ok(Output {
            item: FunctionSignature {
                parameters: Arc::new(parameters),
                return_type: Arc::new(return_type),
                implied_predicates: Arc::new(implied_predicates),
                elided_lifetimes: Arc::new(elided_lifetimes),
                late_bound_lifetime_parameters: Arc::new(late_bound),
            },
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
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
    fn create(&mut self) -> Lifetime { self.lifetime }
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
        if let Lifetime::Parameter(lifetime) = lifetime {
            if lifetime.parent_id == self.current_function_id {
                self.lifetimes.remove(&lifetime.id);
            }
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

#[pernixc_query::executor(key(parameter::Key), name(ParametersExecutor))]
pub async fn parameters_executor(
    parameter::Key(id): &parameter::Key,
    engine: &TrackedEngine,
) -> Result<Arc<Parameters>, executor::CyclicError> {
    let signature = engine.query(&Key(*id)).await?;
    Ok(signature.parameters)
}

pernixc_register::register!(parameter::Key, ParametersExecutor);

#[pernixc_query::executor(key(return_type::Key), name(ReturnTypeExecutor))]
pub async fn return_type_executor(
    return_type::Key(id): &return_type::Key,
    engine: &TrackedEngine,
) -> Result<Arc<Type>, executor::CyclicError> {
    let signature = engine.query(&Key(*id)).await?;
    Ok(signature.return_type)
}

pernixc_register::register!(return_type::Key, ReturnTypeExecutor);

#[pernixc_query::executor(
    key(implied_predicate::Key),
    name(ImpliedPredicatesExecutor)
)]
pub async fn implied_predicates_executor(
    implied_predicate::Key(id): &implied_predicate::Key,
    engine: &TrackedEngine,
) -> Result<Arc<HashSet<ImpliedPredicate>>, executor::CyclicError> {
    let signature = engine.query(&Key(*id)).await?;
    Ok(signature.implied_predicates)
}

pernixc_register::register!(implied_predicate::Key, ImpliedPredicatesExecutor);

#[pernixc_query::executor(
    key(elided_lifetime::Key),
    name(ElidedLifetimesExecutor)
)]
pub async fn elided_lifetimes_executor(
    elided_lifetime::Key(id): &elided_lifetime::Key,
    engine: &TrackedEngine,
) -> Result<Arc<Arena<ElidedLifetime>>, executor::CyclicError> {
    let signature = engine.query(&Key(*id)).await?;
    Ok(signature.elided_lifetimes)
}

pernixc_register::register!(elided_lifetime::Key, ElidedLifetimesExecutor);

#[pernixc_query::executor(
    key(late_bound_lifetime::Key),
    name(LateBooundLifetimesExecutor)
)]
pub async fn late_bound_lifetimes_executor(
    late_bound_lifetime::Key(id): &late_bound_lifetime::Key,
    engine: &TrackedEngine,
) -> Result<Arc<HashSet<ID<LifetimeParameter>>>, executor::CyclicError> {
    let signature = engine.query(&Key(*id)).await?;
    Ok(signature.late_bound_lifetime_parameters)
}

pernixc_register::register!(
    late_bound_lifetime::Key,
    LateBooundLifetimesExecutor
);
