//! Contains the logic for resolving implementation for traits and markers.

use std::{collections::BTreeSet, ops::Deref, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    implemented::get_implemented,
    implements::get_implements,
    implements_arguments::get_implements_argument,
    variance::Variance,
    where_clause::{self, get_where_clause},
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments, instantiation::Instantiation,
    predicate::Predicate, r#type::Type,
};
use qbice::storage::intern::Interned;

use crate::{
    OverflowError, Succeeded,
    deduction::Deduction,
    environment::{BoxedFuture, Environment, Query},
    lifetime_constraint::LifetimeConstraint,
    normalizer::Normalizer,
    order::{self, Order},
    predicate::marker,
};

/// The cause of an unsatisfied predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum UnsatisfiedCause {
    /// No additional information to elaborate further.
    NoInformation,

    /// The predicate was a "positive marker" and it was not satisfied. The
    /// contained error can be used to elaborate further on the cause of the
    /// unsatisfaction.
    PositiveMarker(marker::PositiveError),
}

/// Failed to satisfy a predicate define in the `implements` where clause.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisfiedPredicate {
    predicate: Predicate,
    span: Option<RelativeSpan>,
    cause: UnsatisfiedCause,
}

impl UnsatisfiedPredicate {
    #[must_use]
    pub(crate) fn heuristic(&self) -> usize {
        match &self.cause {
            UnsatisfiedCause::NoInformation => 1,
            UnsatisfiedCause::PositiveMarker(positive_error) => {
                positive_error.heuristic()
            }
        }
    }
}

/// Failed to satisfy one or more predicates defined in the `implements` where
/// clause.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsatisfiedPredicates {
    unsatisfied_predicate: Arc<[UnsatisfiedPredicate]>,
}

/// An error that can occur during implementation resolution.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Error {
    /// Found no implementation for the given symbol and generic arguments.
    NotFound,

    //// Found an implementation, but its signature is not general enough for
    /// the for-all lifetimes.
    IsNotGeneralEnough(Arc<Implementation>),

    /// An implementation was found, but one or more predicates in the
    /// `implements` where clause were not satisfied.
    UnsatisfiedPredicates(UnsatisfiedPredicates),

    /// Found ambiguous `implements` candidates.
    Ambiguous,

    /// An error for cyclic query.
    Cyclic,
}

impl Error {
    #[must_use]
    pub(crate) fn heuristic(&self) -> usize {
        match self {
            Self::IsNotGeneralEnough(_) => 2,

            Self::UnsatisfiedPredicates(predicates) => {
                predicates
                    .unsatisfied_predicate
                    .iter()
                    .map(UnsatisfiedPredicate::heuristic)
                    .sum::<usize>()
                    + 1
            }

            Self::NotFound | Self::Ambiguous | Self::Cyclic => 1,
        }
    }
}

/// A result of a implementation resolution query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Implementation {
    /// The deduced substitution for the generic arguments of the trait
    /// implementation.
    pub instantiation: Instantiation,

    /// The ID of the resolved implementation.
    pub id: Global<pernixc_symbol::ID>,
}

/// A query for resolving a matching `implements`.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct Resolve {
    /// The `trait` or `marker` to resolve the `implements` for.
    pub implemented_id: Global<pernixc_symbol::ID>,

    /// The generic arguments supplied to the [`Self::implemented_id`]
    pub generic_arguments: GenericArguments,
}

type ResolveResult = Result<Arc<Succeeded<Implementation>>, Error>;

impl Query for Resolve {
    type InProgress = ();
    type Result = ResolveResult;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            let symbol_kind = environment
                .tracked_engine()
                .get_kind(self.implemented_id)
                .await;

            // check that it must be trait or marker
            assert!(matches!(symbol_kind, Kind::Marker));

            // we might be in the implementation site already
            if let Some(result) = is_in_active_implementation(
                self.implemented_id,
                &self.generic_arguments,
                environment,
            )
            .await?
            {
                return Ok(result);
            }

            // the current candidate
            #[allow(clippy::type_complexity)]
            let mut candidate: Option<(
                Global<pernixc_symbol::ID>,
                Deduction,
                BTreeSet<LifetimeConstraint>,
                GenericArguments,
            )> = None;

            let implementations = environment
                .tracked_engine()
                .get_implemented(self.implemented_id)
                .await
                .iter()
                .copied()
                .collect::<Vec<_>>();

            for current_impl_id in implementations {
                let Some(implementation_generic_arguments) = environment
                    .tracked_engine()
                    .get_implements_argument(current_impl_id)
                    .await
                else {
                    continue;
                };

                // build the unification
                let Succeeded {
                    result: deduction,
                    constraints: lifetime_constraints,
                } = match environment
                    .deduce(
                        &implementation_generic_arguments,
                        &self.generic_arguments,
                    )
                    .await
                {
                    Ok(Some(unification)) => unification,

                    Err(error) => {
                        return Err(error);
                    }

                    Ok(None) => continue,
                };

                // compare with the current candidate
                match &mut candidate {
                    Some((
                        candidate_id,
                        candidate_instantiation,
                        candidate_lifetime_constraints,
                        candidate_generic_arguments,
                    )) => {
                        // check which one is more specific
                        match environment
                            .tracked_engine()
                            .query(&order::Key::new(
                                current_impl_id,
                                *candidate_id,
                            ))
                            .await?
                        {
                            Some(Order::Ambiguous | Order::Incompatible) => {
                                return Ok(Err(Error::Ambiguous));
                            }

                            Some(Order::MoreGeneral) | None => {}

                            Some(Order::MoreSpecific) => {
                                *candidate_id = current_impl_id;
                                *candidate_instantiation = deduction;
                                *candidate_lifetime_constraints =
                                    lifetime_constraints;
                                *candidate_generic_arguments =
                                    implementation_generic_arguments
                                        .deref()
                                        .clone();
                            }
                        }
                    }

                    candidate @ None => {
                        *candidate = Some((
                            current_impl_id,
                            deduction,
                            lifetime_constraints,
                            implementation_generic_arguments.deref().clone(),
                        ));
                    }
                }
            }

            match candidate {
                Some((
                    implementation_id,
                    deduction,
                    mut lifetime_constraints,
                    _,
                )) => {
                    // if not general enough, return error
                    if deduction.is_not_general_enough {
                        return Ok(Err(Error::IsNotGeneralEnough(Arc::new(
                            Implementation {
                                instantiation: deduction.instantiation,
                                id: implementation_id,
                            },
                        ))));
                    }

                    let where_clause_predicates = environment
                        .tracked_engine()
                        .get_where_clause(implementation_id)
                        .await;

                    // check if satisfies the predicates
                    match predicate_satisfies(
                        where_clause_predicates,
                        &deduction.instantiation,
                        environment,
                    )
                    .await?
                    {
                        Ok(new_constraints) => {
                            lifetime_constraints.extend(new_constraints);

                            Ok(Ok(Arc::new(Succeeded {
                                result: Implementation {
                                    instantiation: deduction.instantiation,
                                    id: implementation_id,
                                },
                                constraints: lifetime_constraints,
                            })))
                        }

                        Err(err) => Ok(Err(Error::UnsatisfiedPredicates(
                            UnsatisfiedPredicates {
                                unsatisfied_predicate: err,
                            },
                        ))),
                    }
                }

                None => Ok(Err(Error::NotFound)),
            }
        })
    }

    fn on_cyclic(
        &self,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        Err(Error::Cyclic)
    }
}

#[allow(clippy::type_complexity)]
async fn is_in_active_implementation(
    implemented_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<ResolveResult>, OverflowError> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    let mut scope_walker =
        environment.tracked_engine().scope_walker(query_site);

    while let Some(current_id) = scope_walker.next().await {
        let current_id = Global::new(query_site.target_id, current_id);
        let current_kind =
            environment.tracked_engine().get_kind(current_id).await;

        // must be the implementation kind
        if !matches!(
            current_kind,
            Kind::PositiveImplementation | Kind::NegativeImplementation
        ) {
            continue;
        }

        // must be an implementation
        if environment.tracked_engine().get_implements(current_id).await
            != Some(implemented_id)
        {
            continue;
        }

        let Some(implementation_arguments) = environment
            .tracked_engine()
            .get_implements_argument(current_id)
            .await
        else {
            continue;
        };

        let Some(_) = environment
            .subtypes_generic_arguments(
                generic_arguments,
                &implementation_arguments,
            )
            .await?
        else {
            continue;
        };

        if let Some(result) = environment
            .deduce(generic_arguments, &implementation_arguments)
            .await?
        {
            if result.result.is_not_general_enough {
                return Ok(Some(Err(Error::IsNotGeneralEnough(Arc::new(
                    Implementation {
                        instantiation: result.result.instantiation,
                        id: current_id,
                    },
                )))));
            }

            return Ok(Some(Ok(Arc::new(Succeeded {
                result: Implementation {
                    instantiation: result.result.instantiation,
                    id: current_id,
                },
                constraints: result.constraints,
            }))));
        }
    }

    Ok(None)
}

#[allow(clippy::too_many_lines)]
async fn predicate_satisfies(
    predicates: Interned<[where_clause::Predicate]>,
    substitution: &Instantiation,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<
    Result<BTreeSet<LifetimeConstraint>, Arc<[UnsatisfiedPredicate]>>,
    OverflowError,
> {
    // check if satisfies all the predicate
    let mut unsatisfied_predicates = Vec::new();
    let mut constraints = BTreeSet::new();

    for (mut predicate, span) in
        predicates.iter().map(|x| (x.predicate.clone(), x.span))
    {
        predicate.instantiate(substitution);

        match &predicate {
            Predicate::InstanceAssociatedTypeEquality(equality) => {
                match environment
                    .subtypes(
                        Type::InstanceAssociated(equality.lhs.clone()),
                        equality.rhs.clone(),
                        Variance::Covariant,
                    )
                    .await?
                {
                    Some(subtypable)
                        if subtypable
                            .result
                            .forall_lifetime_errors
                            .is_empty() =>
                    {
                        constraints
                            .extend(subtypable.constraints.iter().cloned());
                    }

                    _ => {
                        unsatisfied_predicates.push(UnsatisfiedPredicate {
                            predicate,
                            span,
                            cause: UnsatisfiedCause::NoInformation,
                        });
                    }
                }
            }

            Predicate::ConstantType(constant_type) => {
                match environment.query(constant_type).await? {
                    Some(satisfied) => {
                        constraints
                            .extend(satisfied.constraints.iter().cloned());
                    }
                    None => {
                        unsatisfied_predicates.push(UnsatisfiedPredicate {
                            predicate,
                            span,
                            cause: UnsatisfiedCause::NoInformation,
                        });
                    }
                }
            }

            Predicate::TupleType(tuple_type) => {
                match environment.query(tuple_type).await? {
                    Some(satisfied) => {
                        constraints
                            .extend(satisfied.constraints.iter().cloned());
                    }
                    None => {
                        unsatisfied_predicates.push(UnsatisfiedPredicate {
                            predicate,
                            span,
                            cause: UnsatisfiedCause::NoInformation,
                        });
                    }
                }
            }

            Predicate::PositiveMarker(tr) => {
                match environment.query(tr).await? {
                    Ok(result) => {
                        constraints.extend(result.constraints.iter().cloned());
                    }

                    Err(err) => {
                        unsatisfied_predicates.push(UnsatisfiedPredicate {
                            predicate,
                            span,
                            cause: UnsatisfiedCause::PositiveMarker(err),
                        });
                    }
                }
            }

            Predicate::NegativeMarker(tr) => {
                match environment.query(tr).await? {
                    Some(result) => {
                        constraints.extend(result.constraints.iter().cloned());
                    }

                    None => {
                        unsatisfied_predicates.push(UnsatisfiedPredicate {
                            predicate,
                            span,
                            cause: UnsatisfiedCause::NoInformation,
                        });
                    }
                }
            }

            Predicate::TypeOutlives(pred) => {
                constraints
                    .insert(LifetimeConstraint::TypeOutlives(pred.clone()));
            }

            Predicate::LifetimeOutlives(pred) => {
                constraints
                    .insert(LifetimeConstraint::LifetimeOutlives(pred.clone()));
            }
        }
    }

    Ok(if unsatisfied_predicates.is_empty() {
        Ok(constraints)
    } else {
        Err(unsatisfied_predicates.into())
    })
}
