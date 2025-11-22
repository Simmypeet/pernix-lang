//! Implements the [`Query`] for the [`PositiveMarker`] and [`NegativeMarker`].

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use pernixc_query::{TrackedEngine, runtime::executor};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    lifetime::Lifetime,
    predicate::{NegativeMarker, PositiveMarker, Predicate},
    r#type::Type,
    visitor::{self, Element},
};

use crate::{
    Error, Satisfied, Succeeded,
    adt_fields::get_instantiated_adt_fields,
    environment::{BoxedFuture, Call, DynArc, Environment, Query},
    normalizer::Normalizer,
    resolution::{self, Implementation},
};

/// An enumeration of ways the marker can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PositiveSatisfied {
    /// Found a matching premise.
    Premise,

    /// Found a matching implementation.
    Implementation(Implementation),

    /// Satisfied by proving that all the fields/sub-terms are satisfied.
    Congruence(BTreeMap<PositiveMarker, Arc<Succeeded<Self>>>),

    /// Satisfied by the fact that the query was made in the marker/its
    /// implementation.
    Environment,

    /// Satisfied by co-inductive reasoning.
    Cyclic,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VisitorState {
    Failed,
    AbruptError(Error),
    Succeeded(BTreeMap<PositiveMarker, Arc<Succeeded<PositiveSatisfied>>>),
}

#[derive(Debug)]
struct Visitor<'t, 'p, N: Normalizer> {
    original: &'p PositiveMarker,
    state: VisitorState,
    environment: &'t Environment<'t, N>,
}

impl<N: Normalizer> visitor::AsyncVisitor<Lifetime> for Visitor<'_, '_, N> {
    async fn visit(
        &mut self,
        _: &Lifetime,
        _: <Lifetime as Element>::Location,
    ) -> bool {
        if matches!(
            self.state,
            VisitorState::Failed | VisitorState::AbruptError(_)
        ) {
            return false;
        }

        true
    }
}

impl<N: Normalizer> visitor::AsyncVisitor<Type> for Visitor<'_, '_, N> {
    async fn visit(
        &mut self,
        ty: &Type,
        _: <Type as Element>::Location,
    ) -> bool {
        let VisitorState::Succeeded(states) = &mut self.state else {
            return false;
        };

        let new_query = PositiveMarker {
            marker_id: self.original.marker_id,
            generic_arguments: GenericArguments {
                lifetimes: self.original.generic_arguments.lifetimes.clone(),
                types: {
                    let mut types =
                        self.original.generic_arguments.types.clone();
                    types[0] = ty.clone();
                    types
                },
                constants: self.original.generic_arguments.constants.clone(),
            },
        };

        match self.environment.query(&new_query).await {
            Ok(Some(result)) => {
                states.insert(new_query, result);
                true
            }
            Ok(None) => {
                self.state = VisitorState::Failed;
                false
            }
            Err(err) => {
                self.state = VisitorState::AbruptError(err);
                false
            }
        }
    }
}

impl<N: Normalizer> visitor::AsyncVisitor<Constant> for Visitor<'_, '_, N> {
    async fn visit(
        &mut self,
        _: &Constant,
        _: <Constant as Element>::Location,
    ) -> bool {
        if matches!(
            self.state,
            VisitorState::Failed | VisitorState::AbruptError(_)
        ) {
            return false;
        }

        true
    }
}

async fn try_get_adt_fields(
    ty: &Type,
    engine: &TrackedEngine,
) -> Result<Option<Vec<Type>>, executor::CyclicError> {
    let Type::Symbol(symbol) = ty else {
        return Ok(None);
    };

    if !matches!(engine.get_kind(symbol.id).await, Kind::Enum | Kind::Struct) {
        return Ok(None);
    }

    Ok(Some(
        engine
            .get_instantiated_adt_fields(symbol.id, &symbol.generic_arguments)
            .await?,
    ))
}

impl Query for PositiveMarker {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied>;
    type Error = Error;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
            // if this query was made in marker, then check if the marker is the
            // same as the query one.
            if let Some(result) = is_in_marker(
                self.marker_id,
                &self.generic_arguments,
                environment,
            )
            .await?
            {
                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    PositiveSatisfied::Environment,
                    result.constraints,
                ))));
            }

            // look for the matching premise
            for premise in environment
                .premise()
                .predicates
                .iter()
                .filter_map(Predicate::as_positive_marker)
            {
                // skip if id is different
                if premise.marker_id != self.marker_id {
                    continue;
                }

                let Some(compatiblity) = environment
                    .subtypes_generic_arguments(
                        &self.generic_arguments,
                        &premise.generic_arguments,
                    )
                    .await?
                else {
                    continue;
                };

                if !compatiblity.result.forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    PositiveSatisfied::Premise,
                    compatiblity.constraints,
                ))));
            }

            // manually search for the trait implementation
            if let Some(result) = environment
                .query(&resolution::Resolve::new(
                    self.marker_id,
                    self.generic_arguments.clone(),
                ))
                .await?
            {
                if environment.tracked_engine().get_kind(result.result.id).await
                    == Kind::PositiveImplementation
                {
                    return Ok(Some(Arc::new(Succeeded::with_constraints(
                        PositiveSatisfied::Implementation(Implementation {
                            instantiation: result.result.instantiation.clone(),
                            id: result.result.id,
                            is_not_general_enough: result
                                .result
                                .is_not_general_enough,
                        }),
                        result.constraints.clone(),
                    ))));
                }

                // then it's a negative implementation
                return Ok(None);
            }

            // replace the first type argument with sub-terms/fields and check
            // if all the replacements are satisfied
            if let Some(ty) = self.generic_arguments.types.first() {
                let mut visitor = Visitor {
                    original: self,
                    state: VisitorState::Succeeded(BTreeMap::new()),
                    environment,
                };

                let result = ty.accept_one_level_async(&mut visitor).await;

                match (visitor.state, result) {
                    // can't find the sub-term / failed
                    (VisitorState::Failed, _)
                    | (
                        VisitorState::Succeeded(_),
                        Err(visitor::VisitNonApplicationTermError),
                    ) => {}

                    // abrupt error
                    (VisitorState::AbruptError(error), _) => return Err(error),

                    (VisitorState::Succeeded(mut btree_map), Ok(_)) => {
                        // including fields as well
                        for field_ty in
                            try_get_adt_fields(ty, environment.tracked_engine())
                                .await?
                                .into_iter()
                                .flatten()
                        {
                            let new_query = Self {
                                marker_id: self.marker_id,
                                generic_arguments: GenericArguments {
                                    lifetimes: self
                                        .generic_arguments
                                        .lifetimes
                                        .clone(),
                                    types: {
                                        let mut types = self
                                            .generic_arguments
                                            .types
                                            .clone();
                                        types[0] = field_ty.clone();
                                        types
                                    },
                                    constants: self
                                        .generic_arguments
                                        .constants
                                        .clone(),
                                },
                            };

                            if let Some(result) =
                                environment.query(&new_query).await?
                            {
                                btree_map.insert(new_query, result);
                            } else {
                                return Ok(None);
                            }
                        }

                        return Ok(Some(Arc::new(Succeeded {
                            result: PositiveSatisfied::Congruence(btree_map),
                            constraints: BTreeSet::new(),
                        })));
                    }
                }
            }

            Ok(None)
        })
    }

    fn on_cyclic(
        &self,
        (): Self::Parameter,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[Call<DynArc, DynArc>],
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        Ok(Some(Arc::new(Succeeded::new(
            PositiveSatisfied::Cyclic, /* doesn't matter */
        ))))
    }
}

/// An enumeration of ways the [`Negative`] marker can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum NegativeSatisfied {
    /// Found a matching premise.
    Premise,

    /// Found a matching negative implementation.
    Implementation(Implementation),

    /// Satisfied by proving that the [`Positive`] isn't satisfied and the
    /// generic arguments are definite.
    UnsatisfiedPositive,
}

impl Query for NegativeMarker {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied>;
    type Error = Error;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
            // manually search for the trait implementation
            if let Some(result) = environment
                .query(&resolution::Resolve::new(
                    self.marker_id,
                    self.generic_arguments.clone(),
                ))
                .await?
                && environment.tracked_engine().get_kind(result.result.id).await
                    == Kind::NegativeImplementation
            {
                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::Implementation(Implementation {
                        instantiation: result.result.instantiation.clone(),
                        id: result.result.id,
                        is_not_general_enough: result
                            .result
                            .is_not_general_enough,
                    }),
                    result.constraints.clone(),
                ))));
            }

            // look for the premise that matches
            for marker_premise in environment
                .premise()
                .predicates
                .iter()
                .filter_map(Predicate::as_negative_marker)
            {
                // skip if the trait id is different
                if marker_premise.marker_id != self.marker_id {
                    continue;
                }

                let Some(compatiblity) = environment
                    .subtypes_generic_arguments(
                        &self.generic_arguments,
                        &marker_premise.generic_arguments,
                    )
                    .await?
                else {
                    continue;
                };

                if !compatiblity.result.forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::Premise,
                    compatiblity.constraints,
                ))));
            }

            // must be definite and failed to prove the positive trait
            let Some(definition) = environment
                .generic_arguments_definite(&self.generic_arguments)
                .await?
            else {
                return Ok(None);
            };

            Ok(environment
                .query(&PositiveMarker::new(
                    self.marker_id,
                    self.generic_arguments.clone(),
                ))
                .await?
                .is_none()
                .then(|| {
                    Arc::new(Succeeded::with_constraints(
                        NegativeSatisfied::UnsatisfiedPositive,
                        definition.constraints,
                    ))
                }))
        })
    }
}

async fn is_in_marker(
    marker_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, Error> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    let mut scope_walker =
        environment.tracked_engine().scope_walker(query_site);

    while let Some(current_id) = scope_walker.next().await {
        let current_id = Global::new(query_site.target_id, current_id);

        let Kind::Trait =
            environment.tracked_engine().get_kind(current_id).await
        else {
            continue;
        };

        // must be the same id
        if current_id != marker_id {
            continue;
        }

        let marker_generic_arguments = environment
            .tracked_engine()
            .get_generic_parameters(current_id)
            .await?
            .create_identity_generic_arguments(current_id);

        let Some(compatibility) = environment
            .subtypes_generic_arguments(
                generic_arguments,
                &marker_generic_arguments,
            )
            .await?
        else {
            continue;
        };

        if compatibility.result.forall_lifetime_errors.is_empty() {
            return Ok(Some(Succeeded::satisfied_with(
                compatibility.constraints,
            )));
        }
    }

    Ok(None)
}
