//! Implements the [`Query`] for the [`PositiveMarker`] and [`NegativeMarker`].

use std::{collections::BTreeSet, ops::Deref, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::scope_walker,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    predicate::{NegativeMarker, PositiveMarker, Predicate},
    r#type::Type,
};

use crate::{
    OverflowError, Satisfied, Succeeded,
    adt_fields::{FieldType, get_instantiated_adt_fields},
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
    Substructural,

    /// Satisfied by the fact that the query was made in the marker/its
    /// implementation.
    Environment,

    /// Satisfied by co-inductive reasoning.
    Cyclic,
}

/// An error when trying to structural derive a positive marker.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructuralError {
    /// The field that failed to be satisfied.
    sub_predicate: PositiveMarker,

    /// The error that occurred when trying to satisfy the field.
    error: PositiveError,
}

impl StructuralError {
    /// Gets the sub-predicate that failed to be satisfied.
    #[must_use]
    pub const fn sub_predicate(&self) -> &PositiveMarker { &self.sub_predicate }

    /// Gets the error that occurred when trying to satisfy the field.
    #[must_use]
    pub const fn error(&self) -> &PositiveError { &self.error }
}

/// An error type indiciating that the positive marker failed to be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PositiveError {
    /// Failed to utilize the `implements` matching the marker.
    ImplementationResolution(resolution::Error),

    /// The solver failed to find and implementation for one of the fields.
    Structural(Arc<[StructuralError]>),

    /// Resolved to a negative implementation.
    NegativeMarkerImplementation(Arc<Succeeded<resolution::Implementation>>),

    /// Failed by bad cyclic proof, which happens by equivalences that lead to a
    /// cycle without any progress.
    Cyclic,
}

impl PositiveError {
    #[must_use]
    pub(crate) fn heuristic(&self) -> usize {
        match self {
            Self::ImplementationResolution(error) => error.heuristic(),
            Self::Structural(structural_errors) => structural_errors
                .iter()
                .map(|e| e.error.heuristic())
                .sum::<usize>(),
            Self::NegativeMarkerImplementation(_) => 2,
            Self::Cyclic => 0,
        }
    }
}

async fn try_get_adt_fields(
    ty: &Type,
    engine: &TrackedEngine,
) -> Option<Vec<FieldType>> {
    match ty {
        Type::Error(_)
        | Type::AssociatedSymbol(_)
        | Type::FunctionSignature(_)
        | Type::InstanceAssociated(_)
        | Type::Inference(_)
        | Type::Primitive(_)
        | Type::Parameter(_) => None,

        Type::Symbol(symbol) => {
            if !matches!(
                engine.get_kind(symbol.id()).await,
                Kind::Enum | Kind::Struct
            ) {
                return None;
            }

            Some(engine.get_instantiated_adt_fields(symbol).await)
        }

        Type::Pointer(pointer) => {
            Some(vec![FieldType::new_no_span(pointer.pointee.deref().clone())])
        }

        Type::Reference(reference) => Some(vec![FieldType::new_no_span(
            reference.pointee.deref().clone(),
        )]),

        Type::Array(array) => {
            Some(vec![FieldType::new_no_span(array.r#type.deref().clone())])
        }

        Type::Tuple(tuple) => Some(
            tuple
                .elements()
                .iter()
                .map(|x| FieldType::new_no_span(x.term().clone()))
                .collect(),
        ),

        Type::Phantom(phantom) => {
            Some(vec![FieldType::new_no_span(phantom.0.deref().clone())])
        }
    }
}

type PositiveResult = Result<Arc<Succeeded<PositiveSatisfied>>, PositiveError>;

/// Describes the source of the query for constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum QuerySource {
    /// Used to reason the satisfiability by querying it by an another
    /// equivalent type.
    FromEquivalence,

    /// Normal
    #[default]
    Normal,
}

impl Query for PositiveMarker {
    type InProgress = QuerySource;
    type Result = PositiveResult;

    #[allow(clippy::too_many_lines)]
    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            // if this query was made in marker, then check if the marker is the
            // same as the query one.
            if let Some(result) = is_in_marker(
                self.marker_id(),
                self.generic_arguments(),
                environment,
            )
            .await?
            {
                return Ok(Ok(Arc::new(Succeeded::with_constraints(
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
                if premise.marker_id() != self.marker_id() {
                    continue;
                }

                let Some(compatiblity) = environment
                    .subtypes_generic_arguments(
                        self.generic_arguments(),
                        premise.generic_arguments(),
                    )
                    .await?
                else {
                    continue;
                };

                if !compatiblity.result.forall_lifetime_errors.is_empty() {
                    continue;
                }

                return Ok(Ok(Arc::new(Succeeded::with_constraints(
                    PositiveSatisfied::Premise,
                    compatiblity.constraints,
                ))));
            }

            // manually search for the trait implementation
            let resolve = environment
                .query(&resolution::Resolve::new(
                    self.marker_id(),
                    self.generic_arguments().clone(),
                ))
                .await?;

            let mut current_error = match resolve {
                Ok(result) => {
                    if environment
                        .tracked_engine()
                        .get_kind(result.result.id)
                        .await
                        == Kind::PositiveImplementation
                    {
                        return Ok(Ok(Arc::new(Succeeded::with_constraints(
                            PositiveSatisfied::Implementation(Implementation {
                                instantiation: result
                                    .result
                                    .instantiation
                                    .clone(),
                                id: result.result.id,
                            }),
                            result.constraints.clone(),
                        ))));
                    }

                    PositiveError::NegativeMarkerImplementation(result)
                }
                Err(error) => {
                    // if failed by anything other than not found, then we'll
                    // take the error as the result
                    if !error.is_not_found() {
                        return Ok(Err(
                            PositiveError::ImplementationResolution(error),
                        ));
                    }

                    PositiveError::ImplementationResolution(error)
                }
            };

            // replace the first type argument with sub-terms/fields and check
            // if all the replacements are satisfied
            if let Some(ty) = self.first_type_argument() {
                // try to structurally prove the marker for each field of the
                // type
                if let Some(result) =
                    substructural_derive(self, ty, environment).await?
                {
                    return Ok(result);
                }

                // replace the first type argument with equivalent types and
                // check if any of them are satisfied
                for term in environment.get_equivalences(ty).await?.as_ref() {
                    let mut positive_marker_clone = self.clone();

                    // replace the first type argument with the equivalent type
                    positive_marker_clone
                        .set_first_type_argument(term.result.clone())
                        .unwrap();

                    match environment
                        .query_with(
                            &positive_marker_clone,
                            QuerySource::FromEquivalence,
                        )
                        .await?
                    {
                        // found the success path
                        Ok(result) => {
                            let mut constraints = result.constraints.clone();

                            constraints
                                .extend(term.constraints.iter().cloned());

                            return Ok(Ok(Arc::new(
                                Succeeded::with_constraints(
                                    result.result.clone(),
                                    constraints,
                                ),
                            )));
                        }

                        // if the new error is better than the current error,
                        // then replace it
                        Err(new_error) => {
                            if new_error.heuristic() > current_error.heuristic()
                            {
                                current_error = new_error;
                            }
                        }
                    }
                }
            }

            Ok(Err(current_error))
        })
    }

    fn on_cyclic(
        &self,
        _: Self::InProgress,
        _: Self::InProgress,
        call_stacks: &[Call<DynArc, DynArc>],
    ) -> Result<Arc<Succeeded<PositiveSatisfied>>, PositiveError> {
        for call in call_stacks.iter().skip(1) {
            let (Some(_), Some(in_progress)) = (
                call.query.downcast_ref::<Self>(),
                call.in_progress.downcast_ref::<QuerySource>(),
            ) else {
                continue;
            };

            if *in_progress == QuerySource::Normal {
                return Ok(Arc::new(Succeeded::new(PositiveSatisfied::Cyclic)));
            }
        }

        Err(PositiveError::Cyclic)
    }
}

async fn substructural_derive(
    marker: &PositiveMarker,
    first_type: &Type,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<PositiveResult>, OverflowError> {
    let Some(field_types) =
        try_get_adt_fields(first_type, environment.tracked_engine()).await
    else {
        return Ok(None);
    };

    let mut constraints = BTreeSet::new();
    let mut structural_errors = Vec::new();

    for ty in field_types {
        let mut positive_marker_clone = marker.clone();

        // replace the first type argument with the field type
        positive_marker_clone
            .set_first_type_argument(ty.r#type().clone())
            .unwrap();

        match environment.query(&positive_marker_clone).await? {
            Ok(result) => {
                constraints.extend(result.constraints.iter().cloned());
            }
            Err(error) => structural_errors.push(StructuralError {
                sub_predicate: positive_marker_clone,
                error,
            }),
        }
    }

    if structural_errors.is_empty() {
        Ok(Some(Ok(Arc::new(Succeeded::with_constraints(
            PositiveSatisfied::Substructural,
            constraints,
        )))))
    } else {
        Ok(Some(Err(PositiveError::Structural(structural_errors.into()))))
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
    type InProgress = ();
    type Result = Option<Arc<Succeeded<NegativeSatisfied>>>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            // manually search for the trait implementation
            if let Ok(result) = environment
                .query(&resolution::Resolve::new(
                    self.marker_id(),
                    self.generic_arguments().clone(),
                ))
                .await?
                && environment.tracked_engine().get_kind(result.result.id).await
                    == Kind::NegativeImplementation
            {
                return Ok(Some(Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::Implementation(Implementation {
                        instantiation: result.result.instantiation.clone(),
                        id: result.result.id,
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
                if marker_premise.marker_id() != self.marker_id() {
                    continue;
                }

                let Some(compatiblity) = environment
                    .subtypes_generic_arguments(
                        self.generic_arguments(),
                        marker_premise.generic_arguments(),
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
                .generic_arguments_definite(self.generic_arguments())
                .await?
            else {
                return Ok(None);
            };

            Ok(environment.query(&self.to_positive()).await?.is_err().then(
                || {
                    Arc::new(Succeeded::with_constraints(
                        NegativeSatisfied::UnsatisfiedPositive,
                        definition.constraints,
                    ))
                },
            ))
        })
    }

    fn on_cyclic(
        &self,
        (): Self::InProgress,
        (): Self::InProgress,
        _: &[Call<crate::environment::DynArc, crate::environment::DynArc>],
    ) -> Self::Result {
        None
    }
}

async fn is_in_marker(
    marker_id: Global<pernixc_symbol::ID>,
    generic_arguments: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, OverflowError> {
    let query_site = environment.premise().query_site;

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
            .await
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
