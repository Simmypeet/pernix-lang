use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use pernixc_table::{component::SymbolKind, GlobalID};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    lifetime::Lifetime,
    predicate::{
        NegativeMarker as Negative, PositiveMarker as Positive, Predicate,
    },
    r#type::Type,
    variance::Variance,
    visitor::{self, Element, VisitNonApplicationTermError},
    Model,
};

use crate::{
    environment::{Call, DynArc, Environment, Query},
    normalizer::Normalizer,
    resolution::{self, Implementation},
    term::Term,
    AbruptError, Satisfied, Succeeded,
};

/// An enumeration of ways the marker can be satisfied.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PositiveSatisfied<M: Model> {
    /// Found a matching premise.
    Premise,

    /// Found a matching implementation.
    Implementation(Implementation<M>),

    /// Satisfied by proving that all the fields/sub-terms are satisfied.
    Congruence(BTreeMap<Positive<M>, Arc<Succeeded<PositiveSatisfied<M>, M>>>),

    /// Satisfied by the fact that the query was made in the marker/its
    /// implementation.
    Environment,

    /// Satisfied by co-inductive reasoning.
    Cyclic,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum VisitorState<M: Model> {
    Failed,
    AbruptError(AbruptError),
    Succeeded(BTreeMap<Positive<M>, Arc<Succeeded<PositiveSatisfied<M>, M>>>),
}

#[derive(Debug)]
struct Visitor<'t, 'p, N: Normalizer<M>, M: Model> {
    original: &'p Positive<M>,
    state: VisitorState<M>,
    environment: &'t Environment<'t, M, N>,
}

impl<'t, 'p, N: Normalizer<M>, M: Model> visitor::Visitor<'_, Lifetime<M>>
    for Visitor<'t, 'p, N, M>
{
    fn visit(
        &mut self,
        _: &Lifetime<M>,
        _: <Lifetime<M> as Element>::Location,
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

impl<'t, 'p, N: Normalizer<M>, M: Model> visitor::Visitor<'_, Type<M>>
    for Visitor<'t, 'p, N, M>
{
    fn visit(
        &mut self,
        ty: &Type<M>,
        _: <Type<M> as Element>::Location,
    ) -> bool {
        let VisitorState::Succeeded(states) = &mut self.state else {
            return false;
        };

        let new_query = Positive {
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

        match self.environment.query(&new_query) {
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

impl<'t, 'p, N: Normalizer<M>, M: Model> visitor::Visitor<'_, Constant<M>>
    for Visitor<'t, 'p, N, M>
{
    fn visit(
        &mut self,
        _: &Constant<M>,
        _: <Constant<M> as Element>::Location,
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

impl<M: Model> Query for Positive<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<PositiveSatisfied<M>, M>;
    type Error = AbruptError;

    #[allow(clippy::too_many_lines)]
    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // if this query was made in marker, then check if the marker is the
        // same as the query one.
        if let Some(result) =
            is_in_marker(self.marker_id, &self.generic_arguments, environment)?
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

            let Some(compatiblity) = environment.generic_arguments_compatible(
                &self.generic_arguments,
                &premise.generic_arguments,
                Variance::Invariant,
            )?
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
        match environment
            .resolve_implementation(self.marker_id, &self.generic_arguments)
        {
            Ok(Succeeded {
                result:
                    Implementation { instantiation, id, is_not_general_enough },
                constraints,
            }) => {
                if *environment.table().get::<SymbolKind>(id)
                    == SymbolKind::PositiveMarkerImplementation
                {
                    return Ok(Some(Arc::new(Succeeded::with_constraints(
                        PositiveSatisfied::Implementation(Implementation {
                            instantiation,
                            id,
                            is_not_general_enough,
                        }),
                        constraints,
                    ))));
                }

                // then it's a negative implementation
                return Ok(None);
            }

            Err(resolution::Error::Abrupt(abrupt_error)) => {
                return Err(abrupt_error)
            }

            Err(_) => {}
        }

        // replace the first type argument with sub-terms/fields and check if
        // all the replacements are satisfied
        if let Some(ty) = self.generic_arguments.types.first() {
            let mut visitor = Visitor {
                original: self,
                state: VisitorState::Succeeded(BTreeMap::new()),
                environment,
            };

            let result = ty.accept_one_level(&mut visitor);

            match (visitor.state, result) {
                // can't find the sub-term / failed
                (VisitorState::Failed, _)
                | (
                    VisitorState::Succeeded(_),
                    Err(VisitNonApplicationTermError),
                ) => {}

                // abrupt error
                (VisitorState::AbruptError(error), _) => return Err(error),

                (VisitorState::Succeeded(mut btree_map), Ok(_)) => {
                    // including fields as well
                    for field_ty in ty
                        .get_adt_fields(environment.table())?
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
                                    let mut types =
                                        self.generic_arguments.types.clone();
                                    types[0] = field_ty.clone();
                                    types
                                },
                                constants: self
                                    .generic_arguments
                                    .constants
                                    .clone(),
                            },
                        };

                        if let Some(result) = environment.query(&new_query)? {
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
pub enum NegativeSatisfied<M: Model> {
    /// Found a matching premise.
    Premise,

    /// Found a matching negative implementation.
    Implementation(Implementation<M>),

    /// Satisfied by proving that the [`Positive`] isn't satisfied and the
    /// generic arguments are definite.
    UnsatisfiedPositive,
}

impl<M: Model> Query for Negative<M> {
    type Model = M;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<NegativeSatisfied<M>, M>;
    type Error = AbruptError;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // manually search for the trait implementation
        match environment
            .resolve_implementation(self.marker_id, &self.generic_arguments)
        {
            Ok(Succeeded {
                result:
                    Implementation { instantiation, id, is_not_general_enough },
                constraints,
            }) => {
                if *environment.table().get::<SymbolKind>(id)
                    == SymbolKind::NegativeMarkerImplementation
                {
                    return Ok(Some(Arc::new(Succeeded::with_constraints(
                        NegativeSatisfied::Implementation(Implementation {
                            instantiation,
                            id,
                            is_not_general_enough,
                        }),
                        constraints,
                    ))));
                }
            }

            Err(resolution::Error::Abrupt(abrupt_error)) => {
                return Err(abrupt_error);
            }

            Err(_) => {}
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

            let Some(compatiblity) = environment.generic_arguments_compatible(
                &self.generic_arguments,
                &marker_premise.generic_arguments,
                Variance::Invariant,
            )?
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
        let Some(definition) =
            environment.generic_arguments_definite(&self.generic_arguments)?
        else {
            return Ok(None);
        };
        Ok(environment
            .query(&Positive::new(
                self.marker_id,
                self.generic_arguments.clone(),
            ))?
            .is_none()
            .then(|| {
                Arc::new(Succeeded::with_constraints(
                    NegativeSatisfied::UnsatisfiedPositive,
                    definition.constraints,
                ))
            }))
    }
}

fn is_in_marker<M: Model>(
    marker_id: GlobalID,
    generic_arguments: &GenericArguments<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
    let Some(query_site) = environment.premise().query_site else {
        return Ok(None);
    };

    for current_id in environment.table().scope_walker(query_site) {
        let current_id = GlobalID::new(query_site.target_id, current_id);

        let SymbolKind::Trait =
            *environment.table().get::<SymbolKind>(current_id)
        else {
            continue;
        };

        // must be the same id
        if current_id != marker_id {
            continue;
        }

        let marker_generic_arguments = environment
            .table()
            .query::<GenericParameters>(current_id)
            .ok_or(AbruptError::CyclicDependency)?
            .create_identity_generic_arguments(current_id);

        let Some(compatibility) = environment.generic_arguments_compatible(
            generic_arguments,
            &marker_generic_arguments,
            Variance::Invariant,
        )?
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
