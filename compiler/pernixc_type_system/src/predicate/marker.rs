use std::{collections::BTreeMap, sync::Arc};

use enum_as_inner::EnumAsInner;
use pernixc_table::GlobalID;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    lifetime::Lifetime,
    predicate::{NegativeMarker as Negative, PositiveMarker as Positive},
    r#type::Type,
    visitor::{self, Element},
    Model,
};

use crate::{
    environment::{Call, DynArc, Environment, Query},
    normalizer::Normalizer,
    resolution::Implementation,
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
    Congruence(BTreeMap<Positive<M>, Succeeded<PositiveSatisfied<M>, M>>),

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

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        todo!()
        /*
        // if this query was made in marker, then check if the marker is the
        // same as the query one.
        if let Some(result) = is_in_marker(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        )? {
            return Ok(Some(Succeeded::with_constraints(
                PositiveSatisfied::ByEnvironment,
                result.constraints,
            )));
        }

        // look for the matching premise
        for premise in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_positive_marker)
        {
            // skip if id is different
            if premise.id != self.id {
                continue;
            }

            let Some(compatiblity) =
                self.generic_arguments.compatible_with_context(
                    &premise.generic_arguments,
                    Variance::Invariant,
                    environment,
                    context,
                )?
            else {
                continue;
            };

            if !compatiblity.result.forall_lifetime_errors.is_empty() {
                continue;
            }

            return Ok(Some(Succeeded::with_constraints(
                PositiveSatisfied::ByPremise,
                compatiblity.constraints,
            )));
        }

        // manually search for the trait implementation
        match resolve_implementation_with_context(
            self.id,
            &self.generic_arguments,
            environment,
            context,
        ) {
            Ok(Succeeded {
                result:
                    Implementation {
                        instantiation,
                        id: MarkerImplementationID::Positive(id),
                        is_not_general_enough,
                    },
                constraints,
            }) => {
                return Ok(Some(Succeeded::with_constraints(
                    PositiveSatisfied::ByImplementation(Implementation {
                        instantiation,
                        id,
                        is_not_general_enough,
                    }),
                    constraints,
                )))
            }

            Ok(Succeeded {
                result:
                    Implementation {
                        id: MarkerImplementationID::Negative(_), ..
                    },
                ..
            }) => {
                // no more continuing
                return Ok(None);
            }

            Err(ResolutionError::Overflow(overflow_err)) => {
                return Err(overflow_err)
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
                context,
            };

            let result = ty.accept_one_level(&mut visitor);

            match (visitor.state, result) {
                // can't find the sub-term / failed
                (VisitorState::Failed, _)
                | (
                    VisitorState::Succeeded(_),
                    Err(VisitNonApplicationTermError),
                ) => {}

                // overflow error
                (VisitorState::Overflowed, _) => return Err(OverflowError),

                (VisitorState::Succeeded(mut btree_map), Ok(_)) => {
                    // including fields as well
                    for field_ty in ty
                        .get_adt_fields(environment.table)
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

                        if let Some(Succeeded { result, constraints }) =
                            new_query
                                .query_with_context(environment, context)?
                        {
                            btree_map.insert(new_query, Succeeded {
                                result,
                                constraints,
                            });
                        } else {
                            return Ok(None);
                        }
                    }

                    return Ok(Some(Succeeded {
                        result: PositiveSatisfied::ByCongruence(btree_map),
                        constraints: BTreeSet::new(),
                    }));
                }
            }
        }

        Ok(None)
        */
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
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        todo!()
    }
}

fn is_in_marker<M: Model>(
    marker_id: GlobalID,
    generic_arguments: &GenericArguments<M>,
    environment: &mut Environment<M, impl Normalizer<M>>,
) -> Result<Option<Succeeded<Satisfied, M>>, AbruptError> {
    todo!()
    /*
    let Some(query_site) = environment.premise.query_site else {
        return Ok(None);
    };

    for item_id in
        if let Some(iter) = environment.table.scope_walker(query_site) {
            iter
        } else {
            return Ok(None);
        }
    {
        let ItemID::Marker(env_marker_id) = item_id else {
            continue;
        };

        // must be the same id
        if env_marker_id != marker_id {
            return Ok(None);
        }

        let marker_symbol = environment.table.get(marker_id).unwrap();

        let trait_generic_arguments = marker_symbol
            .generic_declaration()
            .parameters
            .create_identity_generic_arguments(env_marker_id.into());

        let Some(compatiblity) = generic_arguments.compatible_with_context(
            &trait_generic_arguments,
            Variance::Invariant,
            environment,
            context,
        )?
        else {
            return Ok(None);
        };

        if compatiblity.result.forall_lifetime_errors.is_empty() {
            return Ok(Some(Succeeded::satisfied_with(
                compatiblity.constraints,
            )));
        }
    }

    Ok(None)
    */
}
