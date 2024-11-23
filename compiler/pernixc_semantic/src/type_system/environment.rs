//! Contains the definition of [`Environment`]
use std::{fmt::Debug, sync::Arc};

use getset::{CopyGetters, Getters};

use super::{
    definite,
    equality::Equality,
    model::Model,
    normalizer::{self, Normalizer},
    observer::{self, Observer},
    predicate::Predicate,
    query::Context,
    term::{
        r#type::{self, Type},
        GenericArguments,
    },
    Premise,
};
use crate::{
    symbol::table::{State, Table},
    type_system::{
        term::Kind, unification::Unification, visitor::RecursiveIterator,
        Compute, LifetimeUnifyingPredicate,
    },
};

/// A structure that contains the environment of the semantic logic.
#[derive(Debug, Getters, CopyGetters)]
pub struct Environment<
    'a,
    M: Model,
    T: State,
    N: Normalizer<M, T>,
    O: Observer<M, T>,
> {
    /// The premise of the semantic logic.
    #[get = "pub"]
    pub(super) premise: Premise<M>,

    /// The table that contains the information of symbols.
    #[get_copy = "pub"]
    pub(super) table: &'a Table<T>,

    /// The normalizer used to normalize the inference variables.
    #[get_copy = "pub"]
    pub(super) normalizer: &'a N,

    /// The observer used to observe all the query records made throughout the
    /// computation.
    #[get_copy = "pub"]
    pub(super) observer: &'a O,
}

impl<'a, M: Model, T: State, N: Normalizer<M, T>, O: Observer<M, T>> Clone
    for Environment<'a, M, T, N, O>
{
    fn clone(&self) -> Self {
        Self {
            premise: self.premise.clone(),
            table: self.table,
            normalizer: self.normalizer,
            observer: self.observer,
        }
    }
}

/// An enumeration of all errors encountered while creating a new environment.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error<M: Model> {
    /// The prediccates are ambiguous.
    ///
    /// The vector contains the set of predicates that are identical except for
    /// the lifetime arguments.
    AmbiguousPredicates(Vec<Predicate<M>>),

    /// The given predicate premise is definite; meaning that it's trivially
    /// known to be true/false.
    DefinintePremise(Predicate<M>),

    /// The [`Equality::lhs`] occurs in the [`Equality::rhs`].
    RecursiveTraitTypeEqualityPredicate(
        Equality<r#type::TraitMember<M>, Type<M>>,
    ),

    /// Encounters the [`super::Error`] while calculating the requirements for
    /// the given [`Predicate`].
    Overflow(Predicate<M>, super::OverflowError),
}

fn check_definite_predicate<
    T: Clone + Into<Predicate<M>> + Debug,
    M: Model,
    S: State,
    N: Normalizer<M, S>,
    O: Observer<M, S>,
>(
    environment: &mut Environment<M, S, N, O>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<(Predicate<M>, super::OverflowError)>,
    definite_predicates: &mut Vec<Predicate<M>>,
    definite_check: impl Fn(
        &T,
        &Environment<M, S, N, O>,
    ) -> Result<bool, super::OverflowError>,
) {
    // pick a predicate
    'outer: for predicate_i in predicates {
        // remove the predicate before checking
        if remove_on_check {
            assert!(environment
                .premise
                .predicates
                .remove(&predicate_i.clone().into()));
        }

        match definite_check(predicate_i, environment) {
            Ok(true) => {
                // add to the set
                definite_predicates.push(predicate_i.clone().into());

                if remove_on_check {
                    assert!(environment
                        .premise
                        .predicates
                        .insert(predicate_i.clone().into()));
                }
                continue 'outer; // no more checking for this
                                 // predicate.
            }

            Ok(false) => {
                // nothing to worry about
            }

            Err(error) => {
                // add to the overflow set
                overflow_predicates.push((predicate_i.clone().into(), error));

                if remove_on_check {
                    assert!(environment
                        .premise
                        .predicates
                        .insert(predicate_i.clone().into()));
                }
                continue 'outer; // no more checking for this
                                 // predicate.
            }
        }

        if remove_on_check {
            assert!(environment
                .premise
                .predicates
                .insert(predicate_i.clone().into()));
        }
    }
}

fn check_ambiguous_predicates<
    T: Clone + Into<Predicate<M>> + Debug,
    M: Model,
    S: State,
    N: Normalizer<M, S>,
    O: Observer<M, S>,
>(
    environment: &mut Environment<M, S, N, O>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<(Predicate<M>, super::OverflowError)>,
    ambiguous_predicates: &mut Vec<Vec<T>>,
    ambiguity_check: impl Fn(
        &T,
        &T,
        &Environment<M, S, N, O>,
    ) -> Result<bool, super::OverflowError>,
) {
    // pick a predicate
    'outer: for (i, predicate_i) in predicates.iter().enumerate() {
        // remove the predicate before checking
        if remove_on_check {
            assert!(environment
                .premise
                .predicates
                .remove(&predicate_i.clone().into()));
        }

        // check in the ambiguity set
        for ambiguous_predicates_set in &mut *ambiguous_predicates {
            // pick the first element in the set
            let first = &ambiguous_predicates_set[0];

            match ambiguity_check(predicate_i, first, environment) {
                Ok(true) => {
                    // add to the set
                    ambiguous_predicates_set.push(predicate_i.clone());

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }

                Ok(false) => {
                    // nothing to worry about
                }

                Err(error) => {
                    // add to the overflow set
                    overflow_predicates
                        .push((predicate_i.clone().into(), error));

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }
            }
        }

        // create a new set
        for predicate_j in predicates.iter().skip(i + 1) {
            if remove_on_check {
                assert!(environment
                    .premise
                    .predicates
                    .remove(&predicate_j.clone().into()));
            }

            match ambiguity_check(predicate_i, predicate_j, environment) {
                Ok(true) => {
                    // create a new set
                    ambiguous_predicates.push(vec![predicate_i.clone()]);

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_j.clone().into()));
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }

                Ok(false) => {
                    // nothing to worry about
                }

                Err(error) => {
                    // add to the overflow set
                    overflow_predicates
                        .push((predicate_i.clone().into(), error));

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_j.clone().into()));
                        assert!(environment
                            .premise
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }
            }

            if remove_on_check {
                assert!(environment
                    .premise
                    .predicates
                    .insert(predicate_j.clone().into()));
            }
        }

        if remove_on_check {
            assert!(environment
                .premise
                .predicates
                .insert(predicate_i.clone().into()));
        }
    }
}

fn check_ambiguous_generic_arguments<M: Model, S: State>(
    lhs: &GenericArguments<M>,
    rhs: &GenericArguments<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
) -> Result<bool, super::OverflowError> {
    // check if the arguments counts are the same
    if lhs.lifetimes.len() != rhs.lifetimes.len()
        || lhs.types.len() != rhs.types.len()
        || lhs.constants.len() != rhs.constants.len()
    {
        return Ok(false);
    }

    for (lhs_ty, rhs_ty) in lhs.types.iter().zip(rhs.types.iter()) {
        if Unification::new(
            lhs_ty.clone(),
            rhs_ty.clone(),
            Arc::new(LifetimeUnifyingPredicate),
        )
        .query(environment)?
        .is_none()
        {
            return Ok(false);
        }
    }

    for (lhs_const, rhs_const) in lhs.constants.iter().zip(rhs.constants.iter())
    {
        if Equality::new(lhs_const.clone(), rhs_const.clone())
            .query(environment)?
            .is_none()
        {
            return Ok(false);
        }
    }

    Ok(true)
}

impl<'a, M: Model, T: State, N: Normalizer<M, T>, O: Observer<M, T>>
    Environment<'a, M, T, N, O>
{
    /// Creates a new [`Environment`].
    ///
    /// The ambiguous predicates will be removed from the environment and is
    /// extracted out to the vector of [`Error`].
    #[allow(clippy::too_many_lines)]
    pub fn new_with(
        premise: Premise<M>,
        table: &'a Table<T>,
        normalizer: &'a N,
        observer: &'a O,
    ) -> (Self, Vec<Error<M>>) {
        let mut environment = Self { premise, table, normalizer, observer };

        let mut ambiguous_positive_trait_predicates_set = Vec::new();
        let mut ambiguous_negative_trait_predicates_set = Vec::new();
        let mut ambiguous_positive_marker_predicates_set = Vec::new();
        let mut ambiguous_negative_marker_predicates_set = Vec::new();
        let mut ambiguous_constant_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_type_predicates_set = Vec::new();
        let mut ambiguous_trait_type_equality_predicates_set = Vec::new();

        let mut recursive_trait_type_equality_predicates = Vec::new();

        let mut overflow_predicates = Vec::new();
        let mut definite_predicate = Vec::new();

        let mut all_positive_trait_predicates = Vec::new();
        let mut all_negative_trait_predicates = Vec::new();
        let mut all_positive_marker_predicates = Vec::new();
        let mut all_negative_marker_predicates = Vec::new();
        let mut all_constant_type_predicates = Vec::new();
        let mut all_tuple_type_predicates = Vec::new();
        let mut all_trait_type_equality_predicates = Vec::new();

        for predicate in &environment.premise.predicates {
            match predicate {
                Predicate::PositiveTrait(x) => {
                    all_positive_trait_predicates.push(x.clone());
                }

                Predicate::NegativeTrait(x) => {
                    all_negative_trait_predicates.push(x.clone());
                }

                Predicate::PositiveMarker(x) => {
                    all_positive_marker_predicates.push(x.clone());
                }

                Predicate::NegativeMarker(x) => {
                    all_negative_marker_predicates.push(x.clone());
                }

                Predicate::ConstantType(x) => {
                    all_constant_type_predicates.push(x.clone());
                }

                Predicate::TupleType(x) => {
                    all_tuple_type_predicates.push(x.clone());
                }

                Predicate::TraitTypeEquality(x) => {
                    all_trait_type_equality_predicates.push(x.clone());
                }

                Predicate::LifetimeOutlives(_) | Predicate::TypeOutlives(_) => {
                }
            }
        }

        let lifetime_unifier = Arc::new(LifetimeUnifyingPredicate);

        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_positive_trait_predicates,
            &mut overflow_predicates,
            &mut ambiguous_positive_trait_predicates_set,
            |lhs, rhs, environment| {
                // check if the trait is the same
                if lhs.id != rhs.id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_negative_trait_predicates,
            &mut overflow_predicates,
            &mut ambiguous_negative_trait_predicates_set,
            |lhs, rhs, environment| {
                // check if the trait is the same
                if lhs.id != rhs.id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_positive_marker_predicates,
            &mut overflow_predicates,
            &mut ambiguous_positive_marker_predicates_set,
            |lhs, rhs, environment| {
                // check if the marker is the same
                if lhs.id != rhs.id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_negative_marker_predicates,
            &mut overflow_predicates,
            &mut ambiguous_negative_marker_predicates_set,
            |lhs, rhs, environment| {
                // check if the marker is the same
                if lhs.id != rhs.id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_constant_type_predicates,
            &mut overflow_predicates,
            &mut ambiguous_constant_type_predicates_set,
            |lhs, rhs, environment| {
                Unification::new(
                    lhs.0.clone(),
                    rhs.0.clone(),
                    lifetime_unifier.clone(),
                )
                .query(environment)
                .map(|x| x.is_some())
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_tuple_type_predicates,
            &mut overflow_predicates,
            &mut ambiguous_tuple_type_predicates_set,
            |lhs, rhs, environment| {
                Unification::new(
                    lhs.0.clone(),
                    rhs.0.clone(),
                    lifetime_unifier.clone(),
                )
                .query(environment)
                .map(|x| x.is_some())
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            true,
            &all_trait_type_equality_predicates,
            &mut overflow_predicates,
            &mut ambiguous_trait_type_equality_predicates_set,
            |lhs, rhs, environment| {
                Unification::new(
                    Type::TraitMember(lhs.lhs.clone()),
                    Type::TraitMember(rhs.lhs.clone()),
                    lifetime_unifier.clone(),
                )
                .query(environment)
                .map(|x| x.is_some())
            },
        );

        check_definite_predicate(
            &mut environment,
            false,
            &all_positive_trait_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                predicate
                    .generic_arguments
                    .definite_with_context(environment, &mut Context::default())
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_negative_trait_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                predicate
                    .generic_arguments
                    .definite_with_context(environment, &mut Context::default())
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_positive_marker_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                predicate
                    .generic_arguments
                    .definite_with_context(environment, &mut Context::default())
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_negative_marker_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                predicate
                    .generic_arguments
                    .definite_with_context(environment, &mut Context::default())
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_constant_type_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                definite::Definite(predicate.0.clone())
                    .query(environment)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_tuple_type_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                definite::Definite(predicate.0.clone())
                    .query(environment)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            true,
            &all_trait_type_equality_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                definite::Definite(Type::TraitMember(predicate.lhs.clone()))
                    .query(environment)
                    .map(|x| x.is_some())
            },
        );

        for equality in all_trait_type_equality_predicates
            .iter()
            .cloned()
            .map(Predicate::TraitTypeEquality)
        {
            // temporarily remove the equality
            assert!(environment.premise.predicates.remove(&equality));

            // recursively check the equality
            for (kind, _) in RecursiveIterator::new(
                &equality.as_trait_type_equality().unwrap().rhs,
            ) {
                let Kind::Type(ty) = kind else {
                    continue;
                };

                match Unification::new(
                    Type::TraitMember(
                        equality.as_trait_type_equality().unwrap().lhs.clone(),
                    ),
                    ty.clone(),
                    lifetime_unifier.clone(),
                )
                .query(&environment)
                {
                    Ok(Some(_)) => {
                        if !ambiguous_trait_type_equality_predicates_set
                            .iter()
                            .flatten()
                            .any(|x| {
                                x == equality.as_trait_type_equality().unwrap()
                            })
                        {
                            recursive_trait_type_equality_predicates.push(
                                equality
                                    .as_trait_type_equality()
                                    .unwrap()
                                    .clone(),
                            );
                        }
                        break;
                    }

                    Err(error) => {
                        let result = (equality.clone(), error);

                        if !overflow_predicates.contains(&result) {
                            overflow_predicates.push(result);
                        }

                        break;
                    }

                    Ok(None) => {}
                }
            }

            // add the equality back
            assert!(environment.premise.predicates.insert(equality));
        }

        overflow_predicates.sort();
        overflow_predicates.dedup();

        // remove the ambiguous and ill-formed predicates
        for predicate_to_remove in overflow_predicates
            .iter()
            .map(|x| &x.0)
            .cloned()
            .chain(
                ambiguous_positive_trait_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::PositiveTrait),
            )
            .chain(
                ambiguous_negative_trait_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::NegativeTrait),
            )
            .chain(
                ambiguous_positive_marker_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::PositiveMarker),
            )
            .chain(
                ambiguous_negative_marker_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::NegativeMarker),
            )
            .chain(
                ambiguous_constant_type_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::ConstantType),
            )
            .chain(
                ambiguous_tuple_type_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::TupleType),
            )
            .chain(
                ambiguous_trait_type_equality_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::TraitTypeEquality),
            )
            .chain(
                recursive_trait_type_equality_predicates
                    .iter()
                    .cloned()
                    .map(Predicate::TraitTypeEquality),
            )
            .chain(definite_predicate.iter().cloned())
        {
            environment.premise.predicates.remove(&predicate_to_remove);
        }

        let ambiguous_predicates_vecs = ambiguous_positive_trait_predicates_set
            .into_iter()
            .map(|x| x.into_iter().map(Into::into).collect())
            .chain(
                ambiguous_constant_type_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            )
            .chain(
                ambiguous_tuple_type_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            )
            .chain(
                ambiguous_trait_type_equality_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            );

        let errors = overflow_predicates
            .into_iter()
            .map(|(predicate, error)| Error::Overflow(predicate, error))
            .chain(ambiguous_predicates_vecs.map(Error::AmbiguousPredicates))
            .chain(
                recursive_trait_type_equality_predicates
                    .into_iter()
                    .map(Error::RecursiveTraitTypeEqualityPredicate),
            )
            .chain(definite_predicate.into_iter().map(Error::DefinintePremise))
            .collect::<Vec<_>>();

        (environment, errors)
    }
}

impl<'a, M: Model, S: State>
    Environment<'a, M, S, normalizer::NoOp, observer::NoOp>
{
    /// Creates a new [`Environment`].
    ///
    /// The ambiguous predicates will be removed from the environment and is
    /// extracted out to the vector of [`Error`].
    pub fn new(
        premise: Premise<M>,
        table: &'a Table<S>,
    ) -> (Self, Vec<Error<M>>) {
        Self::new_with(premise, table, normalizer::NO_OP, observer::NO_OP)
    }
}

#[cfg(test)]
mod tests;
