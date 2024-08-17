//! Contains the definition of [`Environment`]
use std::{fmt::Debug, sync::Arc};

use getset::{CopyGetters, Getters};

use super::{
    definite,
    equality::Equality,
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    query::Context,
    term::r#type::{self, Type},
    Premise,
};
use crate::{
    symbol::table::{State, Table},
    type_system::{
        term::Kind, unification::Unification, visitor::RecursiveIterator,
        Compute, LifetimeUnifyingPredicate, OverflowError,
    },
};

/// A structure that contains the environment of the semantic logic.
#[derive(Debug, Getters, CopyGetters)]
pub struct Environment<'a, M: Model, T: State, N: Normalizer<M>> {
    /// The premise of the semantic logic.
    #[get = "pub"]
    pub(super) premise: Premise<M>,

    /// The table that contains the information of symbols.
    #[get_copy = "pub"]
    pub(super) table: &'a Table<T>,

    /// The normalizer used to normalize the inference variables.
    #[get_copy = "pub"]
    pub(super) normalizer: &'a N,
}

impl<'a, M: Model, T: State, N: Normalizer<M>> Clone
    for Environment<'a, M, T, N>
{
    fn clone(&self) -> Self {
        Self {
            premise: self.premise.clone(),
            table: self.table,
            normalizer: self.normalizer,
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

    /// Encounters the [`OverflowError`] while calculating the requirements for
    /// the given [`Predicate`].
    OverflowCalculatingRequirement(Predicate<M>),
}
fn check_definite_predicate<
    'a,
    T: Clone + Into<Predicate<M>> + Debug,
    M: Model,
    S: State,
    N: Normalizer<M>,
>(
    environment: &mut Environment<M, S, N>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<Predicate<M>>,
    definite_predicates: &mut Vec<Predicate<M>>,
    definite_check: impl Fn(
        &T,
        &Environment<M, S, N>,
    ) -> Result<bool, OverflowError>,
) {
    // pick a predicate
    'outer: for i in 0..predicates.len() {
        let predicate_i = &predicates[i];

        // remove the predicate before checking
        if remove_on_check {
            assert!(environment
                .premise
                .predicates
                .remove(&predicate_i.clone().into()));
        }

        match definite_check(predicate_i, &environment) {
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

            Err(_) => {
                // add to the overflow set
                overflow_predicates.push(predicate_i.clone().into());

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
    'a,
    T: Clone + Into<Predicate<M>> + Debug,
    M: Model,
    S: State,
    N: Normalizer<M>,
>(
    environment: &mut Environment<M, S, N>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<Predicate<M>>,
    ambiguous_predicates: &mut Vec<Vec<T>>,
    ambiguity_check: impl Fn(
        &T,
        &T,
        &Environment<M, S, N>,
    ) -> Result<bool, OverflowError>,
) {
    // pick a predicate
    'outer: for i in 0..predicates.len() {
        let predicate_i = &predicates[i];

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

                Err(_) => {
                    // add to the overflow set
                    overflow_predicates.push(predicate_i.clone().into());

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
        for j in i + 1..predicates.len() {
            let predicate_j = &predicates[j];

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

                Err(_) => {
                    // add to the overflow set
                    overflow_predicates.push(predicate_i.clone().into());

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

impl<'a, M: Model, T: State, N: Normalizer<M>> Environment<'a, M, T, N> {
    /// Looks for the errors in the environment and returns a vector of errors.
    pub fn diagnose(mut self) -> Vec<Error<M>> {
        let mut ambiguous_trait_predicates_set = Vec::new();
        let mut ambiguous_constant_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_constant_predicates_set = Vec::new();
        let mut ambiguous_trait_type_equality_predicates_set = Vec::new();
        let mut recursive_trait_type_equality_predicates = Vec::new();
        let mut overflow_predicates = Vec::new();
        let mut definite_predicate = Vec::new();

        let all_trait_predicates = self
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait)
            .cloned()
            .collect::<Vec<_>>();
        let all_constant_type_predicates = self
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_constant_type)
            .cloned()
            .collect::<Vec<_>>();
        let all_tuple_type_predicates = self
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_tuple_type)
            .cloned()
            .collect::<Vec<_>>();
        let all_tuple_constant_predicates = self
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_tuple_constant)
            .cloned()
            .collect::<Vec<_>>();
        let all_trait_type_equality_predicates = self
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait_type_equality)
            .cloned()
            .collect::<Vec<_>>();

        let lifetime_unifier = Arc::new(LifetimeUnifyingPredicate);

        check_ambiguous_predicates(
            &mut self,
            false,
            &all_trait_predicates,
            &mut overflow_predicates,
            &mut ambiguous_trait_predicates_set,
            |lhs, rhs, environment| {
                // check if the trait is the same
                if lhs.id != rhs.id {
                    return Ok(false);
                }

                // check if the arguments counts are the same
                if lhs.generic_arguments.lifetimes.len()
                    != rhs.generic_arguments.lifetimes.len()
                    || lhs.generic_arguments.types.len()
                        != rhs.generic_arguments.types.len()
                    || lhs.generic_arguments.constants.len()
                        != rhs.generic_arguments.constants.len()
                {
                    return Ok(false);
                }

                for (lhs_ty, rhs_ty) in lhs
                    .generic_arguments
                    .types
                    .iter()
                    .zip(rhs.generic_arguments.types.iter())
                {
                    if Unification::new(
                        lhs_ty.clone(),
                        rhs_ty.clone(),
                        lifetime_unifier.clone(),
                    )
                    .query(environment)?
                    .is_none()
                    {
                        return Ok(false);
                    }
                }

                for (lhs_const, rhs_const) in lhs
                    .generic_arguments
                    .constants
                    .iter()
                    .zip(rhs.generic_arguments.constants.iter())
                {
                    if Equality::new(lhs_const.clone(), rhs_const.clone())
                        .query(environment)?
                        .is_none()
                    {
                        return Ok(false);
                    }
                }

                // found no incompatibility, the term is ambiguous
                Ok(true)
            },
        );
        check_ambiguous_predicates(
            &mut self,
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
            &mut self,
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
            &mut self,
            false,
            &all_tuple_constant_predicates,
            &mut overflow_predicates,
            &mut ambiguous_tuple_constant_predicates_set,
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
            &mut self,
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
            &mut self,
            false,
            &all_trait_predicates,
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
            &mut self,
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
            &mut self,
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
            &mut self,
            false,
            &all_tuple_constant_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                definite::Definite(predicate.0.clone())
                    .query(environment)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut self,
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
            assert!(self.premise.predicates.remove(&equality));

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
                .query(&self)
                {
                    Ok(Some(_)) => {
                        if ambiguous_trait_type_equality_predicates_set
                            .iter()
                            .flatten()
                            .find(|x| {
                                *x == equality.as_trait_type_equality().unwrap()
                            })
                            .is_none()
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

                    Err(OverflowError) => {
                        if !overflow_predicates.contains(&equality) {
                            overflow_predicates.push(equality.clone());
                        }
                        break;
                    }

                    Ok(None) => {}
                }
            }

            // add the equality back
            assert!(self.premise.predicates.insert(equality));
        }

        overflow_predicates.sort();
        overflow_predicates.dedup();

        // remove the ambiguous and ill-formed predicates
        for predicate_to_remove in overflow_predicates
            .iter()
            .cloned()
            .chain(
                ambiguous_trait_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::Trait),
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
                ambiguous_tuple_constant_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::TupleConstant),
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
            self.premise.predicates.remove(&predicate_to_remove);
        }

        let ambiguous_predicates_vecs: Vec<Vec<Predicate<M>>> =
            ambiguous_trait_predicates_set
                .into_iter()
                .map(|x| x.into_iter().map(|x| x.into()).collect())
                .chain(
                    ambiguous_constant_type_predicates_set
                        .into_iter()
                        .map(|x| x.into_iter().map(|x| x.into()).collect()),
                )
                .chain(
                    ambiguous_tuple_type_predicates_set
                        .into_iter()
                        .map(|x| x.into_iter().map(|x| x.into()).collect()),
                )
                .chain(
                    ambiguous_tuple_constant_predicates_set
                        .into_iter()
                        .map(|x| x.into_iter().map(|x| x.into()).collect()),
                )
                .chain(
                    ambiguous_trait_type_equality_predicates_set
                        .into_iter()
                        .map(|x| x.into_iter().map(|x| x.into()).collect()),
                )
                .collect();

        overflow_predicates
            .into_iter()
            .map(Error::OverflowCalculatingRequirement)
            .chain(
                ambiguous_predicates_vecs
                    .into_iter()
                    .map(Error::AmbiguousPredicates),
            )
            .chain(
                recursive_trait_type_equality_predicates
                    .into_iter()
                    .map(Error::RecursiveTraitTypeEqualityPredicate),
            )
            .chain(definite_predicate.into_iter().map(Error::DefinintePremise))
            .collect()
    }
    /// Creates a new [`Environment`].
    ///
    /// The ambiguous predicates will be removed from the environment and is
    /// extracted out to the vector of [`NewEnvironmentError`].
    #[must_use]
    pub fn new(
        premise: Premise<M>,
        table: &'a Table<T>,
        normalizer: &'a N,
    ) -> Self {
        Self { premise, table, normalizer }
    }
}

// TODO: Add test back
// #[cfg(test)]
// mod tests;
