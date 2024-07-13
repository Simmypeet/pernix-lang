//! Contains the semantic logic of the compiler (i.e. type checking/system).

use std::{collections::HashSet, sync::Arc};

use enum_as_inner::EnumAsInner;
use equality::Equality;
use getset::{CopyGetters, Getters};
use predicate::Outlives;
use query::{Cached, Context, Query, QueryCall};
use term::{constant::Constant, r#type, Kind};
use unification::{Log, Unification};
use visitor::RecursiveIterator;

use self::{
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    term::{lifetime::Lifetime, r#type::Type},
};
use crate::{
    arena::ID,
    symbol::{
        table::{State, Table},
        PositiveTraitImplementation, Trait,
    },
    unordered_pair::UnorderedPair,
};

pub mod compatible;
pub mod deduction;
pub mod definite;
pub mod equality;
pub mod equivalence;
pub mod fresh;
pub mod instantiation;
pub mod mapping;
pub mod matching;
pub mod model;
pub mod normalizer;
pub mod order;
pub mod predicate;
pub mod query;
pub mod simplify;
pub mod sub_term;
pub mod term;
pub mod type_check;
pub mod unification;
pub mod visitor;

/// A struct implementing the [`unification::Predicate`] that allows the
/// lifetime to be unified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct LifetimeUnifyingPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>>
    for LifetimeUnifyingPredicate
{
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

impl<M: Model> unification::Predicate<Constant<M>>
    for LifetimeUnifyingPredicate
{
    fn unifiable(
        &self,
        _: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(None)
    }
}

/// An error that occurs when the number of queries exceeds the limit.
///
/// Due to the fact that the semantic system is partially-decidable, it is
/// possible that the number of queries can be infinite. To prevent this, a
/// limit is set to the number of queries that can be made. However, in most
/// cases, the number of queries should not exceed the limit.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[error("exceeded the limit of the number of queries")]
#[allow(missing_docs)]
pub struct OverflowError;

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Satisfied;

/// Contains constraints related to lifetimes.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint<M: Model> {
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    LifetimeMatching(UnorderedPair<Lifetime<M>>),
}

/// The result of the semantic logic in the success case.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Succeeded<Result, M: Model> {
    /// The result of the query in the success case.
    pub result: Result,

    /// The additional outlives constraints that must be satisfied for the
    /// query to be considered successful.
    pub constraints: HashSet<LifetimeConstraint<M>>,
}

impl<Result, M: Model> Succeeded<Result, M> {
    /// Creates a new [`Succeeded`] with the given result and no constraints.
    #[must_use]
    pub fn new(result: Result) -> Self {
        Self { result, constraints: HashSet::new() }
    }

    /// Creates a new [`Succeeded`] with the given result and constraints.
    #[must_use]
    pub fn with_constraints(
        result: Result,
        constraints: HashSet<LifetimeConstraint<M>>,
    ) -> Self {
        Self { result, constraints }
    }
}

impl<M: Model> Succeeded<Satisfied, M> {
    /// Creates a new [`Succeeded`] with the [`Satisfied`] result.
    #[must_use]
    pub fn satisfied() -> Self { Self::new(Satisfied) }

    /// Creates a new [`Succeeded`] with the [`Satisfied`] result and
    /// constraints.
    #[must_use]
    pub fn satisfied_with(constraints: HashSet<LifetimeConstraint<M>>) -> Self {
        Self::with_constraints(Satisfied, constraints)
    }

    /// Combines the constraints of two satisified results.
    #[must_use]
    pub fn combine(mut self, other: Self) -> Self {
        self.constraints.extend(other.constraints);
        self
    }
}

/// The type alias for the [`Option`] of [`Succeeded`].
///
/// The [`None`] is returned if failed to prove the query. Otherwise, the
/// [`Succeeded`] is returned with the result and additional constraints.
pub type Output<Result, M> = Option<Succeeded<Result, M>>;

/// Contains the premise of the semantic logic.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premise<M: Model> {
    /// List of predicates that will be considered as facts.
    pub predicates: HashSet<Predicate<M>>,

    /// The extra trait context that has a particular effect on the semantic.
    pub trait_context: TraitContext,
}

/// Extra environment content that has a particular effect on the semantic
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    EnumAsInner,
)]
pub enum TraitContext {
    /// The semantic logic is currently taking place in a trait implementation.
    InTraitImplementation(ID<PositiveTraitImplementation>),

    /// The semantic logic is currently taking place in a trait.
    InTrait(ID<Trait>),

    /// The semantic logic is currently taking place in other than the above.
    #[default]
    Normal,
}

/// A structure that contains the environment of the semantic logic.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Environment<'a, M: Model, T: State, N: Normalizer<M>> {
    /// The premise of the semantic logic.
    #[get = "pub"]
    premise: Premise<M>,

    /// The table that contains the information of symbols.
    #[get_copy = "pub"]
    table: &'a Table<T>,

    /// The normalizer used to normalize the inference variables.
    #[get_copy = "pub"]
    normalizer: &'a N,
}

/// An enumeration of all errors encountered while creating a new environment.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum NewEnvironmentError<M: Model> {
    AmbiguousTraitPredicates(Vec<predicate::Trait<M>>),
    AmbiguousConstantTypePredicates(Vec<predicate::ConstantType<Type<M>>>),
    AmbiguousTupleTypePredicates(Vec<predicate::Tuple<Type<M>>>),
    AmbiguousTupleConstantPredicate(Vec<predicate::Tuple<Constant<M>>>),
    AmbiguousTraitTypeEqualityPredicates(
        Vec<Equality<r#type::TraitMember<M>, Type<M>>>,
    ),
    /// The [`Equality::lhs`] occurs in the [`Equality::rhs`].
    RecursiveTraitTypeEqualityPredicate(
        Equality<r#type::TraitMember<M>, Type<M>>,
    ),

    /// Encounters the [`OverflowError`] while calculating the requirements for
    /// the given [`Predicate`].
    OverflowCalculatingRequirement(Predicate<M>),
}

impl<'a, M: Model, T: State, N: Normalizer<M>> Environment<'a, M, T, N> {
    /// Creates a new [`Environment`].
    ///
    /// The ambiguous predicates will be removed from the environment and is
    /// extracted out to the vector of [`NewEnvironmentError`].
    #[must_use]
    pub fn new(
        premise: Premise<M>,
        table: &'a Table<T>,
        normalizer: &'a N,
    ) -> (Self, Vec<NewEnvironmentError<M>>) {
        fn check_ambiguous_predicates<
            'a,
            T: Clone + Into<Predicate<M>>,
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
                            overflow_predicates
                                .push(predicate_i.clone().into());

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

                    match ambiguity_check(predicate_i, predicate_j, environment)
                    {
                        Ok(true) => {
                            // create a new set
                            ambiguous_predicates
                                .push(vec![predicate_i.clone()]);

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
                            overflow_predicates
                                .push(predicate_i.clone().into());

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

                if remove_on_check {
                    assert!(environment
                        .premise
                        .predicates
                        .insert(predicate_i.clone().into()));
                }
            }
        }

        let mut ambiguous_trait_predicates_set = Vec::new();
        let mut ambiguous_constant_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_constant_predicates_set = Vec::new();
        let mut ambiguous_trait_type_equality_predicates_set = Vec::new();
        let mut recursive_trait_type_equality_predicates = Vec::new();
        let mut overflow_predicates = Vec::new();

        let mut environment = Self { premise, table, normalizer };

        let all_trait_predicates = environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait)
            .cloned()
            .collect::<Vec<_>>();
        let all_constant_type_predicates = environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_constant_type)
            .cloned()
            .collect::<Vec<_>>();
        let all_tuple_type_predicates = environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_tuple_type)
            .cloned()
            .collect::<Vec<_>>();
        let all_tuple_constant_predicates = environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_tuple_constant)
            .cloned()
            .collect::<Vec<_>>();
        let all_trait_type_equality_predicates = environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait_type_equality)
            .cloned()
            .collect::<Vec<_>>();

        let lifetime_unifier = Arc::new(LifetimeUnifyingPredicate);

        check_ambiguous_predicates(
            &mut environment,
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
            assert!(environment.premise.predicates.insert(equality));
        }

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
        {
            assert!(environment
                .premise
                .predicates
                .remove(&predicate_to_remove));
        }

        (
            environment,
            overflow_predicates
                .into_iter()
                .map(NewEnvironmentError::OverflowCalculatingRequirement)
                .chain(
                    ambiguous_trait_predicates_set
                        .into_iter()
                        .map(NewEnvironmentError::AmbiguousTraitPredicates),
                )
                .chain(
                    ambiguous_constant_type_predicates_set.into_iter().map(
                        NewEnvironmentError::AmbiguousConstantTypePredicates,
                    ),
                ).chain(
                    ambiguous_tuple_type_predicates_set.into_iter().map(
                        NewEnvironmentError::AmbiguousTupleTypePredicates,
                    ),
                ).chain(
                    ambiguous_tuple_constant_predicates_set.into_iter().map(
                        NewEnvironmentError::AmbiguousTupleConstantPredicate,
                    ),
                ).chain(
                    ambiguous_trait_type_equality_predicates_set.into_iter().map(
                        NewEnvironmentError::AmbiguousTraitTypeEqualityPredicates,
                    ),
                ).chain(
                    recursive_trait_type_equality_predicates.into_iter().map(
                        NewEnvironmentError::RecursiveTraitTypeEqualityPredicate
                    )
                )
                .collect(),
        )
    }
}

/// A trait used for computing the result of the query.
#[allow(private_bounds)]
pub trait Compute: Query {
    /// The error type of the computation
    type Error: From<OverflowError>;

    #[doc(hidden)]
    type Parameter: Default;

    /// The raw implementation of the query.
    ///
    /// The implementation shouldn't include the context's state mutation
    /// (mark_as_done, clear_query).
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error>;

    /// The result to return of the query when the query is cyclic.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[QueryCall<Self::Model>],
    ) -> Result<Option<Self::Result>, Self::Error> {
        Ok(None) /* the default implementation is to make the query fails */
    }

    /// Queries the result.
    #[allow(private_interfaces, private_bounds)]
    fn query(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
    ) -> Result<Option<Self::Result>, Self::Error> {
        let mut context = Context::default();
        self.query_with_context(environment, &mut context)
    }

    /// Queries the result with the explicitly specified context.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn query_with_context(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Option<Self::Result>, Self::Error> {
        self.query_with_context_full(
            environment,
            context,
            Self::Parameter::default(),
            Self::InProgress::default(),
        )
    }

    /// Queries the result with the explicitly specified context, in-progress
    /// state, and additional parameters.
    #[doc(hidden)]
    #[allow(private_interfaces, private_bounds)]
    fn query_with_context_full(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        match context.mark_as_in_progress(self.clone(), in_progress.clone())? {
            Some(Cached::Done(result)) => return Ok(Some(result)),
            Some(Cached::InProgress(new_in_progress)) => {
                let position = context
                    .call_stack()
                    .iter()
                    .position(|x| {
                        Self::from_call(x).map_or(false, |x| &x.query == self)
                    })
                    .expect("should exist");

                // circular dependency
                let result = self.on_cyclic(
                    parameter,
                    in_progress,
                    new_in_progress,
                    &context.call_stack()[position..],
                )?;

                return Ok(result);
            }
            None => { /*no circular dependency, continue...*/ }
        }

        let result =
            self.implementation(environment, context, parameter, in_progress);

        match result {
            Ok(Some(result)) => {
                // remembers the result
                assert!(context.mark_as_done(self, result.clone()));
                Ok(Some(result))
            }
            result @ (Ok(None) | Err(_)) => {
                // reset the query state
                assert!(context.clear_query(self).is_some());
                result
            }
        }
    }
}

#[cfg(test)]
mod tests;
