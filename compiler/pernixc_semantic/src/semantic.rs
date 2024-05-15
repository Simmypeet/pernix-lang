//! Contains the semantic logic of the compiler (i.e. type checking/system).

use std::collections::HashSet;

use enum_as_inner::EnumAsInner;
use getset::{Getters, MutGetters};

use self::{
    equivalent::Equivalent,
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    session::{ExceedLimitError, Limit, Session},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
};
use crate::{
    arena::ID,
    symbol::{Trait, TraitImplementation},
    table::{State, Table},
};

pub mod deduction;
pub mod equality;
pub mod equivalent;
pub mod instantiation;
pub mod mapping;
pub mod matching;
pub mod model;
pub mod normalizer;
pub mod order;
pub mod predicate;
pub mod session;
pub mod simplify;
pub mod sub_term;
pub mod term;
pub mod unification;
pub mod visitor;

/// A tag type signaling that the predicate/query is satisfied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Satisfied;

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
    InTraitImplementation(ID<TraitImplementation>),

    /// The semantic logic is currently taking place in a trait.
    InTrait(ID<Trait>),

    /// The semantic logic is currently taking place in other than the above.
    #[default]
    Normal,
}

/// The foundation truth used to derive further arguments.
#[derive(Debug, Clone, Default, Getters, MutGetters)]
pub struct Premise<M: Model> {
    /// Contains the equivalent classes for lifetimes, types, and constants.
    #[get = "pub"]
    equivalent: Equivalent<M>,

    /// The list of predicates
    #[get = "pub"]
    predicates: HashSet<Predicate<M>>,

    /// The environment of the premise.
    pub trait_context: TraitContext,
}

impl<M: Model> Premise<M> {
    /// Appends the given predicates to the premise.
    pub fn append_from_predicates(
        &mut self,
        predicates: impl Iterator<Item = Predicate<M>>,
    ) {
        for predicate in predicates {
            if let Predicate::TraitTypeEquality(eq) = &predicate {
                self.equivalent
                    .insert(Type::TraitMember(eq.lhs.clone()), eq.rhs.clone());
            }

            self.predicates.insert(predicate);
        }
    }

    /// Creates a new [`Premise`] with the given predicates.
    pub fn from_predicates(
        predicates: impl Iterator<Item = Predicate<M>>,
    ) -> Self {
        let mut premise = Self::default();
        premise.append_from_predicates(predicates);
        premise
    }
}

/// A structure that contains the environment of the semantic logic.
#[derive(Debug, Clone, Copy)]
pub struct Environment<'a, M: Model, T: State, N: Normalizer<M>> {
    /// The premise of the semantic logic.
    pub premise: &'a Premise<M>,

    /// The table that contains the information of symbols.
    pub table: &'a Table<T>,

    /// The normalizer used to normalize the inference variables.
    pub normalizer: &'a N,
}

/// Gets the list of equivalent terms for the given term.
///
/// This including normalized term and equivalent classes.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn get_equivalences<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Vec<T>, ExceedLimitError> {
    let mut equivalences = (term.normalize(environment, limit)?)
        .map_or_else(Vec::new, |result| vec![result]);

    for values in T::get_equivalent_classes(&environment.premise.equivalent) {
        let mut equals = false;
        for value in values.iter() {
            if equality::equals(value, term, environment, limit)? {
                equals = true;
                break;
            }
        }

        if equals {
            equivalences.extend(values.iter().filter(|x| *x != term).cloned());
        }
    }

    Ok(equivalences)
}

#[cfg(test)]
pub(super) mod tests;
