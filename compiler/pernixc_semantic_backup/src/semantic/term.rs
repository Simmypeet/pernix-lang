//! Contains all the declarations of the semantic terms.

use std::{fmt::Debug, hash::Hash};

use enum_as_inner::EnumAsInner;

use self::{constant::Constant, lifetime::Lifetime, r#type::Type};
use super::{
    definite, equality,
    map::Map,
    model::{Entity, Model},
    predicate::Premises,
    session::Session,
    substitution::{Substitute, Substitution},
    unification::{self, Config, Substructural},
    visitor::{self, Element, Visitor},
    Semantic,
};
use crate::table::{State, Success, Table};

pub mod constant;
pub mod lifetime;
pub mod r#type;

/// A trait implemented by `Type`, `Constant` and `Lifetime` providing a common
/// interface for processing the terms.
pub trait Term:
    Eq
    + Hash
    + Clone
    + Debug
    + Ord
    + Element<Model = <Self as Term>::Model>
    + Map<Model = <Self as Term>::Model>
    + Substitute<Model = <Self as Term>::Model>
    + 'static
    + Send
    + Sync
{
    /// The model in which the term is defined.
    type Model: Model;

    /// The type of iterator returned by [`Self::outlives_premises`]
    type OutlivesPremisesIter<'a>: Iterator<
        Item = (&'a Self, &'a Lifetime<<Self as Term>::Model>),
    >
    where
        Self: 'a;

    /// The type of iterator returned by [`Self::constant_type_premises`]
    type ConstantTypePremisesIter<'a>: Iterator<Item = &'a Self>
    where
        Self: 'a;

    /// Normalizes the given term to its canonical form.
    ///
    /// Normally, this used fro normalizing the trait-member term to its
    /// implementation.
    fn normalize<
        S: Semantic<Self>
            + Semantic<Type<<Self as Term>::Model>>
            + Semantic<Constant<<Self as Term>::Model>>
            + Semantic<Lifetime<<Self as Term>::Model>>,
        R: Session<Self>
            + Session<Type<<Self as Term>::Model>>
            + Session<Constant<<Self as Term>::Model>>
            + Session<Lifetime<<Self as Term>::Model>>,
    >(
        &self,
        premises: &Premises<<Self as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> Vec<Self> {
        semantic.normalize(self, premises, table, session)
    }

    /// Checks if the term is *definite*.
    fn definite<
        S: Semantic<Self>
            + Semantic<Type<<Self as Term>::Model>>
            + Semantic<Constant<<Self as Term>::Model>>
            + Semantic<Lifetime<<Self as Term>::Model>>,
        R: Session<Self>
            + Session<Type<<Self as Term>::Model>>
            + Session<Constant<<Self as Term>::Model>>
            + Session<Lifetime<<Self as Term>::Model>>,
    >(
        &self,
        premises: &Premises<<Self as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        definite::definite(self, premises, table, semantic, session)
    }

    /// Unifies the given two terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the given terms cannot be unified.
    fn unify<
        C: Config<Self>
            + Config<Type<<Self as Term>::Model>>
            + Config<Constant<<Self as Term>::Model>>
            + Config<Lifetime<<Self as Term>::Model>>,
        S: Semantic<Self>
            + Semantic<Type<<Self as Term>::Model>>
            + Semantic<Constant<<Self as Term>::Model>>
            + Semantic<Lifetime<<Self as Term>::Model>>,
        R: Session<Self>
            + Session<Type<<Self as Term>::Model>>
            + Session<Constant<<Self as Term>::Model>>
            + Session<Lifetime<<Self as Term>::Model>>,
    >(
        &self,
        rhs: &Self,
        premises: &Premises<<Self as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
        config: &mut C,
    ) -> Option<Substitution<<Self as Term>::Model>> {
        unification::unify(
            self, rhs, premises, table, semantic, session, config,
        )
        .ok()
    }

    /// Checks if the other term is equal to this term.
    fn equals<
        S: Semantic<Self>
            + Semantic<Type<<Self as Term>::Model>>
            + Semantic<Constant<<Self as Term>::Model>>
            + Semantic<Lifetime<<Self as Term>::Model>>,
        R: Session<Self>
            + Session<Type<<Self as Term>::Model>>
            + Session<Constant<<Self as Term>::Model>>
            + Session<Lifetime<<Self as Term>::Model>>,
    >(
        &self,
        other: &Self,
        premises: &Premises<<Self as Term>::Model>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        equality::equals(self, other, premises, table, semantic, session)
    }

    /// Returns an iterator over the outlives premises of this term.
    fn outlives_premises(
        premises: &Premises<<Self as Term>::Model>,
    ) -> Self::OutlivesPremisesIter<'_>;

    /// Returns an iterator over the constant type premises of this term.
    fn constant_type_premises(
        premises: &Premises<<Self as Term>::Model>,
    ) -> Self::ConstantTypePremisesIter<'_>;

    /// Determines if the term is an ADT type.
    ///
    /// Primarily used fro reasoning with marker traits.
    #[doc(hidden)]
    fn is_adt_type_term(&self) -> bool;

    #[doc(hidden)]
    fn get_substructural(
        substructural: &mut Substructural<<Self as Term>::Model>,
    ) -> &mut Vec<(Self, Self)>;

    /// Counts the number of sub-terms in the term.
    fn term_count(&self) -> usize {
        let mut visitor =
            CountVisitor { count: 0, _marker: std::marker::PhantomData };

        let _ = self
            .accept(&mut visitor, visitor::VisitMode::<Success>::OnlySubTerms);

        visitor.count
    }
}

struct CountVisitor<S: Model> {
    count: usize,
    _marker: std::marker::PhantomData<S>,
}

impl<S: Model> Visitor for CountVisitor<S> {
    type Model = S;

    fn visit_type(
        &mut self,
        _: &Type<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        self.count += 1;
        true
    }

    fn visit_lifetime(
        &mut self,
        _: &Lifetime<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        self.count += 1;
        true
    }

    fn visit_constant(
        &mut self,
        _: &Constant<Self::Model>,
        _: visitor::Source,
    ) -> bool {
        self.count += 1;
        true
    }
}

/// Represents a list of generic arguments supplied to a particular generic
/// symbol.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct GenericArguments<S: Model> {
    /// List of lifetime arguments.
    pub lifetimes: Vec<Lifetime<S>>,

    /// List of type arguments.
    pub types: Vec<Type<S>>,

    /// List of constant arguments.
    pub constants: Vec<Constant<S>>,
}

impl<S: Model> Entity for GenericArguments<S> {
    type Model = S;
    type Rebind<A: Model> = GenericArguments<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        GenericArguments {
            lifetimes: self
                .lifetimes
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
            types: self
                .types
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
            constants: self
                .constants
                .into_iter()
                .map(Entity::into_other_model)
                .collect(),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        Some(GenericArguments {
            lifetimes: self
                .lifetimes
                .into_iter()
                .map(Entity::try_into_other_model)
                .collect::<Option<_>>()?,
            types: self
                .types
                .into_iter()
                .map(Entity::try_into_other_model)
                .collect::<Option<_>>()?,
            constants: self
                .constants
                .into_iter()
                .map(Entity::try_into_other_model)
                .collect::<Option<_>>()?,
        })
    }
}

/// A type that can't never be instantiated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Never {}

/// Represents a term where its value is stored as a symbol (i.e., `type` or
/// `const` declaration).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol<ID, S: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the symbol.
    pub generic_arguments: GenericArguments<S>,
}

/// Represents a term where its value is stored as a member of a particular
/// symbol
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MemberSymbol<ID, S: Model> {
    /// The ID of the symbol that contains the value of the term.
    pub id: ID,

    /// The generic arguments supplied to the member.
    pub member_generic_arguments: GenericArguments<S>,

    /// The generic arguments supplied to the parent scope.
    pub parent_generic_arguments: GenericArguments<S>,
}

/// Represents a term that can be unpacked into multiple terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpacked<Parameter, TraitMember> {
    /// Unpack from a parameter.
    Parameter(Parameter),

    /// Unpack from a trait member.
    TraitMember(TraitMember),
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement<Term, Parameter, TraitMember> {
    /// A regular term.
    Regular(Term),

    /// A term that can be unpacked into multiple terms.
    Unpacked(Unpacked<Parameter, TraitMember>),
}

/// Represents a tuple of terms.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Tuple<Term, Parameter, TraitMember>
where
    Self: Into<Term>,
    Term: From<Parameter> + From<TraitMember>,
    Parameter: Clone + TryFrom<Term, Error = Term>,
    TraitMember: Clone + TryFrom<Term, Error = Term>,
{
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<Term, Parameter, TraitMember>>,
}

impl<M: Model> GenericArguments<M> {
    /// Applies the given substitution to the generic arguments.
    pub fn apply(&mut self, substitution: &Substitution<M>) {
        self.lifetimes.iter_mut().for_each(|x| x.apply(substitution));
        self.types.iter_mut().for_each(|x| x.apply(substitution));
        self.constants.iter_mut().for_each(|x| x.apply(substitution));
    }

    /// Checks if all the terms in the generic arguments are definite.
    #[must_use]
    pub fn is_definite<
        S: Semantic<Type<M>> + Semantic<Constant<M>> + Semantic<Lifetime<M>>,
        R: Session<Type<M>> + Session<Constant<M>> + Session<Lifetime<M>>,
    >(
        &self,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> bool {
        self.lifetimes
            .iter()
            .all(|x| x.definite(premises, table, semantic, session))
            && self
                .types
                .iter()
                .all(|x| x.definite(premises, table, semantic, session))
            && self
                .constants
                .iter()
                .all(|x| x.definite(premises, table, semantic, session))
    }
}

#[cfg(test)]
mod tests;
