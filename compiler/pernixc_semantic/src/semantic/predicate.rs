//! Contains various definition of predicates.

mod constant_type;
mod definite;
mod outlives;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::{
    instantiation::{self, Instantiation},
    model::Model,
    sub_term::TermLocation,
    term::{
        constant::Constant,
        lifetime::Lifetime,
        r#type::{self, Type},
        Term,
    },
    visitor::{accept_recursive, Recursive},
};
use crate::table::{self, DisplayObject, State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsForallLifetimeVisitor {
    contains_forall_lifetime: bool,
}

impl<'v, M: Model> Recursive<'v, Lifetime<M>>
    for ContainsForallLifetimeVisitor
{
    fn visit(
        &mut self,
        term: &'v Lifetime<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        if term.is_forall() {
            self.contains_forall_lifetime = true;
            false
        } else {
            true
        }
    }
}

impl<'v, M: Model> Recursive<'v, Type<M>> for ContainsForallLifetimeVisitor {
    fn visit(
        &mut self,
        _: &'v Type<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'v, M: Model> Recursive<'v, Constant<M>>
    for ContainsForallLifetimeVisitor
{
    fn visit(
        &mut self,
        _: &'v Constant<M>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

fn contains_forall_lifetime<T: Term>(term: &T) -> bool {
    let mut visitor =
        ContainsForallLifetimeVisitor { contains_forall_lifetime: false };

    accept_recursive(term, &mut visitor);

    visitor.contains_forall_lifetime
}

/// Enumeration containing either a lifetime or a type outlives predicate.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LifetimeConstraint<M: Model> {
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
}

/// Describes a satisfiability of a certain predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Satisfiability {
    /// The predicate is satisfiable.
    Satisfied,

    /// The predicate is unsatisfiable.
    Unsatisfied,

    /// If all the sub-term of the predicate are satisfiable, then the
    /// predicate is satisfiable.
    Congruent,
}

pub use constant_type::{
    ConstantType, Query as ConstantTypeQuery,
    QuerySource as ConstantTypeQuerySource,
};
pub use definite::{definite, Query as DefiniteQuery};
pub use outlives::{Outlives, Query as OutlivesQuery};
pub use r#trait::{
    resolve_implementation, Implementation, Query as TraitQuery,
    ResolveError as TraitResolveError, Satisfiability as TraitSatisfiability,
    Trait,
};
pub use tuple::{Query as TupleQuery, Tuple};

/// A predicate that can appear in the where clause.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Predicate<M: Model> {
    TraitTypeEquality(Equality<r#type::TraitMember<M>, Type<M>>),
    ConstantType(ConstantType<M>),
    LifetimeOutlives(Outlives<Lifetime<M>>),
    TypeOutlives(Outlives<Type<M>>),
    TupleType(Tuple<Type<M>>),
    TupleConstant(Tuple<Constant<M>>),
    Trait(Trait<M>),
}

impl<T: State, M: Model> table::Display<T> for Predicate<M> {
    fn fmt(
        &self,
        table: &table::Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::TraitTypeEquality(equality) => equality.fmt(table, f),
            Self::ConstantType(constant_type) => constant_type.fmt(table, f),
            Self::LifetimeOutlives(outlives) => outlives.fmt(table, f),
            Self::TypeOutlives(outlives) => outlives.fmt(table, f),
            Self::TupleType(tuple) => tuple.fmt(table, f),
            Self::TupleConstant(tuple) => tuple.fmt(table, f),
            Self::Trait(tr) => tr.fmt(table, f),
        }
    }
}

impl<M: Model> Predicate<M> {
    /// Checks if the predicate contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        match self {
            Self::TraitTypeEquality(equality) => {
                equality.contains_forall_lifetime()
            }
            Self::ConstantType(constant_type) => {
                constant_type.contains_forall_lifetime()
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.contains_forall_lifetime()
            }
            Self::TypeOutlives(outlives) => outlives.contains_forall_lifetime(),
            Self::TupleType(tuple) => tuple.contains_forall_lifetime(),
            Self::TupleConstant(tuple) => tuple.contains_forall_lifetime(),
            Self::Trait(tr) => tr.contains_forall_lifetime(),
        }
    }

    /// Applies the instantiate to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation<M>) {
        match self {
            Self::TraitTypeEquality(equality) => {
                equality.instantiate(substitution);
            }
            Self::ConstantType(constant_type) => {
                constant_type.instantiate(substitution);
            }
            Self::LifetimeOutlives(outlives) => {
                outlives.instantiate(substitution);
            }
            Self::TypeOutlives(outlives) => outlives.instantiate(substitution),
            Self::TupleType(tuple) => tuple.instantiate(substitution),
            Self::TupleConstant(tuple) => tuple.instantiate(substitution),
            Self::Trait(tr) => tr.instantiate(substitution),
        }
    }
}

/// A predicate that two terms are equal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equality<T, U = T> {
    /// The trait member term under comparison.
    pub lhs: T,

    /// The term that the trait member is equivalent to.
    pub rhs: U,
}

impl<S: State, T: table::Display<S>, U: table::Display<S>> table::Display<S>
    for Equality<T, U>
{
    fn fmt(
        &self,
        table: &table::Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} = {}",
            DisplayObject { display: &self.lhs, table },
            DisplayObject { display: &self.rhs, table }
        )
    }
}

impl<T: Term> Equality<T::TraitMember, T> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_forall_lifetime(&self) -> bool {
        let trait_member = T::from(self.lhs.clone());

        contains_forall_lifetime(&trait_member)
            || contains_forall_lifetime(&self.rhs)
    }
}

impl<M: Model> Equality<r#type::TraitMember<M>, Type<M>> {
    /// Applies the instantiation to both [`Equality::lhs`] and
    /// [`Equality::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation<M>) {
        self.lhs.member_generic_arguments.instantiate(instantiation);
        self.lhs.parent_generic_arguments.instantiate(instantiation);
        instantiation::instantiate(&mut self.rhs, instantiation);
    }
}
