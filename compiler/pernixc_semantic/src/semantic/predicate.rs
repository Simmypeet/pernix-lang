//! Contains various definition of predicates.

mod constant_type;
mod definite;
mod outlives;
mod r#trait;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::{
    instantiation::{self, Instantiation},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor::{accept_recursive, Visitor},
};
use crate::table::{self, DisplayObject, State};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ContainsForallLifetimeVisitor {
    contains_forall_lifetime: bool,
}

impl Visitor for ContainsForallLifetimeVisitor {
    fn visit_type(&mut self, _: &Type) -> bool { true }

    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool {
        if lifetime.is_forall() {
            self.contains_forall_lifetime = true;
            false
        } else {
            true
        }
    }

    fn visit_constant(&mut self, _: &Constant) -> bool { true }

    fn visit_type_mut(&mut self, _: &mut Type) -> bool { true }

    fn visit_lifetime_mut(&mut self, lifetime: &mut Lifetime) -> bool {
        if lifetime.is_forall() {
            self.contains_forall_lifetime = true;
            false
        } else {
            true
        }
    }

    fn visit_constant_mut(&mut self, _: &mut Constant) -> bool { true }
}

fn contains_forall_lifetime<T: Term>(term: &T) -> bool {
    let mut visitor =
        ContainsForallLifetimeVisitor { contains_forall_lifetime: false };

    accept_recursive(term, &mut visitor);

    visitor.contains_forall_lifetime
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
    Implementation, LifetimeConstraint, Query as TraitQuery,
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
pub enum Predicate {
    TypeEquality(Equality<Type>),
    ConstantEquality(Equality<Constant>),
    ConstantType(ConstantType),
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    TupleConstant(Tuple<Constant>),
    Trait(Trait),
}

impl<T: State> table::Display<T> for Predicate {
    fn fmt(
        &self,
        table: &table::Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::TypeEquality(equality) => equality.fmt(table, f),
            Self::ConstantEquality(equality) => equality.fmt(table, f),
            Self::ConstantType(constant_type) => constant_type.fmt(table, f),
            Self::LifetimeOutlives(outlives) => outlives.fmt(table, f),
            Self::TypeOutlives(outlives) => outlives.fmt(table, f),
            Self::TupleType(tuple) => tuple.fmt(table, f),
            Self::TupleConstant(tuple) => tuple.fmt(table, f),
            Self::Trait(tr) => tr.fmt(table, f),
        }
    }
}

impl Predicate {
    /// Checks if the predicate contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        match self {
            Self::TypeEquality(equality) => equality.contains_forall_lifetime(),
            Self::ConstantEquality(equality) => {
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
    pub fn instantiate(&mut self, substitution: &Instantiation) {
        match self {
            Self::TypeEquality(equality) => equality.instantiate(substitution),
            Self::ConstantEquality(equality) => {
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
pub struct Equality<T> {
    /// The left-hand side of the equality.
    pub lhs: T,

    /// The right-hand side of the equality.
    pub rhs: T,
}

impl<S: State, T: table::Display<S>> table::Display<S> for Equality<T> {
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

impl<T: Term> Equality<T> {
    /// Checks if the predicate has a `forall` lifetime.
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.lhs)
            || contains_forall_lifetime(&self.rhs)
    }

    /// Applies the instantiation to both [`Equality::lhs`] and
    /// [`Equality::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation::instantiate(&mut self.lhs, instantiation);
        instantiation::instantiate(&mut self.rhs, instantiation);
    }
}

/// An enumeration of all predicates that doesn't include equality.
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
pub enum NonEquality {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    TupleConstant(Tuple<Constant>),
    Trait(Trait),
    ConstantType(ConstantType),
}

impl NonEquality {
    /// Checks if the predicate contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        match self {
            Self::LifetimeOutlives(outlives) => {
                outlives.contains_forall_lifetime()
            }
            Self::TypeOutlives(outlives) => outlives.contains_forall_lifetime(),
            Self::TupleType(tuple) => tuple.contains_forall_lifetime(),
            Self::TupleConstant(tuple) => tuple.contains_forall_lifetime(),
            Self::Trait(tr) => tr.contains_forall_lifetime(),
            Self::ConstantType(constant_type) => {
                constant_type.contains_forall_lifetime()
            }
        }
    }

    /// Applies the instantiation to the predicate.
    pub fn instantiate(&mut self, substitution: &Instantiation) {
        match self {
            Self::LifetimeOutlives(outlives) => {
                outlives.instantiate(substitution);
            }
            Self::TypeOutlives(outlives) => outlives.instantiate(substitution),
            Self::TupleType(tuple) => tuple.instantiate(substitution),
            Self::TupleConstant(tuple) => tuple.instantiate(substitution),
            Self::Trait(tr) => tr.instantiate(substitution),
            Self::ConstantType(constant_type) => {
                constant_type.instantiate(substitution);
            }
        }
    }
}
