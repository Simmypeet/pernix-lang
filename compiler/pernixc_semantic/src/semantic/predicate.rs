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
};

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl Predicate {
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

impl<T: Term> Equality<T> {
    /// Applies the instantiation to both [`Equality::lhs`] and
    /// [`Equality::rhs`].
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation::instantiate(&mut self.lhs, instantiation);
        instantiation::instantiate(&mut self.rhs, instantiation);
    }
}

/// An enumeration of all predicates that doesn't include equality.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
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
