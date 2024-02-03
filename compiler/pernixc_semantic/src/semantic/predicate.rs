//! Contains various definition of predicates.

mod constant_type;
mod definite;
mod outlives;
mod tuple;

use enum_as_inner::EnumAsInner;

use super::term::{constant::Constant, lifetime::Lifetime, r#type::Type};

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

    /* To be implemented */
    Trait,
}

/// A predicate that two terms are equal.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Equality<T> {
    /// The left-hand side of the equality.
    pub lhs: T,

    /// The right-hand side of the equality.
    pub rhs: T,
}

/// An enumeration of all predicates that doesn't include equality.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum NonEquality {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    TupleConstant(Tuple<Constant>),
    ConstantType(ConstantType),
}
