//! Contains various definition of predicates.

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

    /// If all the sub-term of the predicate are satisfiable, then the predicate is satisfiable.
    Congruent,
}

// REXPORTS
pub use definite::{definite, Query as DefiniteQuery};
pub use outlives::{Outlives, Query as OutlivesQuery};
pub use tuple::{Query as TupleQuery, Tuple};

/// An enumeration of all predicates that doesn't include equality.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum NonEquality {
    LifetimeOutlives(Outlives<Lifetime>),
    TypeOutlives(Outlives<Type>),
    TupleType(Tuple<Type>),
    TupleConstant(Tuple<Constant>),
}
