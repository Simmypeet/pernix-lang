//! Contains various definition of predicates.

mod definite;
mod tuple;

pub use definite::{definite, Query as DefiniteQuery};
pub use tuple::{Query as TupleQuery, Tuple};

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
