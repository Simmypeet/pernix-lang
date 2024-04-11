//! Contains the definition of the [`Literal`] enum.

/// Represents a tuple with no elements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EmptyTuple;

/// Contains the different kinds of literals.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Literal {
    EmptyTuple(EmptyTuple),
}
