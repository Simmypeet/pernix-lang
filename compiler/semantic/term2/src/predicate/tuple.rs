use qbice::{Decode, Encode, StableHash};

use crate::{TermRef, predicate::term_ref_contains_error};

/// The predicate payload meaning that the term is a tuple and is unpackable.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Tuple<T> {
    term: T,
}

impl<T> Tuple<T> {
    /// Creates a new tuple-type predicate payload.
    #[must_use]
    pub const fn new(term: T) -> Self { Self { term } }

    /// Returns the tuple operand.
    #[must_use]
    pub const fn term(&self) -> &T { &self.term }

    /// Returns the tuple operand mutably.
    #[must_use]
    pub const fn term_mut(&mut self) -> &mut T { &mut self.term }

    /// Consumes the payload and returns the term.
    #[must_use]
    pub fn into_term(self) -> T { self.term }
}

impl<T> Tuple<T>
where
    for<'a> &'a T: Into<TermRef<'a>>,
{
    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        term_ref_contains_error(self.term().into())
    }
}
