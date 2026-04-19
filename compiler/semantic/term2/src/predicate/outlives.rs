use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{TermRef, lifetime::Lifetime, predicate::term_ref_contains_error};

/// A predicate payload stating that an operand outlives a lifetime.
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
pub struct Outlives<T> {
    operand: T,
    bound: Interned<Lifetime>,
}

impl<T> Outlives<T> {
    /// Creates a new outlives predicate payload.
    #[must_use]
    pub const fn new(operand: T, bound: Interned<Lifetime>) -> Self {
        Self { operand, bound }
    }

    /// Returns the operand.
    #[must_use]
    pub const fn operand(&self) -> &T { &self.operand }

    /// Returns the bound lifetime.
    #[must_use]
    pub const fn bound(&self) -> &Interned<Lifetime> { &self.bound }

    /// Returns the operand mutably.
    #[must_use]
    pub const fn operand_mut(&mut self) -> &mut T { &mut self.operand }

    /// Replaces the bound lifetime.
    pub fn set_bound(&mut self, bound: Interned<Lifetime>) {
        self.bound = bound;
    }

    /// Destructures the payload into its parts.
    #[must_use]
    pub fn into_parts(self) -> (T, Interned<Lifetime>) {
        (self.operand, self.bound)
    }
}

impl<T> Outlives<Interned<T>>
where
    for<'a> &'a Interned<T>: Into<TermRef<'a>>,
{
    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        term_ref_contains_error(self.operand().into())
            || term_ref_contains_error(TermRef::Lifetime(self.bound()))
    }
}
