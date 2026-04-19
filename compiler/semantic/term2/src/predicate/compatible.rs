use qbice::{Decode, Encode, StableHash};

/// A predicate payload representing compatible equality between two values.
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
pub struct Compatible<T, U = T> {
    lhs: T,
    rhs: U,
}

impl<T, U> Compatible<T, U> {
    /// Creates a new compatible-equality payload.
    #[must_use]
    pub const fn new(lhs: T, rhs: U) -> Self { Self { lhs, rhs } }

    /// Returns the left-hand side.
    #[must_use]
    pub const fn lhs(&self) -> &T { &self.lhs }

    /// Returns the right-hand side.
    #[must_use]
    pub const fn rhs(&self) -> &U { &self.rhs }

    /// Returns the left-hand side mutably.
    #[must_use]
    pub const fn lhs_mut(&mut self) -> &mut T { &mut self.lhs }

    /// Returns the right-hand side mutably.
    #[must_use]
    pub const fn rhs_mut(&mut self) -> &mut U { &mut self.rhs }

    /// Destructures the payload into its parts.
    #[must_use]
    pub fn into_parts(self) -> (T, U) { (self.lhs, self.rhs) }
}
