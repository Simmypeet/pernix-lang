use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{TermRef, predicate::term_ref_contains_error, r#type::Type};

/// Represents a type that can be used as a compile-time constant type.
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
pub struct ConstantType {
    r#type: Interned<Type>,
}

impl ConstantType {
    /// Creates a new constant-type predicate payload.
    #[must_use]
    pub const fn new(r#type: Interned<Type>) -> Self { Self { r#type } }

    /// Returns the predicate type.
    #[must_use]
    pub const fn r#type(&self) -> &Interned<Type> { &self.r#type }

    /// Consumes the payload and returns the type.
    #[must_use]
    pub fn into_type(self) -> Interned<Type> { self.r#type }

    /// Checks whether the predicate contains an erroneous term.
    #[must_use]
    pub fn contains_error(&self) -> bool {
        term_ref_contains_error(TermRef::Type(self.r#type()))
    }
}
