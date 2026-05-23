use qbice::{Decode, Encode, StableHash};

use crate::r#type::kind::TyKind;

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
pub struct Binder {
    bound_vars: Vec<TyKind>,
}

impl Binder {
    #[cfg(test)]
    pub(crate) const fn new_for_test(bound_vars: Vec<TyKind>) -> Self {
        Self { bound_vars }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct BoundVariable {
    depth: usize,
    index: usize,
}
