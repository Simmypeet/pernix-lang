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
pub struct BoundVar {
    depth: usize,
    index: usize,
}
