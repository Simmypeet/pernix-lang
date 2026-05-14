use qbice::{Decode, Encode, StableHash};

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
pub enum TyKind {
    Type,
    Lifetime,
    Instance,
}
