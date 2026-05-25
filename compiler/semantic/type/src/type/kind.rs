use enum_as_inner::EnumAsInner;
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
    EnumAsInner,
)]
pub enum TyKind {
    Type,
    Lifetime,
    Instance,
}
