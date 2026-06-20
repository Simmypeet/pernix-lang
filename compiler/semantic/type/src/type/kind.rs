use enum_as_inner::EnumAsInner;
use qbice::{Decode, Encode, Identifiable, StableHash};

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
    Identifiable,
    EnumAsInner,
)]
pub enum TyKind {
    Type,
    Lifetime,
    Instance,
}
