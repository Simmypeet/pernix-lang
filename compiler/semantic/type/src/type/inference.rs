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
pub struct InferenceVariable(u64);

impl InferenceVariable {
    #[must_use]
    pub const fn new(id: u64) -> Self { Self(id) }
}
