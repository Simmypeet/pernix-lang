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
    Default,
    StableHash,
    Encode,
    Decode,
)]
pub struct UniverseIndex(usize);

impl UniverseIndex {
    #[must_use]
    pub const fn root() -> Self { Self(0) }

    #[must_use]
    pub const fn next(&self) -> Self { Self(self.0 + 1) }
}
