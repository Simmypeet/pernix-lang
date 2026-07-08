use qbice::{Decode, Encode, StableHash};

use crate::r#type::{kind::TyKind, universe::UniverseIndex};

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
pub struct InferenceVariable {
    id: u64,
    kind: TyKind,
    universe: UniverseIndex,
}

impl InferenceVariable {
    #[must_use]
    pub const fn new(id: u64, kind: TyKind, universe: UniverseIndex) -> Self {
        Self { id, kind, universe }
    }

    #[must_use]
    pub const fn id(&self) -> u64 { self.id }

    #[must_use]
    pub const fn kind(&self) -> TyKind { self.kind }

    #[must_use]
    pub const fn universe_index(&self) -> UniverseIndex { self.universe }
}
