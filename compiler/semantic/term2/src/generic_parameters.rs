use pernixc_arena::{ID, OrderedArena};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::MemberID;
use qbice::{Decode, Encode, StableHash, storage::intern::Interned};

use crate::{symbol::Symbol, r#type::kind::TyKind};

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
pub struct GenericParameter {
    name: Interned<str>,
    span: Option<RelativeSpan>,
    kind: GenericParameterKind,
}

impl GenericParameter {
    #[must_use]
    pub const fn kind(&self) -> TyKind {
        match &self.kind {
            GenericParameterKind::Lifetime => TyKind::Lifetime,
            GenericParameterKind::Type => TyKind::Type,
            GenericParameterKind::Instance(_) => TyKind::Instance,
        }
    }
}

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
pub struct InstanceParameterKind {
    trait_ref: Option<Symbol>,
}

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
pub enum GenericParameterKind {
    Lifetime,
    Type,
    Instance(InstanceParameterKind),
}

#[derive(Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode)]
pub struct GenericParameters {
    parameters: OrderedArena<GenericParameter>,
}

pub type GenericParameterID = MemberID<ID<GenericParameter>>;
