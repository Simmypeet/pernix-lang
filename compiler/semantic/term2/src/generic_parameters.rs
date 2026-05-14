use pernixc_arena::{ID, OrderedArena};
use pernixc_lexical::tree::RelativeSpan;
#[cfg(test)]
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{GlobalSymbolID, MemberID};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

use crate::{symbol::Symbol, r#type::kind::TyKind};

/// Key for querying generic parameters for a given global symbol ID.
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
    Query,
)]
#[value(Interned<GenericParameters>)]
#[extend(name = get_generic_parameters, by_val)]
pub struct Key {
    /// The global symbol ID to get the generic parameters for.
    pub symbol_id: GlobalSymbolID,
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

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Identifiable,
    Encode,
    Decode,
    derive_more::Index,
)]
pub struct GenericParameters {
    parameters: OrderedArena<GenericParameter>,
}

impl GenericParameters {
    #[must_use]
    pub fn len(&self) -> usize { self.parameters.len() }

    #[must_use]
    pub fn is_empty(&self) -> bool { self.parameters.is_empty() }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (ID<GenericParameter>, &GenericParameter)> {
        self.parameters.iter()
    }

    #[cfg(test)]
    pub(crate) fn from_kinds(
        kinds: impl IntoIterator<Item = GenericParameterKind>,
        engine: &TrackedEngine,
    ) -> Self {
        let mut parameters = OrderedArena::new();

        for (index, kind) in kinds.into_iter().enumerate() {
            parameters.insert(GenericParameter {
                name: engine.intern_unsized(format!("T{index}")),
                span: None,
                kind,
            });
        }

        Self { parameters }
    }
}

pub type GenericParameterID = MemberID<ID<GenericParameter>>;
