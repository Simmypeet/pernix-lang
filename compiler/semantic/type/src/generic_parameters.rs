use pernixc_arena::{ID, OrderedArena};
use pernixc_lexical::tree::RelativeSpan;
#[cfg(test)]
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::{GlobalSymbolID, MemberID};
use qbice::{
    Decode, Encode, Identifiable, Query, StableHash, storage::intern::Interned,
};

use crate::{symbol::TraitRef2, r#type::kind::TyKind};

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
#[value(Interned<GenericParameters2>)]
#[extend(name = get_generic_parameters2, by_val)]
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
    pub const fn new(
        name: Interned<str>,
        span: Option<RelativeSpan>,
        kind: GenericParameterKind,
    ) -> Self {
        Self { name, span, kind }
    }

    #[must_use]
    pub const fn kind(&self) -> TyKind {
        match &self.kind {
            GenericParameterKind::Lifetime => TyKind::Lifetime,
            GenericParameterKind::Type => TyKind::Type,
            GenericParameterKind::Instance(_) => TyKind::Instance,
            GenericParameterKind::EffectSignature => TyKind::EffectSignature,
            GenericParameterKind::EffectRow => TyKind::EffectRow,
        }
    }

    /// If the generic parameter is an instance parameter, returns the
    /// [`TraitRef2`] associated with it. Otherwise, returns `None`.
    #[must_use]
    pub const fn as_trait_ref_instance(&self) -> Option<&TraitRef2> {
        match &self.kind {
            GenericParameterKind::Instance(instance) => instance.trait_ref(),
            GenericParameterKind::Lifetime
            | GenericParameterKind::Type
            | GenericParameterKind::EffectSignature
            | GenericParameterKind::EffectRow => None,
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
    trait_ref: Option<TraitRef2>,
}

impl InstanceParameterKind {
    #[must_use]
    pub const fn new(trait_ref: Option<TraitRef2>) -> Self {
        Self { trait_ref }
    }

    #[must_use]
    pub const fn trait_ref(&self) -> Option<&TraitRef2> {
        self.trait_ref.as_ref()
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
pub enum GenericParameterKind {
    Lifetime,
    Type,
    Instance(InstanceParameterKind),
    EffectSignature,
    EffectRow,
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
pub struct GenericParameters2 {
    parameters: OrderedArena<GenericParameter>,
}

impl GenericParameters2 {
    #[must_use]
    pub fn new(parameters: impl IntoIterator<Item = GenericParameter>) -> Self {
        let mut arena = OrderedArena::new();

        for parameter in parameters {
            arena.insert(parameter);
        }

        Self { parameters: arena }
    }

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

#[cfg(test)]
mod test {
    use pernixc_qbice::create_minimal_engine as create_engine;

    use super::*;

    // input: generic parameters [EffectSignature, EffectRow]
    // premise: {}
    // output: kinds [EffectSignature, EffectRow]
    #[tokio::test]
    async fn effect_generic_parameters_retain_their_kinds() {
        let engine = create_engine().await;
        let parameters = GenericParameters2::from_kinds(
            [
                GenericParameterKind::EffectSignature,
                GenericParameterKind::EffectRow,
            ],
            &engine,
        );

        assert_eq!(
            parameters
                .iter()
                .map(|(_, parameter)| parameter.kind())
                .collect::<Vec<_>>(),
            vec![TyKind::EffectSignature, TyKind::EffectRow]
        );
    }
}
