//! Data definitions for lifetime terms.

use derive_new::new;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::MemberID;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    Never, TermRef,
    error::Error,
    generic_parameters::{LifetimeParameter, LifetimeParameterID},
    inference,
    sub_term::{IterSubTerms, SubTerm, TermLocation},
};

/// Represents a named forall lifetime declared with `for['a]`.
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
    new,
)]
pub struct NamedForall {
    span: RelativeSpan,
    shared_str: Interned<str>,
}

/// Describes where a generated forall lifetime originated from.
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
pub enum FromSemanticElement {
    /// Generated from a `do Effect` annotation.
    DoEffect,
}

/// Represents an implicitly generated forall lifetime.
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
pub struct GeneratedForall {
    from_id: Global<pernixc_symbol::SymbolID>,
    from_semantic_element: FromSemanticElement,
    unique_counter: usize,
}

/// Represents a forall lifetime.
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Forall {
    Named(NamedForall),
    Generated(GeneratedForall),
}

/// Represents an elided lifetime.
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
)]
pub struct ElidedLifetime {
    order: usize,
}

impl NamedForall {
    /// Returns the source span.
    #[must_use]
    pub const fn span(&self) -> &RelativeSpan { &self.span }

    /// Returns the name.
    #[must_use]
    pub const fn shared_str(&self) -> &Interned<str> { &self.shared_str }
}

impl GeneratedForall {
    /// Creates a new generated forall lifetime payload.
    #[must_use]
    pub const fn new(
        from_id: Global<pernixc_symbol::SymbolID>,
        from_semantic_element: FromSemanticElement,
        unique_counter: usize,
    ) -> Self {
        Self { from_id, from_semantic_element, unique_counter }
    }

    /// Returns the source symbol id.
    #[must_use]
    pub const fn from_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.from_id
    }

    /// Returns the originating semantic element.
    #[must_use]
    pub const fn from_semantic_element(&self) -> FromSemanticElement {
        self.from_semantic_element
    }

    /// Returns the uniqueness counter.
    #[must_use]
    pub const fn unique_counter(&self) -> usize { self.unique_counter }
}

impl ElidedLifetime {
    /// Creates a new elided lifetime payload.
    #[must_use]
    pub const fn new(order: usize) -> Self { Self { order } }

    /// Returns the encounter order.
    #[must_use]
    pub const fn order(&self) -> usize { self.order }
}

/// The ID to an elided lifetime.
pub type ElidedLifetimeID = MemberID<pernixc_arena::ID<ElidedLifetime>>;

/// Represents a lifetime term payload.
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
    Identifiable,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Lifetime {
    Inference(inference::Variable<Self>),
    Parameter(LifetimeParameterID),
    Elided(ElidedLifetimeID),
    Forall(Forall),
    Static,
    Erased,
    Error(Error),
}

impl Lifetime {
    /// Creates a lifetime parameter reference.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        lifetime_id: pernixc_arena::ID<LifetimeParameter>,
    ) -> Self {
        Self::Parameter(LifetimeParameterID::new(parent_global_id, lifetime_id))
    }
}

impl SubTerm for Lifetime {
    type SubTypeLocation = Never;
    type SubConstantLocation = Never;
    type SubLifetimeLocation = Never;
    type SubInstanceLocation = Never;
    type ThisSubTermLocation = Never;
}

impl IterSubTerms for Lifetime {
    fn iter_sub_terms<'this>(
        &'this self,
        _: &'this pernixc_qbice::TrackedEngine,
    ) -> impl Iterator<Item = (TermRef<'this>, TermLocation)> + 'this {
        pernixc_coroutine_iter::coroutine_iter!({
            let _ = self;
        })
    }
}
