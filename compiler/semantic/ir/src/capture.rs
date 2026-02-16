//! Defines the [`Captures`] representing the capturing structure used for
//! implementing closures, do blocks, and effect handlers.

use derive_more::Index;
use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Qualifier, Reference, Type},
};
use qbice::{Decode, Encode, StableHash};

use crate::{
    address::Address,
    transform::{self, Transformer, TypeTermSource},
};

pub mod builder;
pub mod pruning;

/// Represents capturing structure used for implementing closures, do blocks,
/// and effect handlers.
#[derive(
    Debug, Clone, PartialEq, Eq, StableHash, Encode, Decode, Default, Index,
)]
pub struct Captures {
    /// All the captures used in the closure.
    #[index]
    captures: Arena<Capture>,
    capture_order: Vec<pernixc_arena::ID<Capture>>,
}

impl transform::Element for Captures {
    async fn transform<
        T: Transformer<Lifetime>
            + Transformer<Type>
            + Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        _: &TrackedEngine,
    ) {
        for (_, capture) in self.captures.iter_mut() {
            transformer
                .transform(
                    &mut capture.address_type,
                    TypeTermSource::Capture,
                    capture.span,
                )
                .await;

            if let CaptureMode::ByReference(reference_mode) =
                &mut capture.capture_mode
            {
                transformer
                    .transform(
                        &mut reference_mode.lifetime,
                        transform::LifetimeTermSource::Capture,
                        capture.span,
                    )
                    .await;
            }
        }
    }
}

impl Captures {
    /// Returns an iterator over all capture IDs in the capturing structure.
    #[must_use]
    pub fn ids(
        &self,
    ) -> impl ExactSizeIterator<Item = pernixc_arena::ID<Capture>> + '_ {
        self.captures.ids()
    }

    /// Returns the declaration order of a capture (including drop order)
    /// in the capturing structure.
    ///
    /// 0 is the first capture to be initialize or dropped.
    #[must_use]
    pub fn declaration_order_of(
        &self,
        capture_id: pernixc_arena::ID<Capture>,
    ) -> usize {
        self.capture_order.iter().position(|id| *id == capture_id).unwrap()
    }

    /// Returns an iterator over all captures in the capturing structure in
    /// declaration order.
    #[must_use]
    pub fn captures_as_order(
        &self,
    ) -> impl ExactSizeIterator<Item = (pernixc_arena::ID<Capture>, &'_ Capture)> + '_
    {
        self.capture_order.iter().copied().map(|x| (x, &self.captures[x]))
    }
}

/// Specifies how a memory is captured from the parent IR.
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
pub enum CaptureMode {
    /// Moves the captured memory address into the closure.
    ByValue,

    /// Borrows the captured memory address into the closure.
    ByReference(ReferenceCaptureMode),
}

/// Represents a variant of [`CaptureMode::ByReference`]
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
pub struct ReferenceCaptureMode {
    /// The lifetime of the captured memory address.
    pub lifetime: Lifetime,

    /// The reference qualifier of the captured memory address.
    pub qualifier: Qualifier,
}

/// Represents a captured memory from the parent IR.
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
pub struct Capture {
    /// The captured memory address from the parent IR.
    pub parent_captured_address: Address,

    /// The type of the captured memory address.
    ///
    /// # NOTE
    ///
    /// This is the type of the [`Self::parent_captured_address`] not directly
    /// the type of the captured memory itself. The type may differ depending
    /// on the capture mode. Use [`Self::get_capture_type`] to get the type
    /// of the captured memory.
    pub address_type: Type,

    /// Determines how the memory is captured into the closure object.
    pub capture_mode: CaptureMode,

    /// The span of the captured memory address.
    pub span: RelativeSpan,
}

impl Capture {
    /// Returns the type of the captured memory.
    #[must_use]
    pub fn get_capture_type(&self) -> Type {
        match &self.capture_mode {
            CaptureMode::ByValue => self.address_type.clone(),
            CaptureMode::ByReference(reference_capture_mode) => {
                Type::Reference(Reference {
                    qualifier: reference_capture_mode.qualifier,
                    lifetime: reference_capture_mode.lifetime.clone(),
                    pointee: Box::new(self.address_type.clone()),
                })
            }
        }
    }
}

/// A collection of all captures used in a function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    StableHash,
    Encode,
    Decode,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct CapturesMap {
    #[index]
    #[index_mut]
    arena: Arena<Captures>,
}

impl CapturesMap {
    /// Creates a new empty [`CaptureMap`].
    #[must_use]
    pub fn new() -> Self { Self { arena: Arena::new() } }

    /// Inserts a new [`Captures`] into the map and returns its ID.
    #[must_use]
    pub fn insert(
        &mut self,
        captures: Captures,
    ) -> pernixc_arena::ID<Captures> {
        self.arena.insert(captures)
    }
}

impl transform::Element for CapturesMap {
    async fn transform<
        T: transform::Transformer<pernixc_term::lifetime::Lifetime>
            + transform::Transformer<Type>
            + transform::Transformer<pernixc_term::constant::Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) {
        for (_, captures) in &mut self.arena {
            captures.transform(transformer, engine).await;
        }
    }
}
