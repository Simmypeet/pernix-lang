//! Contains the definition of [`Register`] and its variants.

use pernixc_base::source_file::Span;

use super::Value;
use crate::{
    ir::address::Address,
    semantic::{
        model::Model,
        term::{lifetime::Lifetime, r#type::Qualifier},
    },
};

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TupleElement<M: Model> {
    Regular(Value<M>),
    Unpacked(Value<M>),
}

impl<M: Model> TupleElement<M> {
    /// Returns a reference to the value.
    #[must_use]
    pub const fn as_value(&self) -> &Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Returns a mutable reference to the value.
    #[must_use]
    pub fn as_value_mut(&mut self) -> &mut Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Consumes the element and returns the value.
    #[must_use]
    pub const fn into_value(self) -> Value<M> {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<M: Model> {
    /// The elements of kthe tuple.
    pub elements: Vec<TupleElement<M>>,

    /// The span where the tuple is created.
    pub span: Option<Span>,
}

/// An enumeration of either moving or copying loads.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LoadKind {
    /// The value is memcpy'd from the address and the value in the address is
    /// invalidated.
    Move,

    /// The value is copied from the address via `Copy` trait.
    Copy,
}

/// Represents a load/read from an address in memory.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Load<M: Model> {
    /// The address where the value is stored and will be read from.
    pub address: Address<M>,

    /// The kind of load.
    pub kind: LoadKind,

    /// The span where the load is created.
    pub span: Option<Span>,
}

/// Obtains a reference at the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf<M: Model> {
    /// The address to the value.
    pub address: Address<M>,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The span where the reference is created.
    pub span: Option<Span>,

    /// The lifetime produced by the reference.
    pub lifetime: Lifetime<M>,
}

/// An enumeration of the different kinds of registers.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Register<M: Model> {
    Tuple(Tuple<M>),
    Load(Load<M>),
    ReferenceOf(ReferenceOf<M>),
}
