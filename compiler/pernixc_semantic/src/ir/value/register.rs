//! Contains the definition of [`Register`] and its variants.

use pernixc_base::source_file::Span;

use super::Value;
use crate::{ir::address::Address, semantic::term::r#type::Qualifier};

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TupleElement {
    Regular(Value),
    Unpacked(Value),
}

impl TupleElement {
    /// Returns a reference to the value.
    #[must_use]
    pub const fn as_value(&self) -> &Value {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Returns a mutable reference to the value.
    #[must_use]
    pub fn as_value_mut(&mut self) -> &mut Value {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }

    /// Consumes the element and returns the value.
    #[must_use]
    pub const fn into_value(self) -> Value {
        match self {
            Self::Regular(value) | Self::Unpacked(value) => value,
        }
    }
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement>,

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
pub struct Load {
    /// The address where the value is stored and will be read from.
    pub address: Address,

    /// The kind of load.
    pub kind: LoadKind,

    /// The span where the load is created.
    pub span: Option<Span>,
}

/// Obtains a reference at the given address.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReferenceOf {
    /// The address to the value.
    pub address: Address,

    /// The qualfier of the reference.
    pub qualifier: Qualifier,

    /// The span where the reference is created.
    pub span: Option<Span>,
}

/// An enumeration of the different kinds of registers.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Register {
    Tuple(Tuple),
    Load(Load),
    ReferenceOf(ReferenceOf),
}
