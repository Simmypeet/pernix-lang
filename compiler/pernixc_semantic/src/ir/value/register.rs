//! Contains the definition of [`Register`] and its variants.

use super::Value;
use crate::ir::address::Address;

/// Represents an element of a [`Tuple`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum TupleElement {
    Regular(Value),
    Unpacked(Value),
}

/// Represents a tuple of values.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement>,
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
}

/// An enumeration of the different kinds of registers.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Register {
    Tuple(Tuple),
    Load(Load),
}
