//! Contains all the definitions of compile-time evaluated constant value.

use crate::symbol::{Index, LocalSubstitution};

/// Represents a trait associated constant, denoted by `trait<args>::constant<args>` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated {
    /// Index referring to a trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// Index referring to the trait associated constant in the [`crate::symbol::Trait::constants`]
    /// field.
    pub associated_index: Index,

    /// The generic arguments supplied to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The generic arguments supplied to the trait associated constant.
    pub associated_substitution: LocalSubstitution,
}

/// Represents a compile-time evaluated constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Constant {
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    TraitAssociated(TraitAssociated),
}
