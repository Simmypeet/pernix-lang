use pernixc_system::arena::ID;

use crate::symbol::{self, ConstantParameterRef, LocalSubstitution, TraitAssociatedConstantRef};

/// Is an enumeration of all possible unpackable constants.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Unpackable {
    Parameter(ConstantParameterRef),
    TraitAssociated(TraitAssociated),
}

/// Represents a single element of a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TupleElement {
    /// A regular singular constant.
    Regular(Constant),
    /// A constant that can be unpacked into multiple elements.
    Unpacked(Unpackable),
}

/// Represents a tuple constant.
///
/// A tuple constant consists of a list of [`TupleElement`]s. In a single tuple, there can be only
/// one [`Unpackable`] element
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple(pub Vec<TupleElement>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array(pub Vec<Constant>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    pub enum_id: ID<symbol::Enum>,
    /// The subsitution of the enum type must be *complete*.
    pub substitution: LocalSubstitution,
    pub variant_index: usize,
    pub constant: Box<Constant>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub struct_id: ID<symbol::Struct>,
    /// The subsitution of the struct type must be *complete*.
    pub substitution: LocalSubstitution,
    pub field_constants: Vec<Constant>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated {
    pub reference: TraitAssociatedConstantRef,
    pub association_substitution: LocalSubstitution,
    pub trait_substitution: LocalSubstitution,
}

/// Reprsents a compile-time evaluated constant expression.
///
/// [`Constant`] represents various `const` declaration values in the language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constant {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Bool(bool),
    Tuple(Tuple),
    Array(Array),
    Enum(Enum),
    Struct(Struct),
    Parameter(ConstantParameterRef),
    TraitAssociated(TraitAssociated),
}
