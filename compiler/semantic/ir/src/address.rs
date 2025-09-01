//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;
use pernixc_arena::ID;
use pernixc_semantic_element::{fields, parameter::Parameter};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::r#type::Qualifier;

use crate::{alloca::Alloca, value::Value};

/// The address points to a field in a struct.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Field {
    /// The address to the struct.
    pub struct_address: Box<Address>,

    /// The field that the address points to.
    pub id: ID<fields::Field>,
}

/// The address points to an element in an array.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Index {
    /// The address to the array.
    pub array_address: Box<Address>,

    /// The index to access.
    pub indexing_value: Value,
}

/// The offset from the start or end of a tuple.
///
/// Primarily used for indexing into a tuple element with an offset.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub enum Offset {
    /// The offset is from the start of the tuple (0-indexed).
    FromStart(usize),

    /// The offset is from the end of the tuple (0-indexed).
    FromEnd(usize),

    /// Points to the non-definite unpacked tuple element.
    Unpacked,
}

/// Interprets the enum addresss as an associated value of a particular variant.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Variant {
    /// The address to the variant.
    pub enum_address: Box<Address>,

    /// The variant of to interpret the enum address as.
    pub id: Global<pernixc_symbol::ID>,
}

/// The address points to an element in a tuple.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Tuple {
    /// The address to the tuple.
    pub tuple_address: Box<Address>,

    /// The offset of the element to access.
    pub offset: Offset,
}

/// The memory pointer is stored in an address.
///
/// # Example
///
/// ```pnx
/// let x = 32;
/// let address = &unique x;
/// *address = 5;
/// ```
///
/// The `*address` is represented by `ReferenceAddress { address:
/// Memory::Alloca(&address) }`.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
)]
pub struct Reference {
    /// The reference qualifier of the memory pointer.
    pub qualifier: Qualifier,

    /// The address where the memory pointer is stored.
    pub reference_address: Box<Address>,
}

/// Represents a real memory location.
///
/// This is used to represent the base address of a memory location for the
/// [`Address`] type.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Memory {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca>),
}

/// Represents an address to a particular location in memory.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Serialize,
    Deserialize,
    StableHash,
    EnumAsInner,
)]
#[allow(missing_docs, clippy::large_enum_variant)]
pub enum Address {
    Memory(Memory),

    Field(Field),
    Tuple(Tuple),
    Index(Index),
    Variant(Variant),
    Reference(Reference),
}

impl Address {
    /// Gets the number of dereference operations found in the address.
    #[must_use]
    pub const fn get_dereference_count(mut self: &Self) -> usize {
        let mut count = 0;

        loop {
            match self {
                Self::Memory(_) => return count,
                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                    count += 1;
                }
            }
        }
    }

    /// Checks if the `self` address is the child of the `parent` address.
    #[must_use]
    pub fn is_child_of(mut self: &Self, parent: &Self) -> bool {
        loop {
            if self == parent {
                return true;
            }

            match self {
                Self::Memory(_) => return false,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Checks if the address has a root that is a reference.
    ///
    /// This checks if the address contains [`Address::Reference`].
    #[must_use]
    pub const fn is_behind_reference(mut self: &Self) -> bool {
        loop {
            match self {
                Self::Memory(_) => return false,

                Self::Reference(_) => return true,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
            }
        }
    }

    /// Gets the root memory of the address.
    #[must_use]
    pub const fn get_root_memory(mut self: &Self) -> &Memory {
        loop {
            match self {
                Self::Memory(memory) => return memory,
                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Index(index) => self = &*index.array_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Checks if the address is behind an index.
    ///
    /// This checks if the address contains [`Address::Index`].
    #[must_use]
    pub const fn is_behind_index(mut self: &Self) -> bool {
        loop {
            match self {
                Self::Memory(_) => return false,

                Self::Index(_) => return true,

                Self::Field(field) => self = &*field.struct_address,
                Self::Tuple(tuple) => self = &*tuple.tuple_address,
                Self::Variant(variant) => self = &*variant.enum_address,
                Self::Reference(reference) => {
                    self = &*reference.reference_address;
                }
            }
        }
    }

    /// Gets the reference qualifier of the address.
    #[must_use]
    pub fn get_reference_qualifier(&self) -> Option<Qualifier> {
        match self {
            Self::Memory(_) => None,
            Self::Field(field) => {
                field.struct_address.get_reference_qualifier()
            }
            Self::Tuple(tuple) => tuple.tuple_address.get_reference_qualifier(),
            Self::Index(index) => index.array_address.get_reference_qualifier(),
            Self::Variant(variant) => {
                variant.enum_address.get_reference_qualifier()
            }
            Self::Reference(reference) => {
                let parent_qualifier =
                    reference.reference_address.get_reference_qualifier();

                Some(
                    reference
                        .qualifier
                        .min(parent_qualifier.unwrap_or(Qualifier::Mutable)),
                )
            }
        }
    }
}
