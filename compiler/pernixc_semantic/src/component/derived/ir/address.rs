//! Contains the definition of [`Address`] and its variants.

use enum_as_inner::EnumAsInner;
use pernixc_arena::{Key, ID};
use serde::{Deserialize, Serialize};

use crate::{
    component::derived::{
        fields::{self},
        function_signature::Parameter,
        ir::{alloca::Alloca, model::Transform, value::Value},
    },
    table::GlobalID,
    term::{
        self,
        r#type::{Qualifier, Type},
    },
};

/// The address points to a field in a struct.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Field<M: term::Model> {
    /// The address to the struct.
    pub struct_address: Box<Address<M>>,

    /// The field that the address points to.
    pub id: ID<fields::Field>,
}

/// The address points to an element in an array.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Index<M: term::Model> {
    /// The address to the array.
    pub array_address: Box<Address<M>>,

    /// The index to access.
    pub indexing_value: Value<M>,
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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Variant<M: term::Model> {
    /// The address to the variant.
    pub enum_address: Box<Address<M>>,

    /// The variant of to interpret the enum address as.
    pub id: GlobalID,
}

/// The address points to an element in a tuple.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Tuple<M: term::Model> {
    /// The address to the tuple.
    pub tuple_address: Box<Address<M>>,

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
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Reference<M: term::Model> {
    /// The reference qualifier of the memory pointer.
    pub qualifier: Qualifier,

    /// The address where the memory pointer is stored.
    pub reference_address: Box<Address<M>>,
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Memory<M: term::Model> {
    Parameter(ID<Parameter>),
    Alloca(ID<Alloca<M>>),
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
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Address<M: term::Model> {
    Memory(Memory<M>),

    Field(Field<M>),
    Tuple(Tuple<M>),
    Index(Index<M>),
    Variant(Variant<M>),
    Reference(Reference<M>),
}

impl<M: term::Model> Address<M> {
    /// Gets the number of dereference operations found in the address.
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
    pub const fn get_root_memory(mut self: &Self) -> &Memory<M> {
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

impl<M: term::Model> Address<M> {
    /// Transforms the [`Address`] to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Address<T::Target>, T::Error> {
        Ok(match self {
            Self::Memory(memory) => match memory {
                Memory::Parameter(id) => Address::Memory(Memory::Parameter(id)),
                Memory::Alloca(id) => Address::Memory(Memory::Alloca(
                    ID::from_index(id.into_index()),
                )),
            },
            Self::Field(field) => Address::Field(Field {
                struct_address: Box::new(
                    field.struct_address.transform_model(transformer)?,
                ),
                id: field.id,
            }),
            Self::Tuple(tuple) => Address::Tuple(Tuple {
                tuple_address: Box::new(
                    tuple.tuple_address.transform_model(transformer)?,
                ),
                offset: tuple.offset,
            }),
            Self::Index(index) => Address::Index(Index {
                array_address: Box::new(
                    index.array_address.transform_model(transformer)?,
                ),
                indexing_value: index
                    .indexing_value
                    .transform_model(transformer)?,
            }),
            Self::Variant(variant) => Address::Variant(Variant {
                enum_address: Box::new(
                    variant.enum_address.transform_model(transformer)?,
                ),
                id: variant.id,
            }),
            Self::Reference(reference_address) => {
                Address::Reference(Reference {
                    qualifier: reference_address.qualifier,
                    reference_address: Box::new(
                        reference_address
                            .reference_address
                            .transform_model(transformer)?,
                    ),
                })
            }
        })
    }
}
