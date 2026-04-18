//! Data definitions for constant terms.

use derive_more::Display;
use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    Never, TermRef,
    error::Error,
    generic_parameters::{ConstantParameter, ConstantParameterID},
    inference,
    sub_term::{self, IterSubTerms, SubTerm},
    tuple::SubTupleLocation,
};

/// Represents a primitive constant.
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
    EnumAsInner,
    Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("{_0}i8")]
    Int8(i8),
    #[display("{_0}i16")]
    Int16(i16),
    #[display("{_0}i32")]
    Int32(i32),
    #[display("{_0}i64")]
    Int64(i64),
    #[display("{_0}isize")]
    Isize(i64),
    #[display("{_0}u8")]
    Uint8(u8),
    #[display("{_0}u16")]
    Uint16(u16),
    #[display("{_0}u32")]
    Uint32(u32),
    #[display("{_0}u64")]
    Uint64(u64),
    #[display("{_0}usize")]
    Usize(u64),
    #[display("{_0}bool")]
    Bool(bool),
}

/// Represents a struct constant.
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
pub struct Struct {
    id: Global<pernixc_symbol::SymbolID>,
    fields: Vec<Interned<Constant>>,
}

/// Represents an enum constant.
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
pub struct Enum {
    variant_id: Global<pernixc_symbol::SymbolID>,
    associated_value: Option<Interned<Constant>>,
}

/// Represents an array constant.
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
pub struct Array {
    elements: Vec<Interned<Constant>>,
}

impl Struct {
    /// Creates a new struct constant payload.
    #[must_use]
    pub const fn new(
        id: Global<pernixc_symbol::SymbolID>,
        fields: Vec<Interned<Constant>>,
    ) -> Self {
        Self { id, fields }
    }

    /// Returns the struct symbol id.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::SymbolID> { self.id }

    /// Returns the field values.
    #[must_use]
    pub fn fields(&self) -> &[Interned<Constant>] { &self.fields }

    /// Returns the field values mutably.
    #[must_use]
    pub fn fields_mut(&mut self) -> &mut [Interned<Constant>] {
        &mut self.fields
    }
}

impl Enum {
    /// Creates a new enum constant payload.
    #[must_use]
    pub const fn new(
        variant_id: Global<pernixc_symbol::SymbolID>,
        associated_value: Option<Interned<Constant>>,
    ) -> Self {
        Self { variant_id, associated_value }
    }

    /// Returns the variant id.
    #[must_use]
    pub const fn variant_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.variant_id
    }

    /// Returns the associated value, if any.
    #[must_use]
    pub const fn associated_value(&self) -> Option<&Interned<Constant>> {
        self.associated_value.as_ref()
    }
}

impl Array {
    /// Creates a new array constant payload.
    #[must_use]
    pub const fn new(elements: Vec<Interned<Constant>>) -> Self {
        Self { elements }
    }

    /// Returns the element values.
    #[must_use]
    pub fn elements(&self) -> &[Interned<Constant>] { &self.elements }

    /// Returns the element values mutably.
    #[must_use]
    pub fn elements_mut(&mut self) -> &mut [Interned<Constant>] {
        &mut self.elements
    }
}

/// Represents a tuple constant.
pub type Tuple = crate::tuple::Tuple<Constant>;

/// Represents a constant term payload.
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
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Constant {
    Primitive(Primitive),
    Inference(inference::Variable<Self>),
    Parameter(ConstantParameterID),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Tuple(Tuple),
    Phantom,
    Error(Error),
}

impl Constant {
    /// Creates a constant parameter reference.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        constant_id: pernixc_arena::ID<ConstantParameter>,
    ) -> Self {
        Self::Parameter(ConstantParameterID::new(parent_global_id, constant_id))
    }
}

impl Default for Constant {
    fn default() -> Self { Self::Tuple(Tuple::unit()) }
}

impl TryFrom<Constant> for Tuple {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Tuple(tuple) => Ok(tuple),
            _ => Err(value),
        }
    }
}

/// The location pointing to a sub-constant term in a constant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubConstantLocation {
    /// The index of the element in a tuple constant.
    #[from]
    Tuple(SubTupleLocation),

    /// The index of the field in a struct constant.
    Struct(usize),

    /// The associated value of an enum constant.
    Enum,

    /// The index of the element in an array constant.
    Array(usize),
}

impl From<SubConstantLocation> for sub_term::TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromConstant(value))
    }
}

impl sub_term::Location<Constant, Constant> for SubConstantLocation {
    fn try_get_sub_term(
        self,
        term: &Constant,
        tracked_engine: &TrackedEngine,
    ) -> Option<Interned<Constant>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => {
                tuple.get_term(&location, tracked_engine)
            }

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields().get(location).cloned()
            }

            (Self::Enum, Constant::Enum(constant)) => {
                constant.associated_value().cloned()
            }

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements().get(location).cloned()
            }

            _ => None,
        }
    }
}

impl SubTerm for Constant {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type SubInstanceLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl IterSubTerms for Constant {
    type TermLocation = SubConstantLocation;

    fn iter_sub_terms<'this>(
        &'this self,
        _: &'this TrackedEngine,
    ) -> impl Iterator<Item = (TermRef<'this>, Self::TermLocation)> + 'this
    {
        pernixc_coroutine_iter::coroutine_iter!({
            match self {
                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Phantom
                | Self::Error(_) => {}

                Self::Struct(struct_constant) => {
                    for (index, field) in
                        struct_constant.fields().iter().enumerate()
                    {
                        yield (
                            TermRef::Constant(field),
                            SubConstantLocation::Struct(index),
                        );
                    }
                }

                Self::Enum(enum_constant) => {
                    if let Some(associated_value) =
                        enum_constant.associated_value()
                    {
                        yield (
                            TermRef::Constant(associated_value),
                            SubConstantLocation::Enum,
                        );
                    }
                }

                Self::Array(array_constant) => {
                    for (index, element) in
                        array_constant.elements().iter().enumerate()
                    {
                        yield (
                            TermRef::Constant(element),
                            SubConstantLocation::Array(index),
                        );
                    }
                }

                Self::Tuple(tuple) => {
                    for (term, location) in tuple
                        .iter_terms_with_location(SubConstantLocation::Tuple)
                    {
                        yield (TermRef::Constant(term), location);
                    }
                }
            }
        })
    }
}
