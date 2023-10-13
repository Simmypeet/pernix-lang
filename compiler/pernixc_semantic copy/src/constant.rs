//! all the definitions of compile-time evaluated constant value.

use enum_as_inner::EnumAsInner;

use crate::{
    symbol::{ConstantParameterRef, LocalSubstitution, LocalVariantRef, TraitConstantRef},
    ty,
};

/// Represents a trait associated constant, denoted by `trait<args>::constant<args>` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated {
    /// The reference of the trait constant symbol.
    pub trait_constant_ref: TraitConstantRef,

    /// The generic arguments supplied to the trait.
    pub trait_substitution: LocalSubstitution,
}

/// Represents a struct constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    /// The type of the struct constant value.
    pub struct_ty: ty::Struct,

    /// The value of each field in the struct constant value.
    pub fields: Vec<Constant>,
}

/// Represents an enum constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    /// The type of the enum constant value.
    pub enum_ty: ty::Enum,

    /// The reference to the variant symbol that this constant represents.
    pub local_variant_ref: LocalVariantRef,

    /// The associated value of the enum (if any).
    pub value: Option<Box<Constant>>,
}

/// Represents an array constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    /// The type of the array constant value.
    pub element_ty: ty::Type,

    /// The value of each element in the array constant value.  
    ///
    /// The type of each element must be the same as the `element_ty` field.
    pub elements: Vec<Constant>,
}

/// Represents an **unpacked** constant value in the tuple, denoted by `...value` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Unpacked {
    Parameter(ConstantParameterRef),
    TraitAssociated(TraitAssociated),
}

/// Represents an element value in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum TupleElement {
    Regular(Constant),
    Unpacked(Unpacked),
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement>,
}

/// Represents a primitive constant value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Primitive {
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Usize(usize),
    Isize(isize),
}

/// Represents a compile-time evaluated constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Constant {
    Primitive(Primitive),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Tuple(Tuple),
    Parameter(ConstantParameterRef),
    TraitAssociated(TraitAssociated),
}

impl Constant {
    /// Checks whether the constant is concrete.
    ///
    /// For a constant to be concrete, it must not contain any generic parameters or associated
    /// constants.
    #[must_use]
    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Primitive(_) => true,
            Self::Parameter(_) | Self::TraitAssociated(_) => false,
            Self::Struct(struct_val) => struct_val.fields.iter().all(Self::is_concrete),
            Self::Enum(enum_val) => enum_val.value.as_ref().map_or(true, |x| x.is_concrete()),
            Self::Array(array_val) => array_val.elements.iter().all(Self::is_concrete),
            Self::Tuple(tuple_val) => tuple_val.elements.iter().all(|x| match x {
                TupleElement::Regular(regular) => regular.is_concrete(),
                TupleElement::Unpacked(_) => false,
            }),
        }
    }

    /// Checks if `self` constant is subset or equal to `other` constant.
    #[must_use]
    pub fn is_subset_or_equal(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        match other {
            Self::Struct(struct_val) => {
                struct_val.fields.iter().any(|x| self.is_subset_or_equal(x))
            }
            Self::Enum(enum_val) => enum_val
                .value
                .as_ref()
                .map_or(false, |x| self.is_subset_or_equal(x)),
            Self::Array(array_val) => array_val
                .elements
                .iter()
                .any(|x| self.is_subset_or_equal(x)),
            Self::Tuple(tuple_val) => tuple_val.elements.iter().any(|x| match (self, x) {
                (this, TupleElement::Regular(regular)) => this.is_subset_or_equal(regular),
                (.., TupleElement::Unpacked(Unpacked::TraitAssociated(trait_associated))) => {
                    if let Self::TraitAssociated(this) = self {
                        if this == trait_associated {
                            return true;
                        }
                    }

                    trait_associated
                        .trait_substitution
                        .constants
                        .iter()
                        .any(|x| self.is_subset_or_equal(x))
                }
                (Self::Parameter(this), TupleElement::Unpacked(Unpacked::Parameter(parameter))) => {
                    this == parameter
                }
                _ => false,
            }),
            Self::TraitAssociated(trait_associated_val) => trait_associated_val
                .trait_substitution
                .constants
                .iter()
                .any(|x| self.is_subset_or_equal(x)),
            _ => false,
        }
    }
}
 