//! Contains all the definition of the types used in the semantic analysis.

use enum_as_inner::EnumAsInner;

use crate::{
    constant,
    symbol::{
        EnumRef, GenericItemRef, LifetimeParameterRef, LocalSubstitution, StructRef, TraitTypeRef,
        TypeParameterRef,
    },
};

/// Contains all the primitive types of the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
    Bool,
    Float32,
    Float64,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Usize,
    Isize,
}

/// Represents an `enum` type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    /// The index of the enum in the [`crate::table::Table::enums`] field.
    pub enum_ref: EnumRef,

    /// The generic arguments substituted to the enum.
    pub substitution: LocalSubstitution,
}

/// Represents a quantifier on a reference or a pointer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Qualifier {
    Restrict,
    Mutable,
    Immutable,
}

/// Represents a reference type, denoted by `&type` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference {
    /// The qualifier of the reference.
    pub qualifier: Qualifier,

    /// The lifetime of the reference.
    pub lifetime: Lifetime,

    /// The inner type of the reference.
    pub operand: Box<Type>,
}

/// Represents a pointer type, denoted by `*type` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    /// The qualifier of the pointer.
    pub qualifier: Qualifier,

    /// The inner type of the pointer.
    pub operand: Box<Type>,
}

/// Represents a lifetime filled in the type system automatically when the lifetime is not
/// specified.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ElidedLifetime {
    /// The [`GenericItemRef`] refers to where the lifetime is elided from, thus giving the elided
    /// lifetime.
    pub generic_item_ref: GenericItemRef,
}

/// Represents a lifetime in the type system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Lifetime {
    /// The `static` lifetime, which is the lifetime of the whole program.
    Static,

    /// Lifetime represented by a generic lifetime parameter.
    Parameter(LifetimeParameterRef),

    /// This lifetime is present when the lifetimes are not specified when required.
    Elided(ElidedLifetime),
}

/// Represents a `struct` type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    /// The reference of the struct in the [`crate::table::Table::structs`] field.
    pub struct_ref: StructRef,

    /// The generic arguments substituted to the struct.
    pub substitution: LocalSubstitution,
}

/// Represents an **unpacked** element type in the tuple, denoted by `...type` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Unpacked {
    Parameter(TypeParameterRef),
    TraitAssociated(TraitAssociated),
}

/// Represents an element type in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum TupleElement {
    /// Represents a singular type in the tuple.
    Regular(Type),

    /// Represents a type that might be expanded into multiple types in the tuple later on.
    Unpacked(Unpacked),
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple {
    /// The elements of the tuple.
    ///
    /// At most only one [`TupleElement::Unpacked`] can be present in the tuple.
    pub elements: Vec<TupleElement>,
}

/// Represents an associated type on the unresolved trait, denoated by
/// `trait<args>::associated<args>` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitAssociated {
    /// The reference of the trait type symbol.
    pub trait_type_ref: TraitTypeRef,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The generic arguments substituted to the associated type.
    pub associated_substitution: LocalSubstitution,
}

/// Represents an array type, denoted by `[type: size]` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    /// The type of the array.
    pub element_ty: Box<Type>,

    /// The constant defining the size of the array.
    ///
    /// The type of the constant must be `usize`
    pub size: Box<constant::Constant>,
}

/// Represents a type of the language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Type {
    Enum(Enum),
    Reference(Reference),
    Pointer(Pointer),
    Struct(Struct),
    Tuple(Tuple),
    Parameter(TypeParameterRef),
    Primitive(Primitive),
    Array(Array),
    TraitAssociated(TraitAssociated),
}

impl Type {
    /// Creates a unit type `()` or empty tuple type.
    #[must_use]
    pub fn unit() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }

    /// Checks whether the type is a *concrete* type or not.
    ///
    /// For a type to be concrete, it must not contain any generic parameters or associated types.
    #[must_use]
    pub fn is_concrete(&self) -> bool {
        match self {
            Self::Enum(enum_ty) => enum_ty.substitution.is_concrete(),
            Self::Reference(reference_ty) => reference_ty.operand.is_concrete(),
            Self::Pointer(pointer_ty) => pointer_ty.operand.is_concrete(),
            Self::Struct(struct_ty) => struct_ty.substitution.is_concrete(),
            Self::Tuple(tuple_ty) => tuple_ty.elements.iter().all(|x| match x {
                TupleElement::Regular(regular) => regular.is_concrete(),
                TupleElement::Unpacked(_) => false,
            }),
            Self::Array(array_ty) => {
                array_ty.element_ty.is_concrete() && array_ty.size.is_concrete()
            }
            Self::Primitive(_) => true,

            // even the substitution is concrete, the associated Self is always not a concrete type.
            // substitute the associated type with a concrete type will result in a concrete type.
            Self::Parameter(_) | Self::TraitAssociated(_) => false,
        }
    }

    /// Checks if `self` type appears on the `other` type or it is equal to `other` type.
    #[must_use]
    pub fn is_subset_or_equal(&self, other: &Self) -> bool {
        if self == other {
            return true;
        }

        match other {
            Self::Enum(enum_ty) => enum_ty
                .substitution
                .types
                .iter()
                .any(|x| self.is_subset_or_equal(x)),
            Self::Reference(reference_ty) => self.is_subset_or_equal(&reference_ty.operand),
            Self::Pointer(pointer_ty) => self.is_subset_or_equal(&pointer_ty.operand),
            Self::Struct(struct_ty) => struct_ty
                .substitution
                .types
                .iter()
                .any(|x| self.is_subset_or_equal(x)),
            Self::Tuple(tuple_ty) => tuple_ty.elements.iter().any(|x| match (self, x) {
                (this, TupleElement::Regular(regular)) => this.is_subset_or_equal(regular),
                (.., TupleElement::Unpacked(Unpacked::TraitAssociated(trait_associated))) => {
                    if let Self::TraitAssociated(this) = self {
                        if this == trait_associated {
                            return true;
                        }
                    }

                    trait_associated
                        .trait_substitution
                        .types
                        .iter()
                        .any(|x| self.is_subset_or_equal(x))
                        || trait_associated
                            .associated_substitution
                            .types
                            .iter()
                            .any(|x| self.is_subset_or_equal(x))
                }
                (Self::Parameter(this), TupleElement::Unpacked(Unpacked::Parameter(parameter))) => {
                    this == parameter
                }
                _ => false,
            }),
            Self::TraitAssociated(trait_associated) => {
                trait_associated
                    .trait_substitution
                    .types
                    .iter()
                    .any(|x| self.is_subset_or_equal(x))
                    || trait_associated
                        .associated_substitution
                        .types
                        .iter()
                        .any(|x| self.is_subset_or_equal(x))
            }
            _ => false,
        }
    }
}
