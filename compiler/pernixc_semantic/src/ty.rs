//! Contains all the definition of the types used in the semantic analysis.

use crate::symbol::{GenericItemRef, GenericParameterRef, Index, LocalSubstitution};

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
}

/// Represents an `enum` type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum {
    /// The index of the enum in the [`crate::table::Table::enums`] field.
    pub index: Index,

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

/// Represents a lifetime in the type system.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lifetime {
    /// The `static` lifetime, which is the lifetime of the whole program.
    Static,

    /// Lifetime represented by a generic lifetime parameter.
    Parameter(GenericParameterRef),

    /// This lifetime is present when the lifetimes are not specified when required.
    ///
    /// The [`GenericItemRef`] refers to where the lifetime is elided from, thus giving the elided
    /// lifetime.
    Elided(GenericItemRef),
}

/// Represents a `struct` type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    /// The index of the struct in the [`crate::table::Table::structs`] field.
    pub struct_index: Index,

    /// The generic arguments substituted to the struct.
    pub substitution: LocalSubstitution,
}

/// Represents an **unpacked** element type in the tuple, denoted by `...type` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Unpacked {
    Parameter(GenericParameterRef),
    TraitAssociated(TraitAssociated),
}

/// Represents an element type in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    /// The index of the trait in the [`crate::table::Table::traits`] field.
    pub trait_index: Index,

    /// The index of the associated type in the [`crate::symbol::Trait::types`] field.
    pub associated_index: Index,

    /// The generic arguments substituted to the trait.
    pub trait_substitution: LocalSubstitution,

    /// The generic arguments substituted to the associated type.
    pub associated_substitution: LocalSubstitution,
}

/// Represents a type of the language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Type {
    Enum(Enum),
    Reference(Reference),
    Pointer(Pointer),
    Struct(Struct),
    Tuple(Tuple),
    Parameter(GenericParameterRef),
    Primitive(Primitive),
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
}
