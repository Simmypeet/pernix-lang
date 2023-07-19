//! This module implements the type system of the compiler.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_system::arena;

use crate::{Enum, LifetimeArgument, Substitution};

/// Enumeration of all possible primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
    Float32,
    Float64,
    Void,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Bool,
}

/// Is an additional qualifier for the reference type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReferenceQualifier {
    /// The reference is mutable.
    Mutable,

    /// The reference is restricted (requires unique access)
    Restrict,
}

/// Represents a reference type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Reference {
    /// The type of the reference.
    pub(super) operand: Box<Type>,

    /// The optional qualifier of the reference.
    pub(super) qualifier: Option<ReferenceQualifier>,

    /// The optional lifetime of the reference.
    pub(super) lifetime_argument: Option<LifetimeArgument>,
}

/// Represents a type from the struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Struct {
    /// The ID of the struct.
    pub(super) struct_id: arena::ID<crate::Struct>,

    /// The generic parameter substitution.
    pub(super) substitution: Substitution,
}

/// Represents a type from the trait associated type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitType {
    /// The ID of the associated type.
    ///
    /// `trait_type_id` already implies the parent trait.
    pub(super) trait_type_id: arena::ID<crate::TraitType>,

    /// The generic parameter substitution for the parent trait part.
    pub(super) trait_substitution: Substitution,

    /// The generic parameter substitution for the associated type part.
    pub(super) trait_type_substitution: Substitution,
}

/// Represents a type symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Type {
    Enum(arena::ID<Enum>),
    Struct(Struct),
    Primitive(Primitive),
    Reference(Reference),
    Parameter(arena::ID<crate::TypeParameter>),
    TraitType(TraitType),
}

impl Type {
    /// Checks if the type is a concrete type.
    #[must_use]
    pub fn is_concrete_type(&self) -> bool {
        match self {
            Self::Enum(..) | Self::Primitive(..) => true,
            Self::Struct(struct_ty) => struct_ty.substitution.is_concrete_substitution(),
            Self::Reference(reference_ty) => reference_ty.operand.is_concrete_type(),
            Self::Parameter(..) | Self::TraitType(..) => false,
        }
    }
}
