//! Contains the definition of [`Type`].

use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, lifetime::Lifetime, Local, MemberSymbol, Never, Substructural, Symbol,
    Term, TupleElement,
};
use crate::{
    arena::ID,
    semantic::{
        predicate::{NonEquality, Outlives, Satisfiability},
        unification::Unification,
        Premise,
    },
    symbol::{self, Enum, GenericID, GlobalID, Struct, TypeParameterID},
};

/// Enumeration of all symbol kinds (as a type term).
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum SymbolKindID {
    Struct(ID<Struct>),
    Enum(ID<Enum>),
    Type(ID<symbol::Type>),
}

impl From<SymbolKindID> for GlobalID {
    fn from(value: SymbolKindID) -> Self {
        match value {
            SymbolKindID::Struct(id) => Self::Struct(id),
            SymbolKindID::Enum(id) => Self::Enum(id),
            SymbolKindID::Type(id) => Self::Type(id),
        }
    }
}

impl From<SymbolKindID> for GenericID {
    fn from(value: SymbolKindID) -> Self {
        match value {
            SymbolKindID::Struct(id) => Self::Struct(id),
            SymbolKindID::Enum(id) => Self::Enum(id),
            SymbolKindID::Type(id) => Self::Type(id),
        }
    }
}

/// Enumeration of either a trait implementation constant or an ADT implementation constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From,
)]
#[allow(missing_docs)]
pub enum MemberSymbolKindID {
    TraitImplementation(ID<symbol::TraitImplementationType>),
    AdtImplementation(ID<symbol::AdtImplementationType>),
    Trait(ID<symbol::TraitType>),
}

impl From<MemberSymbolKindID> for GlobalID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
            MemberSymbolKindID::Trait(id) => id.into(),
        }
    }
}

impl From<MemberSymbolKindID> for GenericID {
    fn from(value: MemberSymbolKindID) -> Self {
        match value {
            MemberSymbolKindID::TraitImplementation(id) => id.into(),
            MemberSymbolKindID::AdtImplementation(id) => id.into(),
            MemberSymbolKindID::Trait(id) => id.into(),
        }
    }
}

/// A qualifier that can be applied to references/pointers.  
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Qualifier {
    Immutable,
    Mutable,
    Restrict,
}

/// Represents a pointer type, denoted by `*QUALIFIDER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    /// The qualifier applied to the pointer.
    pub qualifier: Qualifier,

    /// The type that the pointer points to.
    pub pointee: Box<Type>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime,

    /// The type that the reference points to.
    pub pointee: Box<Type>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
    /// Constant representing the length of the array.
    pub length: Constant,

    /// The type of the elements in the array.
    pub r#type: Box<Type>,
}

/// Contains all primitive types in the language.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Primitive {
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Usize,
    Isize,
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple = super::Tuple<Type>;

/// Represents a type inference variable in hindley-milner type inference.
pub type Inference = Never; /* will be changed */

/// The location pointing to a sub-lifetime term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in a [`Symbol`] type.
    Symbol(usize),

    /// The lifetime of a reference.
    Reference,

    /// A lifetime argument in a [`MemberSymbol`] type.
    MemberSymbol {
        /// True if the lifetime argument is in the parent part, false if the lifetime argument is
        /// in the member part.
        from_parent: bool,

        /// The index of the lifetime argument.
        index: usize,
    },
}

/// The location pointing to a sub-type term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubTypeLocation {
    /// The index of the type argument in a [`Symbol`] type.
    Symbol(usize),

    /// The [`Pointer::pointee`] of a pointer.
    Pointer,

    /// The [`Reference::pointee`] of a reference.
    Reference,

    /// The [`Array::r#type`] of an array.
    Array,

    /// The index of the type element in a [`Tuple`] type.
    Tuple(usize),

    /// The inner type of a [`Local`] type.
    Local,

    /// The type argument in a [`MemberSymbol`] type.
    MemberSymbol {
        /// True if the type argument is in the parent part, false if the type argument is in the
        /// member part.
        from_parent: bool,

        /// The index of the type argument.
        index: usize,
    },
}

/// The location pointing to a sub-constant term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubConstantLocation {
    /// The index of the constant argument in a [`Symbol`] type.
    Symbol(usize),

    /// The constant argument in a [`MemberSymbol`] type.
    MemberSymbol {
        /// True if the constant argument is in the parent part, false if the constant argument is
        /// in the member part.
        from_parent: bool,

        /// The index of the constant argument.
        index: usize,
    },

    /// The [`Array::length`] of an array.
    Array,
}

/// Represents a type term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Type {
    Primitive(Primitive),
    Parameter(TypeParameterID),
    Inference(Inference),
    Symbol(Symbol<SymbolKindID>),
    Pointer(Pointer),
    Reference(Reference),
    Array(Array),
    Tuple(Tuple),
    Local(Local<Self>),

    /// Pleace notice this differences
    ///
    /// In the **AdtImplementation** case, the `parent_generic_arguments` field is **not** deduced
    /// from the implementation directly, bur rather from the ADT that the implementation is for.
    ///
    /// In the **TraitImplementation** case, the `parent_generic_arguments` field **is** deduced
    /// from the implementation.
    MemberSymbol(MemberSymbol<MemberSymbolKindID>),
}

impl TryFrom<Type> for Tuple {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> { value.into_tuple() }
}

impl TryFrom<Type> for TypeParameterID {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> { value.into_parameter() }
}

impl Default for Type {
    fn default() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }
}

macro_rules! get_sub_type_impl {
    ($location:ident, $self:ident, $get:ident, $deref:ident) => {
        match ($location, $self) {
            (SubTypeLocation::Symbol(index), Self::Symbol(symbol)) => {
                symbol.generic_arguments.types.$get(index)
            }

            (SubTypeLocation::Pointer, Self::Pointer(pointer)) => Some(pointer.pointee.$deref()),

            (SubTypeLocation::Reference, Self::Reference(reference)) => {
                Some(reference.pointee.$deref())
            }

            (SubTypeLocation::Array, Self::Array(array)) => Some(array.r#type.$deref()),

            (SubTypeLocation::Tuple(index), Self::Tuple(tuple)) => {
                tuple.elements.$get(index).map(|element| match element {
                    TupleElement::Regular(x) => x,
                    TupleElement::Unpacked(x) => x,
                })
            }

            (SubTypeLocation::Local, Self::Local(local)) => Some(local.0.$deref()),

            (
                SubTypeLocation::MemberSymbol { from_parent, index },
                Self::MemberSymbol(member_symbol),
            ) => {
                if from_parent {
                    member_symbol.parent_generic_arguments.types.$get(index)
                } else {
                    member_symbol.member_generic_arguments.types.$get(index)
                }
            }

            _ => None,
        }
    };
}

macro_rules! get_sub_lifetime_impl {
    ($location:ident, $self:ident, $get:ident, |$reference:ident| $reference_lt:expr) => {
        match ($location, $self) {
            (SubLifetimeLocation::Symbol(index), Self::Symbol(symbol)) => {
                symbol.generic_arguments.lifetimes.$get(index)
            }

            (SubLifetimeLocation::Reference, Self::Reference($reference)) => Some($reference_lt),

            (
                SubLifetimeLocation::MemberSymbol { from_parent, index },
                Self::MemberSymbol(member_symbol),
            ) => {
                if from_parent {
                    member_symbol.parent_generic_arguments.lifetimes.$get(index)
                } else {
                    member_symbol.member_generic_arguments.lifetimes.$get(index)
                }
            }

            _ => None,
        }
    };
}

macro_rules! get_sub_constant_impl {
    ($location:ident, $self:ident, $get:ident, |$array:ident| $array_length:expr) => {
        match ($location, $self) {
            (SubConstantLocation::Symbol(index), Self::Symbol(symbol)) => {
                symbol.generic_arguments.constants.$get(index)
            }

            (SubConstantLocation::Array, Self::Array($array)) => Some($array_length),

            (
                SubConstantLocation::MemberSymbol { from_parent, index },
                Self::MemberSymbol(member_symbol),
            ) => {
                if from_parent {
                    member_symbol.parent_generic_arguments.constants.$get(index)
                } else {
                    member_symbol.member_generic_arguments.constants.$get(index)
                }
            }

            _ => None,
        }
    };
}

impl Term for Type {
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;

    fn get_sub_type(&self, location: Self::SubTypeLocation) -> Option<&Type> {
        get_sub_type_impl!(location, self, get, deref)
    }

    fn get_sub_type_mut(&mut self, location: Self::SubTypeLocation) -> Option<&mut Type> {
        get_sub_type_impl!(location, self, get_mut, deref_mut)
    }

    fn get_sub_lifetime(&self, location: Self::SubLifetimeLocation) -> Option<&Lifetime> {
        get_sub_lifetime_impl!(location, self, get, |reference| &reference.lifetime)
    }

    fn get_sub_lifetime_mut(
        &mut self,
        location: Self::SubLifetimeLocation,
    ) -> Option<&mut Lifetime> {
        get_sub_lifetime_impl!(location, self, get_mut, |reference| &mut reference.lifetime)
    }

    fn get_sub_constant(&self, location: Self::SubConstantLocation) -> Option<&Constant> {
        get_sub_constant_impl!(location, self, get, |array| &array.length)
    }

    fn get_sub_constant_mut(
        &mut self,
        location: Self::SubConstantLocation,
    ) -> Option<&mut Constant> {
        get_sub_constant_impl!(location, self, get_mut, |array| &mut array.length)
    }

    fn substructural_match(&self, other: &Self) -> Option<Substructural> {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => lhs
                .generic_arguments
                .substructural_match(&rhs.generic_arguments, Substructural::default()),

            (Self::Pointer(lhs), Self::Pointer(rhs)) if lhs.qualifier == rhs.qualifier => {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: vec![((*lhs.pointee).clone(), (*rhs.pointee).clone())],
                    constants: Vec::new(),
                })
            }

            (Self::Reference(lhs), Self::Reference(rhs)) if lhs.qualifier == rhs.qualifier => {
                Some(Substructural {
                    lifetimes: vec![(lhs.lifetime, rhs.lifetime)],
                    types: vec![((*lhs.pointee).clone(), (*rhs.pointee).clone())],
                    constants: Vec::new(),
                })
            }

            (Self::Array(lhs), Self::Array(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: vec![((*lhs.r#type).clone(), (*rhs.r#type).clone())],
                constants: vec![(lhs.length.clone(), rhs.length.clone())],
            }),

            (Self::Local(lhs), Self::Local(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: vec![((*lhs.0).clone(), (*rhs.0).clone())],
                constants: Vec::new(),
            }),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => lhs.substructural_match(rhs),

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs)) if lhs.id == rhs.id => lhs
                .parent_generic_arguments
                .substructural_match(&rhs.parent_generic_arguments, Substructural::default())
                .and_then(|x| {
                    lhs.member_generic_arguments
                        .substructural_match(&rhs.member_generic_arguments, x)
                }),

            _ => None,
        }
    }

    fn is_tuple(&self) -> bool { matches!(self, Self::Tuple(..)) }

    fn outlives_predicates<'a>(premise: &'a Premise) -> impl Iterator<Item = &'a Outlives<Self>>
    where
        Self: 'a,
    {
        premise
            .non_equalitiy_predicates
            .iter()
            .filter_map(NonEquality::as_type_outlives)
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Parameter(_) | Self::Inference(_) => Satisfiability::Unsatisfied,

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Local(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::MemberSymbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn get_substructural(substructural: &Substructural) -> &Vec<(Self, Self)> {
        &substructural.types
    }

    fn get_substructural_mut(substructural: &mut Substructural) -> &mut Vec<(Self, Self)> {
        &mut substructural.types
    }

    fn get_unification(unification: &Unification) -> &HashMap<Self, HashSet<Self>> {
        &unification.types
    }

    fn get_unification_mut(unification: &mut Unification) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.types
    }
}

#[cfg(test)]
mod tests;
