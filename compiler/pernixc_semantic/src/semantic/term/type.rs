//! Contains the definition of [`Type`].

use std::collections::{HashMap, HashSet};

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, lifetime::Lifetime, GenericArguments, Local, Match, MemberSymbol, Never,
    SubMemberSymbolTermLocation, SubSymbolTermLocation, SubTupleTermLocation, Substructural,
    Symbol, Term,
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

/// Represents a pointer type, denoted by `*QUALIFIER TYPE` syntax.
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

/// Represents a type inference variable in Hindley Milner type inference.
pub type Inference = Never; /* will be changed */

/// The location pointing to a sub-lifetime term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// The lifetime of a reference.
    Reference,

    /// A lifetime argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),
}

/// The location pointing to a sub-type term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub enum SubTypeLocation {
    /// The index of the type argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// The [`Pointer::pointee`] of a pointer.
    Pointer,

    /// The [`Reference::pointee`] of a reference.
    Reference,

    /// The [`Array::r#type`] of an array.
    Array,

    /// The index of the type element in a [`Tuple`] type.
    #[from]
    Tuple(SubTupleTermLocation),

    /// The inner type of a [`Local`] type.
    Local,

    /// The type argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),
}

/// The location pointing to a sub-constant term in a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From)]
pub enum SubConstantLocation {
    /// The index of the constant argument in a [`Symbol`] type.
    #[from]
    Symbol(SubSymbolTermLocation),

    /// The constant argument in a [`MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolTermLocation),

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

    /// Please notice this differences
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

impl Term for Type {
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type ThisSubTermLocation = SubTypeLocation;

    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<Self::SubLifetimeLocation, Self::SubTypeLocation, Self::SubConstantLocation>,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    Substructural::default(),
                    SubSymbolTermLocation,
                )
            }

            (Self::Pointer(lhs), Self::Pointer(rhs)) if lhs.qualifier == rhs.qualifier => {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Match {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Pointer,
                        rhs_location: SubTypeLocation::Pointer,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Reference(lhs), Self::Reference(rhs)) if lhs.qualifier == rhs.qualifier => {
                Some(Substructural {
                    lifetimes: vec![Match {
                        lhs: lhs.lifetime,
                        rhs: rhs.lifetime,
                        lhs_location: SubLifetimeLocation::Reference,
                        rhs_location: SubLifetimeLocation::Reference,
                    }],
                    types: vec![Match {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Reference,
                        rhs_location: SubTypeLocation::Reference,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Array(lhs), Self::Array(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: vec![Match {
                    lhs: (*lhs.r#type).clone(),
                    rhs: (*rhs.r#type).clone(),
                    lhs_location: SubTypeLocation::Array,
                    rhs_location: SubTypeLocation::Array,
                }],
                constants: vec![Match {
                    lhs: lhs.length.clone(),
                    rhs: rhs.length.clone(),
                    lhs_location: SubConstantLocation::Array,
                    rhs_location: SubConstantLocation::Array,
                }],
            }),

            (Self::Local(lhs), Self::Local(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: vec![Match {
                    lhs: (*lhs.0).clone(),
                    rhs: (*rhs.0).clone(),
                    lhs_location: SubTypeLocation::Local,
                    rhs_location: SubTypeLocation::Local,
                }],
                constants: Vec::new(),
            }),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => lhs.substructural_match(rhs),

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs)) if lhs.id == rhs.id => lhs
                .parent_generic_arguments
                .substructural_match(
                    &rhs.parent_generic_arguments,
                    Substructural::default(),
                    |x| SubMemberSymbolTermLocation {
                        index: x,
                        from_parent: true,
                    },
                )
                .and_then(|x| {
                    lhs.member_generic_arguments.substructural_match(
                        &rhs.member_generic_arguments,
                        x,
                        |x| SubMemberSymbolTermLocation {
                            index: x,
                            from_parent: false,
                        },
                    )
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
            .non_equality_predicates
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

    fn get_unification(unification: &Unification) -> &HashMap<Self, HashSet<Self>> {
        &unification.types
    }

    fn get_unification_mut(unification: &mut Unification) -> &mut HashMap<Self, HashSet<Self>> {
        &mut unification.types
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Match<Self, Self::ThisSubTermLocation>> {
        &substructural.types
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Match<Self, Self::ThisSubTermLocation>> {
        &mut substructural.types
    }

    fn get_generic_arguments(generic_arguments: &GenericArguments) -> &[Self] {
        &generic_arguments.types
    }

    fn get_generic_arguments_mut(generic_arguments: &mut GenericArguments) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }
}

#[cfg(test)]
mod tests;
