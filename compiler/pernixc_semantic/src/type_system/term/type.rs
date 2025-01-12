//! Contains the definition of [`Type`].

use core::fmt;
use std::{
    collections::{BTreeMap, BTreeSet},
    env,
    fmt::Debug,
};

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_table::{
    component::{Member, Name, Parent, SymbolKind},
    DisplayObject, GlobalID, Table,
};
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

use super::{
    constant::Constant, lifetime::Lifetime, Error, GenericArguments, Kind,
    KindMut, MemberSymbol, ModelOf, Never, Symbol, Term,
};
use crate::{
    component::{
        generic_parameters::{
            GenericParameters, TypeParameter, TypeParameterID,
        },
        type_alias::TypeAlias,
    },
    type_system::{
        self,
        equality::Equality,
        instantiation::{self, Instantiation},
        mapping::Mapping,
        matching::{self, Match, Matching},
        model::{Default, Model},
        normalizer::Normalizer,
        predicate::{
            self, resolve_implementation, Outlives, Predicate, ResolutionError,
            Satisfiability,
        },
        sub_term::{
            self, AssignSubTermError, Location, SubMemberSymbolLocation,
            SubSymbolLocation, SubTerm, SubTraitMemberLocation,
            SubTupleLocation, TermLocation,
        },
        unification::{self, Unifier},
        Environment, ResultExt, Succeeded,
    },
};

/// A qualifier that can be applied to references/pointers.
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
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Qualifier {
    #[display(fmt = "immutable")]
    Immutable,
    #[display(fmt = "mutable")]
    Mutable,
}

/// Represents a pointer type, denoted by `*mutable? TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Pointer<M: Model> {
    /// Determines whether the pointer is mutable.
    pub mutable: bool,

    /// The type that the pointer points to.
    pub pointee: Box<Type<M>>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Reference<M: Model> {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime<M>,

    /// The type that the reference points to.
    pub pointee: Box<Type<M>>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Array<M: Model> {
    /// Constant representing the length of the array.
    pub length: Constant<M>,

    /// The type of the elements in the array.
    pub r#type: Box<Type<M>>,
}

/// Contains all primitive types in the language.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::Display,
    EnumIter,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display(fmt = "int8")]
    Int8,
    #[display(fmt = "int16")]
    Int16,
    #[display(fmt = "int32")]
    Int32,
    #[display(fmt = "int64")]
    Int64,
    #[display(fmt = "uint8")]
    Uint8,
    #[display(fmt = "uint16")]
    Uint16,
    #[display(fmt = "uint32")]
    Uint32,
    #[display(fmt = "uint64")]
    Uint64,
    #[display(fmt = "float32")]
    Float32,
    #[display(fmt = "float64")]
    Float64,
    #[display(fmt = "bool")]
    Bool,
    #[display(fmt = "usize")]
    Usize,
    #[display(fmt = "isize")]
    Isize,
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple<M> = super::Tuple<Type<M>>;

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Phantom<M: Model>(pub Box<Type<M>>);

/// The location pointing to a sub-lifetime term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubLifetimeLocation {
    /// The index of lifetime argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The lifetime of a reference.
    Reference,

    /// A lifetime argument in a [`Type::MemberSymbol`] variant.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// A lifetime argument in a [`Type::TraitMember`] variant.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl From<SubLifetimeLocation> for TermLocation {
    fn from(value: SubLifetimeLocation) -> Self {
        Self::Lifetime(sub_term::SubLifetimeLocation::FromType(value))
    }
}

impl<M: Model> Location<Type<M>, Lifetime<M>> for SubLifetimeLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                &mut reference.lifetime
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member
                    .0
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(reference.lifetime.clone())
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).cloned()
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term(location).cloned(),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.0.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location)
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term(location),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.0.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Lifetime<M>> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(&mut reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location)
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term_mut(location),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.0.get_term_mut(location.0)
            }

            _ => None,
        }
    }
}

/// The location pointing to a sub-type term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubTypeLocation {
    /// The index of the type argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The [`Pointer::pointee`] of a pointer.
    Pointer,

    /// The [`Reference::pointee`] of a reference.
    Reference,

    /// The [`Array::type`] of an array.
    Array,

    /// The index of the type element in a [`Type::Tuple`] type.
    #[from]
    Tuple(SubTupleLocation),

    /// The inner type of a [`Type::Phantom`] type.
    Phantom,

    /// The type argument in a [`Type::MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The type argument in a [`Type::TraitMember`] type.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl From<SubTypeLocation> for TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromType(value))
    }
}

impl<M: Model> Location<Type<M>, Type<M>> for SubTypeLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::Pointer, Type::Pointer(pointer)) => &mut *pointer.pointee,

            (Self::Phantom, Type::Phantom(phantom)) => &mut *phantom.0,

            (Self::Reference, Type::Reference(reference)) => {
                &mut *reference.pointee
            }

            (Self::Array, Type::Array(array)) => &mut *array.r#type,

            (Self::Tuple(location), Type::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term)
            }

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member
                    .0
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some((*pointer.pointee).clone())
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some((*reference.pointee).clone())
            }

            (Self::Array, Type::Array(array)) => Some((*array.r#type).clone()),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.term.clone())
                }
                SubTupleLocation::Range { begin, end } => tuple
                    .elements
                    .get(begin..end)
                    .map(|x| Type::Tuple(Tuple { elements: x.to_vec() })),
            },

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(trait_member),
            ) => trait_member.get_term(location).cloned(),

            (Self::Phantom, Type::Phantom(phantom)) => {
                Some((*phantom.0).clone())
            }

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => Some(&*pointer.pointee),

            (Self::Reference, Type::Reference(reference)) => {
                Some(&*reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&*array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| &x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term(location),

            (Self::Phantom, Type::Phantom(phantom)) => Some(&*phantom.0),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Type<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Pointer, Type::Pointer(pointer)) => {
                Some(&mut *pointer.pointee)
            }

            (Self::Reference, Type::Reference(reference)) => {
                Some(&mut *reference.pointee)
            }

            (Self::Array, Type::Array(array)) => Some(&mut *array.r#type),

            (Self::Tuple(location), Type::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get_mut(single).map(|x| &mut x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term_mut(location.0)
            }

            (Self::Phantom, Type::Phantom(phantom)) => Some(&mut *phantom.0),

            _ => None,
        }
    }
}

/// The location pointing to a sub-constant term in a type.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubConstantLocation {
    /// The index of the constant argument in a [`Type::Symbol`] type.
    #[from]
    Symbol(SubSymbolLocation),

    /// The constant argument in a [`Type::MemberSymbol`] type.
    #[from]
    MemberSymbol(SubMemberSymbolLocation),

    /// The [`Array::length`] of an array.
    Array,

    /// The constant argument in a [`Type::TraitMember`] type.
    #[from]
    TraitMember(SubTraitMemberLocation),
}

impl From<SubConstantLocation> for TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromType(value))
    }
}

impl<M: Model> Location<Type<M>, Constant<M>> for SubConstantLocation {
    fn assign_sub_term(
        self,
        term: &mut Type<M>,
        sub_term: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::Array, Type::Array(array)) => &mut array.length,

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol
                .get_term_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member
                    .0
                    .get_term_mut(location.0)
                    .ok_or(AssignSubTermError::InvalidLocation)?
            }

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;
        Ok(())
    }

    fn get_sub_term(self, term: &Type<M>) -> Option<Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location).cloned()
            }

            (Self::Array, Type::Array(array)) => Some(array.length.clone()),

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term(location).cloned(),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term(location.0).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type<M>) -> Option<&Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term(location)
            }

            (Self::Array, Type::Array(array)) => Some(&array.length),

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term(location),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term(location.0)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type<M>) -> Option<&mut Constant<M>> {
        match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location)
            }

            (Self::Array, Type::Array(array)) => Some(&mut array.length),

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term_mut(location.0)
            }

            _ => None,
        }
    }
}

impl<M: Model> SubTerm for Type<M> {
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type ThisSubTermLocation = SubTypeLocation;
}

impl<M: Model> Match for Type<M> {
    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    matching::Substructural::default(),
                    SubSymbolLocation,
                )
            }

            (Self::Pointer(lhs), Self::Pointer(rhs))
                if lhs.mutable == rhs.mutable =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Pointer,
                        rhs_location: SubTypeLocation::Pointer,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Reference(lhs), Self::Reference(rhs))
                if lhs.qualifier == rhs.qualifier =>
            {
                Some(matching::Substructural {
                    lifetimes: vec![Matching {
                        lhs: lhs.lifetime.clone(),
                        rhs: rhs.lifetime.clone(),
                        lhs_location: SubLifetimeLocation::Reference,
                        rhs_location: SubLifetimeLocation::Reference,
                    }],
                    types: vec![Matching {
                        lhs: (*lhs.pointee).clone(),
                        rhs: (*rhs.pointee).clone(),
                        lhs_location: SubTypeLocation::Reference,
                        rhs_location: SubTypeLocation::Reference,
                    }],
                    constants: Vec::new(),
                })
            }

            (Self::Array(lhs), Self::Array(rhs)) => {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.r#type).clone(),
                        rhs: (*rhs.r#type).clone(),
                        lhs_location: SubTypeLocation::Array,
                        rhs_location: SubTypeLocation::Array,
                    }],
                    constants: vec![Matching {
                        lhs: lhs.length.clone(),
                        rhs: rhs.length.clone(),
                        lhs_location: SubConstantLocation::Array,
                        rhs_location: SubConstantLocation::Array,
                    }],
                })
            }

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        matching::Substructural::default(),
                        |x| SubMemberSymbolLocation {
                            index: x,
                            from_parent: true,
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |x| SubMemberSymbolLocation {
                                index: x,
                                from_parent: false,
                            },
                        )
                    })
            }

            (Self::Phantom(lhs), Self::Phantom(rhs)) => {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: vec![Matching {
                        lhs: (*lhs.0).clone(),
                        rhs: (*rhs.0).clone(),
                        lhs_location: SubTypeLocation::Phantom,
                        rhs_location: SubTypeLocation::Phantom,
                    }],
                    constants: Vec::new(),
                })
            }

            _ => None,
        }
    }

    fn get_substructural(
        substructural: &matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.types
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.types
    }
}

/// A new type wrapper representing a trait associated type.
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
    Deref,
    DerefMut,
)]
pub struct TraitMember<M: Model>(pub MemberSymbol<M>);

/// Represents a type term.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    EnumAsInner,
    Serialize,
    Deserialize,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Type<M: Model> {
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    Inference(M::TypeInference),
    #[from]
    Symbol(Symbol<M>),
    #[from]
    Pointer(Pointer<M>),
    #[from]
    Reference(Reference<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Tuple(Tuple<M>),
    #[from]
    Phantom(Phantom<M>),
    #[from]
    MemberSymbol(MemberSymbol<M>),
    #[from]
    TraitMember(TraitMember<M>),
    #[from]
    Error(Error),
}

/// The set of types that can be inferred. Used in type inference.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::Display,
)]
pub enum Constraint {
    /// The type can be inferred into any type.
    ///
    /// The boolean valuie indicates whether the type can be inferred as a unit
    /// type as a default value.
    #[display(fmt = "{{any}}")]
    All(bool),

    /// The type can be any number type. (signed/unsigned/floating)
    #[display(fmt = "{{number}}")]
    Number,

    /// The type can be integer number type. (signed/unsigned)
    #[display(fmt = "{{integer}}")]
    Integer,

    /// The type can be signed number type. (signed integer)
    #[display(fmt = "{{signedInteger}}")]
    SignedInteger,

    /// The type can be signed number type. (signed integer/floating)
    #[display(fmt = "{{signed}}")]
    Signed,

    /// The type can be unsigned number type. (unsigned integer)
    #[display(fmt = "{{unsignedInteger}}")]
    UnsignedInteger,

    /// The type can be only floating number type. (float32/float64)
    #[display(fmt = "{{floating}}")]
    Floating,
}

impl From<Never> for Constraint {
    fn from(value: Never) -> Self { match value {} }
}

/// An enumeration of either a known type or an inferring type.
///
/// This is used for type checking and type inference.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Expected<M: Model> {
    Known(Type<M>),
    Constraint(Constraint),
}

impl<M: Model> From<Never> for Type<M> {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> ModelOf for Type<M> {
    type Model = M;
}

impl<M: Model> TryFrom<Type<M>> for Tuple<M> {
    type Error = Type<M>;

    fn try_from(value: Type<M>) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

impl<M: Model> TryFrom<Type<M>> for TypeParameterID {
    type Error = Type<M>;

    fn try_from(value: Type<M>) -> Result<Self, Self::Error> {
        value.into_parameter()
    }
}

impl<M: Model> std::default::Default for Type<M> {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl<M: Model> Term for Type<M>
where
    Self: ModelOf<Model = M>,
{
    type GenericParameter = TypeParameter;
    type TraitMember = TraitMember<M>;
    type InferenceVariable = M::TypeInference;
    type Rebind<Ms: Model> = Type<Ms>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match term {
            Type::Primitive(primitive) => Self::Primitive(primitive),
            Type::Parameter(parameter) => Self::Parameter(parameter),
            Type::Inference(inference) => {
                Self::Inference(M::TypeInference::from(inference))
            }
            Type::Symbol(symbol) => {
                Self::Symbol(Symbol::from_other_model(symbol))
            }
            Type::Pointer(pointer) => Self::Pointer(Pointer {
                mutable: pointer.mutable,
                pointee: Box::new(Self::from_other_model(*pointer.pointee)),
            }),
            Type::Reference(reference) => Self::Reference(Reference {
                qualifier: reference.qualifier,
                lifetime: Lifetime::from_other_model(reference.lifetime),
                pointee: Box::new(Self::from_other_model(*reference.pointee)),
            }),
            Type::Array(array) => Self::Array(Array {
                length: Constant::from_other_model(array.length),
                r#type: Box::new(Self::from_other_model(*array.r#type)),
            }),
            Type::Tuple(tuple) => Self::Tuple(Tuple::from_other_model(tuple)),
            Type::Phantom(phantom) => Self::Phantom(Phantom(Box::new(
                Self::from_other_model(*phantom.0),
            ))),
            Type::MemberSymbol(member_symbol) => Self::MemberSymbol(
                MemberSymbol::from_other_model(member_symbol),
            ),
            Type::TraitMember(trait_member) => Self::TraitMember(TraitMember(
                MemberSymbol::from_other_model(trait_member.0),
            )),
            Type::Error(Error) => Self::Error(Error),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,
    {
        Ok(match term {
            Type::Primitive(primitive) => Self::Primitive(primitive),
            Type::Parameter(parameter) => Self::Parameter(parameter),
            Type::Inference(inference) => Self::Inference(
                M::TypeInference::try_from(inference).map_err(Into::into)?,
            ),
            Type::Symbol(symbol) => {
                Self::Symbol(Symbol::try_from_other_model(symbol)?)
            }
            Type::Pointer(pointer) => Self::Pointer(Pointer {
                mutable: pointer.mutable,
                pointee: Box::new(Self::try_from_other_model(
                    *pointer.pointee,
                )?),
            }),
            Type::Reference(reference) => Self::Reference(Reference {
                qualifier: reference.qualifier,
                lifetime: Lifetime::try_from_other_model(reference.lifetime)?,
                pointee: Box::new(Self::try_from_other_model(
                    *reference.pointee,
                )?),
            }),
            Type::Array(array) => Self::Array(Array {
                length: Constant::try_from_other_model(array.length)?,
                r#type: Box::new(Self::try_from_other_model(*array.r#type)?),
            }),
            Type::Tuple(tuple) => {
                Self::Tuple(Tuple::try_from_other_model(tuple)?)
            }
            Type::Phantom(phantom) => Self::Phantom(Phantom(Box::new(
                Self::try_from_other_model(*phantom.0)?,
            ))),
            Type::MemberSymbol(member_symbol) => Self::MemberSymbol(
                MemberSymbol::try_from_other_model(member_symbol)?,
            ),
            Type::TraitMember(trait_member) => Self::TraitMember(TraitMember(
                MemberSymbol::try_from_other_model(trait_member.0)?,
            )),
            Type::Error(Error) => Self::Error(Error),
        })
    }

    #[allow(clippy::too_many_lines, private_bounds, private_interfaces)]
    fn normalize(
        &self,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Option<Succeeded<Self, M>>, type_system::AbruptError> {
        match self {
            // transform the trait-member into trait-implementation-type
            // equivalent
            Self::TraitMember(trait_member) => {
                let Some(trait_id) = environment
                    .table()
                    .get::<Parent>(trait_member.id)
                    .map(|x| x.0)
                else {
                    return Ok(None);
                };

                // resolve the trait implementation
                let mut resolution = match resolve_implementation(
                    GlobalID::new(trait_member.id.target_id, trait_id),
                    &trait_member.parent_generic_arguments,
                    environment,
                ) {
                    Ok(resolution) => resolution,

                    Err(ResolutionError::Abrupt(error)) => {
                        return Err(error);
                    }

                    Err(_) => return Ok(None),
                };

                let trait_member_name = environment
                    .table()
                    .get::<Name>(trait_member.id)
                    .unwrap()
                    .clone();

                // not a trait implementation
                if environment
                    .table
                    .get::<SymbolKind>(resolution.result.id)
                    .map_or(true, |x| {
                        *x != SymbolKind::PositiveTraitImplementation
                    })
                {
                    return Ok(None);
                }

                let Some(implementation_member_id) = environment
                    .table
                    .get::<Member>(resolution.result.id)
                    .and_then(|x| {
                        x.0.get(&trait_member_name.0).copied().map(|x| {
                            GlobalID::new(resolution.result.id.target_id, x)
                        })
                    })
                else {
                    return Ok(None);
                };

                // check if is the type
                if environment
                    .table()
                    .get::<SymbolKind>(implementation_member_id)
                    .map_or(true, |x| *x != SymbolKind::TraitImplementationType)
                {
                    return Ok(None);
                }

                // should have no collision and no mismatched generic arguments
                // count
                {
                    let Some(generic_parameter) = environment
                        .table()
                        .query::<GenericParameters>(implementation_member_id)
                        .extract_cyclic_dependency()?
                    else {
                        return Ok(None);
                    };

                    if resolution
                        .result
                        .instantiation
                        .append_from_generic_arguments(
                            trait_member.member_generic_arguments.clone(),
                            implementation_member_id,
                            &generic_parameter,
                        )
                        .map_or(true, |x| !x.is_empty())
                    {
                        return Ok(None);
                    }
                }

                let Some(mut new_term) = environment
                    .table()
                    .query::<TypeAlias>(implementation_member_id)
                    .extract_cyclic_dependency()?
                    .map(|x| M::from_default_type(x.0.clone()))
                else {
                    return Ok(None);
                };

                instantiation::instantiate(
                    &mut new_term,
                    &resolution.result.instantiation,
                );

                Ok(Some(Succeeded::with_constraints(
                    new_term,
                    resolution.constraints,
                )))
            }

            // unpack the tuple
            Self::Tuple(tuple) => {
                let contain_upacked =
                    tuple.elements.iter().any(|x| x.is_unpacked);

                if !contain_upacked {
                    return Ok(None);
                }

                let mut result = Vec::new();

                for element in tuple.elements.iter().cloned() {
                    if element.is_unpacked {
                        match element.term {
                            Self::Tuple(inner) => {
                                result.extend(inner.elements.iter().cloned());
                            }
                            term => {
                                result.push(super::TupleElement {
                                    term,
                                    is_unpacked: true,
                                });
                            }
                        }
                    } else {
                        result.push(element);
                    }
                }

                let new_type = Self::Tuple(Tuple { elements: result });

                Normalizer::normalize_type(&new_type, environment)?.map_or_else(
                    || Ok(Some(Succeeded::new(new_type))),
                    |x| Ok(Some(x)),
                )
            }

            _ => Normalizer::normalize_type(self, environment)?
                .map_or_else(|| Ok(None), |x| Ok(Some(x))),
        }
    }

    fn as_kind(&self) -> Kind<M> { Kind::Type(self) }

    fn as_kind_mut(&mut self) -> KindMut<M> { KindMut::Type(self) }

    fn try_from_kind(
        kind: Kind<Self::Model>,
    ) -> Result<&Self, Kind<Self::Model>> {
        match kind {
            Kind::Type(ty) => Ok(ty),
            a => Err(a),
        }
    }

    fn try_from_kind_mut(
        kind: KindMut<Self::Model>,
    ) -> Result<&mut Self, KindMut<Self::Model>> {
        match kind {
            KindMut::Type(ty) => Ok(ty),
            a => Err(a),
        }
    }

    #[allow(private_bounds, private_interfaces)]
    fn outlives_satisfiability(&self, _: &Lifetime<M>) -> Satisfiability {
        match self {
            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::Error(_) | Self::Inference(_) | Self::Parameter(_) => {
                Satisfiability::Unsatisfied
            }

            Self::MemberSymbol(_)
            | Self::Symbol(_)
            | Self::Pointer(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Tuple(_)
            | Self::TraitMember(_)
            | Self::Phantom(_) => Satisfiability::Congruent,
        }
    }

    fn from_inference(inference: Self::InferenceVariable) -> Self {
        Self::Inference(inference)
    }

    fn as_generic_parameter(&self) -> Option<&TypeParameterID> {
        self.as_parameter()
    }

    fn as_generic_parameter_mut(&mut self) -> Option<&mut TypeParameterID> {
        self.as_parameter_mut()
    }

    fn into_generic_parameter(self) -> Result<TypeParameterID, Self> {
        self.into_parameter()
    }

    fn as_trait_member(&self) -> Option<&TraitMember<M>> {
        match self {
            Self::TraitMember(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn as_trait_member_mut(&mut self) -> Option<&mut TraitMember<M>> {
        match self {
            Self::TraitMember(trait_member) => Some(trait_member),
            _ => None,
        }
    }

    fn into_trait_member(self) -> Result<TraitMember<M>, Self> {
        match self {
            Self::TraitMember(trait_member) => Ok(trait_member),
            _ => Err(self),
        }
    }

    fn as_tuple(&self) -> Option<&Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn as_tuple_mut(&mut self) -> Option<&mut Tuple<M>> {
        match self {
            Self::Tuple(tuple) => Some(tuple),
            _ => None,
        }
    }

    fn into_tuple(self) -> Result<Tuple<M>, Self> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err(self),
        }
    }

    fn as_inference(&self) -> Option<&Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn as_inference_mut(&mut self) -> Option<&mut Self::InferenceVariable> {
        match self {
            Self::Inference(inference) => Some(inference),
            _ => None,
        }
    }

    fn into_inference(self) -> Result<Self::InferenceVariable, Self> {
        match self {
            Self::Inference(inference) => Ok(inference),
            _ => Err(self),
        }
    }

    fn get_adt_fields(&self, table: &Table) -> Option<Vec<Self>> {
        todo!()
        /*
        match self {
            Self::Symbol(Symbol {
                id: SymbolID::Adt(id),
                generic_arguments,
            }) => match *id {
                AdtID::Struct(struct_id) => {
                    let struct_sym = table.get(struct_id)?;

                    let Ok(substitution) =
                        Instantiation::from_generic_arguments(
                            generic_arguments.clone(),
                            struct_id.into(),
                            &struct_sym.generic_declaration.parameters,
                        )
                    else {
                        return None;
                    };

                    Some(
                        struct_sym
                            .fields()
                            .values()
                            .map(|field| {
                                let mut ty =
                                    M::from_default_type(field.r#type.clone());
                                instantiation::instantiate(
                                    &mut ty,
                                    &substitution,
                                );
                                ty
                            })
                            .collect(),
                    )
                }
                AdtID::Enum(enum_id) => {
                    let enum_sym = table.get(enum_id)?;

                    let Ok(substitution) =
                        Instantiation::from_generic_arguments(
                            generic_arguments.clone(),
                            enum_id.into(),
                            &enum_sym.generic_declaration.parameters,
                        )
                    else {
                        return None;
                    };

                    Some(
                        enum_sym
                            .variant_ids_by_name()
                            .values()
                            .copied()
                            .filter_map(|variant| table.get(variant))
                            .filter_map(|variant| {
                                let mut ty = M::from_default_type(
                                    variant.associated_type.as_ref()?.clone(),
                                );

                                instantiation::instantiate(
                                    &mut ty,
                                    &substitution,
                                );
                                Some(ty)
                            })
                            .collect(),
                    )
                }
            },

            _ => None,
        }
        */
    }

    fn as_outlive_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&Outlives<Self>> {
        predicate.as_type_outlives()
    }

    fn as_outlive_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut Outlives<Self>> {
        predicate.as_type_outlives_mut()
    }

    fn into_outlive_predicate(
        predicate: Predicate<M>,
    ) -> Result<Outlives<Self>, Predicate<M>> {
        predicate.into_type_outlives()
    }

    fn as_trait_member_equality_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&Equality<TraitMember<M>, Self>> {
        predicate.as_trait_type_equality()
    }

    fn as_trait_member_equality_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut Equality<TraitMember<M>, Self>> {
        predicate.as_trait_type_equality_mut()
    }

    fn into_trait_member_equality_predicate(
        predicate: Predicate<M>,
    ) -> Result<Equality<TraitMember<M>, Self>, Predicate<M>> {
        predicate.into_trait_type_equality()
    }

    fn as_tuple_predicate(
        predicate: &Predicate<M>,
    ) -> Option<&predicate::Tuple<Self>> {
        predicate.as_tuple_type()
    }

    fn as_tuple_predicate_mut(
        predicate: &mut Predicate<M>,
    ) -> Option<&mut predicate::Tuple<Self>> {
        predicate.as_tuple_type_mut()
    }

    fn into_tuple_predicate(
        predicate: Predicate<M>,
    ) -> Result<predicate::Tuple<Self>, Predicate<M>> {
        predicate.into_tuple_type()
    }

    fn definite_satisfiability(&self) -> Satisfiability {
        match self {
            Self::Error(_) | Self::Parameter(_) | Self::Inference(_) => {
                Satisfiability::Unsatisfied
            }

            Self::Primitive(_) => Satisfiability::Satisfied,

            Self::MemberSymbol(_)
            | Self::Pointer(_)
            | Self::Symbol(_)
            | Self::Reference(_)
            | Self::Array(_)
            | Self::Phantom(_)
            | Self::TraitMember(_)
            | Self::Tuple(_) => Satisfiability::Congruent,
        }
    }

    fn get_instantiation(
        instantiation: &Instantiation<M>,
    ) -> &BTreeMap<Self, Self> {
        &instantiation.types
    }

    fn get_instantiation_mut(
        instantiation: &mut Instantiation<M>,
    ) -> &mut BTreeMap<Self, Self> {
        &mut instantiation.types
    }

    fn get_substructural_unifier<'a, T: Term>(
        substructural: &'a unification::Substructural<T>,
    ) -> impl Iterator<Item = &'a Unifier<Type<T::Model>>>
    where
        Self: 'a,
    {
        substructural.types.values()
    }

    fn get_mapping(mapping: &Mapping<M>) -> &BTreeMap<Self, BTreeSet<Self>> {
        &mapping.types
    }

    fn get_mapping_mut(
        mapping: &mut Mapping<M>,
    ) -> &mut BTreeMap<Self, BTreeSet<Self>> {
        &mut mapping.types
    }

    fn get_generic_arguments(
        generic_arguments: &GenericArguments<M>,
    ) -> &[Self] {
        &generic_arguments.types
    }

    fn get_generic_arguments_mut(
        generic_arguments: &mut GenericArguments<M>,
    ) -> &mut Vec<Self> {
        &mut generic_arguments.types
    }

    fn from_default_model(term: Type<Default>) -> Self {
        M::from_default_type(term)
    }
}

impl<M: Model> pernixc_table::Display for Type<M>
where
    M::TypeInference: pernixc_table::Display,
    Constant<M>: pernixc_table::Display,
    Lifetime<M>: pernixc_table::Display,
{
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Primitive(primitve) => {
                write!(f, "{primitve}")
            }
            Self::Parameter(type_parameter) => {
                write!(
                    f,
                    "{}",
                    table
                        .query::<GenericParameters>(type_parameter.parent)
                        .map_err(|_| fmt::Error)?
                        .types()
                        .get(type_parameter.id)
                        .ok_or(fmt::Error)?
                        .name
                        .as_deref()
                        .unwrap_or("{unknown}")
                )
            }
            Self::Inference(inference) => {
                write!(f, "{}", DisplayObject { table, display: inference })
            }
            Self::Symbol(symbol) => {
                write!(f, "{}", DisplayObject { table, display: symbol })
            }
            Self::Pointer(pointer) => {
                write!(f, "*")?;

                if pointer.mutable {
                    write!(f, "mutable ")?;
                }

                write!(f, "{}", DisplayObject {
                    table,
                    display: &*pointer.pointee
                })
            }
            Self::Reference(reference) => {
                write!(f, "&")?;

                write!(f, "{} ", DisplayObject {
                    table,
                    display: &reference.lifetime
                })?;

                if Qualifier::Mutable == reference.qualifier {
                    write!(f, "mutable ")?;
                }

                write!(f, "{}", DisplayObject {
                    table,
                    display: &*reference.pointee
                })
            }

            Self::Array(array) => {
                write!(
                    f,
                    "[{}: {}]",
                    DisplayObject { table, display: &*array.r#type },
                    DisplayObject { table, display: &array.length },
                )
            }
            Self::Tuple(tuple) => {
                write!(f, "{}", DisplayObject { table, display: tuple })
            }
            Self::TraitMember(ty) => {
                write!(f, "{}", DisplayObject { table, display: &ty.0 })
            }
            Self::Phantom(phantom) => {
                write!(f, "phantom {}", DisplayObject {
                    table,
                    display: &*phantom.0
                })
            }
            Self::MemberSymbol(member_symbol) => {
                write!(f, "{}", DisplayObject { table, display: member_symbol })
            }
            Self::Error(_) => {
                write!(f, "{{error}}")
            }
        }
    }
}

impl<M: Model> Type<M> {
    /// Keeps removing the reference until it reaches a non-reference type.
    ///
    /// This is useful for pattern matching.
    pub const fn reduce_reference(mut self: &Self) -> &Self {
        while let Self::Reference(reference) = self {
            self = &*reference.pointee;
        }
        self
    }

    /// Gets a list of [`ItemID`]s that occur in the type.
    #[must_use]
    pub fn get_item_id_dependencies(
        &self,
        table: &Table,
    ) -> Option<Vec<GlobalID>> {
        let mut occurrences = match self {
            Self::Error(_)
            | Self::Primitive(_)
            | Self::Parameter(_)
            | Self::Inference(_) => {
                return Some(Vec::new());
            }

            Self::Symbol(symbol) => {
                let mut occurrences =
                    symbol.generic_arguments.get_item_id_dependencies(table)?;
                occurrences.push(symbol.id.into());
                occurrences
            }

            Self::MemberSymbol(member_symbol) => {
                let mut occurrences = member_symbol
                    .parent_generic_arguments
                    .get_item_id_dependencies(table)?;
                occurrences.extend(
                    member_symbol
                        .member_generic_arguments
                        .get_item_id_dependencies(table)?,
                );

                occurrences.push(member_symbol.id.into());
                occurrences.push(GlobalID::new(
                    member_symbol.id.target_id,
                    table.get::<Parent>(member_symbol.id)?.0,
                ));

                occurrences
            }

            Self::TraitMember(trait_member) => {
                let mut occurrences = trait_member
                    .0
                    .parent_generic_arguments
                    .get_item_id_dependencies(table)?;
                occurrences.extend(
                    trait_member
                        .0
                        .member_generic_arguments
                        .get_item_id_dependencies(table)?,
                );

                occurrences.push(trait_member.0.id.into());
                occurrences.push(GlobalID::new(
                    trait_member.0.id.target_id,
                    table.get::<Parent>(trait_member.0.id)?.0,
                ));

                occurrences
            }

            Self::Pointer(symbol) => {
                return symbol.pointee.get_item_id_dependencies(table);
            }
            Self::Reference(symbol) => {
                return symbol.pointee.get_item_id_dependencies(table);
            }

            Self::Array(array) => {
                let mut occurrences =
                    array.r#type.get_item_id_dependencies(table)?;
                occurrences
                    .extend(array.length.get_item_id_dependencies(table)?);
                occurrences
            }
            Self::Tuple(tuple) => {
                let mut occurrences = Vec::new();
                for element in &tuple.elements {
                    occurrences
                        .extend(element.term.get_item_id_dependencies(table)?);
                }
                occurrences
            }
            Self::Phantom(phantom) => {
                return phantom.0.get_item_id_dependencies(table);
            }
        };

        occurrences.sort_unstable();
        occurrences.dedup();

        Some(occurrences)
    }
}

// TODO: bring test back
// #[cfg(test)]
// mod tests;
