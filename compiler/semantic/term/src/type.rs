//! Contains the definition of [`Type`] term.

use std::{fmt::Write, ops::Deref};

use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_stable_type_id::Identifiable;

use crate::{
    constant::Constant,
    error::Error,
    generic_arguments::{
        MemberSymbol, SubMemberSymbolLocation, SubSymbolLocation,
        SubTraitMemberLocation, Symbol, TraitMember,
    },
    generic_parameters::{get_generic_parameters, TypeParameterID},
    inference,
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, Location, SubTerm, TermLocation},
    tuple::SubTupleLocation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    StableHash,
    Serialize,
    Deserialize,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Qualifier {
    #[display("immutable")]
    Immutable,
    #[display("mutable")]
    Mutable,
}

/// Represents a pointer type, denoted by `*mutable? TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Pointer {
    /// Determines whether the pointer is mutable.
    pub mutable: bool,

    /// The type that the pointer points to.
    pub pointee: Box<Type>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Reference {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime,

    /// The type that the reference points to.
    pub pointee: Box<Type>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Array {
    /// Constant representing the length of the array.
    pub length: Constant,

    /// The type of the elements in the array.
    pub r#type: Box<Type>,
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
    StableHash,
    Serialize,
    Deserialize,
    derive_more::Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("int8")]
    Int8,
    #[display("int16")]
    Int16,
    #[display("int32")]
    Int32,
    #[display("int64")]
    Int64,
    #[display("uint8")]
    Uint8,
    #[display("uint16")]
    Uint16,
    #[display("uint32")]
    Uint32,
    #[display("uint64")]
    Uint64,
    #[display("float32")]
    Float32,
    #[display("float64")]
    Float64,
    #[display("bool")]
    Bool,
    #[display("usize")]
    Usize,
    #[display("isize")]
    Isize,
}

/// Represents a tuple type, denoted by `(type, type, ...type)` syntax.
pub type Tuple = crate::tuple::Tuple<Type>;

/// Represents a phantom type, denoted by `phantom TYPE` syntax.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct Phantom(pub Box<Type>);

/// Represents a function signature object. Can be used to represent a function
/// pointer.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
)]
pub struct FunctionSignature {
    /// The list of function parameters
    pub parameters: Vec<Type>,

    /// The return type of the function
    pub return_type: Box<Type>,
}

/// A location pointing to either a parameter or the return type of a function
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
pub enum SubFunctionSignatureLocation {
    /// Points to a parameter at the given index in the function signature
    Parameter(usize),

    /// Points to the return type of the function signature
    ReturnType,
}

/// Represents a type term
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Serialize,
    Deserialize,
    Identifiable,
    derive_more::From,
    EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Type {
    #[from]
    Inference(inference::Variable<Self>),
    #[from]
    Primitive(Primitive),
    #[from]
    Parameter(TypeParameterID),
    #[from]
    Symbol(Symbol),
    #[from]
    Pointer(Pointer),
    #[from]
    Reference(Reference),
    #[from]
    Array(Array),
    #[from]
    Tuple(Tuple),
    #[from]
    Phantom(Phantom),
    #[from]
    MemberSymbol(MemberSymbol),
    #[from]
    TraitMember(TraitMember),
    #[from]
    FunctionSignature(FunctionSignature),
    #[from]
    Error(Error),
}

impl Default for Type {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl Type {
    /// Creates a unit type, which is represented as an empty tuple.
    #[must_use]
    pub const fn unit() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }

    /// Keeps removing the reference until it reaches a non-reference type.
    ///
    /// This is useful for pattern matching.
    #[must_use]
    pub const fn reduce_reference(mut self: &Self) -> &Self {
        while let Self::Reference(reference) = self {
            self = &*reference.pointee;
        }
        self
    }
}

impl TryFrom<Type> for Tuple {
    type Error = Type;

    fn try_from(value: Type) -> Result<Self, Self::Error> {
        match value {
            Type::Tuple(tuple) => Ok(tuple),
            _ => Err(value),
        }
    }
}

/// The location pointing to a sub-lifetime term in a type.
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

impl Location<Type, Lifetime> for SubLifetimeLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Lifetime) {
        let reference = match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                &mut reference.lifetime
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term_mut(location).unwrap(),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.0.get_term_mut(location.0).unwrap()
            }

            term => panic!(
                "invalid sub-lifetime location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Lifetime> {
        match (term, self) {
            (Type::Reference(reference), Self::Reference) => {
                Some(reference.lifetime)
            }

            (Type::Symbol(symbol), Self::Symbol(location)) => {
                symbol.get_term(location).copied()
            }

            (
                Type::MemberSymbol(member_symbol),
                Self::MemberSymbol(location),
            ) => member_symbol.get_term(location).copied(),

            (Type::TraitMember(trait_member), Self::TraitMember(location)) => {
                trait_member.0.get_term(location.0).copied()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Lifetime> {
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

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Lifetime> {
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

    /// The return type or a parameter of a function signature.
    #[from]
    FunctionSignature(SubFunctionSignatureLocation),
}

impl From<SubTypeLocation> for TermLocation {
    fn from(value: SubTypeLocation) -> Self {
        Self::Type(sub_term::SubTypeLocation::FromType(value))
    }
}

impl Location<Type, Type> for SubTypeLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Type) {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location).unwrap()
            }

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
            ) => member_symbol.get_term_mut(location).unwrap(),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term_mut(location.0).unwrap()
            }

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(function_signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    function_signature.parameters.get_mut(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&mut *function_signature.return_type)
                }
            }
            .unwrap(),

            term => {
                panic!("invalid sub-type location: {self:?} for term: {term:?}")
            }
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Type> {
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

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get(idx).cloned()
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some((*signature.return_type).clone())
                }
            },

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Type) -> Option<&Type> {
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

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&*signature.return_type)
                }
            },

            _ => None,
        }
    }

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Type> {
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

            (
                Self::FunctionSignature(location),
                Type::FunctionSignature(signature),
            ) => match location {
                SubFunctionSignatureLocation::Parameter(idx) => {
                    signature.parameters.get_mut(idx)
                }
                SubFunctionSignatureLocation::ReturnType => {
                    Some(&mut *signature.return_type)
                }
            },

            _ => None,
        }
    }
}

/// The location pointing to a sub-constant term in a type.
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

impl Location<Type, Constant> for SubConstantLocation {
    fn assign_sub_term(self, term: &mut Type, sub_term: Constant) {
        let reference = match (self, term) {
            (Self::Symbol(location), Type::Symbol(symbol)) => {
                symbol.get_term_mut(location).unwrap()
            }

            (Self::Array, Type::Array(array)) => &mut array.length,

            (
                Self::MemberSymbol(location),
                Type::MemberSymbol(member_symbol),
            ) => member_symbol.get_term_mut(location).unwrap(),

            (Self::TraitMember(location), Type::TraitMember(trait_member)) => {
                trait_member.0.get_term_mut(location.0).unwrap()
            }

            term => panic!(
                "invalid sub-constant location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Type) -> Option<Constant> {
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

    fn get_sub_term_ref(self, term: &Type) -> Option<&Constant> {
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

    fn get_sub_term_mut(self, term: &mut Type) -> Option<&mut Constant> {
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

impl SubTerm for Type {
    type SubLifetimeLocation = SubLifetimeLocation;
    type SubTypeLocation = SubTypeLocation;
    type SubConstantLocation = SubConstantLocation;
    type ThisSubTermLocation = SubTypeLocation;
}

impl Match for Type {
    #[allow(clippy::too_many_lines)]
    fn substructural_match(
        &self,
        other: &Self,
    ) -> Option<
        Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    > {
        match (self, other) {
            (Self::Symbol(lhs), Self::Symbol(rhs)) if lhs.id == rhs.id => {
                lhs.generic_arguments.substructural_match(
                    &rhs.generic_arguments,
                    Substructural::default(),
                    SubSymbolLocation,
                )
            }

            (Self::Pointer(lhs), Self::Pointer(rhs))
                if lhs.mutable == rhs.mutable =>
            {
                Some(Substructural {
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
                Some(Substructural {
                    lifetimes: vec![Matching {
                        lhs: lhs.lifetime,
                        rhs: rhs.lifetime,
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

            (Self::Array(lhs), Self::Array(rhs)) => Some(Substructural {
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
            }),

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
            }

            (Self::TraitMember(lhs), Self::TraitMember(rhs))
                if lhs.0.id == rhs.0.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        Substructural::default(),
                        |x| {
                            SubTraitMemberLocation(SubMemberSymbolLocation {
                                index: x,
                                from_parent: true,
                            })
                        },
                    )
                    .and_then(|x| {
                        lhs.member_generic_arguments.substructural_match(
                            &rhs.member_generic_arguments,
                            x,
                            |x| {
                                SubTraitMemberLocation(
                                    SubMemberSymbolLocation {
                                        index: x,
                                        from_parent: false,
                                    },
                                )
                            },
                        )
                    })
            }

            (Self::MemberSymbol(lhs), Self::MemberSymbol(rhs))
                if lhs.id == rhs.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        Substructural::default(),
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

            (Self::Phantom(lhs), Self::Phantom(rhs)) => Some(Substructural {
                lifetimes: Vec::new(),
                types: vec![Matching {
                    lhs: (*lhs.0).clone(),
                    rhs: (*rhs.0).clone(),
                    lhs_location: SubTypeLocation::Phantom,
                    rhs_location: SubTypeLocation::Phantom,
                }],
                constants: Vec::new(),
            }),

            (Self::FunctionSignature(lhs), Self::FunctionSignature(rhs)) => {
                if lhs.parameters.len() != rhs.parameters.len() {
                    return None;
                }

                let mut substructural = Substructural::default();

                for (i, (lhs, rhs)) in
                    lhs.parameters.iter().zip(rhs.parameters.iter()).enumerate()
                {
                    substructural.types.push(Matching {
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                        lhs_location: SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::Parameter(i),
                        ),
                        rhs_location: SubTypeLocation::FunctionSignature(
                            SubFunctionSignatureLocation::Parameter(i),
                        ),
                    });
                }

                substructural.types.push(Matching {
                    lhs: (*lhs.return_type).clone(),
                    rhs: (*rhs.return_type).clone(),
                    lhs_location: SubTypeLocation::FunctionSignature(
                        SubFunctionSignatureLocation::ReturnType,
                    ),
                    rhs_location: SubTypeLocation::FunctionSignature(
                        SubFunctionSignatureLocation::ReturnType,
                    ),
                });

                Some(substructural)
            }

            _ => None,
        }
    }

    fn get_substructural(
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.types
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.types
    }
}

impl crate::display::Display for Type {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Inference(inference) => {
                let Some(rendering) =
                    formatter.type_inference_map.and_then(|x| x.get(inference))
                else {
                    return write!(formatter, "_");
                };

                match rendering {
                    crate::display::InferenceRendering::Recurse(ty) => {
                        Box::pin(ty.fmt(engine, formatter)).await
                    }
                    crate::display::InferenceRendering::Rendered(flex_str) => {
                        write!(formatter, "{flex_str}")
                    }
                }
            }

            Self::Primitive(primitive) => write!(formatter, "{primitive}"),

            Self::Parameter(member_id) => {
                let generic_parameters = engine
                    .get_generic_parameters(member_id.parent_id)
                    .await
                    .unwrap();

                write!(
                    formatter,
                    "{}",
                    generic_parameters.types()[member_id.id].name
                )
            }

            Self::Symbol(symbol) => {
                Box::pin(symbol.fmt(engine, formatter)).await
            }

            Self::Pointer(pointer) => {
                write!(
                    formatter,
                    "*{}",
                    if pointer.mutable { "mut " } else { "" }
                )?;

                Box::pin(pointer.pointee.deref().fmt(engine, formatter)).await
            }

            Self::Reference(reference) => {
                write!(formatter, "&")?;

                if reference.lifetime.will_be_displayed() {
                    reference.lifetime.fmt(engine, formatter).await?;
                    write!(formatter, " ")?;
                }

                if reference.qualifier == Qualifier::Mutable {
                    write!(formatter, "mut ")?;
                }

                Box::pin(reference.pointee.deref().fmt(engine, formatter)).await
            }

            Self::Array(array) => {
                write!(formatter, "[")?;
                Box::pin(array.r#type.fmt(engine, formatter)).await?;
                write!(formatter, " x ")?;
                Box::pin(array.length.fmt(engine, formatter)).await?;
                write!(formatter, "]")
            }

            Self::Tuple(tuple) => Box::pin(tuple.fmt(engine, formatter)).await,

            Self::Phantom(phantom) => {
                write!(formatter, "phantom ")?;
                Box::pin(phantom.0.fmt(engine, formatter)).await
            }

            Self::MemberSymbol(member_symbol) => {
                Box::pin(member_symbol.fmt(engine, formatter)).await
            }

            Self::TraitMember(trait_member) => {
                Box::pin(trait_member.fmt(engine, formatter)).await
            }

            Self::FunctionSignature(function_signature) => {
                write!(formatter, "function(")?;
                for (i, ty) in function_signature.parameters.iter().enumerate()
                {
                    Box::pin(ty.fmt(engine, formatter)).await?;

                    if i != function_signature.parameters.len() - 1 {
                        write!(formatter, ", ")?;
                    }
                }

                write!(formatter, ") -> ")?;

                Box::pin(function_signature.return_type.fmt(engine, formatter))
                    .await
            }

            Self::Error(_) => {
                write!(formatter, "{{error}}")
            }
        }
    }
}
