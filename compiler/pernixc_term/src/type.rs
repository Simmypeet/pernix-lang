//! Contains the definition of the [`Type`] term.

use std::fmt;

use derive_more::{Deref, DerefMut};
use enum_as_inner::EnumAsInner;
use pernixc_table::{DisplayObject, Table};
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

use crate::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::{GenericParameters, TypeParameterID},
    lifetime::Lifetime,
    matching::{self, Match, Matching},
    sub_term::{
        self, AssignSubTermError, Location, SubMemberSymbolLocation,
        SubSymbolLocation, SubTerm, SubTraitMemberLocation, SubTupleLocation,
        TermLocation,
    },
    Error, MemberSymbol, Model, ModelOf, Never, Symbol,
};

mod arbitrary;

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

impl<M: Model> pernixc_table::Display for TraitMember<M>
where
    GenericArguments<M>: pernixc_table::Display,
{
    fn fmt(&self, table: &Table, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", DisplayObject { display: &self.0, table })
    }
}

/// Represents a function signature object. Can be used to represent a function
/// pointer.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct FunctionSignature<M: Model> {
    /// The list of function parameters
    pub parameters: Vec<Type<M>>,

    /// The return type of the function
    pub return_type: Box<Type<M>>,
}

impl<M: Model> pernixc_table::Display for FunctionSignature<M>
where
    M::TypeInference: pernixc_table::Display,
    Constant<M>: pernixc_table::Display,
    Lifetime<M>: pernixc_table::Display,
{
    fn fmt(&self, table: &Table, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "function(")?;

        for (i, parameter) in self.parameters.iter().enumerate() {
            write!(f, "{}", DisplayObject { table, display: parameter })?;
            if i != self.parameters.len() - 1 {
                write!(f, ", ")?;
            }
        }

        write!(f, "): {}", DisplayObject {
            table,
            display: self.return_type.as_ref()
        })
    }
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
    FunctionSignature(FunctionSignature<M>),
    #[from]
    Error(Error),
}

impl<M: Model> TryFrom<Type<M>> for Tuple<M> {
    type Error = Type<M>;

    fn try_from(value: Type<M>) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

impl<M: Model> From<Never> for Type<M> {
    fn from(never: Never) -> Self { match never {} }
}

impl<M: Model> ModelOf for Type<M> {
    type Model = M;
    type Rebind<U: Model> = Type<U>;

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
            Type::FunctionSignature(function_signature) => {
                Self::FunctionSignature(FunctionSignature {
                    parameters: function_signature
                        .parameters
                        .into_iter()
                        .map(Self::from_other_model)
                        .collect(),

                    return_type: Box::new(Self::from_other_model(
                        *function_signature.return_type,
                    )),
                })
            }
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
            Type::FunctionSignature(function_signature) => {
                Self::FunctionSignature(FunctionSignature {
                    parameters: function_signature
                        .parameters
                        .into_iter()
                        .map(Self::try_from_other_model)
                        .collect::<Result<_, _>>()?,

                    return_type: Box::new(Self::try_from_other_model(
                        *function_signature.return_type,
                    )?),
                })
            }
            Type::Error(Error) => Self::Error(Error),
        })
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
            .ok_or(AssignSubTermError::InvalidLocation)?,

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

            (Self::TraitMember(lhs), Self::TraitMember(rhs))
                if lhs.0.id == rhs.0.id =>
            {
                lhs.parent_generic_arguments
                    .substructural_match(
                        &rhs.parent_generic_arguments,
                        matching::Substructural::default(),
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

            (Self::FunctionSignature(lhs), Self::FunctionSignature(rhs)) => {
                if lhs.parameters.len() != rhs.parameters.len() {
                    return None;
                }

                let mut substructural = matching::Substructural::default();

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
                        .ok_or(fmt::Error)?
                        .types()
                        .get(type_parameter.id)
                        .ok_or(fmt::Error)?
                        .name
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
            Self::FunctionSignature(function_signature) => {
                write!(f, "{}", DisplayObject {
                    table,
                    display: function_signature
                })
            }
            Self::Error(_) => {
                write!(f, "{{error}}")
            }
        }
    }
}
