//! Contains the definition of type model.

use enum_as_inner::EnumAsInner;

use super::{
    constant::Constant, lifetime::Lifetime, GenericArguments, Model, Term, TupleElement, Unpacked,
};
use crate::{
    arena::ID,
    semantic::model::Entity,
    symbol::{Enum, GlobalID, Struct, TraitType, TypeParameterID},
};

/// Either an ID of an enum or an ID of a struct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum AlgebraicKind {
    Enum(ID<Enum>),
    Struct(ID<Struct>),
}

impl From<AlgebraicKind> for GlobalID {
    fn from(value: AlgebraicKind) -> Self {
        match value {
            AlgebraicKind::Enum(id) => Self::Enum(id),
            AlgebraicKind::Struct(id) => Self::Struct(id),
        }
    }
}

/// Represents an algebraic type, which is either an enum or a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Algebraic<S: Model> {
    /// The kind of the algebraic type.
    pub kind: AlgebraicKind,

    /// The generic arguments supplied to the algebraic type.
    pub generic_arguments: GenericArguments<S>,
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
pub struct Pointer<S: Model> {
    /// The qualifier applied to the pointer.
    pub qualifier: Qualifier,

    /// The type that the pointer points to.
    pub pointee: Box<Type<S>>,
}

/// Represents a reference type, denoted by `&'LIFETIME QUALIFIER TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reference<S: Model> {
    /// The qualifier applied to the reference.
    pub qualifier: Qualifier,

    /// The lifetime that the reference lives in.
    pub lifetime: Lifetime<S>,

    /// The type that the reference points to.
    pub pointee: Box<Type<S>>,
}

/// Represents an array type, denoted by `[ELEMENT: LENGTH]` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<S: Model> {
    /// Constant representing the length of the array.
    pub length: Constant<S>,

    /// The type of the elements in the array.
    pub element: Box<Type<S>>,
}

/// Represents a trait member type, denoted by `TRAIT<GENERIC_ARGUMENTS>::MEMBER<GENERIC_ARGUMENTS>`
/// syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMember<S: Model> {
    /// The ID to the trait type.
    pub trait_type_id: ID<TraitType>,

    /// The generic arguments supplied to the trait.
    pub trait_generic_arguments: GenericArguments<S>,

    /// The generic arugments supplied to the trait type member.
    pub member_generic_arguments: GenericArguments<S>,
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
pub type Tuple<S> = super::Tuple<Type<S>, TypeParameterID, TraitMember<S>>;

/// Represents a local type, denoted by `local TYPE` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<S: Model>(pub Box<Type<S>>);

/// Represents a type in the language.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Type<S: Model>
where
    Tuple<S>: Into<Self>,
{
    Primitive(Primitive),
    Inference(S::TypeInference),
    Algebraic(Algebraic<S>),
    Pointer(Pointer<S>),
    Reference(Reference<S>),
    Array(Array<S>),
    TraitMember(TraitMember<S>),
    Parameter(TypeParameterID),
    Tuple(Tuple<S>),
    Local(Local<S>),
}

impl<M: Model> Term for Type<M> {
    type Model = M;
}

impl<S: Model> From<Tuple<S>> for Type<S> {
    fn from(value: Tuple<S>) -> Self { Self::Tuple(value) }
}

impl<S: Model> TryFrom<Type<S>> for Tuple<S> {
    type Error = Type<S>;

    fn try_from(value: Type<S>) -> Result<Self, Self::Error> { value.into_tuple() }
}

impl<S: Model> TryFrom<Type<S>> for TraitMember<S> {
    type Error = Type<S>;

    fn try_from(value: Type<S>) -> Result<Self, Self::Error> { value.into_trait_member() }
}

impl<S: Model> TryFrom<Type<S>> for TypeParameterID {
    type Error = Type<S>;

    fn try_from(value: Type<S>) -> Result<Self, Self::Error> { value.into_parameter() }
}

impl<S: Model> From<TypeParameterID> for Type<S> {
    fn from(value: TypeParameterID) -> Self { Self::Parameter(value) }
}

impl<S: Model> From<TraitMember<S>> for Type<S> {
    fn from(value: TraitMember<S>) -> Self { Self::TraitMember(value) }
}

impl<S: Model> Default for Type<S> {
    fn default() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }
}

impl<S: Model> Entity for Type<S> {
    type Model = S;
    type Rebind<A: Model> = Type<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
        S::ForallLifetime: Into<T::ForallLifetime>,
    {
        match self {
            Self::Primitive(primitive) => Type::Primitive(primitive),
            Self::Inference(inference) => Type::Inference(inference.into()),
            Self::Algebraic(algebraic) => Type::Algebraic(Algebraic {
                kind: algebraic.kind,
                generic_arguments: algebraic.generic_arguments.into_other_model(),
            }),
            Self::Pointer(pointer) => Type::Pointer(Pointer {
                qualifier: pointer.qualifier,
                pointee: Box::new(pointer.pointee.into_other_model()),
            }),
            Self::Reference(reference) => Type::Reference(Reference {
                qualifier: reference.qualifier,
                lifetime: reference.lifetime.into_other_model(),
                pointee: Box::new(reference.pointee.into_other_model()),
            }),
            Self::Array(array) => Type::Array(Array {
                length: array.length.into_other_model(),
                element: Box::new(array.element.into_other_model()),
            }),
            Self::TraitMember(trait_member) => Type::TraitMember(TraitMember {
                trait_type_id: trait_member.trait_type_id,
                trait_generic_arguments: trait_member.trait_generic_arguments.into_other_model(),
                member_generic_arguments: trait_member.member_generic_arguments.into_other_model(),
            }),
            Self::Parameter(parameter) => Type::Parameter(parameter),
            Self::Tuple(tuple) => {
                let mut elements = Vec::with_capacity(tuple.elements.len());
                for element in tuple.elements {
                    elements.push(match element {
                        TupleElement::Regular(constant) => {
                            TupleElement::Regular(constant.into_other_model())
                        }
                        TupleElement::Unpacked(unpacked) => match unpacked {
                            Unpacked::Parameter(parameter) => {
                                TupleElement::Unpacked(Unpacked::Parameter(parameter))
                            }
                            Unpacked::TraitMember(trait_member) => {
                                TupleElement::Unpacked(Unpacked::TraitMember(TraitMember {
                                    trait_type_id: trait_member.trait_type_id,
                                    trait_generic_arguments: trait_member
                                        .trait_generic_arguments
                                        .into_other_model(),
                                    member_generic_arguments: trait_member
                                        .member_generic_arguments
                                        .into_other_model(),
                                }))
                            }
                        },
                    });
                }
                Type::Tuple(Tuple { elements })
            }
            Self::Local(local) => Type::Local(Local(Box::new(local.0.into_other_model()))),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
        S::ForallLifetime: TryInto<T::ForallLifetime>,
    {
        match self {
            Self::Primitive(primitive) => Some(Type::Primitive(primitive)),
            Self::Inference(inference) => inference.try_into().ok().map(Type::Inference),
            Self::Algebraic(algebraic) => {
                algebraic
                    .generic_arguments
                    .try_into_other_model()
                    .map(|generic_arguments| {
                        Type::Algebraic(Algebraic {
                            kind: algebraic.kind,
                            generic_arguments,
                        })
                    })
            }
            Self::Pointer(pointer) => pointer.pointee.try_into_other_model().map(|pointee| {
                Type::Pointer(Pointer {
                    qualifier: pointer.qualifier,
                    pointee: Box::new(pointee),
                })
            }),
            Self::Reference(reference) => {
                let lifetime = reference.lifetime.try_into_other_model()?;
                let pointee = reference.pointee.try_into_other_model()?;
                Some(Type::Reference(Reference {
                    qualifier: reference.qualifier,
                    lifetime,
                    pointee: Box::new(pointee),
                }))
            }
            Self::Array(array) => {
                let length = array.length.try_into_other_model()?;
                let element = array.element.try_into_other_model()?;
                Some(Type::Array(Array {
                    length,
                    element: Box::new(element),
                }))
            }
            Self::TraitMember(trait_member) => {
                let trait_generic_arguments = trait_member
                    .trait_generic_arguments
                    .try_into_other_model()?;
                let member_generic_arguments = trait_member
                    .member_generic_arguments
                    .try_into_other_model()?;

                Some(Type::TraitMember(TraitMember {
                    trait_type_id: trait_member.trait_type_id,
                    trait_generic_arguments,
                    member_generic_arguments,
                }))
            }
            Self::Parameter(parameter) => Some(Type::Parameter(parameter)),
            Self::Tuple(tuple) => Some(Type::Tuple(Tuple {
                elements: tuple
                    .elements
                    .into_iter()
                    .map(|x| match x {
                        TupleElement::Regular(regular) => {
                            regular.try_into_other_model().map(TupleElement::Regular)
                        }
                        TupleElement::Unpacked(Unpacked::Parameter(parameter)) => {
                            Some(TupleElement::Unpacked(Unpacked::Parameter(parameter)))
                        }
                        TupleElement::Unpacked(Unpacked::TraitMember(trait_member)) => {
                            Some(TupleElement::Unpacked(Unpacked::TraitMember(TraitMember {
                                trait_type_id: trait_member.trait_type_id,
                                trait_generic_arguments: trait_member
                                    .trait_generic_arguments
                                    .try_into_other_model()?,
                                member_generic_arguments: trait_member
                                    .member_generic_arguments
                                    .try_into_other_model()?,
                            })))
                        }
                    })
                    .collect::<Option<_>>()?,
            })),
            Self::Local(local) => local
                .0
                .try_into_other_model()
                .map(|r#type| Type::Local(Local(Box::new(r#type)))),
        }
    }
}
