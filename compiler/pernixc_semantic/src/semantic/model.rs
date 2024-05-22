//! Contains the definition of [`Model`].

use std::{fmt::Debug, hash::Hash};

use super::term::{
    constant::{self, Constant},
    lifetime::Lifetime,
    r#type::{self, Pointer, Reference, Type},
    GenericArguments, Local, MemberSymbol, Never, Symbol, Tuple, TupleElement,
};

/// The model that the terms will be based on.
///
/// The model is used for defining the inferences that can be made in the terms.
pub trait Model:
    Debug
    + Clone
    + Copy
    + PartialEq
    + Eq
    + PartialOrd
    + Ord
    + Hash
    + std::default::Default
    + 'static
    + Send
    + Sync
{
    /// The type to use for lifetime inference.
    type LifetimeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Into<Lifetime<Self>>;

    /// The type to use for type inference.
    type TypeInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Into<Type<Self>>;

    /// The type to use for constant inference.
    type ConstantInference: Debug
        + Clone
        + PartialEq
        + Eq
        + PartialOrd
        + Ord
        + Hash
        + 'static
        + Send
        + Sync
        + Into<Constant<Self>>;

    /// Converts a type from the default model to the current model.
    fn from_default_type(ty: Type<Default>) -> Type<Self> {
        match ty {
            Type::Primitive(primitve) => Type::Primitive(primitve),
            Type::Parameter(parameter) => Type::Parameter(parameter),
            Type::Inference(never) => match never {},
            Type::Symbol(symbol) => Type::Symbol(Symbol {
                id: symbol.id,
                generic_arguments: GenericArguments::from_default_model(
                    symbol.generic_arguments,
                ),
            }),
            Type::Pointer(pointer) => Type::Pointer(Pointer {
                pointee: Box::new(Self::from_default_type(*pointer.pointee)),
                qualifier: pointer.qualifier,
            }),
            Type::Reference(reference) => Type::Reference(Reference {
                qualifier: reference.qualifier,
                lifetime: Self::from_default_lifetime(reference.lifetime),
                pointee: Box::new(Self::from_default_type(*reference.pointee)),
            }),
            Type::Array(array) => Type::Array(r#type::Array {
                length: Self::from_default_constant(array.length),
                r#type: Box::new(Self::from_default_type(*array.r#type)),
            }),
            Type::Tuple(tuple) => Type::Tuple(Tuple {
                elements: tuple
                    .elements
                    .into_iter()
                    .map(|x| match x {
                        TupleElement::Regular(x) => {
                            TupleElement::Regular(Self::from_default_type(x))
                        }
                        TupleElement::Unpacked(x) => {
                            TupleElement::Unpacked(Self::from_default_type(x))
                        }
                    })
                    .collect(),
            }),
            Type::Local(local) => {
                Type::Local(Local(Box::new(Self::from_default_type(*local.0))))
            }
            Type::Phantom(phantom) => Type::Phantom(r#type::Phantom(Box::new(
                Self::from_default_type(*phantom.0),
            ))),
            Type::MemberSymbol(member_symbol) => {
                Type::MemberSymbol(MemberSymbol {
                    id: member_symbol.id,
                    member_generic_arguments:
                        GenericArguments::from_default_model(
                            member_symbol.member_generic_arguments,
                        ),
                    parent_generic_arguments:
                        GenericArguments::from_default_model(
                            member_symbol.parent_generic_arguments,
                        ),
                })
            }
            Type::TraitMember(trait_member) => {
                Type::TraitMember(MemberSymbol {
                    id: trait_member.id,
                    member_generic_arguments:
                        GenericArguments::from_default_model(
                            trait_member.member_generic_arguments,
                        ),
                    parent_generic_arguments:
                        GenericArguments::from_default_model(
                            trait_member.parent_generic_arguments,
                        ),
                })
            }
        }
    }

    /// Converts a lifetime from the default model to the current model.
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        match lifetime {
            Lifetime::Static => Lifetime::Static,
            Lifetime::Parameter(parameter) => Lifetime::Parameter(parameter),
            Lifetime::Inference(never) => match never {},
            Lifetime::Forall(forall) => Lifetime::Forall(forall),
        }
    }

    /// Converts a constant from the default model to the current model.
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        match constant {
            Constant::Primitive(primitve) => Constant::Primitive(primitve),
            Constant::Inference(inference) => match inference {},
            Constant::Struct(value) => Constant::Struct(constant::Struct {
                id: value.id,
                fields: value
                    .fields
                    .into_iter()
                    .map(Self::from_default_constant)
                    .collect(),
            }),
            Constant::Enum(value) => Constant::Enum(constant::Enum {
                variant_id: value.variant_id,
                associated_value: value
                    .associated_value
                    .map(|x| Box::new(Self::from_default_constant(*x))),
            }),
            Constant::Array(array) => Constant::Array(constant::Array {
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::from_default_constant)
                    .collect(),
            }),
            Constant::Parameter(parameter) => Constant::Parameter(parameter),
            Constant::Local(local) => Constant::Local(Local(Box::new(
                Self::from_default_constant(*local.0),
            ))),
            Constant::Tuple(tuple) => Constant::Tuple(Tuple {
                elements: tuple
                    .elements
                    .into_iter()
                    .map(|x| match x {
                        TupleElement::Regular(x) => TupleElement::Regular(
                            Self::from_default_constant(x),
                        ),
                        TupleElement::Unpacked(x) => TupleElement::Unpacked(
                            Self::from_default_constant(x),
                        ),
                    })
                    .collect(),
            }),
            Constant::Phantom(_) => Constant::Phantom(constant::Phantom),
        }
    }
}

/// The default model where all inferences are [`Never`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Default;

impl Model for Default {
    type LifetimeInference = Never;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<Default>) -> Type<Self> { ty }
    fn from_default_lifetime(lifetime: Lifetime<Default>) -> Lifetime<Self> {
        lifetime
    }
    fn from_default_constant(constant: Constant<Default>) -> Constant<Self> {
        constant
    }
}
