//! Contains the definition of constant model

use std::iter::Empty;

use enum_as_inner::EnumAsInner;

use super::{
    lifetime::Lifetime, r#type::Type, GenericArguments, MemberSymbol, Model, Symbol, Term,
    TupleElement, Unpacked,
};
use crate::{
    arena::ID,
    semantic::{model::Entity, predicate::Premises, unification::Substructural},
    symbol::{self, ConstantParameterID, GlobalID, Variant},
};

/// Enumeration of either a trait implementation constant or an ADT implementation constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum ImplementationKindID {
    Trait(ID<symbol::TraitImplementationConstant>),
    Adt(ID<symbol::AdtImplementationConstant>),
}

impl From<ImplementationKindID> for GlobalID {
    fn from(value: ImplementationKindID) -> Self {
        match value {
            ImplementationKindID::Trait(id) => id.into(),
            ImplementationKindID::Adt(id) => id.into(),
        }
    }
}

/// Represents a primitive constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Primitive {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint16(u16),
    Uint32(u32),
    Uint64(u64),
    Bool(bool),
    Usize(usize),
    Isize(isize),
}

/// Represents a struct constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct<S: Model> {
    /// The ID to the struct.
    pub struct_id: ID<symbol::Struct>,

    /// The generic arguments supplied to the struct type.
    pub generic_arguments: GenericArguments<S>,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant<S>>,
}

/// Represents an enum constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<S: Model> {
    /// The variant that the enum constant value is.
    pub variant_id: ID<Variant>,

    /// The generic arguments supplied to the enum type.
    pub generic_arguments: GenericArguments<S>,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant<S>>>,
}

/// Represents an array constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array<S: Model> {
    /// The type of the array element.
    pub element_ty: Box<Type<S>>,

    /// The value of each element in the array constant value.
    pub elements: Vec<Constant<S>>,
}

/// Represents a trait associated constant, denoted by `trait<args>::constant<args>` syntax.
pub type TraitMember<S> = MemberSymbol<ID<symbol::TraitConstant>, S>;

/// Represents a tuple constant value, denoted by `(value, value, ...value)` syntax.
pub type Tuple<S> = super::Tuple<Constant<S>, ConstantParameterID, TraitMember<S>>;

/// Represents a local constant value, denoted by `local CONSTANT` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local<S: Model>(pub Box<Constant<S>>);

/// Represents a compile-time evaluated constant.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Constant<S: Model> {
    Primitive(Primitive),
    Inference(S::ConstantInference),
    Struct(Struct<S>),
    Enum(Enum<S>),
    Array(Array<S>),
    Parameter(ConstantParameterID),
    TraitMember(TraitMember<S>),
    Local(Local<S>),
    Tuple(Tuple<S>),
    Symbol(Symbol<ID<symbol::Constant>, S>),

    /// Pleace notice this differences
    ///
    /// In the **AdtImplementation** case, the `parent_generic_arguments` field is **not** deduced
    /// from the implementation directly, bur rather from the ADT that the implementation is for.
    ///
    /// In the **TraitImplementation** case, the `parent_generic_arguments` field **is** deduced
    /// from the implementation.
    Implementation(MemberSymbol<ImplementationKindID, S>),
}

impl<M: Model> Term for Constant<M> {
    type ConstantTypePremisesIter<'a> = std::iter::Empty<&'a Self>
    where
        Self: 'a;
    type Model = M;
    type OutlivesPremisesIter<'a> = Empty<(&'a Self, &'a Lifetime<M>)>
    where
        Self: 'a;

    fn outlives_premises(_: &Premises<<Self as Term>::Model>) -> Self::OutlivesPremisesIter<'_> {
        std::iter::empty()
    }

    fn is_adt_type_term(&self) -> bool { false }

    fn constant_type_premises(
        _: &Premises<<Self as Term>::Model>,
    ) -> Self::ConstantTypePremisesIter<'_> {
        std::iter::empty()
    }

    fn get_substructural(
        substructural: &mut Substructural<<Self as Term>::Model>,
    ) -> &mut Vec<(Self, Self)> {
        &mut substructural.constants
    }
}

impl<S: Model> From<Tuple<S>> for Constant<S> {
    fn from(value: Tuple<S>) -> Self { Self::Tuple(value) }
}

impl<S: Model> TryFrom<Constant<S>> for Tuple<S> {
    type Error = Constant<S>;

    fn try_from(value: Constant<S>) -> Result<Self, Self::Error> { value.into_tuple() }
}

impl<S: Model> TryFrom<Constant<S>> for TraitMember<S> {
    type Error = Constant<S>;

    fn try_from(value: Constant<S>) -> Result<Self, Self::Error> { value.into_trait_member() }
}

impl<S: Model> TryFrom<Constant<S>> for ConstantParameterID {
    type Error = Constant<S>;

    fn try_from(value: Constant<S>) -> Result<Self, Self::Error> { value.into_parameter() }
}

impl<S: Model> Default for Constant<S> {
    fn default() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }
}

impl<S: Model> From<ConstantParameterID> for Constant<S> {
    fn from(value: ConstantParameterID) -> Self { Self::Parameter(value) }
}

impl<S: Model> From<TraitMember<S>> for Constant<S> {
    fn from(value: TraitMember<S>) -> Self { Self::TraitMember(value) }
}

impl<S: Model> Entity for Constant<S> {
    type Model = S;
    type Rebind<A: Model> = Constant<A>;

    fn into_other_model<T: Model>(self) -> Self::Rebind<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LifetimeInference: Into<T::LifetimeInference>,
        S::ScopedLifetime: Into<T::ScopedLifetime>,
    {
        match self {
            Self::Primitive(primitive) => Constant::Primitive(primitive),
            Self::Local(local) => Constant::Local(Local(Box::new(local.0.into_other_model()))),
            Self::Inference(inference) => Constant::Inference(inference.into()),
            Self::Struct(struct_constant) => Constant::Struct(Struct {
                struct_id: struct_constant.struct_id,
                generic_arguments: struct_constant.generic_arguments.into_other_model(),
                fields: struct_constant
                    .fields
                    .into_iter()
                    .map(Self::into_other_model)
                    .collect(),
            }),
            Self::Enum(enum_constant) => Constant::Enum(Enum {
                variant_id: enum_constant.variant_id,
                generic_arguments: enum_constant.generic_arguments.into_other_model(),
                associated_value: enum_constant
                    .associated_value
                    .map(|v| Box::new(v.into_other_model())),
            }),
            Self::Array(array) => Constant::Array(Array {
                element_ty: Box::new(array.element_ty.into_other_model()),
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::into_other_model)
                    .collect(),
            }),
            Self::Parameter(parameter) => Constant::Parameter(parameter),
            Self::TraitMember(trait_member) => Constant::TraitMember(TraitMember {
                id: trait_member.id,
                parent_generic_arguments: trait_member.parent_generic_arguments.into_other_model(),
                member_generic_arguments: trait_member.member_generic_arguments.into_other_model(),
            }),
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
                                    id: trait_member.id,
                                    parent_generic_arguments: trait_member
                                        .parent_generic_arguments
                                        .into_other_model(),
                                    member_generic_arguments: trait_member
                                        .member_generic_arguments
                                        .into_other_model(),
                                }))
                            }
                        },
                    });
                }
                Constant::Tuple(Tuple { elements })
            }
            Self::Symbol(symbol) => Constant::Symbol(Symbol {
                id: symbol.id,
                generic_arguments: symbol.generic_arguments.into_other_model(),
            }),
            Self::Implementation(symbol) => Constant::Implementation(MemberSymbol {
                id: symbol.id,
                parent_generic_arguments: symbol.parent_generic_arguments.into_other_model(),
                member_generic_arguments: symbol.member_generic_arguments.into_other_model(),
            }),
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::Rebind<T>>
    where
        S::ConstantInference: TryInto<T::ConstantInference>,
        S::TypeInference: TryInto<T::TypeInference>,
        S::LifetimeInference: TryInto<T::LifetimeInference>,
        S::ScopedLifetime: TryInto<T::ScopedLifetime>,
    {
        match self {
            Self::Primitive(primitive) => Some(Constant::Primitive(primitive)),
            Self::Inference(inference) => inference.try_into().ok().map(Constant::Inference),
            Self::Local(loca) => Some(Constant::Local(Local(Box::new(
                loca.0.try_into_other_model()?,
            )))),
            Self::Struct(struct_constant) => Some(Constant::Struct(Struct {
                struct_id: struct_constant.struct_id,
                generic_arguments: struct_constant.generic_arguments.try_into_other_model()?,
                fields: struct_constant
                    .fields
                    .into_iter()
                    .map(Self::try_into_other_model)
                    .collect::<Option<_>>()?,
            })),
            Self::Enum(enum_constant) => Some(Constant::Enum(Enum {
                variant_id: enum_constant.variant_id,
                generic_arguments: enum_constant.generic_arguments.try_into_other_model()?,
                associated_value: if let Some(variant) = enum_constant.associated_value {
                    Some(Box::new(variant.try_into_other_model()?))
                } else {
                    None
                },
            })),
            Self::Array(array) => Some(Constant::Array(Array {
                element_ty: Box::new(array.element_ty.try_into_other_model()?),
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::try_into_other_model)
                    .collect::<Option<_>>()?,
            })),
            Self::Parameter(parameter) => Some(Constant::Parameter(parameter)),
            Self::TraitMember(trait_member) => Some(Constant::TraitMember(TraitMember {
                id: trait_member.id,
                parent_generic_arguments: trait_member
                    .parent_generic_arguments
                    .try_into_other_model()?,
                member_generic_arguments: trait_member
                    .member_generic_arguments
                    .try_into_other_model()?,
            })),
            Self::Tuple(tuple) => Some(Constant::Tuple(Tuple {
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
                                id: trait_member.id,
                                parent_generic_arguments: trait_member
                                    .parent_generic_arguments
                                    .try_into_other_model()?,
                                member_generic_arguments: trait_member
                                    .member_generic_arguments
                                    .try_into_other_model()?,
                            })))
                        }
                    })
                    .collect::<Option<_>>()?,
            })),
            Self::Symbol(symbol) => Some(Constant::Symbol(Symbol {
                id: symbol.id,
                generic_arguments: symbol.generic_arguments.try_into_other_model()?,
            })),
            Self::Implementation(symbol) => Some(Constant::Implementation(MemberSymbol {
                id: symbol.id,
                parent_generic_arguments: symbol.parent_generic_arguments.try_into_other_model()?,
                member_generic_arguments: symbol.member_generic_arguments.try_into_other_model()?,
            })),
        }
    }
}

#[cfg(test)]
mod tests;
