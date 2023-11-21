//! Contains the definition of constant model

use enum_as_inner::EnumAsInner;

use super::{r#type::Type, substitute_tuple_term, Entity, GenericArguments, Model};
use crate::{
    arena::ID,
    symbol::{self, ConstantParameterID, TraitConstant, Variant},
};

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
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitMember<S: Model> {
    /// The reference of the trait constant symbol.
    pub trait_constant_id: ID<TraitConstant>,

    /// The generic arguments supplied to the trait.
    pub trait_arguments: GenericArguments<S>,
}

/// Represents an **unpacked** constant value in the tuple, denoted by `...value` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Unpacked<S: Model> {
    Parameter(ConstantParameterID),
    TraitMember(TraitMember<S>),
}

/// Represents an element value in the tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum TupleElement<S: Model> {
    Regular(Constant<S>),
    Unpacked(Unpacked<S>),
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)` syntax.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tuple<S: Model> {
    /// The elements of the tuple.
    pub elements: Vec<TupleElement<S>>,
}

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
    Tuple(Tuple<S>),
}

impl<S: Model> Default for Constant<S> {
    fn default() -> Self {
        Self::Tuple(Tuple {
            elements: Vec::new(),
        })
    }
}

impl<S: Model> Entity<S> for Constant<S> {
    type This<A: Model> = Constant<A>;

    fn into_other_model<T: Model>(self) -> Self::This<T>
    where
        S::ConstantInference: Into<T::ConstantInference>,
        S::TypeInference: Into<T::TypeInference>,
        S::LocalRegion: Into<T::LocalRegion>,
        S::ForallRegion: Into<T::ForallRegion>,
    {
        match self {
            Self::Primitive(primitive) => Constant::Primitive(primitive),
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
                trait_constant_id: trait_member.trait_constant_id,
                trait_arguments: trait_member.trait_arguments.into_other_model(),
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
                                    trait_constant_id: trait_member.trait_constant_id,
                                    trait_arguments: trait_member
                                        .trait_arguments
                                        .into_other_model(),
                                }))
                            }
                        },
                    });
                }
                Constant::Tuple(Tuple { elements })
            }
        }
    }

    fn apply(&mut self, substitution: &super::Substitution<S>) {
        if let Some(ok) = substitution.constants.get(self).cloned() {
            *self = ok;
            return;
        };

        match self {
            Self::Struct(structs) => {
                structs.generic_arguments.apply(substitution);

                for field in &mut structs.fields {
                    field.apply(substitution);
                }
            }
            Self::Enum(enums) => {
                enums.generic_arguments.apply(substitution);

                if let Some(associated_value) = &mut enums.associated_value {
                    associated_value.apply(substitution);
                }
            }
            Self::Array(array) => {
                array.element_ty.apply(substitution);

                for element in &mut array.elements {
                    element.apply(substitution);
                }
            }
            Self::TraitMember(trait_member) => {
                trait_member.trait_arguments.apply(substitution);
            }
            Self::Tuple(tuple) => {
                substitute_tuple_term!(self, self, tuple, substitution);
            }
            _ => {}
        }
    }

    fn try_into_other_model<T: Model>(self) -> Option<Self::This<T>>
    where
        <S as Model>::ConstantInference: TryInto<T::ConstantInference>,
        <S as Model>::TypeInference: TryInto<T::TypeInference>,
        <S as Model>::LocalRegion: TryInto<T::LocalRegion>,
        <S as Model>::ForallRegion: TryInto<T::ForallRegion>,
    {
        match self {
            Self::Primitive(primitive) => Some(Constant::Primitive(primitive)),
            Self::Inference(inference) => inference.try_into().ok().map(Constant::Inference),
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
                trait_constant_id: trait_member.trait_constant_id,
                trait_arguments: trait_member.trait_arguments.try_into_other_model()?,
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
                                trait_constant_id: trait_member.trait_constant_id,
                                trait_arguments: trait_member
                                    .trait_arguments
                                    .try_into_other_model()?,
                            })))
                        }
                    })
                    .collect::<Option<_>>()?,
            })),
        }
    }
}
