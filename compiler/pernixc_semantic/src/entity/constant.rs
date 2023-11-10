//! Contains the definition of constant model

use enum_as_inner::EnumAsInner;

use super::{r#type::Type, GenericArguments, Model, Never};
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

impl<T> Constant<T>
where
    T: Model<TypeInference = Never, ConstantInference = Never, RegionContext = Never>,
{
    /// Converts this constant into another model.
    #[must_use]
    pub fn into_other_model<S: Model>(self) -> Constant<S> {
        match self {
            Self::Primitive(primitive) => Constant::Primitive(primitive),
            Self::Inference(never) => match never {},
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
}
