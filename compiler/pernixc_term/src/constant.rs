//! Contains the definition of [`Constant`]
use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use pernixc_table::GlobalID;
use serde::{Deserialize, Serialize};

use crate::{
    generic_parameter::ConstantParameterID,
    lifetime::Lifetime,
    matching::{self, Match},
    r#type::Type,
    sub_term::{
        self, AssignSubTermError, Location, SubTerm, SubTupleLocation,
        TermLocation,
    },
    Error, Model, ModelOf, Never,
};

mod arbitrary;

/// Represents a primitive constant.
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
pub enum Primitive {
    #[display(fmt = "{_0}")]
    Integer(i128),
    #[display(fmt = "{_0}")]
    Bool(bool),
}

/// Represents a struct constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Struct<M: Model> {
    /// The ID to the struct.
    pub id: GlobalID,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant<M>>,
}

/// Represents an enum constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Enum<M: Model> {
    /// The variant that the enum constant value is.
    pub variant_id: GlobalID,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant<M>>>,
}

/// Represents an array constant value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Array<M: Model> {
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant<M>>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple<M> = super::Tuple<Constant<M>>;

/// Represents a compile-time constant term.
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
#[non_exhaustive]
pub enum Constant<M: Model> {
    #[from]
    Primitive(Primitive),
    Inference(M::ConstantInference),
    #[from]
    Struct(Struct<M>),
    #[from]
    Enum(Enum<M>),
    #[from]
    Array(Array<M>),
    #[from]
    Parameter(ConstantParameterID),
    #[from]
    Tuple(Tuple<M>),

    Phantom,
    #[from]
    Error(Error),
}

impl<M: Model> From<Never> for Constant<M> {
    fn from(value: Never) -> Self { match value {} }
}

impl<M: Model> ModelOf for Constant<M> {
    type Model = M;
    type Rebind<U: Model> = Constant<U>;

    fn from_other_model<U: Model>(term: Self::Rebind<U>) -> Self
    where
        M::LifetimeInference: From<U::LifetimeInference>,
        M::TypeInference: From<U::TypeInference>,
        M::ConstantInference: From<U::ConstantInference>,
    {
        match term {
            Constant::Primitive(primitive) => Self::Primitive(primitive),
            Constant::Inference(inference) => {
                Self::Inference(M::ConstantInference::from(inference))
            }
            Constant::Struct(value) => Self::Struct(Struct {
                id: value.id,
                fields: value
                    .fields
                    .into_iter()
                    .map(Self::from_other_model)
                    .collect(),
            }),
            Constant::Enum(value) => Self::Enum(Enum {
                variant_id: value.variant_id,
                associated_value: value
                    .associated_value
                    .map(|x| Box::new(Self::from_other_model(*x))),
            }),
            Constant::Array(array) => Self::Array(Array {
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::from_other_model)
                    .collect(),
            }),
            Constant::Parameter(parameter) => Self::Parameter(parameter),
            Constant::Tuple(tuple) => {
                Self::Tuple(Tuple::from_other_model(tuple))
            }
            Constant::Phantom => Self::Phantom,
            Constant::Error(Error) => Self::Error(Error),
        }
    }

    fn try_from_other_model<U: Model, E>(
        term: Self::Rebind<U>,
    ) -> Result<Self, E>
    where
        M::LifetimeInference: TryFrom<U::LifetimeInference, Error = E>,
        M::TypeInference: TryFrom<U::TypeInference, Error = E>,
        M::ConstantInference: TryFrom<U::ConstantInference, Error = E>,

        Self: Sized,
    {
        Ok(match term {
            Constant::Primitive(primitive) => Self::Primitive(primitive),
            Constant::Inference(inference) => {
                Self::Inference(M::ConstantInference::try_from(inference)?)
            }
            Constant::Struct(value) => Self::Struct(Struct {
                id: value.id,
                fields: value
                    .fields
                    .into_iter()
                    .map(Self::try_from_other_model)
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Constant::Enum(value) => match value.associated_value {
                Some(associated_value) => {
                    let associated_value =
                        Self::try_from_other_model(*associated_value)?;

                    Self::Enum(Enum {
                        variant_id: value.variant_id,
                        associated_value: Some(Box::new(associated_value)),
                    })
                }
                None => Self::Enum(Enum {
                    variant_id: value.variant_id,
                    associated_value: None,
                }),
            },
            Constant::Array(array) => Self::Array(Array {
                elements: array
                    .elements
                    .into_iter()
                    .map(Self::try_from_other_model)
                    .collect::<Result<Vec<_>, _>>()?,
            }),
            Constant::Parameter(parameter) => Self::Parameter(parameter),
            Constant::Tuple(tuple) => {
                Self::Tuple(Tuple::try_from_other_model(tuple)?)
            }
            Constant::Phantom => Self::Phantom,
            Constant::Error(Error) => Self::Error(Error),
        })
    }
}

impl<M: Model> TryFrom<Constant<M>> for Tuple<M> {
    type Error = Constant<M>;

    fn try_from(value: Constant<M>) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

/// The location pointing to a sub-constant term in a constant.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From,
)]
pub enum SubConstantLocation {
    /// The index of the element in a [`Tuple`] constant.
    #[from]
    Tuple(SubTupleLocation),

    /// The index of the field in a [`Struct`] constant.
    Struct(usize),

    /// The value in a [`Enum::associated_value`] field (if any).
    Enum,

    /// The index of the element in an [`Array`] constant.
    Array(usize),
}

impl From<SubConstantLocation> for TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromConstant(value))
    }
}

impl<M: Model> Location<Constant<M>, Constant<M>> for SubConstantLocation {
    fn assign_sub_term(
        self,
        term: &mut Constant<M>,
        sub_term: Constant<M>,
    ) -> Result<(), AssignSubTermError> {
        let reference = match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term)
            }

            (Self::Struct(location), Constant::Struct(constant)) => constant
                .fields
                .get_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => constant,

            (Self::Array(location), Constant::Array(constant)) => constant
                .elements
                .get_mut(location)
                .ok_or(AssignSubTermError::InvalidLocation)?,

            _ => return Err(AssignSubTermError::InvalidLocation),
        };

        *reference = sub_term;

        Ok(())
    }

    fn get_sub_term(self, term: &Constant<M>) -> Option<Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.term.clone())
                }
                SubTupleLocation::Range { begin, end } => tuple
                    .elements
                    .get(begin..end)
                    .map(|x| Constant::Tuple(Tuple { elements: x.to_vec() })),
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get(location).cloned()
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(constant.deref().clone()),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get(location).cloned()
            }

            _ => None,
        }
    }

    fn get_sub_term_ref(self, term: &Constant<M>) -> Option<&Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| &x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get(location)
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(&**constant),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get(location)
            }

            _ => None,
        }
    }

    fn get_sub_term_mut(
        self,
        term: &mut Constant<M>,
    ) -> Option<&mut Constant<M>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get_mut(single).map(|x| &mut x.term)
                }
                SubTupleLocation::Range { .. } => None,
            },

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get_mut(location)
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => Some(&mut *constant),

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get_mut(location)
            }

            _ => None,
        }
    }
}

impl<M: Model> Location<Constant<M>, Type<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant<M>,
        _: Type<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant<M>) -> Option<Type<M>> { match self {} }

    fn get_sub_term_ref(self, _: &Constant<M>) -> Option<&Type<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant<M>) -> Option<&mut Type<M>> {
        match self {}
    }
}

impl<M: Model> Location<Constant<M>, Lifetime<M>> for Never {
    fn assign_sub_term(
        self,
        _: &mut Constant<M>,
        _: Lifetime<M>,
    ) -> Result<(), AssignSubTermError> {
        match self {}
    }

    fn get_sub_term(self, _: &Constant<M>) -> Option<Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_ref(self, _: &Constant<M>) -> Option<&Lifetime<M>> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant<M>) -> Option<&mut Lifetime<M>> {
        match self {}
    }
}

impl<M: Model> SubTerm for Constant<M> {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl<M: Model> Match for Constant<M> {
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
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.id == rhs.id && lhs.fields.len() == rhs.fields.len() =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .fields
                        .iter()
                        .zip(rhs.fields.iter())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| matching::Matching {
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                            lhs_location: SubConstantLocation::Struct(idx),
                            rhs_location: SubConstantLocation::Struct(idx),
                        })
                        .collect(),
                })
            }

            (Self::Enum(lhs), Self::Enum(rhs))
                if lhs.variant_id == rhs.variant_id =>
            {
                match (&lhs.associated_value, &rhs.associated_value) {
                    (Some(lhs), Some(rhs)) => Some(matching::Substructural {
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        constants: vec![matching::Matching {
                            lhs: lhs.deref().clone(),
                            rhs: rhs.deref().clone(),
                            lhs_location: SubConstantLocation::Enum,
                            rhs_location: SubConstantLocation::Enum,
                        }],
                    }),
                    (None, None) => Some(matching::Substructural::default()),
                    _ => None,
                }
            }

            (Self::Array(lhs), Self::Array(rhs))
                if lhs.elements.len() == rhs.elements.len() =>
            {
                Some(matching::Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .elements
                        .iter()
                        .cloned()
                        .zip(rhs.elements.iter().cloned())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| matching::Matching {
                            lhs,
                            rhs,
                            lhs_location: SubConstantLocation::Array(idx),
                            rhs_location: SubConstantLocation::Array(idx),
                        })
                        .collect(),
                })
            }

            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.substructural_match(rhs)
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
    ) -> &Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.constants
    }

    fn get_substructural_mut(
        substructural: &mut matching::Substructural<
            M,
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<matching::Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.constants
    }
}
