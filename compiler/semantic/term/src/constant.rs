//! Contains the definition of [`Constant`] term.

use std::{fmt::Write, ops::Deref};

use derive_more::Display;
use enum_as_inner::EnumAsInner;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;

use crate::{
    Never,
    error::Error,
    generic_parameters::{ConstantParameterID, get_generic_parameters},
    inference,
    lifetime::Lifetime,
    matching::{Match, Matching, Substructural},
    sub_term::{self, Location, SubTerm, TermLocation},
    tuple::SubTupleLocation,
    r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

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
    StableHash,
    Serialize,
    Deserialize,
    EnumAsInner,
    Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("{_0}i8")]
    Int8(i8),
    #[display("{_0}i16")]
    Int16(i16),
    #[display("{_0}i32")]
    Int32(i32),
    #[display("{_0}i64")]
    Int64(i64),
    #[display("{_0}isize")]
    Isize(i64),
    #[display("{_0}u8")]
    Uint8(u8),
    #[display("{_0}u16")]
    Uint16(u16),
    #[display("{_0}u32")]
    Uint32(u32),
    #[display("{_0}u64")]
    Uint64(u64),
    #[display("{_0}usize")]
    Usize(u64),
    #[display("{_0}bool")]
    Bool(bool),
}

/// Represents a struct constant value.
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
pub struct Struct {
    /// The ID to the struct.
    pub id: Global<pernixc_symbol::ID>,

    /// The fields of the struct constant value.
    pub fields: Vec<Constant>,
}

/// Represents an enum constant value.
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
pub struct Enum {
    /// The variant that the enum constant value is.
    pub variant_id: Global<pernixc_symbol::ID>,

    /// The associated value of the enum constant value (if any).
    pub associated_value: Option<Box<Constant>>,
}

/// Represents an array constant value.
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
    /// The value of each element in the array constant value.
    pub elements: Vec<Constant>,
}

/// Represents a tuple constant value, denoted by `(value, value, ...value)`
/// syntax.
pub type Tuple = crate::tuple::Tuple<Constant>;

/// Represents a compile-time constant term.
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
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Constant {
    #[from]
    Primitive(Primitive),
    #[from]
    Inference(inference::Variable<Self>),
    #[from]
    Parameter(ConstantParameterID),
    #[from]
    Struct(Struct),
    #[from]
    Enum(Enum),
    #[from]
    Array(Array),
    #[from]
    Tuple(Tuple),
    Phantom,
    #[from]
    Error(Error),
}

impl Default for Constant {
    fn default() -> Self { Self::Tuple(Tuple { elements: Vec::new() }) }
}

impl From<Never> for Constant {
    fn from(never: Never) -> Self { match never {} }
}

impl TryFrom<Constant> for Tuple {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> {
        value.into_tuple()
    }
}

/// The location pointing to a sub-constant term in a constant.
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

impl Location<Constant, Constant> for SubConstantLocation {
    fn assign_sub_term(self, term: &mut Constant, sub_term: Constant) {
        let reference = match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => {
                return tuple.assign_sub_term(location, sub_term);
            }

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields.get_mut(location).unwrap()
            }

            (
                Self::Enum,
                Constant::Enum(Enum {
                    associated_value: Some(constant), ..
                }),
            ) => constant,

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements.get_mut(location).unwrap()
            }

            term => panic!(
                "invalid sub-constant location: {self:?} for term: {term:?}"
            ),
        };

        *reference = sub_term;
    }

    fn get_sub_term(self, term: &Constant) -> Option<Constant> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => match location {
                SubTupleLocation::Single(single) => {
                    tuple.elements.get(single).map(|x| x.term.clone())
                }
                SubTupleLocation::Range(range) => tuple
                    .elements
                    .get(range.to_std_range())
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

    fn get_sub_term_ref(self, term: &Constant) -> Option<&Constant> {
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

    fn get_sub_term_mut(self, term: &mut Constant) -> Option<&mut Constant> {
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

impl Location<Constant, Type> for Never {
    fn assign_sub_term(self, _: &mut Constant, _: Type) { match self {} }

    fn get_sub_term(self, _: &Constant) -> Option<Type> { match self {} }

    fn get_sub_term_ref(self, _: &Constant) -> Option<&Type> { match self {} }

    fn get_sub_term_mut(self, _: &mut Constant) -> Option<&mut Type> {
        match self {}
    }
}

impl Location<Constant, Lifetime> for Never {
    fn assign_sub_term(self, _: &mut Constant, _: Lifetime) { match self {} }

    fn get_sub_term(self, _: &Constant) -> Option<Lifetime> { match self {} }

    fn get_sub_term_ref(self, _: &Constant) -> Option<&Lifetime> {
        match self {}
    }

    fn get_sub_term_mut(self, _: &mut Constant) -> Option<&mut Lifetime> {
        match self {}
    }
}

impl From<Never> for TermLocation {
    fn from(never: Never) -> Self { match never {} }
}

impl SubTerm for Constant {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl Match for Constant {
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
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.id == rhs.id && lhs.fields.len() == rhs.fields.len() =>
            {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .fields
                        .iter()
                        .zip(rhs.fields.iter())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| Matching {
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
                    (Some(lhs), Some(rhs)) => Some(Substructural {
                        lifetimes: Vec::new(),
                        types: Vec::new(),
                        constants: vec![Matching {
                            lhs: lhs.deref().clone(),
                            rhs: rhs.deref().clone(),
                            lhs_location: SubConstantLocation::Enum,
                            rhs_location: SubConstantLocation::Enum,
                        }],
                    }),
                    (None, None) => Some(Substructural::default()),
                    _ => None,
                }
            }

            (Self::Array(lhs), Self::Array(rhs))
                if lhs.elements.len() == rhs.elements.len() =>
            {
                Some(Substructural {
                    lifetimes: Vec::new(),
                    types: Vec::new(),
                    constants: lhs
                        .elements
                        .iter()
                        .cloned()
                        .zip(rhs.elements.iter().cloned())
                        .enumerate()
                        .map(|(idx, (lhs, rhs))| Matching {
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
        substructural: &Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &substructural.constants
    }

    fn get_substructural_mut(
        substructural: &mut Substructural<
            Self::SubLifetimeLocation,
            Self::SubTypeLocation,
            Self::SubConstantLocation,
        >,
    ) -> &mut Vec<Matching<Self, Self::ThisSubTermLocation>> {
        &mut substructural.constants
    }
}

impl crate::display::Display for Constant {
    async fn fmt(
        &self,
        engine: &pernixc_query::TrackedEngine,
        formatter: &mut crate::display::Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Primitive(primitive) => write!(formatter, "{primitive}"),

            Self::Inference(inference) => {
                let Some(rendering) = formatter
                    .configuration()
                    .constant_inferences()
                    .and_then(|x| x.get(inference))
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

            Self::Parameter(member_id) => {
                let generic_parameters = engine
                    .get_generic_parameters(member_id.parent_id)
                    .await
                    .unwrap();

                write!(
                    formatter,
                    "{}",
                    generic_parameters.constants()[member_id.id].name
                )
            }

            Self::Struct(stu) => {
                let qualified_name = engine.get_qualified_name(stu.id).await;

                write!(formatter, "{qualified_name} {{ ")?;
                for (i, field) in stu.fields.iter().enumerate() {
                    Box::pin(field.fmt(engine, formatter)).await?;

                    if i != stu.fields.len() - 1 {
                        write!(formatter, ", ")?;
                    }
                }
                write!(formatter, " }}")
            }

            Self::Enum(en) => {
                let qualified_name =
                    engine.get_qualified_name(en.variant_id).await;

                write!(formatter, "{qualified_name}")?;

                if let Some(variant) = en.associated_value.as_ref() {
                    write!(formatter, "(")?;
                    Box::pin(variant.fmt(engine, formatter)).await?;
                    write!(formatter, ")")?;
                }

                Ok(())
            }

            Self::Array(array) => {
                write!(formatter, "[")?;

                for (i, element) in array.elements.iter().enumerate() {
                    Box::pin(element.fmt(engine, formatter)).await?;

                    if i != array.elements.len() - 1 {
                        write!(formatter, ", ")?;
                    }
                }

                write!(formatter, "]")?;

                Ok(())
            }

            Self::Tuple(tuple) => Box::pin(tuple.fmt(engine, formatter)).await,

            Self::Phantom => write!(formatter, "phantom"),

            Self::Error(_) => write!(formatter, "{{error}}"),
        }
    }
}
