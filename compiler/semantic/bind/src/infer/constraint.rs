//! Contains the definition of [`Type`].

use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use qbice::{Decode, Encode, StableHash};

/// Implements by a constraint type. Representing a restrict domain of what
/// terms can be inferred.
pub trait Constraint: Debug + Clone + Eq + Display {
    /// The type of terms that can be inferred.
    type Term: Debug + Clone + Eq + Hash;

    /// Checks if the given term satisfies the constraint.
    fn satisfies(&self, term: &Self::Term) -> bool;

    /// Tries to combine this constraint with another constraint.
    fn combine(&self, another: &Self) -> Option<Self>
    where
        Self: Sized;
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
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
pub enum Type {
    /// The type can be inferred into any type.
    ///
    /// The boolean valuie indicates whether the type can be inferred as a unit
    /// type as a default value.
    #[display("{{any}}")]
    All(bool),

    /// The type can be any number type. (signed/unsigned/floating)
    #[display("{{number}}")]
    Number,

    /// The type can be integer number type. (signed/unsigned)
    #[display("{{integer}}")]
    Integer,

    /// The type can be signed number type. (signed integer)
    #[display("{{signedInteger}}")]
    SignedInteger,

    /// The type can be signed number type. (signed integer/floating)
    #[display("{{signed}}")]
    Signed,

    /// The type can be unsigned number type. (unsigned integer)
    #[display("{{unsignedInteger}}")]
    UnsignedInteger,

    /// The type can be only floating number type. (float32/float64)
    #[display("{{floating}}")]
    Floating,
}

impl Constraint for Type {
    type Term = pernixc_term::r#type::Type;

    fn satisfies(&self, term: &pernixc_term::r#type::Type) -> bool {
        use pernixc_term::r#type::Primitive;

        match self {
            Self::All(_) => true,
            Self::Number => matches!(
                term,
                pernixc_term::r#type::Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Float32
                        | Primitive::Float64
                        | Primitive::Usize
                        | Primitive::Isize
                )
            ),
            Self::Signed => matches!(
                term,
                pernixc_term::r#type::Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Float32
                        | Primitive::Float64
                        | Primitive::Isize
                )
            ),
            Self::UnsignedInteger => matches!(
                term,
                pernixc_term::r#type::Type::Primitive(
                    Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Usize
                )
            ),
            Self::Floating => {
                matches!(
                    term,
                    pernixc_term::r#type::Type::Primitive(
                        Primitive::Float32 | Primitive::Float64
                    )
                )
            }
            Self::Integer => matches!(
                term,
                pernixc_term::r#type::Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Uint8
                        | Primitive::Uint16
                        | Primitive::Uint32
                        | Primitive::Uint64
                        | Primitive::Usize
                        | Primitive::Isize
                )
            ),
            Self::SignedInteger => matches!(
                term,
                pernixc_term::r#type::Type::Primitive(
                    Primitive::Int8
                        | Primitive::Int16
                        | Primitive::Int32
                        | Primitive::Int64
                        | Primitive::Isize
                )
            ),
        }
    }

    fn combine(&self, another: &Self) -> Option<Self> {
        match self {
            Self::All(default_as_unit) => Some(match *another {
                Self::All(other_as_unit) => {
                    Self::All(*default_as_unit || other_as_unit)
                }

                another => another,
            }),

            Self::Number => Some(match *another {
                Self::All(_) => Self::Number,
                another => another,
            }),

            Self::Integer => match *another {
                Self::All(_) | Self::Number => Some(Self::Integer),

                Self::Signed => Some(Self::SignedInteger),

                another @ (Self::Integer
                | Self::SignedInteger
                | Self::UnsignedInteger) => Some(another),

                Self::Floating => None,
            },

            Self::UnsignedInteger => match *another {
                Self::All(_)
                | Self::Number
                | Self::Integer
                | Self::UnsignedInteger => Some(Self::UnsignedInteger),

                Self::SignedInteger | Self::Signed | Self::Floating => None,
            },

            Self::SignedInteger => match *another {
                Self::All(_)
                | Self::Number
                | Self::Integer
                | Self::SignedInteger
                | Self::Signed => Some(Self::SignedInteger),

                Self::UnsignedInteger | Self::Floating => None,
            },

            Self::Signed => match *another {
                Self::Signed | Self::All(_) | Self::Number => {
                    Some(Self::Signed)
                }

                Self::SignedInteger | Self::Integer => {
                    Some(Self::SignedInteger)
                }

                Self::Floating => Some(Self::Floating),

                Self::UnsignedInteger => None,
            },

            Self::Floating => match *another {
                Self::All(_) | Self::Number | Self::Signed | Self::Floating => {
                    Some(Self::Floating)
                }

                Self::UnsignedInteger | Self::Integer | Self::SignedInteger => {
                    None
                }
            },
        }
    }
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
    StableHash,
    Encode,
    Decode,
    derive_more::Display,
)]
#[display("_")]
pub struct Constant;

impl Constraint for Constant {
    type Term = pernixc_term::constant::Constant;

    fn satisfies(&self, _: &pernixc_term::constant::Constant) -> bool { true }

    fn combine(&self, _: &Self) -> Option<Self>
    where
        Self: Sized,
    {
        Some(*self)
    }
}
