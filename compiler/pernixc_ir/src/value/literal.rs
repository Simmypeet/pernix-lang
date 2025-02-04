//! Contains the definition of [`Literal`].

use enum_as_inner::EnumAsInner;
use pernixc_source_file::Span;
use pernixc_term::{
    lifetime::Lifetime,
    r#type::{Array, Primitive, Qualifier, Reference, Type},
};
use serde::{Deserialize, Serialize};

use crate::model::Transform;

/// Represents a numeric literal value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Numeric<M: pernixc_term::Model> {
    /// The numeric value for the integer part as a string.
    pub integer_string: std::string::String,

    /// The numeric value for the decimal part as a string.
    pub decimal_stirng: Option<std::string::String>,

    /// The type of the numeric value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type<M>,

    /// The span location of the numeric value.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a boolean value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Boolean {
    /// The value of the boolean.
    pub value: bool,

    /// The span location of the boolean.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// A placeholder for a value that was failed to bind.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Error<M: pernixc_term::Model> {
    /// The expected type of the value.
    pub r#type: Type<M>,

    /// The span location of the errornous expression.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a unit value (empty tuple).
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Unit {
    /// The span location of the unit value.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// A placeholder for a value that can never be reached.
///
/// This is generally used for unreachable branches in the control flow.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Unreachable<M: pernixc_term::Model> {
    /// The type of the unreachable value.
    pub r#type: Type<M>,

    /// The span location of the unreachable value.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a string literal value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct String {
    /// The value of the string.
    pub value: Vec<u8>,

    /// The span location of the string.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a character literal value.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Character<M: pernixc_term::Model> {
    /// The value of the character.
    pub character: char,

    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type<M>,

    /// The span location of the character.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a phantom value created by the `phantom` keyword.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Phantom<M: pernixc_term::Model> {
    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type<M>,

    /// The span location of the phantom keyword.
    #[serde(skip)]
    pub span: Option<Span>,
}

/// Represents a literal value.
///
/// A literal value is a value that is directly represented in the source code
/// and does not require any computation to be evaluated.
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
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Literal<M: pernixc_term::Model> {
    Numeric(Numeric<M>),
    Boolean(Boolean),
    Error(Error<M>),
    Unit(Unit),
    String(String),
    Character(Character<M>),
    Unreachable(Unreachable<M>),
    Phantom(Phantom<M>),
}

impl<M: pernixc_term::Model> Literal<M> {
    /// Transforms the literal to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Literal<T::Target>, T::Error> {
        Ok(match self {
            Self::Numeric(numeric) => Literal::Numeric(Numeric {
                integer_string: numeric.integer_string,
                decimal_stirng: numeric.decimal_stirng,
                r#type: transformer
                    .transform(numeric.r#type, numeric.span.as_ref())?,
                span: numeric.span,
            }),
            Self::Boolean(boolean) => Literal::Boolean(boolean),
            Self::Error(error) => Literal::Error(Error {
                r#type: transformer
                    .transform(error.r#type, error.span.as_ref())?,
                span: error.span,
            }),
            Self::Unit(unit) => Literal::Unit(unit),
            Self::String(s) => Literal::String(s),
            Self::Character(character) => Literal::Character(Character {
                character: character.character,
                r#type: transformer
                    .transform(character.r#type, character.span.as_ref())?,
                span: character.span,
            }),
            Self::Unreachable(unreachable) => {
                Literal::Unreachable(Unreachable {
                    r#type: transformer.transform(
                        unreachable.r#type,
                        unreachable.span.as_ref(),
                    )?,
                    span: unreachable.span,
                })
            }
            Self::Phantom(phantom) => Literal::Phantom(Phantom {
                r#type: transformer
                    .transform(phantom.r#type, phantom.span.as_ref())?,
                span: phantom.span,
            }),
        })
    }

    /// Returns the type of the literal value.
    pub fn r#type(&self) -> Type<M> {
        match self {
            Self::Numeric(n) => n.r#type.clone(),
            Self::Boolean(_) => Type::Primitive(Primitive::Bool),
            Self::Error(e) => e.r#type.clone(),
            Self::Unit(_) => {
                Type::Tuple(pernixc_term::Tuple { elements: Vec::new() })
            }
            // &'static [uint8: len]
            Self::String(string) => Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Array(Array {
                    r#type: Box::new(Type::Primitive(Primitive::Uint8)),
                    length: pernixc_term::constant::Constant::Primitive(
                        pernixc_term::constant::Primitive::Usize(
                            string.value.len() as u128,
                        ),
                    ),
                })),
            }),
            Self::Character(c) => c.r#type.clone(),
            Self::Unreachable(u) => u.r#type.clone(),
            Self::Phantom(p) => p.r#type.clone(),
        }
    }

    /// Returns the span location of the literal value.
    pub const fn span(&self) -> Option<&Span> {
        match self {
            Self::Numeric(n) => n.span.as_ref(),
            Self::Boolean(b) => b.span.as_ref(),
            Self::Error(e) => e.span.as_ref(),
            Self::Unit(u) => u.span.as_ref(),
            Self::String(s) => s.span.as_ref(),
            Self::Character(c) => c.span.as_ref(),
            Self::Unreachable(u) => u.span.as_ref(),
            Self::Phantom(p) => p.span.as_ref(),
        }
    }
}
