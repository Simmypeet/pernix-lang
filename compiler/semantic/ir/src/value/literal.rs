//! Contains the definition of [`Literal`].

use std::ops::Deref;

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::runtime::executor::CyclicError;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_term::{
    lifetime::Lifetime,
    tuple,
    r#type::{Array, Primitive, Qualifier, Reference, Type},
};
use pernixc_type_system::normalizer::Normalizer;

use crate::{
    Values,
    transform::{Transformer, TypeTermSource},
    value::{Environment, TypeOf},
};

/// Represents a numeric literal value.
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
    StableHash,
)]
pub struct Numeric {
    /// The numeric value for the integer part as a string.
    pub integer_string: SharedStr,

    /// The numeric value for the decimal part as a string.
    pub decimal_string: Option<SharedStr>,

    /// The type of the numeric value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type,

    /// The span location of the numeric value.
    pub span: RelativeSpan,
}

/// Represents a boolean value.
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
    StableHash,
)]
pub struct Boolean {
    /// The value of the boolean.
    pub value: bool,

    /// The span location of the boolean.
    pub span: RelativeSpan,
}

/// A placeholder for a value that was failed to bind.
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
    StableHash,
)]
pub struct Error {
    /// The expected type of the value.
    pub r#type: Type,

    /// The span location of the errornous expression.
    pub span: RelativeSpan,
}

/// Represents a unit value (empty tuple).
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
    StableHash,
)]
pub struct Unit {
    /// The span location of the unit value.
    pub span: RelativeSpan,
}

/// A placeholder for a value that can never be reached.
///
/// This is generally used for unreachable branches in the control flow.
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
    StableHash,
)]
pub struct Unreachable {
    /// The type of the unreachable value.
    pub r#type: Type,

    /// The span location of the unreachable value.
    pub span: RelativeSpan,
}

/// Represents a string literal value.
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
    StableHash,
)]
pub struct String {
    /// The value of the string.
    pub value: SharedStr,

    /// The span location of the string.
    pub span: RelativeSpan,
}

/// Represents a character literal value.
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
    StableHash,
)]
pub struct Character {
    /// The value of the character.
    pub character: char,

    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type,

    /// The span location of the character.
    pub span: RelativeSpan,
}

/// Represents a phantom value created by the `phantom` keyword.
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
    StableHash,
)]
pub struct Phantom {
    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type,

    /// The span location of the phantom keyword.
    pub span: RelativeSpan,
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
    StableHash,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Literal {
    Numeric(Numeric),
    Boolean(Boolean),
    Error(Error),
    Unit(Unit),
    String(String),
    Character(Character),
    Unreachable(Unreachable),
    Phantom(Phantom),
}

impl Literal {
    /// Returns the type of the literal value.
    #[must_use]
    pub fn r#type(&self) -> Type {
        match self {
            Self::Numeric(n) => n.r#type.clone(),
            Self::Boolean(_) => Type::Primitive(Primitive::Bool),
            Self::Error(e) => e.r#type.clone(),
            Self::Unit(_) => Type::Tuple(tuple::Tuple { elements: Vec::new() }),
            // &'static [uint8: len]
            Self::String(string) => Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Array(Array {
                    r#type: Box::new(Type::Primitive(Primitive::Uint8)),
                    length: pernixc_term::constant::Constant::Primitive(
                        pernixc_term::constant::Primitive::Usize(
                            string.value.len() as u64,
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
    #[must_use]
    pub const fn span(&self) -> &RelativeSpan {
        match self {
            Self::Numeric(n) => &n.span,
            Self::Boolean(b) => &b.span,
            Self::Error(e) => &e.span,
            Self::Unit(u) => &u.span,
            Self::String(s) => &s.span,
            Self::Character(c) => &c.span,
            Self::Unreachable(u) => &u.span,
            Self::Phantom(p) => &p.span,
        }
    }
}

impl TypeOf<&Literal> for Values {
    async fn type_of<N: Normalizer>(
        &self,
        literal: &Literal,
        environment: &Environment<'_, N>,
    ) -> Result<pernixc_type_system::Succeeded<Type>, pernixc_type_system::Error>
    {
        environment
            .type_environment
            .simplify(literal.r#type())
            .await
            .map(|x| x.deref().clone())
    }
}

impl Literal {
    /// Transforms the types in the literal using the provided transformer.
    pub async fn transform<T: Transformer<Type>>(
        &mut self,
        transformer: &mut T,
    ) -> Result<(), CyclicError> {
        match self {
            Self::Numeric(numeric) => {
                transformer
                    .transform(
                        &mut numeric.r#type,
                        TypeTermSource::Numeric,
                        numeric.span,
                    )
                    .await
            }

            Self::Error(error) => {
                transformer
                    .transform(
                        &mut error.r#type,
                        TypeTermSource::Error,
                        error.span,
                    )
                    .await
            }

            Self::Character(character) => {
                transformer
                    .transform(
                        &mut character.r#type,
                        TypeTermSource::Character,
                        character.span,
                    )
                    .await
            }

            Self::Unreachable(unreachable) => {
                transformer
                    .transform(
                        &mut unreachable.r#type,
                        TypeTermSource::Error,
                        unreachable.span,
                    )
                    .await
            }

            Self::Phantom(phantom) => {
                transformer
                    .transform(
                        &mut phantom.r#type,
                        TypeTermSource::Phantom,
                        phantom.span,
                    )
                    .await
            }

            Self::String(_) | Self::Unit(_) | Self::Boolean(_) => Ok(()),
        }
    }
}
