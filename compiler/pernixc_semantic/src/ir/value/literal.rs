//! Contains the definition of [`Literal`].

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::{
    ir::Transform,
    type_system::{
        model::Model,
        term::{
            self, constant,
            lifetime::Lifetime,
            r#type::{Array, Primitive, Qualifier, Reference, Type},
        },
    },
};

/// Represents a numeric literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric<M: Model> {
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
    pub span: Span,
}

/// Represents a boolean value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean.
    pub value: bool,

    /// The span location of the boolean.
    pub span: Span,
}

/// A placeholder for a value that was failed to bind.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error<M: Model> {
    /// The expected type of the value.
    pub r#type: Type<M>,

    /// The span location of the errornous expression.
    pub span: Span,
}

/// Represents a unit value (empty tuple).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unit {
    /// The span location of the unit value.
    pub span: Span,
}

/// A placeholder for a value that can never be reached.
///
/// This is generally used for unreachable branches in the control flow.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unreachable<M: Model> {
    /// The type of the unreachable value.
    pub r#type: Type<M>,

    /// The span location of the unreachable value.
    pub span: Span,
}

/// Represents a string literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String {
    /// The value of the string.
    pub value: Vec<u8>,

    /// The span location of the string.
    pub span: Span,
}

/// Represents a character literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Character<M: Model> {
    /// The value of the character.
    pub character: char,

    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type<M>,

    /// The span location of the character.
    pub span: Span,
}

/// Represents a phantom value created by the `phantom` keyword.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Phantom<M: Model> {
    /// The type of the character value.
    ///
    /// The type is explicitly annotate here since it can be determined by
    /// type inference.
    pub r#type: Type<M>,

    /// The span location of the phantom keyword.
    pub span: Span,
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
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Literal<M: Model> {
    Numeric(Numeric<M>),
    Boolean(Boolean),
    Error(Error<M>),
    Unit(Unit),
    String(String),
    Character(Character<M>),
    Unreachable(Unreachable<M>),
    Phantom(Phantom<M>),
}

impl<M: Model> Literal<M> {
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
                    .transform(numeric.r#type, numeric.span.clone())?,
                span: numeric.span,
            }),
            Self::Boolean(boolean) => Literal::Boolean(boolean),
            Self::Error(error) => Literal::Error(Error {
                r#type: transformer
                    .transform(error.r#type, error.span.clone())?,
                span: error.span,
            }),
            Self::Unit(unit) => Literal::Unit(unit),
            Self::String(s) => Literal::String(s),
            Self::Character(character) => Literal::Character(Character {
                character: character.character,
                r#type: transformer
                    .transform(character.r#type, character.span.clone())?,
                span: character.span,
            }),
            Self::Unreachable(unreachable) => {
                Literal::Unreachable(Unreachable {
                    r#type: transformer.transform(
                        unreachable.r#type,
                        unreachable.span.clone(),
                    )?,
                    span: unreachable.span,
                })
            }
            Self::Phantom(phantom) => Literal::Phantom(Phantom {
                r#type: transformer
                    .transform(phantom.r#type, phantom.span.clone())?,
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
            Self::Unit(_) => Type::Tuple(term::Tuple { elements: Vec::new() }),
            // &'static [uint8: len]
            Self::String(string) => Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Array(Array {
                    r#type: Box::new(Type::Primitive(Primitive::Uint8)),
                    length: term::constant::Constant::Primitive(
                        constant::Primitive::Integer(string.value.len() as i128),
                    ),
                })),
            }),
            Self::Character(c) => c.r#type.clone(),
            Self::Unreachable(u) => u.r#type.clone(),
            Self::Phantom(p) => p.r#type.clone(),
        }
    }

    /// Returns the span location of the literal value.
    pub const fn span(&self) -> &Span {
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
