//! Contains the definition of [`Literal`].

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::type_system::{
    model::Model,
    term::{
        self, constant,
        lifetime::Lifetime,
        r#type::{Array, Primitive, Qualifier, Reference, Type},
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
    pub span: Option<Span>,
}

/// Represents a boolean value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean.
    pub value: bool,

    /// The span location of the boolean.
    pub span: Option<Span>,
}

/// A placeholder for a value that was failed to bind.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error<M: Model> {
    /// The expected type of the value.
    pub r#type: Type<M>,

    /// The span location of the errornous expression.
    pub span: Option<Span>,
}

/// Represents a unit value (empty tuple).
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unit {
    /// The span location of the unit value.
    pub span: Option<Span>,
}

/// A placeholder for a value that can never be reached.
///
/// This is generally used for unreachable branches in the control flow.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Unreachable<M: Model> {
    /// The type of the unreachable value.
    pub r#type: Type<M>,

    /// The span location of the unreachable value.
    pub span: Option<Span>,
}

/// Represents a string literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct String {
    /// The value of the string.
    pub value: Vec<u8>,

    /// The span location of the string.
    pub span: Option<Span>,
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
}

impl<M: Model> Literal<M> {
    /// Returns the type of the literal value.
    pub fn r#type(&self) -> Type<M> {
        match self {
            Literal::Numeric(n) => n.r#type.clone(),
            Literal::Boolean(_) => Type::Primitive(Primitive::Bool),
            Literal::Error(e) => e.r#type.clone(),
            Literal::Unit(_) => {
                Type::Tuple(term::Tuple { elements: Vec::new() })
            }
            // &'static [uint8: len]
            Literal::String(string) => Type::Reference(Reference {
                qualifier: Qualifier::Immutable,
                lifetime: Lifetime::Static,
                pointee: Box::new(Type::Array(Array {
                    r#type: Box::new(Type::Primitive(Primitive::Uint8)),
                    length: term::constant::Constant::Primitive(
                        constant::Primitive::Integer(string.value.len() as i128),
                    ),
                })),
            }),
            Literal::Character(c) => c.r#type.clone(),
            Literal::Unreachable(u) => u.r#type.clone(),
        }
    }

    /// Returns the span location of the literal value.
    pub fn span(&self) -> Option<&Span> {
        match self {
            Literal::Numeric(n) => n.span.as_ref(),
            Literal::Boolean(b) => b.span.as_ref(),
            Literal::Error(e) => e.span.as_ref(),
            Literal::Unit(u) => u.span.as_ref(),
            Literal::String(s) => s.span.as_ref(),
            Literal::Character(c) => c.span.as_ref(),
            Literal::Unreachable(u) => u.span.as_ref(),
        }
    }
}
