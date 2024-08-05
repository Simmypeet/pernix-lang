//! Contains the definition of [`Literal`].

use enum_as_inner::EnumAsInner;
use pernixc_base::source_file::Span;

use crate::type_system::{
    model::Model,
    term::{
        self,
        r#type::{Primitive, Type},
    },
};

/// Represents a numeric literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Numeric<M: Model> {
    /// The numeric value for the integer part as a string.
    pub integer_string: String,

    /// The numeric value for the decimal part as a string.
    pub decimal_stirng: Option<String>,

    /// The type of the numeric value.
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
            Literal::Unreachable(u) => u.span.as_ref(),
        }
    }
}
