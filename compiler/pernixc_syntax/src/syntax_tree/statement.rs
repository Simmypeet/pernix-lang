use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Identifier, Keyword, Punctuation};
use pernixc_source::{SourceElement, Span, SpanError};

use super::{
    expression::{Expression, Functional, Imperative},
    TypeAnnotation,
};

/// Represents a statement syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// Statement:
///     Declarative
///     | Expressive
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Statement {
    Declarative(Declarative),
    Expressive(Expressive),
}

impl SourceElement for Statement {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Declarative(declaration) => declaration.span(),
            Self::Expressive(expression) => expression.span(),
        }
    }
}

/// Represents a declarative statement syntax tree node
///
/// Declarative statements are statements that introduce a new symbol into the current scope.
///
/// Syntax Synopsis:
/// ``` text
/// Declarative:
///     VariableDeclaration
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Declarative {
    VariableDeclaration(VariableDeclaration),
}

impl SourceElement for Declarative {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::VariableDeclaration(declaration) => declaration.span(),
        }
    }
}

/// Represents a variable declaration syntax tree node
///
/// Syntax Synopsis:
/// ``` text
/// VariableDeclaration:
///     VariableTypeBindingSpecifier Identifier '=' Expression ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub let_keyword: Keyword,
    pub mutable_keyword: Option<Keyword>,
    pub identifier: Identifier,
    pub type_annotation: Option<TypeAnnotation>,
    pub equals: Punctuation,
    pub expression: Expression,
    pub semicolon: Punctuation,
}

impl SourceElement for VariableDeclaration {
    fn span(&self) -> Result<Span, SpanError> {
        self.let_keyword.span()?.join(&self.semicolon.span)
    }
}

/// Represents an expressive statement syntax tree node
///
/// Expressive statements are statements that interest in the side effects of evaluating an
/// expression.
///
/// Syntax Synopsis:
/// ``` text
/// Expressive:
///     Semi
///     | Imperative
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum Expressive {
    Semi(Semi),
    Imperative(Imperative),
}

impl SourceElement for Expressive {
    fn span(&self) -> Result<Span, SpanError> {
        match self {
            Self::Semi(expression) => expression.span(),
            Self::Imperative(expression) => expression.span(),
        }
    }
}

/// Represents a semi statement syntax tree node
///
/// Semi statements are statements that are a functional expression followed by a semicolon.
///
/// Syntax Synopsis:
/// ``` text
/// Semi:
///     Functional ';'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Semi {
    pub expression: Functional,
    pub semicolon: Punctuation,
}

impl SourceElement for Semi {
    fn span(&self) -> Result<Span, SpanError> { self.expression.span()?.join(&self.semicolon.span) }
}
