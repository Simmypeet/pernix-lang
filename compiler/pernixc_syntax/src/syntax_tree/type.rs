//! Contains all definition of type syntax trees.

#![allow(missing_docs)]

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_handler::Handler;
use pernixc_lexical::{
    token::{Keyword, KeywordKind, Punctuation},
    token_stream::Delimiter,
};
use pernixc_source_file::{SourceElement, Span};

use super::{
    Constant, Elided, EnclosedConnectedList, Lifetime, Parse, ParseExt,
    SyntaxTree,
};
use crate::{
    error,
    state_machine::{
        parse::{self, Branch, ExpectExt},
        StateMachine,
    },
    syntax_tree::QualifiedIdentifier,
};

pub mod strategy;

/// Syntax Synopsis:
/// ``` txt
/// Primitive:
///     'bool'
///     | 'float32'
///     | 'float64'
///     | 'int8'
///     | 'int16'
///     | 'int32'
///     | 'int64'
///     | 'uint8'
///     | 'uint16'
///     | 'uint32'
///     | 'uint64'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Primitive {
    Bool(Keyword),
    Float32(Keyword),
    Float64(Keyword),
    Int8(Keyword),
    Int16(Keyword),
    Int32(Keyword),
    Int64(Keyword),
    Uint8(Keyword),
    Uint16(Keyword),
    Uint32(Keyword),
    Uint64(Keyword),
    Usize(Keyword),
    Isize(Keyword),
}
impl SyntaxTree for Primitive {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            KeywordKind::Bool.to_owned().map(Self::Bool),
            KeywordKind::Float32.to_owned().map(Self::Float32),
            KeywordKind::Float64.to_owned().map(Self::Float64),
            KeywordKind::Int8.to_owned().map(Self::Int8),
            KeywordKind::Int16.to_owned().map(Self::Int16),
            KeywordKind::Int32.to_owned().map(Self::Int32),
            KeywordKind::Int64.to_owned().map(Self::Int64),
            KeywordKind::Uint8.to_owned().map(Self::Uint8),
            KeywordKind::Uint16.to_owned().map(Self::Uint16),
            KeywordKind::Uint32.to_owned().map(Self::Uint32),
            KeywordKind::Uint64.to_owned().map(Self::Uint64),
            KeywordKind::Usize.to_owned().map(Self::Usize),
            KeywordKind::Isize.to_owned().map(Self::Isize),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Primitive {
    fn span(&self) -> Span {
        match self {
            Self::Bool(token)
            | Self::Float32(token)
            | Self::Float64(token)
            | Self::Int8(token)
            | Self::Int16(token)
            | Self::Int32(token)
            | Self::Int64(token)
            | Self::Uint8(token)
            | Self::Uint16(token)
            | Self::Uint32(token)
            | Self::Uint64(token)
            | Self::Usize(token)
            | Self::Isize(token) => token.span.clone(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Reference:
///      Qualifier Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
#[allow(missing_docs)]
pub struct Reference {
    #[get = "pub"]
    ampersand: Punctuation,
    #[get = "pub"]
    lifetime: Option<Lifetime>,
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    operand: Box<Type>,
}

impl SourceElement for Reference {
    fn span(&self) -> Span { self.ampersand.span().join(&self.operand.span()) }
}

impl SyntaxTree for Reference {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '&'.to_owned(),
            Lifetime::parse.or_none(),
            KeywordKind::Mutable.to_owned().or_none(),
            Type::parse.map(Box::new),
        )
            .map(|(ampersand, lifetime, mutable_keyword, operand)| Self {
                ampersand,
                lifetime,
                mutable_keyword,
                operand,
            })
            .parse(state_machine, handler)
    }
}

impl Reference {
    /// Destructs the `Reference` into its components.
    #[must_use]
    pub fn destruct(
        self,
    ) -> (Punctuation, Option<Lifetime>, Option<Keyword>, Box<Type>) {
        (self.ampersand, self.lifetime, self.mutable_keyword, self.operand)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Unpackable:
///     '...'? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Unpackable {
    #[get = "pub"]
    ellipsis: Option<(Punctuation, Punctuation, Punctuation)>,
    #[get = "pub"]
    r#type: Box<Type>,
}

impl SyntaxTree for Unpackable {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            (
                '.'.to_owned(),
                '.'.no_skip().to_owned(),
                '.'.no_skip().to_owned(),
            )
                .commit_in(3)
                .or_none(),
            Type::parse.map(Box::new),
        )
            .map(|(ellipsis, r#type)| Self { ellipsis, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Unpackable {
    fn span(&self) -> Span {
        match &self.ellipsis {
            Some((left, ..)) => left.span.join(&self.r#type.span()),
            None => self.r#type.span(),
        }
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Tuple:
///     '(' UnpackableList? ')'
///     ;
/// ```
pub type Tuple = EnclosedConnectedList<Unpackable, Punctuation>;

impl SyntaxTree for Tuple {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        Unpackable::parse
            .enclosed_connected_list(','.to_owned(), Delimiter::Parenthesis)
            .parse(state_machine, handler)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Array:
///     '[' Type ':' Constant ']'
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Array {
    #[get = "pub"]
    left_bracket: Punctuation,
    #[get = "pub"]
    operand: Box<Type>,
    #[get = "pub"]
    colon: Punctuation,
    #[get = "pub"]
    constant: Constant,
    #[get = "pub"]
    right_bracket: Punctuation,
}

impl SyntaxTree for Array {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (Type::parse.map(Box::new), ':'.to_owned(), Constant::parse)
            .step_into(Delimiter::Bracket)
            .map(|(open, tree, close)| Self {
                left_bracket: open.clone(),
                operand: tree.0,
                colon: tree.1,
                constant: tree.2,
                right_bracket: close.clone(),
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Array {
    fn span(&self) -> Span {
        self.left_bracket.span.join(&self.right_bracket.span)
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Pointer:
///     '*' mutable? Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Pointer {
    #[get = "pub"]
    asterisk: Punctuation,
    #[get = "pub"]
    mutable_keyword: Option<Keyword>,
    #[get = "pub"]
    operand: Box<Type>,
}

impl SyntaxTree for Pointer {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            '*'.to_owned(),
            KeywordKind::Mutable.to_owned().or_none(),
            Type::parse.map(Box::new),
        )
            .map(|(asterisk, mutable_keyword, operand)| Self {
                asterisk,
                mutable_keyword,
                operand,
            })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Pointer {
    fn span(&self) -> Span { self.asterisk.span.join(&self.operand.span()) }
}

/// Syntax Synopsis:
///
/// ``` txt
/// Phantom:
///     'phantom' Type
///     ;
/// ```
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Phantom {
    #[get = "pub"]
    phantom_keyword: Keyword,
    #[get = "pub"]
    r#type: Box<Type>,
}

impl SyntaxTree for Phantom {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (KeywordKind::Phantom.to_owned(), Type::parse.map(Box::new))
            .map(|(phantom_keyword, r#type)| Self { phantom_keyword, r#type })
            .parse(state_machine, handler)
    }
}

impl SourceElement for Phantom {
    fn span(&self) -> Span {
        self.phantom_keyword.span.join(&self.r#type.span())
    }
}

/// Syntax Synopsis:
/// ``` txt
/// Type:
///     Primitive
///     | QualifiedIdentifier
///     | Reference
///     | Pointer
///     | Tuple
///     | Local
///     | Array
///     | Phantom
///     | Elided
///     ;
/// ```
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
pub enum Type {
    Primitive(Primitive),
    QualifiedIdentifier(QualifiedIdentifier),
    Reference(Reference),
    Pointer(Pointer),
    Tuple(Tuple),
    Array(Array),
    Phantom(Phantom),
    Elided(Elided),
}

impl SyntaxTree for Type {
    fn parse(
        state_machine: &mut StateMachine,
        handler: &dyn Handler<error::Error>,
    ) -> parse::Result<Self> {
        (
            Primitive::parse.map(Self::Primitive),
            QualifiedIdentifier::parse.map(Self::QualifiedIdentifier),
            Reference::parse.map(Self::Reference),
            Pointer::parse.map(Self::Pointer),
            Tuple::parse.map(Self::Tuple),
            Array::parse.map(Self::Array),
            Phantom::parse.map(Self::Phantom),
            Elided::parse.map(Self::Elided),
        )
            .branch()
            .parse(state_machine, handler)
    }
}

impl SourceElement for Type {
    fn span(&self) -> Span {
        match self {
            Self::Primitive(primitive) => primitive.span(),
            Self::QualifiedIdentifier(qualified) => qualified.span(),
            Self::Reference(reference) => reference.span(),
            Self::Pointer(pointer) => pointer.span(),
            Self::Tuple(tuple) => tuple.span(),
            Self::Array(array) => array.span(),
            Self::Phantom(phantom) => phantom.span(),
            Self::Elided(elided) => elided.span(),
        }
    }
}

#[cfg(test)]
mod test;
