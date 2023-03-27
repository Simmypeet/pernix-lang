//! Contains all kinds of errors that can occur while parsing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{KeywordKind, Token};
use thiserror::Error;

/// An identifier is expected but found an another invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IdentifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// A type specifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeSpecifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// An expression syntax is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExpressionExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// A item syntax is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ItemExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// A field group syntax is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FieldGroupExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// An access modifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AccessModifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// A punctuation of a particular character is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PunctuationExpected {
    /// The character of the expected punctuation.
    pub expected: char,
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// A keyword of a particular kind is expected but found an other invalid token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeywordExpected {
    /// The kind of the expected keyword.
    pub expected: KeywordKind,
    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the
/// source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error, From)]
#[error("Encountered a syntactic error while parsing the source code.")]
#[allow(missing_docs)]
pub enum SyntacticError {
    IdentifierExpected(IdentifierExpected),
    TypeSpecifierExpected(TypeSpecifierExpected),
    ExpressionExpected(ExpressionExpected),
    ItemExpected(ItemExpected),
    FieldGroupExpected(FieldGroupExpected),
    AccessModifierExpected(AccessModifierExpected),
    PunctuationExpected(PunctuationExpected),
    KeywordExpected(KeywordExpected),
}
