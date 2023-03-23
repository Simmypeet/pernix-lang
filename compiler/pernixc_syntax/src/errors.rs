use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::{Keyword, Token};
use thiserror::Error;

/// A punctuation of a particular character is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PunctuationExpected {
    pub expected: char,
    pub found:    Option<Token>,
}

/// A keyword of a particular kind is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KeywordExpected {
    pub expected: Keyword,
    pub found:    Option<Token>,
}

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error)]
#[error("Encountered a syntactic error while parsing the source code.")]
pub enum SyntacticError {
    /// An identifier is expected but found an another invalid token.
    IdentifierExpected(Option<Token>),

    /// A type specifier syntax is expected but found an other invalid token.
    TypeSpecifierExpected(Option<Token>),

    /// An expression syntax is expected but found an other invalid token.
    ExpressionExpected(Option<Token>),

    /// A item syntax is expected but found an other invalid token.
    ItemExpected(Option<Token>),

    /// A field group syntax is expected but found an other invalid token.
    FieldGroupExpected(Option<Token>),

    /// An access modifier syntax is expected but found an other invalid token.
    AccessModifierExpected(Option<Token>),

    PunctuationExpected(PunctuationExpected),
    KeywordExpected(KeywordExpected),
}
