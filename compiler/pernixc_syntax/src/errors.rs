use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Token;
use thiserror::Error;

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, Error)]
#[error("Encountered a syntactic error while parsing the source code.")]
pub enum SyntacticError {
    /// An identifier is expected but found an another invalid token.
    IdentifierExpected(Option<Token>),

    /// A type specifier syntax is expected but found an other invalid token.
    TypeSpecifierExpected(Option<Token>),
}

