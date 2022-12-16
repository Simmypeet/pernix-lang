use pernix_lexer::token::{Keyword, Token};

use crate::abstract_syntax_tree::{
    declaration::UsingDirective, PositionWrapper,
};

/// Represent an enumeration containing all possible contexts that the error can
///  occur in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Context {
    Declaration,
    Statement,
    Expression,
}

/// Represent an enumeration containing all possible errors that can occur
/// during parsing.
#[derive(Debug, Clone)]
pub enum Error<'a> {
    LexicalError(pernix_lexer::error::Error),
    KeywordExpected {
        expected_keyword: Keyword,
        found_token: Token<'a>,
    },
    IdentifierExpected {
        found_token: Token<'a>,
    },
    PunctuatorExpected {
        expected_punctuator: char,
        found_token: Token<'a>,
    },
    UnexpectedToken {
        context: Context,
        found_token: Token<'a>,
    },
    UsingDirectiveMustAppearPriorToAllDeclarations {
        using_directive: PositionWrapper<UsingDirective<'a>>,
    },
}
