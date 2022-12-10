use pernix_lexer::token::{Keyword, Token};
use pernix_project::source_code::SourceCode;

/// Enumeration containing all possible errors that can occur during parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<'a> {
    LexicalError(pernix_lexer::error::Error<'a>),
    KeywordExpected {
        expected_keyword: Keyword,
        found_token: Token<'a>,
        source_reference: &'a SourceCode,
    },
    IdentifierExpected {
        found_token: Token<'a>,
        source_reference: &'a SourceCode,
    },
    PunctuatorExpected {
        expected_punctuator: char,
        found_token: Token<'a>,
        source_reference: &'a SourceCode,
    },
}
