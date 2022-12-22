use pernix_diagnostics::{
    highlight_source_code, print_message, HighlightStyle, Severity,
};
use pernix_lexer::token::{Keyword, Token};
use pernix_project::source_code::{SourceCode, SourcePositionWithoutByteIndex};

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

/// Represent a struct that contains a [`SyntacticError`] and a reference to the
/// source code.
#[derive(Debug, Clone)]
pub struct Error<'a> {
    pub(crate) source_reference: &'a SourceCode,
    pub(crate) syntactic_error: SyntacticError<'a>,
}

impl<'a> Error<'a> {
    /// Return a reference to the source reference of this [`Error`].
    pub fn source_reference(&self) -> &SourceCode {
        self.source_reference
    }

    /// Return a reference to the syntactic error of this [`Error`].
    pub fn syntactic_error(&self) -> &SyntacticError<'a> {
        &self.syntactic_error
    }

    /// Print the error message of this [`Error`]
    pub fn print_error(&self) {
        match &self.syntactic_error {
            SyntacticError::LexicalError(lexical_error) => {
                lexical_error.print_error();
            }
            SyntacticError::KeywordExpected { expected_keyword, found_token } => {
                print_message(format!("`{}` keyword expected found `{}`", expected_keyword.get_keyword_string(), found_token.lexeme()).as_str(), Severity::Error, Some("STA"));

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Range(found_token.position_range().start.into()..found_token.position_range().end.into()),
                    None,
                )
            }
            SyntacticError::IdentifierExpected { expected_at } => {
                print_message("identifier expected", Severity::Error, Some("STA"));

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Position(*expected_at),
                    Some("identifier expected here"),
                )
            }
            SyntacticError::PunctuatorExpected { expected_punctuator, expected_at } => {
                print_message(format!("`{}` punctuator expected", expected_punctuator).as_str(), Severity::Error, Some("STA"));

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Position(*expected_at),
                    Some(format!("{} expected here", expected_punctuator).as_str()),
                )
            }
            SyntacticError::UnexpectedToken { context: _, found_token } => {
                print_message(format!("unexpected token found `{}`", found_token.lexeme()).as_str(), Severity::Error, Some("STA"));

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Range(found_token.position_range().start.into()..found_token.position_range().end.into()),
                    None,
                )
            }
            SyntacticError::UsingDirectiveMustAppearPriorToAllDeclarations { using_directive } => {
                print_message("using directive must appear prior to all declarations", Severity::Error, Some("STA"));

                highlight_source_code(
                    self.source_reference,
                    HighlightStyle::Range(using_directive.position.start.into()..using_directive.position.end.into()),
                    Some("try moving this using directive to the top of the file/namespace"),
                )
            }
        }
    }
}

/// Represent an enumeration containing all possible errors that can occur
/// during parsing.
#[derive(Debug, Clone)]
pub enum SyntacticError<'a> {
    LexicalError(pernix_lexer::error::Error<'a>),
    KeywordExpected {
        expected_keyword: Keyword,
        found_token: Token<'a>,
    },
    IdentifierExpected {
        expected_at: SourcePositionWithoutByteIndex,
    },
    PunctuatorExpected {
        expected_punctuator: char,
        expected_at: SourcePositionWithoutByteIndex,
    },
    UnexpectedToken {
        context: Context,
        found_token: Token<'a>,
    },
    UsingDirectiveMustAppearPriorToAllDeclarations {
        using_directive: PositionWrapper<UsingDirective<'a>>,
    },
}
