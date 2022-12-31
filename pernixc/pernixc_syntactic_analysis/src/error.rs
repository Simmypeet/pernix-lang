use std::ops::Range;

use pernixc_common::{
    printing::{highlight_source_file, print_message, HighlightStyle, Severity},
    source_file::{SourceFile, TextPosition},
};
use pernixc_lexical_analysis::token::Keyword;

/// Represent an enumeration of all possible parsing context.
#[derive(Clone, Copy)]
pub enum ParsingContext {
    Statement,
    Expression,
    Declaration,
    File,
}

/// Is an enumeration of all the possible errors that can occur during the lexical
/// analysis phase.
#[derive(Clone)]
pub enum SyntacticError {
    IdentifierExpected {
        expected_position: TextPosition,
    },
    PunctuatorExpected {
        expected_position: TextPosition,
        expected_punctuator: char,
    },
    KeywordExpected {
        expected_position: TextPosition,
        expected_keyword: Keyword,
    },
    UnexpectedToken {
        unexpected_position: Range<TextPosition>,
        parsing_context: ParsingContext,
    },
    UsingDirectiveMustBeDeclaredPriorToAnyOtherDeclaration {
        using_directive_position: Range<TextPosition>,
    },
    AccessModifierExpected {
        expected_position: Range<TextPosition>,
    },
}

impl SyntacticError {
    /// Return the error number of this [`SyntacticError`].
    pub fn get_error_number(&self) -> usize {
        match self {
            SyntacticError::IdentifierExpected { .. } => 0,
            SyntacticError::PunctuatorExpected { .. } => 1,
            SyntacticError::KeywordExpected { .. } => 2,
            SyntacticError::UnexpectedToken { .. } => 3,
            SyntacticError::UsingDirectiveMustBeDeclaredPriorToAnyOtherDeclaration { .. } => 4,
            SyntacticError::AccessModifierExpected { .. } => 5,
        }
    }

    /// Print the error message of this [`SyntacticError`].
    pub fn print_error(&self, source_file_reference: &SourceFile) {
        let category = format!("SXA{:03}", self.get_error_number());

        match &self {
            SyntacticError::IdentifierExpected { expected_position } => {
                print_message(
                    "an identifier was expected",
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Position((*expected_position).into()),
                    None,
                )
            }
            SyntacticError::PunctuatorExpected {
                expected_position,
                expected_punctuator,
            } => {
                print_message(
                    format!("`{}` was expected", expected_punctuator).as_str(),
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Position((*expected_position).into()),
                    None,
                )
            }
            SyntacticError::KeywordExpected {
                expected_position,
                expected_keyword,
            } => {
                print_message(
                    format!("`{}` was expected", expected_keyword.get_keyword_string()).as_str(),
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Position((*expected_position).into()),
                    None,
                )
            }
            SyntacticError::UnexpectedToken {
                unexpected_position,
                parsing_context,
            } => {
                let message = match parsing_context {
                    ParsingContext::Statement => "unexpected token in statement",
                    ParsingContext::Expression => "unexpected token in expression",
                    ParsingContext::Declaration => "unexpected token in declaration",
                    ParsingContext::File => "unexpected token in file-level",
                };

                print_message(message, Severity::Error, Some(&category));

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(unexpected_position.clone()),
                    None,
                )
            }
            SyntacticError::UsingDirectiveMustBeDeclaredPriorToAnyOtherDeclaration {
                using_directive_position,
            } => {
                print_message(
                    "using directives must be declared prior to any other declaration",
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(using_directive_position.clone()),
                    Some("consider moving the using directive to the top of the file or namespace"),
                )
            }
            SyntacticError::AccessModifierExpected { expected_position } => {
                print_message("access modifier expected", Severity::Error, Some(&category));

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(expected_position.clone()),
                    None,
                )
            }
        }
    }
}
