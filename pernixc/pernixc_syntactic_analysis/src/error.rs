use std::ops::Range;

use pernixc_common::{
    printing::{highlight_source_file, print_message, HighlightStyle, Severity},
    source_file::{SourceFile, TextPosition},
};
use pernixc_lexical_analysis::token::Keyword;

/// Represent an enumeration of all possible parsing context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParsingContext {
    Statement,
    Expression,
    Declaration,
    ClassMemberDeclaration,
}

/// Is an enumeration of all the possible errors that can occur during the lexical
/// analysis phase.
#[derive(Debug, Clone)]
pub enum SyntacticError {
    /// An identifier was expected.
    IdentifierExpected { expected_position: TextPosition },

    /// A particular punctuation was expected.
    PunctuationExpected {
        expected_position: TextPosition,
        expected_punctuation: char,
    },

    /// A particular keyword was expected.
    KeywordExpected {
        expected_position: TextPosition,
        expected_keyword: Keyword,
    },

    /// The token is not expected in the current parsing context.
    UnexpectedToken {
        unexpected_position_range: Range<TextPosition>,
        parsing_context: ParsingContext,
    },

    /// A module declaration was expected at the top of the file.
    ModuleDeclarationExpected {
        unexpected_token_position_range: Range<TextPosition>,
    },
}

impl SyntacticError {
    /// Return the error number of this [`SyntacticError`].
    pub fn get_error_number(&self) -> usize {
        match self {
            SyntacticError::IdentifierExpected { .. } => 0,
            SyntacticError::PunctuationExpected { .. } => 1,
            SyntacticError::KeywordExpected { .. } => 2,
            SyntacticError::UnexpectedToken { .. } => 3,
            SyntacticError::ModuleDeclarationExpected { .. } => 4,
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
            SyntacticError::PunctuationExpected {
                expected_position,
                expected_punctuation: expected_punctuator,
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
                    format!(
                        "`{}` was expected",
                        match expected_keyword {
                            Keyword::New => "new",
                            Keyword::Public => "public",
                            Keyword::Private => "private",
                            Keyword::Function => "function",
                            Keyword::Module => "module",
                            Keyword::Using => "using",
                            Keyword::Void => "void",
                            Keyword::Int8 => "int8",
                            Keyword::Int16 => "int16",
                            Keyword::Int32 => "int32",
                            Keyword::Int64 => "int64",
                            Keyword::Uint8 => "uint8",
                            Keyword::Uint16 => "uint16",
                            Keyword::Uint32 => "uint32",
                            Keyword::Uint64 => "uint64",
                            Keyword::Float32 => "float32",
                            Keyword::Float64 => "float64",
                            Keyword::Bool => "bool",
                            Keyword::If => "if",
                            Keyword::Else => "else",
                            Keyword::While => "while",
                            Keyword::Break => "break",
                            Keyword::Continue => "continue",
                            Keyword::Return => "return",
                            Keyword::Class => "class",
                            Keyword::Mutable => "mutable",
                            Keyword::Let => "let",
                            Keyword::Var => "var",
                            Keyword::Export => "export",
                            Keyword::Import => "import",
                        }
                    )
                    .as_str(),
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
                unexpected_position_range: unexpected_position,
                parsing_context,
            } => {
                let message = match parsing_context {
                    ParsingContext::Statement => "unexpected token in statement",
                    ParsingContext::Expression => "unexpected token in expression",
                    ParsingContext::Declaration => "unexpected token in declaration",
                    ParsingContext::ClassMemberDeclaration => {
                        "unexpected token in class member declaration"
                    }
                };

                print_message(message, Severity::Error, Some(&category));

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(unexpected_position.clone()),
                    None,
                )
            }
            SyntacticError::ModuleDeclarationExpected {
                unexpected_token_position_range: unexpected_token_position,
            } => {
                print_message(
                    "a module declaration was expected at the top of the file",
                    Severity::Error,
                    Some(&category),
                );

                highlight_source_file(
                    source_file_reference,
                    HighlightStyle::Range(unexpected_token_position.clone()),
                    None,
                )
            }
        }
    }
}
