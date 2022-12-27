use std::ops::Range;

use pernixc_common::source_file::{SourceFile, TextPosition};
use pernixc_lexical_analysis::token::Keyword;

/// Represent a struct that contains a [`SyntacticError`] and a reference to the
/// source file.
#[derive(Debug, Clone)]
pub struct Error<'src> {
    pub(crate) source_file: &'src SourceFile,
    pub(crate) syntactic_errors: Vec<SyntacticError>,
}

impl<'src> Error<'src> {
    /// Return a reference to the source file of this [`Error`].
    pub fn source_file(&self) -> &'src SourceFile {
        self.source_file
    }

    /// Return a reference to the syntactic errors of this [`Error`].
    pub fn syntactic_errors(&self) -> &[SyntacticError] {
        self.syntactic_errors.as_ref()
    }
}

/// Represent an enumeration of all possible parsing context.
#[derive(Debug, Clone, Copy)]
pub enum ParsingContext {
    Statement,
    Expression,
    Declaration,
}

/// Is an enumeration containing all syntactic errors.
#[derive(Debug, Clone)]
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
