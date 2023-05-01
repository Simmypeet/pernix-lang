//! Contains all kinds of lexical errors that can occur while tokenizing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_print::LogSeverity;
use pernixc_source::Span;
use thiserror::Error;

/// The source code contains an invalid escape character sequences.
///
/// The spans represent the locations of the invalid escape character sequences.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct InvalidEscapeCharacterSequences {
    /// The spans represent the locations of the invalid escape character sequences.
    #[get = "pub"]
    pub(super) span: Span,
}

impl InvalidEscapeCharacterSequences {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "`{}` is an invalid escape character sequence",
                self.span.str()
            )
            .as_str(),
        );
        pernixc_print::print_source_code(&self.span, None);
        println!();
    }
}

/// The source code contains an unclosed double quote.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct UnterminatedStringLiteral {
    /// The span pointing to the unclosed double quote.
    pub(super) span: Span,
}

impl UnterminatedStringLiteral {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "found an unclosed double quote");
        pernixc_print::print_source_code(&self.span, Some("this double quote is unclosed"));
        println!();
    }
}

/// The source code contains an unclosed `/*` comment.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct UnterminatedDelimitedComment {
    /// The span of the unclosed `/*` that starts the comment.
    #[get = "pub"]
    pub(super) span: Span,
}

impl UnterminatedDelimitedComment {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "found an unclosed `/*` comment");
        pernixc_print::print_source_code(&self.span, Some("this `/*` comment is unclosed"));
        println!();
    }
}

/// The source code contains an empty character literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct EmptyCharacterLiteral {
    /// The span of the empty character literal.
    #[get = "pub"]
    pub(super) span: Span,
}

impl EmptyCharacterLiteral {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(LogSeverity::Error, "found an empty character literal");
        pernixc_print::print_source_code(&self.span, None);
        println!();
    }
}

/// In a character literal, it contains the control character that must be escaped explicitly.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ControlCharactersMustBeEscaped {
    /// The span of the control character that must be escaped explicitly.
    #[get = "pub"]
    pub(super) span: Span,
}

impl ControlCharactersMustBeEscaped {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "found a control character that must be escaped explicitly",
        );
        pernixc_print::print_source_code(
            &Span::new(
                self.span.source_file().clone(),
                self.span.start(),
                self.span.start() + 1,
            )
            .unwrap(),
            Some(
                "the character following this single quote here is a control character and it \
                 must be escaped explicitly",
            ),
        );
        println!();
    }
}

/// Is an enumeration containing all kinds of lexical errors that can occur while tokenizing the
/// source code.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, Error, From)]
#[error("Encountered an lexical error while tokenizing the source code.")]
pub enum Error {
    InvalidEscapeCharacterSequences(InvalidEscapeCharacterSequences),
    UnterminatedStringLiteral(UnterminatedStringLiteral),
    UnterminatedDelimitedComment(UnterminatedDelimitedComment),
    EmptyCharacterLiteral(EmptyCharacterLiteral),
    ControlCharactersMustBeEscaped(ControlCharactersMustBeEscaped),
}

impl Error {
    /// Prints the error message to the console
    pub fn print(&self) {
        match self {
            Self::InvalidEscapeCharacterSequences(error) => error.print(),
            Self::UnterminatedStringLiteral(error) => error.print(),
            Self::UnterminatedDelimitedComment(error) => error.print(),
            Self::EmptyCharacterLiteral(error) => error.print(),
            Self::ControlCharactersMustBeEscaped(error) => error.print(),
        }
    }
}
