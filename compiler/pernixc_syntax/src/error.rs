//! Contains all kinds of errors that can occur while parsing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::CopyGetters;
use pernixc_lexical::token::{KeywordKind, Token};
use pernixc_print::LogSeverity;
use pernixc_source::Span;
use thiserror::Error;

/// An identifier is expected but found an another invalid token.
#[derive(Debug, Clone)]
pub struct IdentifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

fn found_string(found: &Option<Token>) -> String {
    let found = found.as_ref();
    let Some(token) = found else {
        return "`end of file`".to_string();
    };

    match token {
        Token::WhiteSpaces(_) => "whitespaces".to_string(),
        Token::Identifier(_) => format!("`{}` identifier", token.span().str()),
        Token::Keyword(_) => format!("`{}` keyword", token.span().str()),
        Token::Punctuation(_) | Token::NumericLiteral(_) => {
            format!("`{}`", token.span().str())
        }
        Token::Comment(_) => "comment".to_string(),
    }
}

impl IdentifierExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "an identifier is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(token.span(), Some("identifier expected here"));
        }
    }
}

/// A type specifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct TypeSpecifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl TypeSpecifierExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "a type specifier syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some("type specifier syntax expected here"),
            );
        }
    }
}

/// An expression syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct ExpressionExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl ExpressionExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "an expression syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(token.span(), Some("expression syntax expected here"));
        }
    }
}

/// A struct member syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct StructMemberExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl StructMemberExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "a member syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(token.span(), Some("member syntax expected here"));
        }
    }
}

/// An item syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct ItemExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl ItemExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "an item syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(token.span(), Some("item syntax expected here"));
        }
    }
}

/// An access modifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct AccessModifierExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl AccessModifierExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "an access modifier syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some("access modifier syntax expected here"),
            );
        }
    }
}

/// A punctuation of a particular character is expected but found an other invalid token.
#[derive(Debug, Clone, CopyGetters)]
pub struct PunctuationExpected {
    /// The character of the expected punctuation.
    pub expected: char,

    /// The invalid token that was found.
    pub found: Option<Token>,
}

/// Prints the error message to the console
impl PunctuationExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "a punctuation of character `{}` is expected, found: {}",
                self.expected,
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some(
                    format!("punctuation of character `{}` expected here", self.expected).as_str(),
                ),
            );
        }
    }
}

/// A keyword of a particular kind is expected but found an other invalid token.
#[derive(Debug, Clone, CopyGetters)]
pub struct KeywordExpected {
    /// The kind of the expected keyword.
    pub expected: KeywordKind,

    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl KeywordExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "a keyword of `{}` is expected, found: {}",
                self.expected.as_str(),
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some(format!("keyword of `{}` expected here", self.expected.as_str()).as_str()),
            );
        }
    }
}

/// A generic arugment/parameter list cannot be empty.
#[derive(Debug, Clone)]
pub struct GenericArgumentParameterListCannotBeEmpty {
    /// The span of the generic argument/parameter.
    pub span: Span,
}

impl GenericArgumentParameterListCannotBeEmpty {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            "a generic argument/parameter list cannot be empty",
        );
        pernixc_print::print_source_code(
            &self.span,
            Some("generic argument/parameter list cannot be empty"),
        );
    }
}

/// A trait member syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct TraitMemberExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl TraitMemberExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "a trait member syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some("trait member syntax expected here"),
            );
        }
    }
}

/// An implements member syntax is expected but found an other invalid token.
#[derive(Debug, Clone)]
pub struct ImplementsMemberExpected {
    /// The invalid token that was found.
    pub found: Option<Token>,
}

impl ImplementsMemberExpected {
    /// Prints the error message to the console
    pub fn print(&self) {
        pernixc_print::print(
            LogSeverity::Error,
            format!(
                "an implements member syntax is expected, found: {}",
                found_string(&self.found)
            )
            .as_str(),
        );
        if let Some(token) = self.found.as_ref() {
            pernixc_print::print_source_code(
                token.span(),
                Some("implements member syntax expected here"),
            );
        }
    }
}

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the
#[derive(Debug, Clone, EnumAsInner, Error, From)]
#[error("encountered a syntactic error while parsing the source code.")]
#[allow(missing_docs)]
pub enum Error {
    IdentifierExpected(IdentifierExpected),
    TypeSpecifierExpected(TypeSpecifierExpected),
    ExpressionExpected(ExpressionExpected),
    ItemExpected(ItemExpected),
    AccessModifierExpected(AccessModifierExpected),
    PunctuationExpected(PunctuationExpected),
    KeywordExpected(KeywordExpected),
    StructMemberExpected(StructMemberExpected),
    TraitMemberExpected(TraitMemberExpected),
    ImplementsMemberExpected(ImplementsMemberExpected),
    GenericArgumentParameterListCannotBeEmpty(GenericArgumentParameterListCannotBeEmpty),
}

impl Error {
    /// Prints the error message to the console
    pub fn print(&self) {
        match self {
            Self::IdentifierExpected(e) => e.print(),
            Self::TypeSpecifierExpected(e) => e.print(),
            Self::ExpressionExpected(e) => e.print(),
            Self::ItemExpected(e) => e.print(),
            Self::AccessModifierExpected(e) => e.print(),
            Self::PunctuationExpected(e) => e.print(),
            Self::KeywordExpected(e) => e.print(),
            Self::StructMemberExpected(e) => e.print(),
            Self::GenericArgumentParameterListCannotBeEmpty(e) => e.print(),
            Self::TraitMemberExpected(e) => e.print(),
            Self::ImplementsMemberExpected(e) => e.print(),
        }
    }
}
