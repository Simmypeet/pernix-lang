//! Contains all kinds of errors that can occur while parsing the source code.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::{KeywordKind, Token};
use pernixc_print::LogSeverity;
use pernixc_source::SourceElement;
use thiserror::Error;

/// An identifier is expected but found an another invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct IdentifierExpected {
    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
}

fn found_string(found: &Option<Token>) -> String {
    let found = found.as_ref();
    let Some(token) = found else {
        return "`end of file`".to_string();
    };

    match token {
        Token::WhiteSpace(_) => "whitespaces".to_string(),
        Token::Identifier(_) => format!("`{}` identifier", token.span().str()),
        Token::Keyword(_) => format!("`{}` keyword", token.span().str()),
        Token::Punctuation(_) | Token::NumericLiteral(_) => {
            format!("`{}`", token.span().str())
        }
        Token::StringLiteral(_) => "string literal".to_string(),
        Token::CharacterLiteral(_) => "character literal".to_string(),
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
            pernixc_print::print_source_code(&token.span(), Some("identifier expected here"));
        }
        println!();
    }
}

/// A type specifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct TypeSpecifierExpected {
    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
                &token.span(),
                Some("type specifier syntax expected here"),
            );
        }
        println!();
    }
}

/// An expression syntax is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ExpressionExpected {
    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
            pernixc_print::print_source_code(
                &token.span(),
                Some("expression syntax expected here"),
            );
        }
        println!();
    }
}

/// A member syntax is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct MemberExpected {
    /// The invalid token that was found.
    pub(super) found: Option<Token>,
}

impl MemberExpected {
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
            pernixc_print::print_source_code(&token.span(), Some("member syntax expected here"));
        }
        println!();
    }
}

/// An item syntax is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ItemExpected {
    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
            pernixc_print::print_source_code(&token.span(), Some("item syntax expected here"));
        }
        println!();
    }
}

/// An access modifier syntax is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct AccessModifierExpected {
    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
                &token.span(),
                Some("access modifier syntax expected here"),
            );
        }
        println!();
    }
}

/// A punctuation of a particular character is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct PunctuationExpected {
    /// The character of the expected punctuation.
    #[get_copy = "pub"]
    pub(super) expected: char,

    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
                &token.span(),
                Some(
                    format!("punctuation of character `{}` expected here", self.expected).as_str(),
                ),
            );
        }
        println!();
    }
}

/// A keyword of a particular kind is expected but found an other invalid token.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct KeywordExpected {
    /// The kind of the expected keyword.
    #[get_copy = "pub"]
    pub(super) expected: KeywordKind,

    /// The invalid token that was found.
    #[get = "pub"]
    pub(super) found: Option<Token>,
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
                &token.span(),
                Some(format!("keyword of `{}` expected here", self.expected.as_str()).as_str()),
            );
        }
        println!();
    }
}

/// Is an enumeration containing all kinds of syntactic errors that can occur while parsing the

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, Error, From)]
#[error("Encountered a syntactic error while parsing the source code.")]
#[allow(missing_docs)]
pub enum Error {
    IdentifierExpected(IdentifierExpected),
    TypeSpecifierExpected(TypeSpecifierExpected),
    ExpressionExpected(ExpressionExpected),
    ItemExpected(ItemExpected),
    AccessModifierExpected(AccessModifierExpected),
    PunctuationExpected(PunctuationExpected),
    KeywordExpected(KeywordExpected),
    MemberExpected(MemberExpected),
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
            Self::MemberExpected(e) => e.print(),
        }
    }
}