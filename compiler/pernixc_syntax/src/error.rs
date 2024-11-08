//! Contains the definition of [`Error`]

use std::convert::Infallible;

use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
};
use pernixc_lexical::{token::Token, token_stream::TokenKind};

use crate::{expect::Expected, state_machine::parse::Unexpected};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error<'a> {
    pub found: Option<(&'a TokenKind, usize)>,
    pub node_index: usize,
    pub expected: Vec<Expected>,
}

impl Error {
    fn get_expected_string(&self) -> String {
        match self.expected_syntaxes.len() {
            0 => panic!("empty"),
            1 => self.expected_syntaxes[0].get_expected_string(),
            _ => {
                let mut expected_string = String::new();
                for (index, syntax) in self.expected_syntaxes.iter().enumerate()
                {
                    if index == self.expected_syntaxes.len() - 1 {
                        expected_string.push_str(" or ");
                    } else if index != 0 {
                        expected_string.push_str(", ");
                    }

                    expected_string.push_str(&syntax.get_expected_string());
                }

                expected_string
            }
        }
    }
}

impl Report<()> for Error {
    type Error = Infallible;

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        let expected_string = self.get_expected_string();

        let found_string = match &self.found {
            Found::Unexpected(Unexpected {
                unexpected: Token::Comment(..),
                ..
            }) => "comment token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Identifier(..),
                ..
            }) => "identifier token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Keyword(keyword),
                ..
            }) => {
                format!("keyword token `{}`", keyword.kind.as_str())
            }
            Found::Unexpected(Unexpected {
                unexpected: Token::WhiteSpaces(..),
                ..
            }) => "white spaces token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Punctuation(punctuation),
                ..
            }) => {
                format!("punctuation token `{}`", punctuation.punctuation)
            }
            Found::Unexpected(Unexpected {
                unexpected: Token::Numeric(..),
                ..
            }) => "numeric token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::Character(..),
                ..
            }) => "character literal token".to_string(),
            Found::Unexpected(Unexpected {
                unexpected: Token::String(..),
                ..
            }) => "string literal token".to_string(),

            Found::EndOfFile(_) => "EOF".to_string(),
        };

        let message = if self.expected_syntaxes.is_empty() {
            format!(
                "expected {}, but found an unexpected {found_string}",
                expected_string
            )
        } else {
            format!("found an unexpected {found_string}")
        };

        Ok(Diagnostic {
            span: self.found.span(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
