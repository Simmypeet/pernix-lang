//! Contains the definition of [`Error`]

use std::{convert::Infallible, sync::Arc};

use getset::Getters;
use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
    source_file::{SourceFile, Span},
};
use pernixc_lexical::{token::Token, token_stream::Delimiter};

use crate::expect::Expected;

/// An unexpected token or end of file
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Found {
    Token(Token),
    EndOfFile(Arc<SourceFile>),
}

impl Found {
    /// Gets the source file where the [`Found`] token is located
    pub fn source_file(&self) -> &Arc<SourceFile> {
        match self {
            Found::Token(token) => token.span().source_file(),
            Found::EndOfFile(source_file) => source_file,
        }
    }
}

/// Represents an error that occurred during parsing
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Getters)]
pub struct Error {
    /// Gets the found token or end of file
    #[get = "pub"]
    found: Found,
    prior_insignificant: Option<Token>,
    diagnostic_span: Span,

    /// Gets the expected tokens
    #[get = "pub"]
    expected: Vec<Expected>,
}

impl Error {
    /// Creates a new [`Error`] instance
    ///
    /// # Panics
    ///
    /// - If `expected` is empty
    /// - If `found` source file is different from `prior_insignificant` source
    ///   file
    /// - If `found` span cannot be merged with `prior_insignificant` span
    pub fn new(
        found: Found,
        prior_insignificant: Option<Token>,
        expected: Vec<Expected>,
    ) -> Self {
        assert!(!expected.is_empty(), "expected cannot be empty");

        let diagnostic_span = match (&found, &prior_insignificant) {
            (Found::Token(token), None) => token.span().clone(),
            (Found::Token(token), Some(prior_token)) => {
                prior_token.span().join(&token.span())
            }
            (Found::EndOfFile(arc), None) => {
                Span::new(arc.clone(), 0, arc.content().len())
            }
            (Found::EndOfFile(arc), Some(a)) => {
                Span::new(arc.clone(), a.span().end(), arc.content().len())
            }
        };

        Self { found, prior_insignificant, diagnostic_span, expected }
    }
}

impl Report<()> for Error {
    type Error = Infallible;

    fn report(&self, (): ()) -> Result<Diagnostic, Self::Error> {
        let expected_string = self
            .expected
            .iter()
            .map(|expected| match expected {
                Expected::Identifier(_) => "identifier".to_string(),
                Expected::Numeric(_) => "numeric literal".to_string(),
                Expected::String(_) => "string literal".to_string(),
                Expected::Character(_) => "character literal".to_string(),
                Expected::Punctuation(a) => format!("`{}` punctuation", a),
                Expected::Keyword(keyword_kind) => {
                    format!("`{keyword_kind}` keyword",)
                }
                Expected::Delimited(delimiter) => {
                    format!("`{}` punctuation", match delimiter {
                        Delimiter::Parenthesis => '(',
                        Delimiter::Brace => '{',
                        Delimiter::Bracket => '[',
                    })
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let found_string = match &self.found {
            Found::Token(token) => match token {
                Token::WhiteSpaces(_) => "whitespace".to_string(),
                Token::Identifier(_) => "identifier".to_string(),
                Token::Keyword(keyword) => {
                    format!("`{}` keyword", keyword.kind)
                }
                Token::Punctuation(punctuation) => {
                    format!("`{}` punctuation", punctuation.punctuation)
                }
                Token::Numeric(_) => "numeric literal".to_string(),
                Token::Comment(_) => "comment".to_string(),
                Token::Character(_) => "character literal".to_string(),
                Token::String(_) => "string literal".to_string(),
            },

            Found::EndOfFile(_) => "EOF".to_string(),
        };

        let message = format!(
            "expected {}, but found an unexpected {}",
            expected_string, found_string
        );

        Ok(Diagnostic {
            span: self.diagnostic_span.clone(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        })
    }
}
