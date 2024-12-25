//! Contains the definition of [`Error`]

use std::{convert::Infallible, sync::Arc};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::{
    diagnostic::{Diagnostic, Report},
    log::Severity,
    source_file::{SourceFile, Span},
};
use pernixc_lexical::{
    token::Token,
    token_stream::{Delimiter, Location, TokenKind, Tree},
};

use crate::{expect::Expected, state_machine::parse::Unexpected};

/// An unexpected token or end of file
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
#[allow(missing_docs)]
pub enum Found {
    Token(Token),
    EndOfFile(Arc<SourceFile>),
}

impl Found {
    /// Gets the source file where the [`Found`] token is located
    #[must_use]
    pub fn source_file(&self) -> &Arc<SourceFile> {
        match self {
            Self::Token(token) => token.span().source_file(),
            Self::EndOfFile(source_file) => source_file,
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

/// An error that occurs when trying to convert an unexpected error
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    thiserror::Error,
    displaydoc::Display,
)]
#[allow(clippy::module_name_repetitions)]
pub enum ConvertUnexpectedError {
    /**
    Either the token index or the node index is invalid for the state machine's
    token tree
     */
    InvalidIndex,

    /**
    The expected iterator is empty
     */
    EmptyExpectedIterator,
}

fn find_prior_insignificant_token<'a>(
    tree: &Tree<'a>,
    node_index: usize,
    token_index: usize,
) -> Option<&'a TokenKind> {
    let node = tree.get_node(node_index).unwrap();
    let mut current_token_index = token_index;

    while current_token_index != 0 {
        let next = current_token_index - 1;

        match node.token_stream().get(next).unwrap() {
            TokenKind::Token(token) => {
                if token.is_significant() {
                    break;
                }
            }
            TokenKind::Delimited(_) => break,
        }

        current_token_index = next;
    }

    if current_token_index == token_index {
        None
    } else {
        tree.get_token(&Location::new(node_index, current_token_index))
    }
}

impl Error {
    /// Converts the [`Unexpected`] error into an [`Error`] instance
    ///
    /// # Errors
    ///
    /// See [`ConvertUnexpectedError`] for possible errors
    pub fn new(
        tree: &Tree,
        unexpected: Unexpected,
        expected: Vec<Expected>,
    ) -> Result<Self, ConvertUnexpectedError> {
        if expected.is_empty() {
            return Err(ConvertUnexpectedError::EmptyExpectedIterator);
        }

        let (found, tok_index) = match unexpected.token_index {
            Some(token_index) => (
                match tree
                    .get_token(&Location::new(
                        unexpected.node_index,
                        token_index,
                    ))
                    .ok_or(ConvertUnexpectedError::InvalidIndex)?
                {
                    TokenKind::Token(token) => Found::Token(token.clone()),
                    TokenKind::Delimited(delimited) => {
                        Found::Token(Token::Punctuation(delimited.open.clone()))
                    }
                },
                token_index,
            ),

            None => {
                if unexpected.node_index == 0 {
                    (
                        Found::EndOfFile(
                            tree.root_token_stream().source_file().clone(),
                        ),
                        tree.root_token_stream().len(),
                    )
                } else {
                    (
                        Found::Token(Token::Punctuation(
                            tree.get_node(unexpected.node_index)
                                .ok_or(ConvertUnexpectedError::InvalidIndex)?
                                .as_delimited()
                                .unwrap()
                                .0
                                .close
                                .clone(),
                        )),
                        tree.get_node(unexpected.node_index)
                            .ok_or(ConvertUnexpectedError::InvalidIndex)?
                            .token_stream()
                            .len(),
                    )
                }
            }
        };

        let prior_insignificant = find_prior_insignificant_token(
            tree,
            unexpected.node_index,
            tok_index,
        )
        .map(|token| match token {
            TokenKind::Token(token) => token.clone(),
            TokenKind::Delimited(delimited) => {
                Token::Punctuation(delimited.open.clone())
            }
        });

        Ok(Self {
            diagnostic_span: prior_insignificant.as_ref().map_or_else(
                || match &found {
                    Found::Token(token) => token.span().clone(),
                    Found::EndOfFile(arc) => Span::new(
                        arc.clone(),
                        if arc.content().is_empty() {
                            0
                        } else {
                            arc.content().len() - 1
                        },
                        arc.content().len(),
                    )
                    .unwrap(),
                },
                |prior_token| match &found {
                    Found::Token(token) => {
                        prior_token.span().join(token.span()).unwrap()
                    }
                    Found::EndOfFile(arc) => {
                        Span::to_end(arc.clone(), prior_token.span().end())
                            .unwrap()
                    }
                },
            ),
            found,
            prior_insignificant,
            expected,
        })
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
                Expected::Punctuation(a) => format!("`{a}` punctuation"),
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
            "expected {expected_string}, but found an unexpected \
             {found_string}",
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
