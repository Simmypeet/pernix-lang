//! Contains the definition of [`Error`]

use std::sync::Arc;

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_diagnostic::{Diagnostic, Report};
use pernixc_lexical::{
    token::Token,
    token_stream::{DelimiterKind, FragmentKind, Location, TokenKind, Tree},
};
use pernixc_log::Severity;
use pernixc_source_file::{SourceFile, Span};

use crate::{
    expect::{Expected, Fragment},
    state_machine::parse::Unexpected,
};

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
            TokenKind::Fragment(_) => break,
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
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn new(
        tree: &Tree,
        unexpected: Unexpected,
        expected: Vec<Expected>,
    ) -> Self {
        assert!(!expected.is_empty(), "expected cannot be empty");

        let (found, tok_index) = unexpected.token_index.map_or_else(
            || {
                if unexpected.node_index == 0 {
                    (
                        Found::EndOfFile(
                            tree.root_token_stream().source_file().clone(),
                        ),
                        tree.root_token_stream().len(),
                    )
                } else {
                    (
                        match &tree
                            .get_node(unexpected.node_index)
                            .unwrap()
                            .as_fragment()
                            .unwrap()
                            .0
                            .kind
                        {
                            FragmentKind::Delimiter(delimiter) => Found::Token(
                                Token::Punctuation(delimiter.close.clone()),
                            ),
                            FragmentKind::Indentation(indentation) => {
                                indentation.ending_token.clone().map_or_else(
                                    || {
                                        // should never happen, because it
                                        // should've been caught by upper
                                        // if statement
                                        Found::EndOfFile(
                                            tree.root_token_stream()
                                                .source_file()
                                                .clone(),
                                        )
                                    },
                                    Found::Token,
                                )
                            }
                        },
                        tree.get_node(unexpected.node_index)
                            .unwrap()
                            .token_stream()
                            .len(),
                    )
                }
            },
            |token_index| {
                (
                    match tree
                        .get_token(&Location::new(
                            unexpected.node_index,
                            token_index,
                        ))
                        .expect("invalid token index")
                    {
                        TokenKind::Token(token) => Found::Token(token.clone()),
                        TokenKind::Fragment(fragment) => Found::Token(
                            Token::Punctuation(match &fragment.kind {
                                FragmentKind::Delimiter(delimiter) => {
                                    delimiter.open.clone()
                                }
                                FragmentKind::Indentation(indentation) => {
                                    indentation.colon.clone()
                                }
                            }),
                        ),
                    },
                    token_index,
                )
            },
        );

        let prior_insignificant = find_prior_insignificant_token(
            tree,
            unexpected.node_index,
            tok_index,
        )
        .map(|token| match token {
            TokenKind::Token(token) => token.clone(),
            TokenKind::Fragment(fragment) => {
                Token::Punctuation(match &fragment.kind {
                    FragmentKind::Delimiter(delimiter) => {
                        delimiter.open.clone()
                    }
                    FragmentKind::Indentation(indentation) => {
                        indentation.colon.clone()
                    }
                })
            }
        });

        Self {
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
                    ),
                },
                |prior_token| match &found {
                    Found::Token(token) => {
                        prior_token.span().join(token.span())
                    }
                    Found::EndOfFile(arc) => {
                        Span::to_end(arc.clone(), prior_token.span().end())
                    }
                },
            ),
            found,
            prior_insignificant,
            expected,
        }
    }
}

impl Report<()> for Error {
    fn report(&self, (): ()) -> Diagnostic {
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
                Expected::Fragment(delimiter) => match delimiter {
                    Fragment::Delimited(delimiter_kind) => {
                        format!("`{}` punctuation", match delimiter_kind {
                            DelimiterKind::Parenthesis => '(',
                            DelimiterKind::Brace => '{',
                            DelimiterKind::Bracket => '[',
                        })
                    }
                    Fragment::Indetation => "indentation block".to_string(),
                },
                Expected::NewLine(_) => "new line".to_string(),
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
                Token::NewLine(_) => "new line".to_string(),
            },

            Found::EndOfFile(_) => "EOF".to_string(),
        };

        let message = format!(
            "expected {expected_string}, but found an unexpected \
             {found_string}",
        );

        Diagnostic {
            span: self.diagnostic_span.clone(),
            message,
            severity: Severity::Error,
            help_message: None,
            related: Vec::new(),
        }
    }
}
