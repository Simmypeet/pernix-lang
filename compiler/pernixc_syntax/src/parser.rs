//! Contains the [`Parser`] logic.

use std::sync::Arc;

use derive_more::{Deref, DerefMut};
use displaydoc::Display;
use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, SourceFile, Span},
};
use pernixc_lexical::{
    token::{Punctuation, Token},
    token_stream::{Delimited, Delimiter, TokenKind, TokenStream},
};
use syntax::Syntax;

use crate::error::{self, Found, SyntaxKind, Unexpected};

pub mod expect;
pub mod syntax;

/**
encountered a fatal syntax error, which aborts the parsing process; the error
information has been reported to the designated error handler.
 */
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Display,
    thiserror::Error,
)]
pub struct Error;

/// Provides a way to iterate over a token stream.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum TokenProvider<'a> {
    /// Iterating at the top level of the token stream.
    TokenStream(&'a TokenStream),

    /// Iterating inside a delimited token stream.
    Delimited(&'a Delimited),
}

impl<'a> TokenProvider<'a> {
    /// Gets the token stream of the current token provider.
    #[must_use]
    pub const fn token_stream(&self) -> &'a TokenStream {
        match self {
            TokenProvider::TokenStream(token_stream) => token_stream,
            TokenProvider::Delimited(delimited) => &delimited.token_stream,
        }
    }
}

/// Represents a single frame of the parser's stack, responsible for reading a
/// token stream in that given token stream level.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Frame<'a> {
    token_provider: TokenProvider<'a>,
    current_index: usize,
}

/// Represents the read value of the [`Frame`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Reading {
    /// A singular token.
    Unit(Token),

    /// Found an opening delimiter token, which means that the parser can step
    /// into a new delimited frame.
    IntoDelimited(Delimiter, Punctuation),

    /// Found a closing delimiter token, which means that the parser should
    /// step out of the current delimited frame.
    DelimitedEnd(Delimiter, Punctuation),

    /// End of file.
    Eof,
}

impl Reading {
    /// Gets the read token inside the [`Reading`] as `Option<Token>`
    ///
    /// # Returns
    ///
    /// Returns `None` if the [`Reading`] is [`Reading::Eof`].
    #[must_use]
    pub fn into_token(self) -> Option<Token> {
        match self {
            Self::Unit(token) => Some(token),
            Self::IntoDelimited(_, pun) | Self::DelimitedEnd(_, pun) => {
                Some(Token::Punctuation(pun))
            }
            Self::Eof => None,
        }
    }
}

impl<'a> Frame<'a> {
    /// Checks if the current [`Frame`] doesn't have any more significant
    /// [`TokenTree`]s to parse.
    #[must_use]
    pub fn is_exhausted(&self) -> bool {
        let token_stream = self.token_provider.token_stream();
        for i in self.current_index..self.token_provider.token_stream().len() {
            if !matches!(
                token_stream.get(i),
                Some(TokenKind::Token(
                    Token::WhiteSpaces(..) | Token::Comment(..)
                ))
            ) {
                return false;
            }
        }
        true
    }

    /// Checks if the current [`Frame`] has reached the end of the
    /// [`TokenStream`].
    #[must_use]
    pub fn is_end(&self) -> bool {
        self.current_index >= self.token_provider.token_stream().len()
    }

    fn get_reading(&self, token: Option<&TokenKind>) -> Reading {
        token.map_or_else(
            || {
                match self.token_provider {
                    // end of file
                    TokenProvider::TokenStream(..) => Reading::Eof,
                    TokenProvider::Delimited(delimited) => {
                        Reading::DelimitedEnd(
                            delimited.delimiter,
                            delimited.close.clone(),
                        )
                    }
                }
            },
            |token| match token {
                TokenKind::Token(token) => Reading::Unit(token.clone()),
                TokenKind::Delimited(delimited) => Reading::IntoDelimited(
                    delimited.delimiter,
                    delimited.open.clone(),
                ),
            },
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Reading {
        self.get_reading(
            self.token_provider.token_stream().get(self.current_index),
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` with the given index
    /// offset of the [`Frame`].
    ///
    /// # Returns
    ///
    /// `None` if `offset + current_index` is less than zero or greer than
    /// `self.token_provider.token_stream().len() + 1`
    #[must_use]
    #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)]
    pub fn peek_offset(&self, offset: isize) -> Option<Reading> {
        let index = self.current_index as isize + offset;
        let index = if index < 0 {
            return None;
        } else {
            index as usize
        };

        if index > self.token_provider.token_stream().len() + 1 {
            return None;
        }

        Some(self.get_reading(self.token_provider.token_stream().get(index)))
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and
    /// increments the `current_index` by 1.
    #[must_use = "use `forward` instead if you don't need the token and just \
                  want to increment the index"]
    pub fn next_token(&mut self) -> Reading {
        let token = self.peek();

        // increment the index
        self.forward();

        token
    }

    /// Peeks for the significant [`Reading`] from the current position
    /// (including current position).
    ///
    /// # Returns
    ///
    /// Returns the significant token and its index.
    pub fn peek_significant(&self) -> (Reading, usize) {
        for i in self.current_index..=self.token_provider.token_stream().len() {
            let reading =
                self.get_reading(self.token_provider.token_stream().get(i));

            if reading.as_unit().map_or(true, Token::is_significant) {
                return (reading, i);
            }
        }

        (
            match self.token_provider {
                // end of file
                TokenProvider::TokenStream(..) => Reading::Eof,
                TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                    delimited.delimiter,
                    delimited.close.clone(),
                ),
            },
            self.token_provider.token_stream().len(),
        )
    }

    /// Increments the `current_index` by 1.
    pub fn forward(&mut self) {
        // increment the index
        if !self.is_end() {
            self.current_index += 1;
        }
    }

    /// Skips any insignificant [`Token`]s, returns the next significant
    /// [`Token`] found, and increments the `current_index` afterward.
    pub fn next_significant_token(&mut self) -> Reading {
        let token = self.stop_at_significant();

        // increment the index
        self.forward();

        token
    }

    /// Makes the current [`Frame`] point to the significant [`Token`] if
    /// currently not.
    ///
    /// # Returns
    /// The significant [`Token`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if !matches!(
                token,
                Reading::Unit(Token::WhiteSpaces(..) | Token::Comment(..))
            ) {
                return token;
            }

            self.forward();
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                delimited.delimiter,
                delimited.close.clone(),
            ),
        }
    }

    /// Makes the current position stops at the first token that satisfies the
    /// predicate.
    pub fn stop_at(&mut self, predicate: impl Fn(&Reading) -> bool) -> Reading {
        while !self.is_end() {
            let token = self.peek();

            if predicate(&token) {
                return token;
            }

            self.current_index += 1;
        }

        match self.token_provider {
            TokenProvider::TokenStream(..) => Reading::Eof,
            TokenProvider::Delimited(delimited) => Reading::DelimitedEnd(
                delimited.delimiter,
                delimited.close.clone(),
            ),
        }
    }
}

/// The parser of the compiler.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,

    /// The source file being parsed.
    source_file: Arc<SourceFile>,
}

/// Represents a tree that is delimited by a pair of punctuations.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DelimitedTree<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The inner tree inside the delimiter.
    pub tree: T,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<T> SourceElement for DelimitedTree<T> {
    fn span(&self) -> Span { self.open.span.join(&self.close.span).unwrap() }
}

/// Represents a result of [`Parser::step_into()`] function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StepIntoTree<T> {
    /// The opening delimiter.
    pub open: Punctuation,

    /// The tree inside the delimiter.
    pub tree: Result<T, Error>,

    /// The closing delimiter.
    pub close: Punctuation,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from the given token stream.
    ///
    /// The source file argument should be the source file that the token stream
    /// is from. The source file doesn't influence the parsing process, but it
    /// is used to provide better error messages.
    #[must_use]
    pub const fn new(
        token_stream: &'a TokenStream,
        source_file: Arc<SourceFile>,
    ) -> Self {
        Self {
            current_frame: Frame {
                token_provider: TokenProvider::TokenStream(token_stream),
                current_index: 0,
            },
            stack: Vec::new(),
            source_file,
        }
    }

    /// Parses the given condition and returns the output if the condition is
    /// met.
    pub fn parse<S: Syntax>(
        &mut self,
        syntax: S,
        handler: &dyn Handler<error::Error>,
    ) -> Result<S::Output, Error> {
        match syntax.step(self, handler) {
            Ok(result) => Ok(result),

            Err(syntax::Error::ConditionNotMet(i, iter)) => {
                self.current_index = i;

                handler.receive(error::Error {
                    expected_syntaxes: iter.into_iter().collect(),
                    found: self.reading_to_found(self.get_reading(
                        self.current_frame.token_provider.token_stream().get(i),
                    )),
                });

                // make progress
                self.forward();

                Err(Error)
            }

            Err(syntax::Error::Commited(er)) => Err(er),
        }
    }

    /// Steps into the [`Delimited`] token stream and parses the content within
    /// the delimiters.
    ///
    /// The parser's position must be at the delimited token stream.
    pub fn step_into<T>(
        &mut self,
        delimiter: Delimiter,
        f: impl FnOnce(&mut Self) -> Result<T, Error>,
        handler: &dyn Handler<error::Error>,
    ) -> Result<StepIntoTree<T>, Error> {
        self.current_frame.stop_at_significant();
        let raw_token_tree = self
            .current_frame
            .token_provider
            .token_stream()
            .get(self.current_frame.current_index);

        // move after the whole delimited list
        self.current_frame.forward();

        let expected = match delimiter {
            Delimiter::Parenthesis => '(',
            Delimiter::Brace => '{',
            Delimiter::Bracket => '[',
        };

        let delimited_stream = if let Some(token_tree) = raw_token_tree {
            match token_tree {
                TokenKind::Delimited(delimited_tree)
                    if delimited_tree.delimiter == delimiter =>
                {
                    delimited_tree
                }
                found => {
                    handler.receive(error::Error {
                        expected_syntaxes: vec![SyntaxKind::Punctuation(
                            expected,
                        )],
                        found: match found {
                            TokenKind::Token(token) => {
                                Found::Unexpected(Unexpected {
                                    prior_insignificant: self
                                        .peek_offset(-1)
                                        .and_then(Reading::into_token)
                                        .filter(|x| !x.is_significant()),
                                    unexpected: token.clone(),
                                })
                            }
                            TokenKind::Delimited(delimited_tree) => {
                                Found::Unexpected(Unexpected {
                                    prior_insignificant: self
                                        .peek_offset(-1)
                                        .and_then(Reading::into_token)
                                        .filter(|x| !x.is_significant()),
                                    unexpected: Token::Punctuation(
                                        delimited_tree.open.clone(),
                                    ),
                                })
                            }
                        },
                    });

                    return Err(Error);
                }
            }
        } else {
            handler.receive(error::Error {
                expected_syntaxes: vec![SyntaxKind::Punctuation(expected)],
                found: self.reading_to_found(self.get_reading(None)),
            });

            return Err(Error);
        };

        // creates a new frame
        let new_frame = Frame {
            token_provider: TokenProvider::Delimited(delimited_stream),
            current_index: 0,
        };

        // pushes the current frame onto the stack and replaces the current
        // frame with the new one
        self.stack.push(std::mem::replace(&mut self.current_frame, new_frame));

        let open = delimited_stream.open.clone();

        let tree = f(self);

        // pops the current frame off the stack
        let new_frame = self.stack.pop().ok_or(Error)?;

        // the current frame must be at the end
        if !self.current_frame.is_exhausted() {
            let expected = match self
                .current_frame
                .token_provider
                .as_delimited()
                .unwrap()
                .delimiter
            {
                Delimiter::Parenthesis => ')',
                Delimiter::Brace => '}',
                Delimiter::Bracket => ']',
            };

            handler.receive(error::Error {
                expected_syntaxes: vec![SyntaxKind::Punctuation(expected)],
                found: self.reading_to_found(self.peek()),
            });
        }

        let close_punctuation = self
            .current_frame
            .token_provider
            .as_delimited()
            .unwrap()
            .close
            .clone();

        // replaces the current frame with the popped one
        self.current_frame = new_frame;

        Ok(StepIntoTree { open, tree, close: close_punctuation })
    }

    /// Tries to parse the given function and returns the output if the parsing
    /// is successful. Otherwise, the parser will revert to the original
    /// position.
    pub fn try_parse<M>(
        &mut self,
        parse_fn: impl FnOnce(&mut Parser) -> Result<M, Error>,
    ) -> Option<M> {
        let current_index = self.current_frame.current_index;

        match parse_fn(self) {
            Ok(result) => Some(result),
            Err(Error) => {
                self.current_frame.current_index = current_index;
                None
            }
        }
    }

    /// Converts the reading to [`Found`] for error reporting.
    pub(crate) fn reading_to_found(&self, reading: Reading) -> Found {
        reading.into_token().map_or_else(
            || Found::EndOfFile(self.source_file.clone()),
            |x| {
                Found::Unexpected(Unexpected {
                    prior_insignificant: self
                        .peek_offset(-1)
                        .and_then(Reading::into_token)
                        .filter(|x| !x.is_significant()),
                    unexpected: x,
                })
            },
        )
    }
}
