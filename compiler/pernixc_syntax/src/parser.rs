//! Contaisn the definition of the [`Parser`].

use derive_more::{Deref, DerefMut, From};
use enum_as_inner::EnumAsInner;
use pernixc_lexical::{
    token::{Identifier, Keyword, KeywordKind, Punctuation, Token},
    token_stream::{Delimited, Delimiter, TokenStream, TokenTree},
};
use pernixc_system::diagnostic::Handler;
use thiserror::Error;

use crate::{
    error::{Error as SyntacticError, IdentifierExpected, KeywordExpected, PunctuationExpected},
    syntax_tree::ConnectedList,
};

/// Is an enumeration of all the possible [`Token`] stream providers.
#[derive(Debug, Clone, Copy, Hash, EnumAsInner, From)]
pub enum TokenProvider<'a> {
    /// Top-level [`TokenStream`], not enclosed in any delimiters.
    TokenStream(&'a TokenStream),

    /// Delimited [`TokenStream`], enclosed in a pair of delimiters.
    Delimited(&'a Delimited),
}

impl<'a> TokenProvider<'a> {
    /// Gets the [`TokenStream`] from the [`TokenProvider`].
    #[must_use]
    pub fn token_stream(&self) -> &'a TokenStream {
        match self {
            TokenProvider::TokenStream(stream) => stream,
            TokenProvider::Delimited(delimited) => &delimited.token_stream,
        }
    }
}

/// Represents a subset of parsing logic that handles on a specific range of [`TokenTree`]s.
#[derive(Debug, Clone, Hash)]
pub struct Frame<'a> {
    /// The [`TokenProvider`] that provides the [`TokenStream`] to parse.
    pub token_provider: TokenProvider<'a>,

    current_index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error(
    "Is an error that occurs when encountering a fatal syntax error that stops the creation of \
     the syntax tree completely."
)]
#[allow(missing_docs)]
pub struct Error;

/// Is a specialized [`Result`] type for the parser.
pub type Result<T> = std::result::Result<T, Error>;

impl<'a> Frame<'a> {
    /// Checks if the current position of the [`Frame`] is exhausted (at the end of the
    /// [`TokenStream`])
    #[must_use]
    pub fn is_exhausted(&self) -> bool {
        self.current_index >= self.token_provider.token_stream().len()
    }

    fn to_token_output(token: &'a TokenTree) -> Token {
        match token {
            TokenTree::Token(token) => token.clone(),
            TokenTree::Delimited(token) => Token::Punctuation(token.open.clone()),
        }
    }

    fn last_token_output(&self) -> Option<Token> {
        match self.token_provider {
            TokenProvider::TokenStream(..) => None,
            TokenProvider::Delimited(delimited) => {
                Some(Token::Punctuation(delimited.close.clone()))
            }
        }
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`].
    #[must_use]
    pub fn peek(&self) -> Option<Token> {
        self.token_provider
            .token_stream()
            .get(self.current_index)
            .map_or_else(
                || self.last_token_output(),
                |token| Some(Self::to_token_output(token)),
            )
    }

    /// Returns a [`Token`] pointing by the `current_index` with the given index offset of the
    /// [`Frame`].
    #[must_use]
    pub fn peek_offset(&self, offset: isize) -> Option<Token> {
        let index = self.current_index.checked_add(offset.try_into().ok()?)?;

        self.token_provider.token_stream().get(index).map_or_else(
            || {
                if index == self.current_index {
                    self.last_token_output()
                } else {
                    None
                }
            },
            |token| Some(Self::to_token_output(token)),
        )
    }

    /// Returns a [`Token`] pointing by the `current_index` of the [`Frame`] and increments the
    /// `current_index` by 1.
    pub fn next_token(&mut self) -> Option<Token> {
        let token = self.peek();

        // increment the index
        self.forward();

        token
    }

    /// Forwards the `current_index` by 1 if the [`Frame`] is not exhausted.
    pub fn forward(&mut self) {
        // increment the index
        if !self.is_exhausted() {
            self.current_index += 1;
        }
    }

    /// Skips any insignificant [`Token`]s, returns the next significant [`Token`] found, and
    /// increments the `current_index` afterward.
    pub fn next_significant_token(&mut self) -> Option<Token> {
        let token = self.stop_at_significant();

        // increment the index
        self.forward();

        token
    }

    /// Makes the current [`Frame`] point to the significant [`Token`] if currently not.
    ///
    /// # Returns
    /// The significant [`Token`] if found, otherwise `None`.
    pub fn stop_at_significant(&mut self) -> Option<Token> {
        while !self.is_exhausted() {
            let token = self.peek();

            if !matches!(token, Some(Token::WhiteSpace(..) | Token::Comment(..))) {
                return token;
            }

            self.forward();
        }

        None
    }

    /// Expects the next [`Token`] to be an [`Identifier`], and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not an [`Identifier`].
    pub fn parse_identifier(
        &mut self,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Identifier> {
        match self.next_significant_token() {
            Some(Token::Identifier(ident)) => Ok(ident),
            found => {
                handler.recieve(SyntacticError::IdentifierExpected(IdentifierExpected {
                    found,
                }));
                Err(Error)
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Keyword`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Keyword`] of specific kind.
    pub fn parse_keyword(
        &mut self,
        expected: KeywordKind,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Keyword> {
        match self.next_significant_token() {
            Some(Token::Keyword(keyword_token)) if keyword_token.keyword == expected => {
                Ok(keyword_token)
            }
            found => {
                handler.recieve(SyntacticError::KeywordExpected(KeywordExpected {
                    expected,
                    found,
                }));
                Err(Error)
            }
        }
    }

    /// Expects the next [`Token`] to be a [`Punctuation`] of specific kind, and returns it.
    ///
    /// # Errors
    /// If the next [`Token`] is not a [`Punctuation`] of specific kind.
    pub fn parse_punctuation(
        &mut self,
        expected: char,
        skip_insignificant: bool,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<Punctuation> {
        match if skip_insignificant {
            self.next_significant_token()
        } else {
            self.next_token()
        } {
            Some(Token::Punctuation(punctuation_token))
                if punctuation_token.punctuation == expected =>
            {
                Ok(punctuation_token)
            }
            found => {
                handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                    expected,
                    found,
                }));
                Err(Error)
            }
        }
    }

    /// Performs a rollback if the parsing fails.
    #[allow(clippy::missing_errors_doc)]
    pub fn try_parse<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let starting_index = self.current_index;
        match parser(self) {
            Ok(value) => Ok(value),
            Err(Error) => {
                self.current_index = starting_index;
                Err(Error)
            }
        }
    }

    /// Makes the current position stops at the first token that satisfies the predicate.
    pub fn stop_at(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<Token> {
        while !self.is_exhausted() {
            let token = self.peek()?;

            if predicate(&token) {
                return Some(token);
            }

            self.current_index += 1;
        }

        None
    }
}

impl<'a> Frame<'a> {
    /// Parses a list of items that are separated by a particular separator.
    ///
    /// This function is useful for parsing patterns of elements that are separated by a single
    /// character, such as comma-separated lists of expressions; where the list is enclosed in
    /// parentheses, brackets, or braces.
    pub(crate) fn parse_enclosed_list_manual<T>(
        &mut self,
        delimiter: char,
        separator: char,
        mut parse_item: impl FnMut(&mut Self) -> Result<T>,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<(Option<ConnectedList<T, Punctuation>>, Punctuation)> {
        let mut first = None;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        // check for empty list
        match self.stop_at_significant() {
            Some(Token::Punctuation(punc)) if punc.punctuation == delimiter => {
                self.next_token();
                return Ok((None, punc));
            }
            None => handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                expected: delimiter,
                found: None,
            })),
            _ => (),
        };

        if let Ok(value) = parse_item(self) {
            first = Some(value);
        } else {
            let token = self.stop_at(|token| match token {
                Token::Punctuation(punc) => {
                    punc.punctuation == delimiter || punc.punctuation == separator
                }
                _ => false,
            });

            // if found delimiter, return empty list
            if let Some(Token::Punctuation(token)) = token {
                if token.punctuation == delimiter {
                    self.next_token();
                    return Ok((None, token));
                }
            }
        }

        let delimiter = loop {
            match self.stop_at_significant() {
                Some(Token::Punctuation(separator_token))
                    if separator_token.punctuation == separator =>
                {
                    // eat the separator
                    self.next_token();

                    match self.stop_at_significant() {
                        Some(Token::Punctuation(delimiter_token))
                            if delimiter_token.punctuation == delimiter =>
                        {
                            // eat the delimiter
                            self.next_token();

                            trailing_separator = Some(separator_token);
                            break delimiter_token;
                        }
                        _ => (),
                    }

                    parse_item(self).map_or_else(
                        |_| {
                            self.stop_at(|token| {
                                matches!(
                                    token,
                                    Token::Punctuation(punc) if punc.punctuation == delimiter ||
                                        punc.punctuation == separator
                                )
                            });
                        },
                        |value| {
                            if first.is_none() {
                                first = Some(value);
                            } else {
                                rest.push((separator_token.clone(), value));
                            }
                        },
                    );
                }
                Some(Token::Punctuation(delimiter_token))
                    if delimiter_token.punctuation == delimiter =>
                {
                    // eat the delimiter
                    self.next_token();

                    break delimiter_token;
                }
                found => {
                    handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                        expected: delimiter,
                        found,
                    }));
                    return Err(Error);
                }
            }
        };

        Ok((
            first.map(|first| ConnectedList {
                first,
                rest,
                trailing_separator,
            }),
            delimiter,
        ))
    }
}

/// The parser of the compiler.
#[derive(Debug, Clone, Deref, DerefMut)]
pub struct Parser<'a> {
    #[deref]
    #[deref_mut]
    current_frame: Frame<'a>,
    stack: Vec<Frame<'a>>,
    trying_stack: Vec<usize>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from the given token stream.
    #[must_use]
    pub fn new(token_stream: &'a TokenStream) -> Self {
        Self {
            current_frame: Frame {
                token_provider: TokenProvider::TokenStream(token_stream),
                current_index: 0,
            },
            stack: Vec::new(),
            trying_stack: Vec::new(),
        }
    }

    /// Steps into the delimited token stream of the given delimiter for the next significant
    /// token.
    ///
    /// # Errors
    /// - If the next significant token is not a delimited token tree.
    pub fn step_into(
        &mut self,
        delimiter: Delimiter,
        handler: &impl Handler<SyntacticError>,
    ) -> Result<()> {
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
                TokenTree::Delimited(delimited_tree) if delimited_tree.delimiter == delimiter => {
                    delimited_tree
                }
                found => {
                    handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                        expected,
                        found: Some(match found {
                            TokenTree::Token(token) => token.clone(),
                            TokenTree::Delimited(delimited_tree) => {
                                Token::Punctuation(delimited_tree.open.clone())
                            }
                        }),
                    }));

                    return Err(Error);
                }
            }
        } else {
            handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                expected,
                found: None,
            }));

            return Err(Error);
        };

        // creates a new frame
        let new_frame = Frame {
            token_provider: TokenProvider::Delimited(delimited_stream),
            current_index: 0,
        };

        // pushes the current frame onto the stack and replaces the current frame with the new one
        self.stack
            .push(std::mem::replace(&mut self.current_frame, new_frame));

        Ok(())
    }

    /// Steps out from the current frame, replacing it with the frame on top of the stack.
    ///
    /// # Errors
    /// If the current [`Frame`]'s token provider is not [`Delimited`].
    pub fn step_out(&mut self, handler: &impl Handler<SyntacticError>) -> Result<()> {
        if let Some(threshold) = self.trying_stack.last().copied() {
            assert!(self.stack.len() > threshold);
        }

        // pops the current frame off the stack
        let Some(new_frame) = self.stack.pop() else {
            return Err(Error);
        };

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

            handler.recieve(SyntacticError::PunctuationExpected(PunctuationExpected {
                expected,
                found: self.current_frame.peek(),
            }));
        }

        // replaces the current frame with the popped one
        self.current_frame = new_frame;

        Ok(())
    }

    /// Performs a rollback if the parsing fails.
    #[allow(clippy::missing_errors_doc)]
    pub fn try_parse<T>(&mut self, parser: impl FnOnce(&mut Self) -> Result<T>) -> Result<T> {
        let current_frame_copy = self.current_frame.clone();
        self.trying_stack.push(self.stack.len());

        match parser(self) {
            Ok(value) => {
                self.trying_stack.pop();
                Ok(value)
            }
            Err(err) => {
                let stack_len = self.trying_stack.pop().unwrap();

                // the stack must be the same as before the call
                self.stack.truncate(stack_len);

                // restore the current Frame
                self.current_frame = current_frame_copy;

                Err(err)
            }
        }
    }
}
