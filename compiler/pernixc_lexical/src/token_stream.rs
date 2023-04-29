//! Contains the [`TokenStream`] struct and its related types.

use std::ops::Index;

use derive_getters::Dissolve;
use derive_more::Deref;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Iterator;
use pernixc_system::error_handler::ErrorHandler;

use crate::{
    error::LexicalError,
    token::{Token, TokenizationError},
};

/// Is a list of tokenized [`Token`]s.
///
/// This struct is the final output of the lexical analysis phase and is meant to be used by the
/// next stage of the compilation process.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Deref, Dissolve)]
pub struct TokenStream {
    #[deref]
    tokens: Vec<Token>,
}

#[allow(clippy::inline_always)]
impl TokenStream {
    /// Tokenizes the given source code.
    ///
    /// This function tokenizes the given iterator of source code by calling the
    /// [`Token::tokenize()`] repeatedly until the iterator is exhausted.
    ///
    /// # Parameters
    /// - `source_file_iterator`: The iterator that iterates over the source code.
    ///
    /// # Returns
    /// A tuple containing the stream of successfully tokenized tokens and a list of lexical errors
    /// encountered during tokenization.
    #[must_use]
    pub fn tokenize(
        mut source_file_iterator: Iterator,
        handler: &impl ErrorHandler<LexicalError>,
    ) -> Self {
        // list of tokens to return
        let mut tokens = Vec::new();

        loop {
            // Tokenizes the next token
            match Token::tokenize(&mut source_file_iterator, handler) {
                Ok(token) => tokens.push(token),
                Err(TokenizationError::EndOfSourceCodeIteratorArgument) => {
                    break Self { tokens };
                }
                Err(TokenizationError::FatalLexicalError) => (),
            }
        }
    }

    /// Returns a cursor over the token stream.
    #[must_use]
    pub fn cursor(&self) -> Cursor {
        Cursor {
            token_stream: &self.tokens,
            position: CursorPosition::Before,
        }
    }
}

impl Index<usize> for TokenStream {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output { &self.tokens[index] }
}

/// Represents the position of a cursor over a [`TokenStream`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, EnumAsInner)]
pub enum CursorPosition {
    /// The cursor is before the first token of the token stream.
    ///
    /// Reads will return `None`.
    Before,

    /// The cursor is pointing to a valid token of the token stream.
    ///
    /// Reads will return the token pointed by the cursor.
    Valid(usize),

    /// The cursor is after the last token of the token stream.
    ///
    /// Reads will return `None`.
    After,
}

impl PartialOrd for CursorPosition {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Self::Before => match other {
                Self::Before => Some(std::cmp::Ordering::Equal),
                Self::Valid(..) | Self::After => Some(std::cmp::Ordering::Less),
            },
            Self::Valid(index) => match other {
                Self::Before => Some(std::cmp::Ordering::Greater),
                Self::Valid(other_index) => index.partial_cmp(other_index),
                Self::After => Some(std::cmp::Ordering::Less),
            },
            Self::After => match other {
                Self::Before => Some(std::cmp::Ordering::Greater),
                Self::Valid(..) | Self::After => Some(std::cmp::Ordering::Equal),
            },
        }
    }
}

/// Represents a bidirectional cursor over a [`TokenStream`].
#[derive(Debug, Clone, Copy)]
pub struct Cursor<'a> {
    token_stream: &'a [Token],
    position: CursorPosition,
}

impl<'a> Cursor<'a> {
    /// Returns the current token pointed by the cursor.
    #[must_use]
    pub fn read(&self) -> Option<&'a Token> {
        match self.position {
            CursorPosition::Valid(index) => Some(&self.token_stream[index]),
            _ => None,
        }
    }

    /// Reads the current token and moves the cursor to the next token.
    pub fn next_token(&mut self) -> Option<&'a Token> {
        match self.position {
            CursorPosition::Valid(index) => {
                let result = self.token_stream.get(index);
                if index == self.token_stream.len() - 1 {
                    self.position = CursorPosition::After;
                } else {
                    self.position = CursorPosition::Valid(index + 1);
                }
                result
            }
            CursorPosition::Before => {
                if self.token_stream.is_empty() {
                    self.position = CursorPosition::After;
                } else {
                    self.position = CursorPosition::Valid(0);
                }
                None
            }
            CursorPosition::After => None,
        }
    }

    /// Reads the current token and moves the cursor to the previous token.
    pub fn previous_token(&mut self) -> Option<&'a Token> {
        match self.position {
            CursorPosition::Valid(index) => {
                let result = self.token_stream.get(index);
                if index == 0 {
                    self.position = CursorPosition::Before;
                } else {
                    self.position = CursorPosition::Valid(index - 1);
                }
                result
            }
            CursorPosition::Before => None,
            CursorPosition::After => {
                if self.token_stream.is_empty() {
                    self.position = CursorPosition::Before;
                } else {
                    self.position = CursorPosition::Valid(self.token_stream.len() - 1);
                }
                None
            }
        }
    }

    /// Sets the position of the cursor to the given position.
    ///
    /// If the given position is valid, the cursor will be moved to the given position and `true`
    /// will be returned. Otherwise, the cursor will not be moved and `false` will be returned.
    pub fn set_position(&mut self, position: CursorPosition) -> bool {
        if let CursorPosition::Valid(index) = position {
            if index < self.token_stream.len() {
                self.position = position;
                true
            } else {
                false
            }
        } else {
            self.position = position;
            true
        }
    }

    /// Moves the cursor to the next token until the given predicate returns `true`.
    ///
    /// The token that caused the predicate to return `true` will be consumed by the cursor.
    pub fn next_token_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<&'a Token> {
        loop {
            match self.next_token() {
                Some(token) => {
                    if predicate(token) {
                        return Some(token);
                    }
                }
                None => return None,
            }
        }
    }

    /// Moves the cursor to the previous token until the given predicate returns `true`.
    ///
    /// The token that caused the predicate to return `true` will be consumed by the cursor.
    pub fn previous_token_until(
        &mut self,
        predicate: impl Fn(&Token) -> bool,
    ) -> Option<&'a Token> {
        loop {
            match self.previous_token() {
                Some(token) => {
                    if predicate(token) {
                        return Some(token);
                    }
                }
                None => return None,
            }
        }
    }

    /// Moves the cursor to the next token until the given predicate returns `true`.
    ///
    /// The cursor will be moved to the token that caused the predicate to return `true`.
    pub fn forward_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<&'a Token> {
        loop {
            match self.read() {
                Some(token) => {
                    if predicate(token) {
                        return Some(token);
                    }
                    self.next_token();
                }
                None => return None,
            }
        }
    }

    /// Moves the cursor to the previous token until the given predicate returns `true`.
    ///
    /// The cursor will be moved to the token that caused the predicate to return `true`.
    pub fn backward_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<&'a Token> {
        loop {
            match self.read() {
                Some(token) => {
                    if predicate(token) {
                        return Some(token);
                    }
                    self.previous_token();
                }
                None => return None,
            }
        }
    }

    /// Returns the position of the cursor.
    #[must_use]
    pub fn position(&self) -> CursorPosition { self.position }
}
