//! Contains the [`Parser`] type that is used to parse a stream of tokens into a syntax tree.

use std::fmt::Debug;

use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Punctuation, Token},
    token_stream::{Cursor, CursorPosition},
};
use pernixc_system::error_handler::ErrorHandler;
use thiserror::Error;

use crate::error::{Error, IdentifierExpected, PunctuationExpected};

/// Represents a state machine that parses a stream of tokens into a syntax tree.
///
/// The parser takes a [Cursor] as input and produces various kinds of syntax trees as
/// result. The parser provides various `parse_*` methods that can be used to parse different kinds
/// of syntax trees.
#[derive(Debug, Getters)]
pub struct Parser<'a> {
    pub(super) cursor: Cursor<'a>,
}

/// Indicates that the given token stream cursor is not pointing at a valid position when creating a
/// [`Parser`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The given token stream cursor must be pointing at a valid position")]
pub struct InvalidTokenStreamCursorError;

impl<'a> Parser<'a> {
    /// Creates a new parser that will start parsing the token stream from the given cursor.
    ///
    /// # Errors
    /// - [`InvalidTokenStreamCursorError`] is returned if the given cursor is not pointing at a
    ///   valid position.
    pub fn new(cursor: Cursor<'a>) -> Result<Self, InvalidTokenStreamCursorError> {
        if !matches!(cursor.position(), CursorPosition::Valid(..)) {
            return Err(InvalidTokenStreamCursorError);
        }

        Ok(Self { cursor })
    }

    /// Returns the next token in the token stream.
    ///
    /// If the token iterator is exhausted, this method returns `None`.
    pub fn next_token(&mut self) -> Option<Token> { self.cursor.next_token().cloned() }

    /// Returns the next token in the token stream without consuming it.
    ///
    /// If the token iterator is exhausted, this method returns `None`.
    pub fn peek_token(&mut self) -> Option<Token> { self.cursor.read().cloned() }

    /// Finds the next significant token in the token stream, consuming all insignificant tokens
    /// along the way and consuming the significant token as well.
    ///
    /// A token is considered significant if it is not a whitespace or a comment.
    ///
    /// Returns [`None`] if the token iterator is exhausted before a significant token is found.
    pub fn next_significant_token(&mut self) -> Option<Token> {
        self.cursor
            .next_token_until(|token| !matches!(token, Token::WhiteSpace(..) | Token::Comment(..)))
            .cloned()
    }

    /// Finds the next significant token in the token stream, consuming all insignificant tokens
    /// along the way and without consuming the significant token, resulting the iterator pointing
    /// at the significant token.
    ///
    /// A token is considered significant if it is not a whitespace or a comment.
    ///
    /// Returns [`None`] if the token iterator is exhausted before a significant token is found.
    pub fn peek_significant_token(&mut self) -> Option<Token> {
        self.cursor
            .forward_until(|token| !matches!(token, Token::WhiteSpace(..) | Token::Comment(..)))
            .cloned()
    }

    /// Expects the next significant token to be [`Identifier`] and returns it if it is.
    /// Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_identifier(&mut self, handler: &impl ErrorHandler<Error>) -> Option<Identifier> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Identifier(identifier)) => Some(identifier),
            found => {
                handler.recieve(Error::IdentifierExpected(IdentifierExpected { found }));
                None
            }
        }
    }

    /// Expects the next significant token to be [`Punctuation`] with the given character and
    /// returns it if it is. Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_punctuation(
        &mut self,
        expected: char,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<Punctuation> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation == expected => {
                Some(punctuation)
            }
            Some(token) => {
                handler.recieve(
                    PunctuationExpected {
                        expected,
                        found: Some(token),
                    }
                    .into(),
                );
                None
            }
            None => {
                handler.recieve(
                    PunctuationExpected {
                        expected,
                        found: None,
                    }
                    .into(),
                );
                None
            }
        }
    }

    fn delimiter_predicate<const CHAR: char>(token: &Token) -> bool {
        let Some(punctuation) = token.as_punctuation() else {
            return false
        };

        punctuation.punctuation == CHAR
    }

    /// Skips all tokens until the given predicate returns `true`.
    ///
    /// The function ignores predicate check for the tokens that are enclosed in a pair of
    /// delimiters such as parentheses, brackets, braces, etc.
    ///
    /// The function positions the cursor after the token that caused the predicate to return
    /// `true`.
    pub fn next_token_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<Token> {
        while let Some(token) = self.next_token() {
            match token {
                // found ( or [ or {, skip until the corresponding closing delimiter
                Token::Punctuation(punc) if punc.punctuation == '(' => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(punc) if punc.punctuation == '[' => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(punc) if punc.punctuation == '{' => {
                    self.next_token_until(Self::delimiter_predicate::<'}'>);
                }
                _ => {
                    if predicate(&token) {
                        return Some(token);
                    }
                }
            }
        }

        None
    }

    /// Skips all tokens until the given predicate returns `true`.
    ///
    /// The function ignores predicate check for the tokens that are enclosed in a pair of
    /// delimiters such as parentheses, brackets, braces, etc.
    ///
    /// The function positions the cursor at the token that caused the predicate to return `true`.
    pub fn forward_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<Token> {
        while let Some(token) = self.peek_token() {
            match token {
                // found ( or [ or {, skip until the corresponding closing delimiter
                Token::Punctuation(punc) if punc.punctuation == '(' => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(punc) if punc.punctuation == '[' => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(punc) if punc.punctuation == '{' => {
                    self.next_token_until(Self::delimiter_predicate::<'}'>);
                }
                _ => {
                    if predicate(&token) {
                        return Some(token);
                    }
                }
            }

            self.next_token();
        }

        None
    }

    /// Tries to parse a syntax tree that rolls back the cursor if it fails.
    ///
    /// # Parameters
    /// - `f`: The function to parse the syntax tree. The function should return `None` if it fails.
    pub fn try_parse<T>(&mut self, f: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let starting_cursor = self.cursor;

        if let Some(value) = f(self) {
            Some(value)
        } else {
            self.cursor = starting_cursor;
            None
        }
    }
}
#[cfg(test)]
mod tests;
