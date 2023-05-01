//! Contains the [`Parser`] type that is used to parse a stream of tokens into a syntax tree.

use std::fmt::Debug;

use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Punctuation, Token},
    token_stream::{Cursor, CursorPosition},
};
use pernixc_system::error_handler::ErrorHandler;
use thiserror::Error;

use crate::{
    error::{Error, IdentifierExpected, PunctuationExpected},
    syntax_tree::ConnectedList,
};

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
    pub fn next_token(&mut self) -> Option<&'a Token> { self.cursor.next_token() }

    /// Returns the next token in the token stream without consuming it.
    ///
    /// If the token iterator is exhausted, this method returns `None`.
    pub fn peek_token(&mut self) -> Option<&'a Token> { self.cursor.read() }

    /// Finds the next significant token in the token stream, consuming all insignificant tokens
    /// along the way and consuming the significant token as well.
    ///
    /// A token is considered significant if it is not a whitespace or a comment.
    ///
    /// Returns [`None`] if the token iterator is exhausted before a significant token is found.
    pub fn next_significant_token(&mut self) -> Option<&'a Token> {
        self.cursor
            .next_token_until(|token| !matches!(token, Token::WhiteSpace(..) | Token::Comment(..)))
    }

    /// Finds the next significant token in the token stream, consuming all insignificant tokens
    /// along the way and without consuming the significant token, resulting the iterator pointing
    /// at the significant token.
    ///
    /// A token is considered significant if it is not a whitespace or a comment.
    ///
    /// Returns [`None`] if the token iterator is exhausted before a significant token is found.
    pub fn peek_significant_token(&mut self) -> Option<&'a Token> {
        self.cursor
            .forward_until(|token| !matches!(token, Token::WhiteSpace(..) | Token::Comment(..)))
    }

    /// Expects the next significant token to be [`Identifier`] and returns it if it is.
    /// Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_identifier(
        &mut self,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<&'a Identifier> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Identifier(identifier)) => Some(identifier),
            Some(token) => {
                handler.recieve(
                    IdentifierExpected {
                        found: Some(token.clone()),
                    }
                    .into(),
                );
                None
            }
            None => {
                handler.recieve(IdentifierExpected { found: None }.into());
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
    ) -> Option<&'a Punctuation> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation == expected => {
                Some(punctuation)
            }
            Some(token) => {
                handler.recieve(
                    PunctuationExpected {
                        expected,
                        found: Some(token.clone()),
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
    pub fn next_token_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<&'a Token> {
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
                    if predicate(token) {
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
    pub fn forward_until(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<&'a Token> {
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
                    if predicate(token) {
                        return Some(token);
                    }
                }
            }

            self.next_token();
        }

        None
    }

    /// Parses a list of items that are separated by a particular separator.
    ///
    /// This function is useful for parsing patterns of elements that are separated by a single
    /// character, such as comma-separated lists of expressions; where the list is enclosed in
    /// parentheses, brackets, or braces.
    pub(super) fn parse_enclosed_list<T: Debug>(
        &mut self,
        delimiter: char,
        separator: char,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
        handler: &impl ErrorHandler<Error>,
    ) -> Option<(Option<ConnectedList<T, Punctuation>>, Punctuation)> {
        let mut first = None;
        let mut rest = Vec::new();
        let mut trailing_separator = None;

        // check for empty list
        match self.peek_significant_token() {
            Some(Token::Punctuation(punc)) if punc.punctuation == delimiter => {
                self.next_token();
                return Some((None, punc.clone()));
            }
            None => handler.recieve(
                PunctuationExpected {
                    expected: delimiter,
                    found: None,
                }
                .into(),
            ),
            _ => (),
        };

        if let Some(value) = parse_item(self) {
            first = Some(value);
        } else {
            let token = self.forward_until(|token| match token {
                Token::Punctuation(punc) => {
                    punc.punctuation == delimiter || punc.punctuation == separator
                }
                _ => false,
            });

            // if found delimiter, return empty list
            if let Some(Token::Punctuation(token)) = token {
                if token.punctuation == delimiter {
                    self.next_token();
                    return Some((None, token.clone()));
                }
            }
        }

        let delimiter = loop {
            match self.peek_significant_token() {
                Some(Token::Punctuation(separator_token))
                    if separator_token.punctuation == separator =>
                {
                    // eat the separator
                    self.next_token();

                    match self.peek_significant_token() {
                        Some(Token::Punctuation(delimiter_token))
                            if delimiter_token.punctuation == delimiter =>
                        {
                            // eat the delimiter
                            self.next_token();

                            trailing_separator = Some(separator_token.clone());
                            break delimiter_token;
                        }
                        _ => (),
                    }

                    parse_item(self).map_or_else(
                        || {
                            self.forward_until(|token| {
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
                token => {
                    handler.recieve(
                        PunctuationExpected {
                            expected: delimiter,
                            found: token.cloned(),
                        }
                        .into(),
                    );
                    return None;
                }
            }
        };

        Some((
            first.map(|first| ConnectedList {
                first,
                rest,
                trailing_separator,
            }),
            delimiter.clone(),
        ))
    }
}

#[cfg(test)]
mod tests;
