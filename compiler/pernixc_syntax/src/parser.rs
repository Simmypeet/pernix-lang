use getset::Getters;
use pernixc_lexical::{
    token::{IdentifierToken, Token},
    token_stream::{CursorPosition, TokenStreamCursor},
};
use thiserror::Error;

use crate::errors::SyntacticError;

/// Represents a state machine that parses a stream of tokens into a syntax tree.
///
/// The parser takes a [`TokenStreamCursor`] as input and produces various kinds of syntax trees as
/// result. The parser provides various `parse_*` methods that can be used to parse different kinds
/// of syntax trees.
#[derive(Debug, Getters)]
pub struct Parser<'a> {
    pub(super) cursor:  TokenStreamCursor<'a>,
    /// Gets the list of errors that have been produced by the parser.
    #[get = "pub"]
    errors:             Vec<SyntacticError>,
    pub produce_errors: bool,
}

/// Indicates that the given token stream cursor is not pointing at a valid position when creating a
/// [`Parser`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The given token stream cursor must be pointing at a valid position")]
pub struct InvalidTokenStreamCursorError;

impl<'a> Parser<'a> {
    /// Creates a new parser that will start parsing the token stream from the given cursor.
    ///
    /// The given `cursor` must be a cursor pointing at the valid position of the token stream.
    pub fn new(cursor: TokenStreamCursor<'a>) -> Result<Self, InvalidTokenStreamCursorError> {
        if !matches!(cursor.position(), CursorPosition::Valid(..)) {
            return Err(InvalidTokenStreamCursorError);
        }

        Ok(Self {
            cursor,
            errors: Vec::new(),
            produce_errors: true,
        })
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

    /// Expects the next significant token to be [`IdentifierToken`] and returns it if it is.
    /// Otherwise, returns [`None`].
    pub fn expect_identifier(&mut self) -> Option<&'a IdentifierToken> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Identifier(identifier)) => Some(identifier),
            Some(token) => {
                self.report_error(SyntacticError::IdentifierExpected(Some(token.clone())));
                None
            }
            None => {
                self.report_error(SyntacticError::IdentifierExpected(None));
                None
            }
        }
    }

    /// Stores the given error into the error list of the parser.
    ///
    /// The error is discarded if the parser is not configured to produce errors.
    pub fn report_error(&mut self, error: SyntacticError) {
        if self.produce_errors {
            self.errors.push(error);
        }
    }

    /// Takes the list of errors that have been produced by the parser.
    ///
    /// The list of errors is cleared after this method is called.
    pub fn take_errors(&mut self) -> Vec<SyntacticError> { std::mem::take(&mut self.errors) }
}
