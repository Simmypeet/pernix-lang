use getset::Getters;
use pernixc_lexical::{
    token::{IdentifierToken, PunctuationToken, Token},
    token_stream::{CursorPosition, TokenStreamCursor},
};
use thiserror::Error;

use crate::{
    errors::{PunctuationExpected, SyntacticError},
    syntax_tree::ConnectedList,
};

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
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
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

    /// Expects the next significant token to be [`PunctuationToken`] with the given character and
    /// returns it if it is. Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_punctuation(&mut self, expected: char) -> Option<&'a PunctuationToken> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation == expected => {
                Some(punctuation)
            }
            Some(token) => {
                self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                    expected,
                    found: Some(token.clone()),
                }));
                None
            }
            None => {
                self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                    expected,
                    found: None,
                }));
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

    fn delimiter_predicate<const CHAR: char>(token: &Token) -> bool {
        matches!(
            token,
            Token::Punctuation(PunctuationToken {
                punctuation,
                ..
            }) if *punctuation == CHAR
        )
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
                Token::Punctuation(PunctuationToken {
                    punctuation: '(', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(PunctuationToken {
                    punctuation: '[', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(PunctuationToken {
                    punctuation: '{', ..
                }) => {
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
                Token::Punctuation(PunctuationToken {
                    punctuation: '(', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(PunctuationToken {
                    punctuation: '[', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(PunctuationToken {
                    punctuation: '{', ..
                }) => {
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
    pub(super) fn parse_enclosed_list<T>(
        &mut self,
        delimiter: char,
        separator: char,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<(Option<ConnectedList<T, PunctuationToken>>, PunctuationToken)> {
        let mut first = None;
        let mut rest = Vec::new();

        if let Some(value) = parse_item(self) {
            first = Some(value);
        } else {
            let token = self.forward_until(|token| match token {
                Token::Punctuation(PunctuationToken { punctuation, .. }) => {
                    *punctuation == delimiter || *punctuation == separator
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

                    let value = parse_item(self);

                    // add value to the list
                    if let Some(value) = value {
                        if first.is_none() {
                            first = Some(value);
                        } else {
                            rest.push((separator_token.clone(), value));
                        }
                    } else {
                        // skip to either the next separator or the delimiter
                        self.forward_until(|token| {
                            matches!(
                                token,
                                Token::Punctuation(PunctuationToken {
                                    punctuation,
                                    ..
                                }) if *punctuation == delimiter || *punctuation == separator
                            )
                        });
                    }
                }
                Some(Token::Punctuation(delimiter_token))
                    if delimiter_token.punctuation == delimiter =>
                {
                    // eat the delimiter
                    self.next_token();

                    break delimiter_token;
                }
                token => {
                    self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                        expected: delimiter,
                        found:    token.cloned(),
                    }));
                    return None;
                }
            }
        };

        Some((
            first.map(|first| ConnectedList { first, rest }),
            delimiter.clone(),
        ))
    }
}

#[cfg(test)]
mod tests;
