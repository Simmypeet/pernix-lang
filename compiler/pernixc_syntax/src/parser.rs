//! Contains the [`Parser`] type that is used to parse a stream of tokens into a syntax tree.

use std::fmt::Debug;

use getset::Getters;
use pernixc_lexical::{
    token::{Identifier, Punctuation, Token},
    token_stream::{Cursor, CursorPosition},
};
use thiserror::Error;

use crate::{
    errors::{IdentifierExpected, PunctuationExpected, SyntacticError},
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
    /// Gets the list of errors that have been produced by the parser.
    #[get = "pub"]
    pub(crate) errors: Vec<SyntacticError>,

    /// The flag that indicates whether the parser should produce errors that are encountered
    /// during parsing.
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
    /// # Errors
    /// - [`InvalidTokenStreamCursorError`] is returned if the given cursor is not pointing at a
    ///   valid position.
    pub fn new(cursor: Cursor<'a>) -> Result<Self, InvalidTokenStreamCursorError> {
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

    /// Expects the next significant token to be [`Identifier`] and returns it if it is.
    /// Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_identifier(&mut self) -> Option<&'a Identifier> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Identifier(identifier)) => Some(identifier),
            Some(token) => {
                self.report_error(
                    IdentifierExpected {
                        found: Some(*token),
                    }
                    .into(),
                );
                None
            }
            None => {
                self.report_error(IdentifierExpected { found: None }.into());
                None
            }
        }
    }

    /// Expects the next significant token to be [`Punctuation`] with the given character and
    /// returns it if it is. Otherwise, returns [`None`].
    ///
    /// The error is reported to the error list of the parser if the parser is configured to produce
    /// errors.
    pub fn expect_punctuation(&mut self, expected: char) -> Option<&'a Punctuation> {
        let token = self.next_significant_token();

        match token {
            Some(Token::Punctuation(punctuation)) if punctuation.punctuation == expected => {
                Some(punctuation)
            }
            Some(token) => {
                self.report_error(
                    PunctuationExpected {
                        expected,
                        found: Some(*token),
                    }
                    .into(),
                );
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
            Token::Punctuation(Punctuation {
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
                Token::Punctuation(Punctuation {
                    punctuation: '(', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(Punctuation {
                    punctuation: '[', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(Punctuation {
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
                Token::Punctuation(Punctuation {
                    punctuation: '(', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<')'>);
                }
                Token::Punctuation(Punctuation {
                    punctuation: '[', ..
                }) => {
                    self.next_token_until(Self::delimiter_predicate::<']'>);
                }
                Token::Punctuation(Punctuation {
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
    ///
    /// This function is useful for parsing patterns of elements that are separated by a single
    /// character, such as comma-separated lists of expressions; where the list is enclosed in
    /// parentheses, brackets, or braces.
    pub(super) fn parse_enclosed_list<T: Debug>(
        &mut self,
        delimiter: char,
        separator: char,
        mut parse_item: impl FnMut(&mut Self) -> Option<T>,
    ) -> Option<(Option<ConnectedList<T, Punctuation>>, Punctuation)> {
        let mut first = None;
        let mut rest = Vec::new();

        // check for empty list
        match self.peek_significant_token() {
            Some(Token::Punctuation(punc)) if punc.punctuation == delimiter => {
                self.next_token();
                return Some((None, *punc));
            }
            None => self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                expected: delimiter,
                found: None,
            })),
            _ => (),
        };

        if let Some(value) = parse_item(self) {
            first = Some(value);
        } else {
            let token = self.forward_until(|token| match token {
                Token::Punctuation(Punctuation { punctuation, .. }) => {
                    *punctuation == delimiter || *punctuation == separator
                }
                _ => false,
            });

            // if found delimiter, return empty list
            if let Some(Token::Punctuation(token)) = token {
                if token.punctuation == delimiter {
                    self.next_token();
                    return Some((None, *token));
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

                    // add value to the list
                    parse_item(self).map_or_else(
                        || {
                            // skip to either the next separator or the delimiter
                            self.forward_until(|token| {
                                matches!(
                                    token,
                                    Token::Punctuation(Punctuation {
                                        punctuation,
                                        ..
                                    }) if *punctuation == delimiter || *punctuation == separator
                                )
                            });
                        },
                        |value| {
                            if first.is_none() {
                                first = Some(value);
                            } else {
                                rest.push((*separator_token, value));
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
                    self.report_error(SyntacticError::PunctuationExpected(PunctuationExpected {
                        expected: delimiter,
                        found: token.copied(),
                    }));
                    return None;
                }
            }
        };

        Some((first.map(|first| ConnectedList { first, rest }), *delimiter))
    }

    /// Tries to parse a syntax tree node with rollback on failure.
    pub fn try_parse<T>(&mut self, parse: impl FnOnce(&mut Self) -> Option<T>) -> Option<T> {
        let produce_errors_record = self.produce_errors;
        let cursor_position = self.cursor.position();

        // disable error production
        if produce_errors_record {
            self.produce_errors = false;
        }

        let result = parse(self);

        if result.is_none() {
            // restore the cursor position
            self.cursor.set_position(cursor_position);

            // restore the error production
            if produce_errors_record {
                self.produce_errors = true;
            }
        }

        result
    }

    /// Tries to parse a syntax tree that has an ambiguity between two possible syntaxes.
    pub(super) fn ambiguity_resolution<T1, T2>(
        &mut self,
        first: impl FnOnce(&mut Self) -> Option<T1>,
        second: impl FnOnce(&mut Self) -> Option<T2>,
    ) -> Option<FirstOrSecond<T1, T2>> {
        // count the number of significant tokens that are eaten by each parser
        fn count_fn(original_cursor: &mut Cursor, comparing_cursor: &Cursor) -> usize {
            let mut significant_token_eaten = 0;
            let starting_cursor_position = original_cursor.position();
            while original_cursor.position() < comparing_cursor.position() {
                if let Some(token) = original_cursor.next_token() {
                    if !matches!(token, Token::WhiteSpace(..) | Token::Comment(..)) {
                        significant_token_eaten += 1;
                    }
                }
            }
            original_cursor.set_position(starting_cursor_position);
            significant_token_eaten
        }

        let mut first_parser = Parser {
            cursor: self.cursor,
            produce_errors: self.produce_errors,
            errors: Vec::new(),
        };
        let mut second_parser = Parser {
            cursor: self.cursor,
            produce_errors: self.produce_errors,
            errors: Vec::new(),
        };

        let (first_result, second_result) = (first(&mut first_parser), second(&mut second_parser));
        let (first_token_eaten, second_token_eaten) = (
            count_fn(&mut self.cursor, &first_parser.cursor),
            count_fn(&mut self.cursor, &second_parser.cursor),
        );

        match (first_result, second_result) {
            (Some(first_result), None) if second_token_eaten + 1 == first_token_eaten => {
                // choose first
                self.cursor = first_parser.cursor;
                self.errors.append(&mut first_parser.errors);
                Some(FirstOrSecond::First(first_result))
            }
            (None, Some(second_result)) if first_token_eaten + 1 == second_token_eaten => {
                // choose second
                self.cursor = second_parser.cursor;
                self.errors.append(&mut second_parser.errors);
                Some(FirstOrSecond::Second(second_result))
            }
            (first_result, second_result) => {
                // choose the one that has the most significant tokens eaten
                if first_token_eaten > second_token_eaten {
                    self.cursor = first_parser.cursor;
                    self.errors.append(&mut first_parser.errors);
                    first_result.map(FirstOrSecond::First)
                } else {
                    self.cursor = second_parser.cursor;
                    self.errors.append(&mut second_parser.errors);
                    second_result.map(FirstOrSecond::Second)
                }
            }
        }
    }
}

pub(super) enum FirstOrSecond<T1, T2> {
    First(T1),
    Second(T2),
}

#[cfg(test)]
mod tests;
