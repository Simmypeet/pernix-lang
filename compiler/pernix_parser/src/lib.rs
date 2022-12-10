pub mod abstract_syntax_tree;
pub mod error;

use abstract_syntax_tree::{declaration::Declaration, PositiionWrapper};
use error::Error;
use pernix_lexer::{
    token::{Keyword, Token, TokenKind},
    Lexer,
};
use pernix_project::source_code::SourceCode;

/// Represents a struct that parases the given token stream into abstract syntax
/// trees
pub struct Parser<'a> {
    // The lexer that is used to generate the token stream
    lexer: Lexer<'a>,
    // the accumulated tokens that have been tokenized by the lexer so far
    accumulated_tokens: Vec<Token<'a>>,
    // the accumulated errors that have been found during parsing so far
    accumulated_errors: Vec<Error<'a>>,
    // The current position in the source code
    current_position: usize,
    // Flag that indicates whether the parser should produce errors into the
    // list or not
    produce_errors: bool,
}

pub struct Program {
    pub declarations: Vec<Declaration>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser instance from the given source code
    pub fn new(source_code: &'a SourceCode) -> Self {
        let mut lexer = Lexer::new(source_code);
        let mut accumulated_errors = Vec::new();

        // get the first token
        let accumulated_tokens = vec![{
            let token_return;
            loop {
                match lexer.lex() {
                    Ok(token) => {
                        token_return = token;
                        break;
                    }
                    Err(err) => {
                        accumulated_errors.push(Error::LexicalError(err));
                    }
                }
            }
            token_return
        }];

        Self {
            lexer,
            accumulated_tokens,
            accumulated_errors,
            current_position: 0,
            produce_errors: true,
        }
    }

    /// Gets the source code that is being parsed.
    pub fn source_code(&self) -> &'a SourceCode {
        self.lexer.source_code()
    }

    // Gets the current token stream and moves the current position forward
    pub(crate) fn next(&mut self) -> &Token<'a> {
        // need to generate more tokens
        if self.current_position == self.accumulated_tokens.len() - 1 {
            let new_token;
            loop {
                match self.lexer.lex() {
                    Ok(token) => {
                        new_token = token;
                        break;
                    }
                    Err(err) => {
                        self.accumulated_errors.push(Error::LexicalError(err));
                    }
                }
            }

            // append the new token to the accumulated tokens
            self.accumulated_tokens.push(new_token);
        }
        // panic if the current position is greater than or equal the length
        else if self.current_position >= self.accumulated_tokens.len() {
            panic!("current position is greater than or equal the length of the accumulated tokens");
        }

        // increment the current position
        self.current_position += 1;

        &self.accumulated_tokens[self.current_position - 1]
    }

    // Moves the current position to the next significant token and emits it
    pub(crate) fn next_significant(&mut self) -> &Token<'a> {
        self.move_to_significant();
        self.next()
    }

    /// Moves the current position to the next significant token
    pub(crate) fn move_to_significant(&mut self) {
        while !self.peek().is_significant_token() {
            self.next();
        }
    }

    // Gets the current token without moving the current position forward
    fn peek(&self) -> &Token<'a> {
        &self.accumulated_tokens[self.current_position]
    }

    // Gets the token back by one position without moving the current position
    fn peek_back(&self) -> &Token<'a> {
        &self.accumulated_tokens[self.current_position - 1]
    }

    /// Clears the accumulated errors and returns them
    pub fn pop_errors(&mut self) -> Vec<Error<'a>> {
        let mut errors = Vec::new();
        std::mem::swap(&mut errors, &mut self.accumulated_errors);
        errors
    }

    // Appends the given error to the list of accumulated errors if the
    // `produce_errors` flag is set to true. Moreover, it also rolls the parser
    // position back to the `rollback_position`.
    #[must_use]
    pub fn append_error<T>(&mut self, error: Error<'a>) -> Option<T> {
        if self.produce_errors {
            self.accumulated_errors.push(error);
        }

        None
    }

    /// Parses the current token stream as a qualifier string
    ///
    /// Qualifier:
    ///     `IDENTIFIER` |
    ///     `IDENTIFIER` `.` Qualifier
    pub fn parse_qualifier(&mut self) -> Option<PositiionWrapper<String>> {
        self.move_to_significant();
        let starting_position = self.peek().position_range().start;

        // the string to return
        let mut string = String::new();

        // expect the first identifier
        if let TokenKind::Identifier = self.next().token_kind() {
            string.push_str(self.peek_back().lexeme());
        } else {
            return self.append_error(Error::IdentifierExpected {
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        // found additional scopes
        while matches!(self.peek().token_kind(), TokenKind::Punctuator('.')) {
            // eat the dot
            self.next();

            string.push('.');

            // expect the next identifier
            if let TokenKind::Identifier = self.next().token_kind() {
                string.push_str(self.peek_back().lexeme());
            } else {
                return self.append_error(Error::IdentifierExpected {
                    found_token: self.peek_back().clone(),
                    source_reference: self.source_code(),
                });
            }
        }

        Some(PositiionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: string,
        })
    }

    /// Parses the current token stream as a using declaration
    ///
    /// Using_Declaration:
    ///   `using` Qualifier `;`
    pub fn parse_using_declaration(
        &mut self,
    ) -> Option<PositiionWrapper<Declaration>> {
        // move to the first significant token
        self.move_to_significant();
        let starting_position = self.peek().position_range().start;

        // expect the `using` keyword
        if !matches!(
            self.next().token_kind(),
            TokenKind::Keyword(Keyword::Using)
        ) {
            return self.append_error(Error::KeywordExpected {
                expected_keyword: Keyword::Using,
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        let qualifier = self.parse_qualifier()?;

        // expect the semicolon
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(';')
        ) {
            return self.append_error(Error::PunctuatorExpected {
                expected_punctuator: ';',
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        Some(PositiionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: Declaration::UsingDeclaration {
                namespace_name: qualifier,
            },
        })
    }
}

#[cfg(test)]
mod test {
    use pernix_lexer::token::TokenKind;
    use pernix_project::source_code::{SourceCode, SourcePosition};

    use crate::{
        abstract_syntax_tree::{declaration::Declaration, PositiionWrapper},
        Parser, error::Error,
    };

    // Checks if the parser can parse a using declaration
    #[test]
    fn parse_using_declaration() {
        let source_code = SourceCode::new(
            "using Simmypeet.Program;\nusing Another;\nusing Missing.SemiColon"
                .to_string(),
            String::new(),
        );

        let mut parser = Parser::new(&source_code);

        // parse the first using declaration
        {
            let using_declaration = parser.parse_using_declaration().unwrap();

            assert_eq!(
                using_declaration.value,
                Declaration::UsingDeclaration {
                    namespace_name: PositiionWrapper {
                        position: SourcePosition { line: 1, column: 7 }
                            ..SourcePosition {
                                line: 1,
                                column: 24
                            },
                        value: "Simmypeet.Program".to_string(),
                    }
                }
            );

            assert_eq!(
                using_declaration.position.start,
                SourcePosition { line: 1, column: 1 }
            );

            assert_eq!(
                using_declaration.position.end,
                SourcePosition {
                    line: 1,
                    column: 25
                }
            );
        }

        // parse the second using declaration
        {
            let using_declaration = parser.parse_using_declaration().unwrap();

            assert_eq!(
                using_declaration.value,
                Declaration::UsingDeclaration {
                    namespace_name: PositiionWrapper {
                        position: SourcePosition { line: 2, column: 7 }
                            ..SourcePosition {
                                line: 2,
                                column: 14
                            },
                        value: "Another".to_string(),
                    }
                }
            );

            assert_eq!(
                using_declaration.position.start,
                SourcePosition { line: 2, column: 1 }
            );

            assert_eq!(
                using_declaration.position.end,
                SourcePosition {
                    line: 2,
                    column: 15
                }
            );
        }

        // the last using declaration is missing a semicolon
        {
            let using_declaration = parser.parse_using_declaration();

            assert!(using_declaration.is_none());

            // eject the error
            let errors = parser.pop_errors();

            assert_eq!(errors.len(), 1);

            let error = errors.first().unwrap();

            assert!(
                matches!(
                    error,
                    crate::Error::PunctuatorExpected {
                        expected_punctuator: ';',
                        found_token,
                        source_reference
                    } if found_token.lexeme() == ""
                    && *source_reference == &source_code
                    && found_token.position_range().start == SourcePosition {
                        line: 3,
                        column: 24
                    }
                    && found_token.position_range().end == SourcePosition {
                        line: 3,
                        column: 25
                    }
                    && *found_token.token_kind() == TokenKind::EndOfFile
                ),
                "Unexpected error: {:?}",
                error
            );
        }
    }

    // Checks if the parser can parse a qualifier
    #[test]
    fn parse_qualifier_test() {
        let source_code = SourceCode::new(
            "Simmypeet.Program  Another.Simmypeet.Program  Error.Qualifier. Last.Error.Qualifier.".to_string(),
            String::new(),
        );

        let mut parser = Parser::new(&source_code);

        // parse first qualifier
        {
            let qualifier = parser.parse_qualifier().unwrap();

            assert_eq!(qualifier.value, "Simmypeet.Program");

            assert_eq!(
                qualifier.position.start,
                SourcePosition { line: 1, column: 1 }
            );
            assert_eq!(
                qualifier.position.end,
                SourcePosition {
                    line: 1,
                    column: 18
                }
            );
        }

        // parse second qualifier
        {
            let qualifier = parser.parse_qualifier().unwrap();

            assert_eq!(qualifier.value, "Another.Simmypeet.Program");

            assert_eq!(
                qualifier.position.start,
                SourcePosition {
                    line: 1,
                    column: 20
                }
            );
            assert_eq!(
                qualifier.position.end,
                SourcePosition {
                    line: 1,
                    column: 45
                }
            );
        }

        // the next qualifier is incomplete and should be rejected
        {
            assert!(parser.parse_qualifier().is_none());

            // eject the parser's accumulated errors
            let errors = parser.pop_errors();

            assert_eq!(errors.len(), 1);

            let error = errors.first().unwrap();

            assert!(
                matches!(
                    error,
                    Error::IdentifierExpected {
                        found_token,
                        source_reference
                    } if found_token.lexeme() == " "
                    && *source_reference == &source_code
                    && found_token.position_range().start == SourcePosition {
                        line: 1,
                        column: 63
                    }
                    && found_token.position_range().end == SourcePosition {
                        line: 1,
                        column: 64
                    }
                    && *found_token.token_kind() == TokenKind::Space
                ),
                "Unexpected error: {:?}",
                error
            );
        }

        // the last qualifier is also incomplete found eof first
        {
            assert!(parser.parse_qualifier().is_none());

            // eject the parser's accumulated errors
            let errors = parser.pop_errors();

            assert_eq!(errors.len(), 1);

            let error = errors.first().unwrap();

            assert!(
                matches!(
                    error,
                    Error::IdentifierExpected {
                        found_token,
                        source_reference
                    } if found_token.lexeme() == ""
                    && *source_reference == &source_code
                    && found_token.position_range().start == SourcePosition {
                        line: 1,
                        column: 85
                    }
                    && found_token.position_range().end == SourcePosition {
                        line: 1,
                        column: 86
                    }
                    && *found_token.token_kind() == TokenKind::EndOfFile
                ),
                "Unexpected error: {:?}",
                error
            );
        }
    }
}
