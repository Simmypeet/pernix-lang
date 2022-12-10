pub mod abstract_syntax_tree;
pub mod error;

use abstract_syntax_tree::{declaration::Declaration, PositiionWrapper};
use error::{Context, Error};
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

/// A struct representation of a Pernix source code program.
pub struct Program<'a> {
    declarations: Vec<PositiionWrapper<Declaration<'a>>>,
}

impl<'a> Program<'a> {
    /// Returns a reference to the declarations of this [`Program`].
    pub fn declarations(&self) -> &[PositiionWrapper<Declaration>] {
        self.declarations.as_ref()
    }
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

    fn parenthesis_delimiter_predicate<const C: char>(
        token: &Token<'a>,
    ) -> bool {
        matches!(token.token_kind(), TokenKind::Punctuator(c) if *c == C)
    }

    // skips the tokens until the predicate returns true. The parser also
    // skips the tokens inside pairs of brackets and parenthesis
    pub(crate) fn skip_to(&mut self, predicate: impl Fn(&Token<'a>) -> bool) {
        // keep skipping tokens until the predicate returns true
        while !predicate(self.peek())
            && !matches!(self.peek().token_kind(), TokenKind::EndOfFile)
        {
            // if found open bracket or open parenthesis, skip to the closing
            // bracket or parenthesis
            if matches!(
                self.peek().token_kind(),
                TokenKind::Punctuator('(')
                    | TokenKind::Punctuator('{')
                    | TokenKind::Punctuator('[')
            ) {
                let predicate = match self.peek().token_kind() {
                    TokenKind::Punctuator('(') => {
                        Self::parenthesis_delimiter_predicate::<')'>
                    }
                    TokenKind::Punctuator('{') => {
                        Self::parenthesis_delimiter_predicate::<'}'>
                    }
                    TokenKind::Punctuator('[') => {
                        Self::parenthesis_delimiter_predicate::<']'>
                    }
                    _ => unreachable!(),
                };

                self.next();
                self.skip_to(predicate);
            }
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
    ) -> Option<PositiionWrapper<Declaration<'a>>> {
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

    /// Parses the current token stream as a namespace declaration
    ///
    /// Namespace_Declaration:
    ///    `namespace` Qualifier `{` Declaration* `}`
    pub fn parse_namespace_declaration(
        &mut self,
    ) -> Option<PositiionWrapper<Declaration<'a>>> {
        // move to the first significant token
        self.move_to_significant();
        let starting_position = self.peek().position_range().start;

        // expect the `namespace` keyword
        if !matches!(
            self.next().token_kind(),
            TokenKind::Keyword(Keyword::Namespace)
        ) {
            return self.append_error(Error::KeywordExpected {
                expected_keyword: Keyword::Namespace,
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        let namespace_name = self.parse_qualifier()?;

        // expect the opening curly bracket
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('{')
        ) {
            return self.append_error(Error::PunctuatorExpected {
                expected_punctuator: '{',
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        // loop through all the declarations
        let mut declarations = Vec::new();

        // move to the next significant token
        self.move_to_significant();

        // loop through all the declarations until closing curly bracket or
        // EOF is found
        while !matches!(
            self.peek().token_kind(),
            TokenKind::Punctuator('}') | TokenKind::EndOfFile
        ) {
            let declaration = match self.peek().token_kind() {
                TokenKind::Keyword(Keyword::Namespace) => {
                    self.parse_namespace_declaration()
                }
                TokenKind::Keyword(Keyword::Using) => {
                    self.parse_using_declaration()
                }
                _ => self.append_error(Error::UnexpectedToken {
                    context: Context::Namespace,
                    found_token: self.peek().clone(),
                    source_reference: self.source_code(),
                }),
            };

            if let Some(declaration) = declaration {
                declarations.push(declaration);
            } else {
                // skip to the next available declaration all delimeter
                self.skip_to(|token| {
                    matches!(
                        token.token_kind(),
                        TokenKind::Keyword(Keyword::Namespace)
                            | TokenKind::Keyword(Keyword::Using)
                            | TokenKind::Punctuator('}')
                    )
                });
            }

            // move to the next significant token
            self.move_to_significant();
        }

        // expect the closing curly bracket
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('}')
        ) {
            return self.append_error(Error::PunctuatorExpected {
                expected_punctuator: '}',
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        Some(PositiionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: Declaration::NamespaceDeclaration {
                namespace_name,
                declarations,
            },
        })
    }

    /// Parses all token stream as a program
    ///
    /// Program:
    ///  Declaration*
    pub fn parse_program(&mut self) -> Option<Program<'a>> {
        let mut program = Program {
            declarations: Vec::new(),
        };

        // move to the next significant token
        self.move_to_significant();

        while !matches!(self.peek().token_kind(), TokenKind::EndOfFile) {
            let declaration = match self.peek().token_kind() {
                TokenKind::Keyword(Keyword::Namespace) => {
                    self.parse_namespace_declaration()
                }
                TokenKind::Keyword(Keyword::Using) => {
                    self.parse_using_declaration()
                }
                _ => self.append_error(Error::UnexpectedToken {
                    context: Context::Program,
                    found_token: self.peek().clone(),
                    source_reference: self.source_code(),
                }),
            };

            if let Some(declaration) = declaration {
                program.declarations.push(declaration);
            } else {
                // skip to the next available declaration all delimeter
                self.skip_to(|token| {
                    matches!(
                        token.token_kind(),
                        TokenKind::Keyword(Keyword::Namespace)
                            | TokenKind::Keyword(Keyword::Using)
                    )
                });
            }

            // move to the next significant token
            self.move_to_significant();
        }

        Some(program)
    }
}

#[cfg(test)]
mod test {
    use pernix_lexer::token::TokenKind;
    use pernix_project::source_code::{SourceCode, SourcePosition};

    use crate::{
        abstract_syntax_tree::{declaration::Declaration, PositiionWrapper},
        error::Error,
        Parser,
    };

    // Checks if the parser can parse a program
    #[test]
    fn parse_program_test() {
        let source = "using Math; namespace Simmypeet.Program {}";
        let source_code = SourceCode::new(source.to_string(), String::new());
        let mut parser = Parser::new(&source_code);

        let program = parser.parse_program();

        assert!(program.is_some());

        let program = program.unwrap();

        assert_eq!(program.declarations.len(), 2);

        match &program.declarations[0].value {
            Declaration::UsingDeclaration { namespace_name } => {
                assert_eq!(namespace_name.value, "Math");
            }
            _ => panic!("Expected using declaration"),
        }

        match &program.declarations[1].value {
            Declaration::NamespaceDeclaration {
                namespace_name,
                declarations,
            } => {
                assert_eq!(namespace_name.value, "Simmypeet.Program");
                assert_eq!(declarations.len(), 0);
            }
            _ => panic!("Expected namespace declaration"),
        }
    }

    // Checks if the parser can parse a namespace declaration with error handling
    #[test]
    fn parse_namespace_declaration_with_error_handling_test() {
        let source_code = SourceCode::new(
            "namespace foo { using; using bar; }".to_string(),
            String::new(),
        );
        let mut parser = Parser::new(&source_code);

        let namespace_declaration = parser.parse_namespace_declaration();

        assert!(namespace_declaration.is_some());

        let namespace_declaration = namespace_declaration.unwrap();

        assert_eq!(
            namespace_declaration.position.start,
            SourcePosition { line: 1, column: 1 }
        );
        assert_eq!(
            namespace_declaration.position.end,
            SourcePosition {
                line: 1,
                column: 36
            }
        );

        match namespace_declaration.value {
            Declaration::NamespaceDeclaration {
                namespace_name,
                declarations,
            } => {
                assert_eq!(namespace_name.value, "foo");
                assert_eq!(declarations.len(), 1);

                // there should be only one declaration as the first using delcaration
                // is invalid

                match &declarations[0].value {
                    Declaration::UsingDeclaration { namespace_name } => {
                        assert_eq!(namespace_name.value, "bar");
                    }
                    _ => panic!("Unexpected declaration"),
                }

                // expect one IdentifierExpected error
                let error = parser.pop_errors();

                assert_eq!(error.len(), 1);

                match &error[0] {
                    Error::IdentifierExpected {
                        found_token,
                        source_reference: _,
                    } => {
                        assert!(matches!(
                            found_token.token_kind(),
                            TokenKind::Punctuator(';')
                        ));
                    }
                    _ => panic!("Unexpected error"),
                }
            }
            _ => panic!("Unexpected declaration"),
        }
    }

    // Checks if the parser can parse a namespace declaration
    #[test]
    fn parse_namespace_declaration_test() {
        let source_code = SourceCode::new(
            "namespace foo { using bar; }".to_string(),
            String::new(),
        );
        let mut parser = Parser::new(&source_code);

        let namespace_declaration = parser.parse_namespace_declaration();

        assert!(namespace_declaration.is_some());

        let namespace_declaration = namespace_declaration.unwrap();

        assert_eq!(
            namespace_declaration.position.start,
            SourcePosition { line: 1, column: 1 }
        );
        assert_eq!(
            namespace_declaration.position.end,
            SourcePosition {
                line: 1,
                column: 29
            }
        );

        match namespace_declaration.value {
            Declaration::NamespaceDeclaration {
                namespace_name,
                declarations,
            } => {
                assert_eq!(namespace_name.value, "foo");
                assert_eq!(declarations.len(), 1);

                let using_declaration = declarations.get(0).unwrap();

                match &using_declaration.value {
                    Declaration::UsingDeclaration { namespace_name } => {
                        assert_eq!(namespace_name.value, "bar");
                    }
                    _ => panic!("Expected a using declaration"),
                }
            }
            _ => panic!("Expected a namespace declaration"),
        };

        assert!(parser.pop_errors().len() == 0);
    }

    // Checks if the parser can skip the tokens based on the given predicate
    #[test]
    fn skip_to_test() {
        let source_code =
            SourceCode::new("() {{} ([])} }".to_string(), String::new());
        let mut parser = Parser::new(&source_code);

        // the parser should skip to the last curly bracket
        parser.skip_to(|token| {
            matches!(token.token_kind(), TokenKind::Punctuator('}'))
        });

        assert!(matches!(
            parser.peek().token_kind(),
            TokenKind::Punctuator('}')
        ));
        assert_eq!(
            parser.peek().position_range().start,
            SourcePosition {
                line: 1,
                column: 14
            }
        );
        assert_eq!(
            parser.peek().position_range().end,
            SourcePosition {
                line: 1,
                column: 15
            }
        );

        // the token doesn't exist the parser should stop at the end of the file
        parser.skip_to(|token| {
            matches!(token.token_kind(), TokenKind::Punctuator('.'))
        });

        assert!(matches!(parser.peek().token_kind(), TokenKind::EndOfFile));
        assert_eq!(
            parser.peek().position_range().start,
            SourcePosition {
                line: 1,
                column: 15
            }
        );
        assert_eq!(
            parser.peek().position_range().end,
            SourcePosition {
                line: 1,
                column: 16
            }
        );
    }

    // Checks if the parser can parse a using declaration
    #[test]
    fn parse_using_declaration_test() {
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
                        source_reference: _
                    } if found_token.lexeme() == ""
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
                        source_reference: _
                    } if found_token.lexeme() == " "
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
                        source_reference: _
                    } if found_token.lexeme() == ""
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
