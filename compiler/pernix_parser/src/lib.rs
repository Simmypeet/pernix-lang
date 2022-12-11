pub mod abstract_syntax_tree;
pub mod error;

use abstract_syntax_tree::{
    declaration::Declaration, expression::Expression, PositiionWrapper,
};
use error::{Context, Error};
use pernix_lexer::{
    token::{Keyword, Token, TokenKind},
    Lexer,
};
use pernix_project::source_code::SourceCode;

use crate::abstract_syntax_tree::UnaryOperator;

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
    fn next(&mut self) -> &Token<'a> {
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
    fn next_significant(&mut self) -> &Token<'a> {
        self.move_to_significant();
        self.next()
    }

    /// Moves the current position to the next significant token
    fn move_to_significant(&mut self) {
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
    fn skip_to(&mut self, predicate: impl Fn(&Token<'a>) -> bool) {
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

    /// Parses the current token stream as a qualified name string
    ///
    /// Qualified_Name:
    ///     `IDENTIFIER` |
    ///     `IDENTIFIER` `.` Qualified_Name
    pub fn parse_qualified_name(
        &mut self,
    ) -> Option<PositiionWrapper<&'a str>> {
        self.move_to_significant();
        let starting_position = self.peek().position_range().start;

        // expect the first identifier
        if !matches!(self.next().token_kind(), TokenKind::Identifier) {
            return self.append_error(Error::IdentifierExpected {
                found_token: self.peek_back().clone(),
                source_reference: self.source_code(),
            });
        }

        // found additional scopes
        while matches!(self.peek().token_kind(), TokenKind::Punctuator('.')) {
            // eat the dot
            self.next();

            // expect the next identifier
            if !matches!(self.next().token_kind(), TokenKind::Identifier) {
                return self.append_error(Error::IdentifierExpected {
                    found_token: self.peek_back().clone(),
                    source_reference: self.source_code(),
                });
            }
        }

        Some(PositiionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: &self.source_code().source_code()[starting_position
                .byte_index
                ..self.peek_back().position_range().end.byte_index],
        })
    }

    /// Parses the current token stream as a using_statement
    ///
    /// Using_Statement:
    ///   `using` Qualified_Name `;`
    pub fn parse_using_statement(
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

        let qualified_name = self.parse_qualified_name()?;

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
            value: Declaration::UsingStatement {
                namespace_name: qualified_name,
            },
        })
    }

    /// Parses the current token stream as a namespace declaration
    ///
    /// Namespace_Declaration:
    ///    `namespace` Qualified_Name `{` Declaration* `}`
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

        let namespace_name = self.parse_qualified_name()?;

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
                    self.parse_using_statement()
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
                    self.parse_using_statement()
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

    fn parse_unary_expression(
        &mut self,
    ) -> Option<PositiionWrapper<Expression<'a>>> {
        self.move_to_significant();
        let operator_position_range = self.peek().position_range().clone();

        let operator = match self.next().token_kind() {
            TokenKind::Punctuator('!') => UnaryOperator::LogicalNot,
            TokenKind::Punctuator('-') => UnaryOperator::Minus,
            TokenKind::Punctuator('+') => UnaryOperator::Plus,
            _ => {
                return self.append_error(Error::UnexpectedToken {
                    context: Context::Expression,
                    found_token: self.peek_back().clone(),
                    source_reference: self.source_code(),
                })
            }
        };

        let operand = self.parse_primary_expression()?;

        Some(PositiionWrapper {
            position: operator_position_range.start
                ..self.peek_back().position_range().end,
            value: Expression::UnaryExpression {
                operator: PositiionWrapper {
                    position: operator_position_range,
                    value: operator,
                },
                operand: Box::new(operand),
            },
        })
    }

    fn parse_expression(&mut self) -> Option<PositiionWrapper<Expression<'a>>> {
        todo!();
    }

    // Parses the current token stream as a primary expression
    fn parse_primary_expression(
        &mut self,
    ) -> Option<PositiionWrapper<Expression<'a>>> {
        self.move_to_significant();

        match self.peek().token_kind().clone() {
            TokenKind::Punctuator('!')
            | TokenKind::Punctuator('-')
            | TokenKind::Punctuator('+') => self.parse_unary_expression(),
            TokenKind::LiteralConstant(val) => {
                let position = self.peek().position_range().clone();

                // eat literal token
                self.next();

                Some(PositiionWrapper {
                    position,
                    value: Expression::LiteralExpression(val),
                })
            }
            _ => self.append_error(error::Error::UnexpectedToken {
                context: Context::Expression,
                found_token: self.peek().clone(),
                source_reference: self.source_code(),
            }),
        }
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
            Declaration::UsingStatement { namespace_name } => {
                assert_eq!(namespace_name.value, "Math");
            }
            _ => panic!("Expected using_statement"),
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
            SourcePosition {
                line: 1,
                column: 1,
                byte_index: 0
            }
        );
        assert_eq!(
            namespace_declaration.position.end,
            SourcePosition {
                line: 1,
                column: 36,
                byte_index: 35
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
                    Declaration::UsingStatement { namespace_name } => {
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
            SourcePosition {
                line: 1,
                column: 1,
                byte_index: 0
            }
        );
        assert_eq!(
            namespace_declaration.position.end,
            SourcePosition {
                line: 1,
                column: 29,
                byte_index: 28
            }
        );

        match namespace_declaration.value {
            Declaration::NamespaceDeclaration {
                namespace_name,
                declarations,
            } => {
                assert_eq!(namespace_name.value, "foo");
                assert_eq!(declarations.len(), 1);

                let using_statement = declarations.get(0).unwrap();

                match &using_statement.value {
                    Declaration::UsingStatement { namespace_name } => {
                        assert_eq!(namespace_name.value, "bar");
                    }
                    _ => panic!("Expected a using statement"),
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
                column: 14,
                byte_index: 13
            }
        );
        assert_eq!(
            parser.peek().position_range().end,
            SourcePosition {
                line: 1,
                column: 15,
                byte_index: 14
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
                column: 15,
                byte_index: 14
            }
        );
        assert_eq!(
            parser.peek().position_range().end,
            SourcePosition {
                line: 1,
                column: 15,
                byte_index: 14
            }
        );
    }

    // Checks if the parser can parse a using_statement
    #[test]
    fn parse_using_statement_test() {
        let source_code = SourceCode::new(
            "using Simmypeet.Program;\nusing Another;\nusing Missing.SemiColon"
                .to_string(),
            String::new(),
        );

        let mut parser = Parser::new(&source_code);

        // parse the first using_statement
        {
            let using_statement = parser.parse_using_statement().unwrap();

            assert_eq!(
                using_statement.value,
                Declaration::UsingStatement {
                    namespace_name: PositiionWrapper {
                        position: SourcePosition {
                            line: 1,
                            column: 7,
                            byte_index: 6
                        }..SourcePosition {
                            line: 1,
                            column: 24,
                            byte_index: 23
                        },
                        value: "Simmypeet.Program",
                    }
                }
            );

            assert_eq!(
                using_statement.position.start,
                SourcePosition {
                    line: 1,
                    column: 1,
                    byte_index: 0
                }
            );

            assert_eq!(
                using_statement.position.end,
                SourcePosition {
                    line: 1,
                    column: 25,
                    byte_index: 24
                }
            );
        }

        // parse the second using_statement
        {
            let using_statement = parser.parse_using_statement().unwrap();

            assert_eq!(
                using_statement.value,
                Declaration::UsingStatement {
                    namespace_name: PositiionWrapper {
                        position: SourcePosition {
                            line: 2,
                            column: 7,
                            byte_index: 31
                        }..SourcePosition {
                            line: 2,
                            column: 14,
                            byte_index: 38
                        },
                        value: "Another",
                    }
                }
            );

            assert_eq!(
                using_statement.position.start,
                SourcePosition {
                    line: 2,
                    column: 1,
                    byte_index: 25
                }
            );

            assert_eq!(
                using_statement.position.end,
                SourcePosition {
                    line: 2,
                    column: 15,
                    byte_index: 39
                }
            );
        }

        // the last using_statement is missing a semicolon
        {
            let using_statement = parser.parse_using_statement();

            assert!(using_statement.is_none());

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
                   && *found_token.token_kind() == TokenKind::EndOfFile
                ),
                "Unexpected error: {:?}",
                error
            );
        }
    }

    // Checks if the parser can parse a qualified name
    #[test]
    fn parse_qualified_name_test() {
        let source_code = SourceCode::new(
            "Simmypeet.Program  Another.Simmypeet.Program  Error.Qualifier. Last.Error.Qualifier.".to_string(),
            String::new(),
        );

        let mut parser = Parser::new(&source_code);

        // parse first qualifier
        {
            let qualifier = parser.parse_qualified_name().unwrap();

            assert_eq!(qualifier.value, "Simmypeet.Program");

            assert_eq!(
                qualifier.position.start,
                SourcePosition {
                    line: 1,
                    column: 1,
                    byte_index: 0
                }
            );
            assert_eq!(
                qualifier.position.end,
                SourcePosition {
                    line: 1,
                    column: 18,
                    byte_index: 17
                }
            );
        }

        // parse second qualifier
        {
            let qualifier = parser.parse_qualified_name().unwrap();

            assert_eq!(qualifier.value, "Another.Simmypeet.Program");

            assert_eq!(
                qualifier.position.start,
                SourcePosition {
                    line: 1,
                    column: 20,
                    byte_index: 19
                }
            );
            assert_eq!(
                qualifier.position.end,
                SourcePosition {
                    line: 1,
                    column: 45,
                    byte_index: 44
                }
            );
        }

        // the next qualifier is incomplete and should be rejected
        {
            assert!(parser.parse_qualified_name().is_none());

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
                        column: 63,
                        byte_index: 62
                    }
                    && found_token.position_range().end == SourcePosition {
                        line: 1,
                        column: 64,
                        byte_index: 63
                    }
                    && *found_token.token_kind() == TokenKind::Space
                ),
                "Unexpected error: {:?}",
                error
            );
        }

        // the last qualifier is also incomplete found eof first
        {
            assert!(parser.parse_qualified_name().is_none());

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
                        column: 85,
                        byte_index: 84
                    }
                    && found_token.position_range().end == SourcePosition {
                        line: 1,
                        column: 85,
                        byte_index: 84
                    }
                    && *found_token.token_kind() == TokenKind::EndOfFile
                ),
                "Unexpected error: {:?}",
                error
            );
        }
    }
}
