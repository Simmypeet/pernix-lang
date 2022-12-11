pub mod abstract_syntax_tree;
pub mod error;

use abstract_syntax_tree::{
    declaration::{Declaration, NamespaceDeclaration, Type, UsingDirective},
    expression::{
        BinaryExpression, Expression, FunctionCallExpression, UnaryExpression,
    },
    BinaryOperator, PositionWrapper,
};
use error::{Context, Error};
use pernix_lexer::{
    token::{Keyword, Token, TokenKind},
    Lexer,
};
use pernix_project::source_code::SourceCode;

use crate::abstract_syntax_tree::UnaryOperator;

/// Represent a state-machine data structure that is used to parse a Pernix
/// source code program.
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

/// Represent an AST structure that represents a Pernix source code program.
pub struct Program<'a> {
    declarations: Vec<PositionWrapper<Declaration<'a>>>,
    using_directives: Vec<PositionWrapper<UsingDirective<'a>>>,
}

impl<'a> Program<'a> {
    /// Returns a reference to the declarations of this [`Program`].
    pub fn declarations(&self) -> &[PositionWrapper<Declaration>] {
        self.declarations.as_ref()
    }

    /// Returns a reference to the using directives of this [`Program`].
    pub fn using_directives(&self) -> &[PositionWrapper<UsingDirective>] {
        self.using_directives.as_ref()
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

    // Moves the current position to the next significant token
    fn move_to_significant(&mut self) {
        while !self.peek().is_significant_token() {
            self.next();
        }
    }

    // Moves the current position to the next significant token and reads it
    // without moving the current position forward
    fn peek_significant(&mut self) -> &Token<'a> {
        self.move_to_significant();
        self.peek()
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
    fn append_error<T>(&mut self, error: Error<'a>) -> Option<T> {
        if self.produce_errors {
            self.accumulated_errors.push(error);
        }

        None
    }

    ////////////////////////////////////////////////////////////////////////////
    /// DECLARATIONS
    ////////////////////////////////////////////////////////////////////////////

    /// Parse the current token stream as a qualified name string.
    ///
    /// Qualified_Name:
    ///     `Identifier` (`.` `Identifier`)*
    pub fn parse_qualified_name(&mut self) -> Option<PositionWrapper<&'a str>> {
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

        Some(PositionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: &self.source_code().source_code()[starting_position
                .byte_index
                ..self.peek_back().position_range().end.byte_index],
        })
    }

    /// Parse the current token stream as a using_statement.
    ///
    /// Using_Directive:
    ///     `using` Qualified_Name `;`
    pub fn parse_using_directive(
        &mut self,
    ) -> Option<PositionWrapper<UsingDirective<'a>>> {
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

        Some(PositionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: UsingDirective {
                namespace_name: qualified_name,
            },
        })
    }

    // Parse a list of using directives
    fn parse_using_directive_list(
        &mut self,
    ) -> Option<Vec<PositionWrapper<UsingDirective<'a>>>> {
        let mut using_directives = Vec::new();

        while matches!(
            self.peek_significant().token_kind(),
            TokenKind::Keyword(Keyword::Using)
        ) {
            using_directives.push(self.parse_using_directive()?);
        }

        Some(using_directives)
    }

    /// Parse the current token stream as a namespace declaration.
    ///
    /// Namespace_Declaration:
    ///     `namespace` Qualified_Name `{` Using_Directive* Declaration* `}`
    pub fn parse_namespace_declaration(
        &mut self,
    ) -> Option<PositionWrapper<Declaration<'a>>> {
        // move to the first significant token
        self.move_to_significant();
        let starting_position = self.peek().position_range().start;

        let namespace_skip_predicate = |token: &Token| {
            matches!(
                token.token_kind(),
                TokenKind::Keyword(Keyword::Namespace)
                    | TokenKind::Keyword(Keyword::Using)
            )
        };

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

        // parse the using directives
        let using_directives = match self.parse_using_directive_list() {
            Some(using_directives) => using_directives,
            None => {
                self.skip_to(namespace_skip_predicate);
                Vec::new()
            }
        };

        // loop through all the declarations
        let mut declarations = Vec::new();

        // loop through all the declarations until closing curly bracket or
        // EOF is found
        while !matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator('}') | TokenKind::EndOfFile
        ) {
            let declaration = match self.peek().token_kind() {
                TokenKind::Keyword(Keyword::Namespace) => {
                    self.parse_namespace_declaration()
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
                self.skip_to(namespace_skip_predicate);
            }
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

        Some(PositionWrapper {
            position: starting_position..self.peek_back().position_range().end,
            value: Declaration::NamespaceDeclaration(NamespaceDeclaration {
                namespace_name,
                using_directives,
                declarations,
            }),
        })
    }

    /// Parse all token stream as a program
    ///
    /// Program:
    ///     Using_Directive* Declaration*
    pub fn parse_program(&mut self) -> Option<Program<'a>> {
        let mut program = Program {
            declarations: Vec::new(),
            using_directives: Vec::new(),
        };

        let program_skip_predicate = |token: &Token| {
            matches!(token.token_kind(), TokenKind::Keyword(Keyword::Namespace))
        };

        // parse the using directives
        if let Some(using_directives) = self.parse_using_directive_list() {
            program.using_directives = using_directives;
        } else {
            self.skip_to(program_skip_predicate);
        }

        while !matches!(
            self.peek_significant().token_kind(),
            TokenKind::EndOfFile
        ) {
            let declaration = match self.peek().token_kind() {
                TokenKind::Keyword(Keyword::Namespace) => {
                    self.parse_namespace_declaration()
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
                self.skip_to(program_skip_predicate);
            }

            // move to the next significant token
            self.peek_significant();
        }

        Some(program)
    }

    /// Parses the current token stream as a type
    pub fn parse_type(&mut self) -> Option<PositionWrapper<Type<'a>>> {
        // currently we support only primitive types by using identifiers
        let qualified_name = self.parse_qualified_name()?;

        Some(PositionWrapper {
            position: qualified_name.position,
            value: Type::QualifiedName(qualified_name.value),
        })
    }

    ////////////////////////////////////////////////////////////////////////////
    /// Expresions
    ////////////////////////////////////////////////////////////////////////////

    // parse an expression with the unary operator
    fn parse_unary_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
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

        Some(PositionWrapper {
            position: operator_position_range.start
                ..self.peek_back().position_range().end,
            value: Expression::UnaryExpression(UnaryExpression {
                operator: PositionWrapper {
                    position: operator_position_range,
                    value: operator,
                },
                operand: Box::new(operand),
            }),
        })
    }

    fn parse_binary_operator_roll_back(
        &mut self,
    ) -> Option<PositionWrapper<BinaryOperator>> {
        let starting_index = self.current_position;

        // move to the next significant token
        self.move_to_significant();
        let operator_position_range = self.peek().position_range().clone();

        let bin_op = match self.next().token_kind() {
            TokenKind::Punctuator('+') => BinaryOperator::Add,
            TokenKind::Punctuator('-') => BinaryOperator::Subtract,
            TokenKind::Punctuator('*') => BinaryOperator::Asterisk,
            TokenKind::Punctuator('/') => BinaryOperator::Slash,
            TokenKind::Punctuator('%') => BinaryOperator::Percent,
            TokenKind::Punctuator('<') | TokenKind::Punctuator('>') => {
                let current_punctuator = match self.peek_back().token_kind() {
                    TokenKind::Punctuator(c) => *c,
                    _ => unreachable!(),
                };

                if matches!(
                    self.peek().token_kind(),
                    TokenKind::Punctuator('=')
                ) {
                    self.next();
                    match current_punctuator {
                        '<' => BinaryOperator::LessThanEqual,
                        '>' => BinaryOperator::GreaterThanEqual,
                        _ => unreachable!(),
                    }
                } else {
                    match current_punctuator {
                        '<' => BinaryOperator::LessThan,
                        '>' => BinaryOperator::GreaterThan,
                        _ => unreachable!(),
                    }
                }
            }
            TokenKind::Punctuator('=') => {
                if matches!(
                    self.peek().token_kind(),
                    TokenKind::Punctuator('=')
                ) {
                    self.next();
                    BinaryOperator::Equal
                } else {
                    self.current_position = starting_index;
                    return None;
                }
            }
            TokenKind::Punctuator('!') => {
                if matches!(
                    self.peek().token_kind(),
                    TokenKind::Punctuator('=')
                ) {
                    self.next();
                    BinaryOperator::NotEqual
                } else {
                    self.current_position = starting_index;
                    return None;
                }
            }
            TokenKind::Punctuator('&') => {
                if matches!(
                    self.peek().token_kind(),
                    TokenKind::Punctuator('&')
                ) {
                    self.next();
                    BinaryOperator::LogicalAnd
                } else {
                    self.current_position = starting_index;
                    return None;
                }
            }
            TokenKind::Punctuator('|') => {
                if matches!(
                    self.peek().token_kind(),
                    TokenKind::Punctuator('|')
                ) {
                    self.next();
                    BinaryOperator::LogicalOr
                } else {
                    self.current_position = starting_index;
                    return None;
                }
            }
            _ => {
                self.current_position = starting_index;
                return None;
            }
        };

        Some(PositionWrapper {
            position: operator_position_range.start
                ..self.peek_back().position_range().end,
            value: bin_op,
        })
    }

    fn get_binary_operator_precedence(operator: BinaryOperator) -> usize {
        match operator {
            BinaryOperator::LogicalOr => 1,
            BinaryOperator::LogicalAnd => 2,
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => 3,
            BinaryOperator::Add | BinaryOperator::Subtract => 4,
            BinaryOperator::Asterisk | BinaryOperator::Slash => 5,
            BinaryOperator::Percent => 6,
        }
    }

    fn parse_expression_bin(
        &mut self,
        parent_precedence: usize,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        let mut left = self.parse_primary_expression()?;

        loop {
            let starting_index = self.current_position;

            let binary_op = match self.parse_binary_operator_roll_back() {
                Some(op) => op,
                None => return Some(left),
            };
            let binary_op_precedence =
                Self::get_binary_operator_precedence(binary_op.value);

            // if the precedence of the binary operator is less than the
            // precedence of the parent operator, then we have to stop
            // parsing the expression
            if binary_op_precedence <= parent_precedence {
                // roll back the current position
                self.current_position = starting_index;
                break;
            }

            let right = self.parse_expression_bin(binary_op_precedence)?;

            left = PositionWrapper {
                position: left.position.start..right.position.end,
                value: Expression::BinaryExpression(BinaryExpression {
                    left: Box::new(left),
                    operator: binary_op,
                    right: Box::new(right),
                }),
            }
        }

        Some(left)
    }

    /// Parse the current token stream as an expression
    pub fn parse_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        self.parse_expression_bin(0)
    }

    // parses a list of arguments
    fn parse_argument_list(
        &mut self,
    ) -> Option<Vec<PositionWrapper<Expression<'a>>>> {
        // starts with a opening parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('(')
        ) {
            return self.append_error(Error::PunctuatorExpected {
                expected_punctuator: '(',
                found_token: self.peek().clone(),
                source_reference: self.source_code(),
            });
        }

        // list of arguments to return
        let mut arguments = Vec::new();

        // empty argument list
        if matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator(')')
        ) {
            // move to the next significant token
            self.next();
            return Some(arguments);
        }

        loop {
            let argument = self.parse_expression()?;
            arguments.push(argument);

            match self.next_significant().token_kind() {
                TokenKind::Punctuator(',') => {}
                TokenKind::Punctuator(')') => break,
                _ => {
                    return self.append_error(Error::PunctuatorExpected {
                        expected_punctuator: ')',
                        found_token: self.peek().clone(),
                        source_reference: self.source_code(),
                    })
                }
            }
        }

        Some(arguments)
    }

    // parses an expression that starts with an identifier
    fn parse_identifier_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        // move to the next significant token
        self.move_to_significant();
        let starting_position = self.peek().position_range().clone();

        let qualified_name = self.parse_qualified_name()?;

        match self.peek_significant().token_kind() {
            TokenKind::Punctuator('(') => {
                let arguments = self.parse_argument_list()?;

                Some(PositionWrapper {
                    position: starting_position.start
                        ..self.peek_back().position_range().end,
                    value: Expression::FunctionCallExpression(
                        FunctionCallExpression {
                            function_name: qualified_name,
                            arguments,
                        },
                    ),
                })
            }
            // it's just an identifier
            _ => Some(PositionWrapper {
                position: qualified_name.position,
                value: Expression::IdentifierExpression(qualified_name.value),
            }),
        }
    }

    // Parses the current token stream as a primary expression
    fn parse_primary_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        self.move_to_significant();

        match self.peek().token_kind().clone() {
            TokenKind::Punctuator('!')
            | TokenKind::Punctuator('-')
            | TokenKind::Punctuator('+') => self.parse_unary_expression(),
            TokenKind::LiteralConstant(val) => {
                let position = self.peek().position_range().clone();

                // eat literal token
                self.next();

                Some(PositionWrapper {
                    position,
                    value: Expression::LiteralExpression(val),
                })
            }
            TokenKind::Identifier => self.parse_identifier_expression(),
            TokenKind::Punctuator('(') => {
                // eat the opening parenthesis
                self.next();

                let expression = self.parse_expression()?;

                // expect a closing parenthesis
                if !matches!(
                    self.next_significant().token_kind(),
                    TokenKind::Punctuator(')')
                ) {
                    return self.append_error(Error::PunctuatorExpected {
                        expected_punctuator: ')',
                        found_token: self.peek().clone(),
                        source_reference: self.source_code(),
                    });
                } else {
                    Some(expression)
                }
            }
            _ => {
                // make progress
                self.next();

                self.append_error(error::Error::UnexpectedToken {
                    context: Context::Expression,
                    found_token: self.peek().clone(),
                    source_reference: self.source_code(),
                })
            }
        }
    }
}

#[cfg(test)]
mod test;
