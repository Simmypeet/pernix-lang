use pernixc_lexical_analysis::{
    token::{Keyword, Token, TokenType},
    token_stream::TokenStream,
};

use crate::{
    abstract_syntax_tree::{
        declaration::{
            AccessModifier, ClassDeclarationAST, ClassFieldDeclarationAST,
            ClassMemberDeclarationAST, ClassMethodDeclarationAST, DeclarationAST, ParameterAST,
            PrimitiveTypeUnit, QualifiedTypeAnnotation, TypeAnnotationAST, TypeUnitAST,
        },
        expression::{
            BinaryExpressionAST, BinaryOperator, ClassFiledInitializationAST,
            ClassInstantiationExpressionAST, ExpressionAST, FunctionCallExpressionAST,
            LiteralExpressionAST, MemberFieldAccessExpressionAST, QualifiedNameExpressionAST,
            UnaryExpressionAST, UnaryOperator,
        },
        statement::{
            BlockScopeStatementAST, IfElseStatementAST, ReturnStatementAST, StatementAST,
            VariableDeclarationStatementAST, WhileStatementAST,
        },
        FileAST, ImportModuleAST, ModuleAST, PositionWrapper,
    },
    error::{ParsingContext, SyntacticError},
};

/// A parser that parses a [`TokenStream`] into a [`SyntaxTree`]. The parser is
/// implemented as a recursive descent parser with operator precedence parsing.
#[derive(Debug)]
pub struct Parser<'token, 'src> {
    token_stream: &'token TokenStream<'src>,
    current: usize,
    accumulated_errors: Vec<SyntacticError>,
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// PUBLIC API
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Create a new [`Parser`] that parses the given [`TokenStream`].
    pub fn new(token_stream: &'token TokenStream<'src>) -> Self {
        Self {
            token_stream,
            current: 0,
            accumulated_errors: Vec::new(),
        }
    }

    /// Return the position where the parser is currently at in the token stream. It indicates the
    /// index of the next token to be parsed.
    pub fn current_token_stream_position(&self) -> usize {
        self.current
    }

    /// Parse the token stream at the current position into a [`FileAST`].
    pub fn parse_file(&mut self) -> Result<FileAST<'src>, Vec<SyntacticError>>
    where
        'token: 'src,
    {
        match self.parse_file_internal() {
            Some(file) => Ok(file),
            None => Err(std::mem::take(&mut self.accumulated_errors)),
        }
    }

    /// Parse the token stream at the current position into a [`DeclarationAST`].
    pub fn parse_declaration(
        &mut self,
    ) -> Result<PositionWrapper<DeclarationAST<'src>>, Vec<SyntacticError>>
    where
        'token: 'src,
    {
        match self.parse_declaration_internal() {
            Some(declaration) => Ok(declaration),
            None => Err(std::mem::take(&mut self.accumulated_errors)),
        }
    }

    /// Parse the token stream at the current position into a [`StatementAST`].
    pub fn parse_statement(
        &mut self,
    ) -> Result<PositionWrapper<StatementAST<'src>>, Vec<SyntacticError>>
    where
        'token: 'src,
    {
        match self.parse_statement_internal() {
            Some(statement) => Ok(statement),
            None => Err(std::mem::take(&mut self.accumulated_errors)),
        }
    }

    /// Parse the token stream at the current position into a [`ExpressionAST`].
    pub fn parse_expression(
        &mut self,
    ) -> Result<PositionWrapper<ExpressionAST<'src>>, Vec<SyntacticError>>
    where
        'token: 'src,
    {
        match self.parse_expression_internal() {
            Some(expression) => Ok(expression),
            None => Err(std::mem::take(&mut self.accumulated_errors)),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// HELPER FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Check if the given token type is a significant token.
    fn is_significant_token(token_type: &TokenType) -> bool {
        match token_type {
            TokenType::Space | TokenType::Comment(_) => false,
            _ => true,
        }
    }

    /// Move the current token to the next significant token.
    fn move_to_significant_token(&mut self) {
        while self.current < self.token_stream.len() {
            let token = &self.token_stream[self.current];

            if Self::is_significant_token(&token.token_type) {
                break;
            }

            // if the `current` is the last token, then break
            if self.current == self.token_stream.len() - 1 {
                break;
            }

            self.current += 1;
        }
    }

    /// Return the current token pointed by the `current` field and move the
    /// `current` field to the next token. If the `current` is the last token,
    /// then return the last token without moving the `current` field.
    fn next(&mut self) -> Token<'src> {
        // if the `current` is the last token, then return the last token
        if self.current == self.token_stream.len() - 1 {
            self.token_stream[self.current].clone()
        } else {
            self.current += 1;
            self.token_stream[self.current - 1].clone()
        }
    }

    /// Peek the current toke and return a reference to it.
    fn peek(&self) -> Token<'src> {
        self.token_stream[self.current].clone()
    }

    /// Move the `current` to the next significant token and return a reference to
    /// it. If the `current` is the last token, then return the last token without
    /// moving the `current` field.
    fn peek_significant_token(&mut self) -> Token<'src> {
        self.move_to_significant_token();
        self.peek()
    }

    /// Peek the token at the given `offset` and return a reference to it. If the
    /// `offset` is out of bounds, then return the first or last token.
    fn peek_with_offset(&self, offset: isize) -> Token<'src> {
        let index = (self.current as isize + offset).clamp(0, self.token_stream.len() as isize - 1)
            as usize;

        self.token_stream[index].clone()
    }

    /// Similar to [`Parser::next`], but the function will move the `current` to the next
    /// significant token before returning the next token.
    fn next_significant_token(&mut self) -> Token<'src> {
        self.move_to_significant_token();
        self.next()
    }

    /// Create a new [`Error`] from the given [`SyntacticError`].
    fn create_error<T>(&mut self, error: SyntacticError) -> Option<T> {
        self.accumulated_errors.push(error);
        return None;
    }

    fn parenthesis_delimiter_predicate<const C: char>(token: &Token) -> bool {
        matches!(token.token_type, TokenType::Punctuation(c) if c == C)
    }

    /// Skip the tokens until the predicate returns true. The parser also skips the tokens inside
    /// pairs of brackets and parenthesis
    fn skip_to(&mut self, predicate: impl Fn(&Token) -> bool) {
        // keep skipping tokens until the predicate returns true
        while !predicate(&self.peek()) && !matches!(self.peek().token_type, TokenType::EndOfFile) {
            // if found open bracket or open parenthesis, skip to the closing
            // bracket or parenthesis
            if matches!(
                self.peek().token_type,
                TokenType::Punctuation('(')
                    | TokenType::Punctuation('{')
                    | TokenType::Punctuation('[')
            ) {
                let predicate = match self.peek().token_type {
                    TokenType::Punctuation('(') => Self::parenthesis_delimiter_predicate::<')'>,
                    TokenType::Punctuation('{') => Self::parenthesis_delimiter_predicate::<'}'>,
                    TokenType::Punctuation('[') => Self::parenthesis_delimiter_predicate::<']'>,
                    _ => unreachable!(),
                };

                self.next();
                self.skip_to(predicate);
            }
            self.next();
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// EXPECT FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Expect the next token to be a [`TokenType::Identifier`]
    fn expect_identifier(&mut self) -> Option<PositionWrapper<&'src str>> {
        // move to the next significant token
        let ident = self.next_significant_token();

        if let TokenType::Identifier(value) = ident.token_type {
            Some(PositionWrapper {
                position_range: ident.position_range.clone(),
                value,
            })
        } else {
            self.create_error(SyntacticError::IdentifierExpected {
                expected_position: ident.position_range.start.clone(),
            })
        }
    }

    /// Expect the next token to be a [`TokenType::Punctuation`]
    fn expect_punctuation(&mut self, expected: char) -> Option<Token<'src>> {
        // move to the next significant token
        let punct = self.next_significant_token();

        if !matches!(punct.token_type, TokenType::Punctuation(p) if p == expected) {
            self.create_error(SyntacticError::PunctuationExpected {
                expected_punctuation: expected,
                expected_position: punct.position_range.start,
            })
        } else {
            Some(punct)
        }
    }

    /// Expect the next token to be a [`TokenType::Keyword`]
    fn expect_keyword(&mut self, expected: Keyword) -> Option<Token<'src>> {
        // move to the next significant token
        let keyword = self.next_significant_token();

        if !matches!(keyword.token_type, TokenType::Keyword(k) if k == expected) {
            self.create_error(SyntacticError::KeywordExpected {
                expected_keyword: expected,
                expected_position: keyword.position_range.start,
            })
        } else {
            Some(keyword)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// COMMON PARSING FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Parse the current token pointed by the `current` field as a qualified name.
    fn parse_qualified_name(&mut self) -> Option<PositionWrapper<&'src str>>
    where
        'token: 'src,
    {
        // move to the next significant token
        self.move_to_significant_token();

        // get the first byte index of the qualified name
        let first_byte_index = self.peek().byte_index_range.start;
        let first_position = self.peek().position_range.start;

        self.expect_identifier()?;

        // get the consecutive identifiers
        while matches!(self.peek().token_type, TokenType::Punctuation(':'))
            && matches!(
                self.peek_with_offset(1).token_type,
                TokenType::Punctuation(':')
            )
        {
            self.next();
            self.next();
            self.expect_identifier()?;
        }

        // get the last byte index of the qualified name
        let last_byte_index = self.peek().byte_index_range.start;

        // string slice of the qualified name
        let qualified_name_string =
            &self.token_stream.source_code()[first_byte_index..last_byte_index];

        Some(PositionWrapper {
            position_range: first_position..self.peek().position_range.start,
            value: qualified_name_string,
        })
    }

    /// Parse a list of elements separated by a separator and terminated by a
    /// terminator.
    fn parse_separated_list<const SEPARATOR: char, const TERMINATOR: char, Element>(
        &mut self,
        mut parse_element: impl FnMut(&mut Self) -> Option<Element>,
    ) -> Option<Vec<Element>> {
        let mut first_element = true;
        let mut found_error = false;
        let mut elements = Vec::new();

        // keep looping until the terminator is found or the end of file is reached
        while !matches!(
            self.peek_significant_token().token_type,
            TokenType::Punctuation(c) if c == TERMINATOR
        ) && !matches!(
            self.peek_significant_token().token_type,
            TokenType::EndOfFile
        ) {
            // if this is not the first element, expect a separator
            if !first_element {
                if let None = self.expect_punctuation(SEPARATOR) {
                    found_error = true;

                    // skip to either the separator or the terminator
                    self.skip_to(|token| {
                        matches!(
                            token.token_type,
                            TokenType::Punctuation(c) if c == SEPARATOR || c == TERMINATOR
                        )
                    });
                    first_element = false;
                    continue;
                }
            }

            first_element = false;

            // parse the element
            if let Some(element) = parse_element(self) {
                elements.push(element);
            } else {
                found_error = true;

                // skip to either the separator or the terminator
                self.skip_to(|token| {
                    matches!(
                        token.token_type,
                        TokenType::Punctuation(c) if c == SEPARATOR || c == TERMINATOR
                    )
                });
                continue;
            }
        }

        // expect closing parenthesis
        self.expect_punctuation(TERMINATOR)?;

        if !found_error {
            Some(elements)
        } else {
            None
        }
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// EXPRESSION PARSING FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Parse the current token pointed by the `current` field as an expression.
    fn parse_expression_internal(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>>
    where
        'token: 'src,
    {
        self.parse_expression_bin(0)
    }

    /// Parse the current token pointed by the `current` field as a binary operator.
    fn parse_expression_bin(
        &mut self,
        parent_precedence: usize,
    ) -> Option<PositionWrapper<ExpressionAST<'src>>>
    where
        'token: 'src,
    {
        let mut left = self.parse_primary_expression()?;

        loop {
            let starting_index = self.current;

            let binary_op = match self.parse_binary_operator() {
                Some(op) => op,
                None => return Some(left),
            };
            let binary_op_precedence = Self::get_binary_operator_precedence(binary_op.value);

            // if the precedence of the binary operator is less than the
            // precedence of the parent operator, then we have to stop
            // parsing the expression
            if binary_op_precedence <= parent_precedence {
                // roll back the current position
                self.current = starting_index;
                break;
            }

            let right = self.parse_expression_bin(binary_op_precedence)?;

            left = PositionWrapper {
                position_range: left.position_range.start..right.position_range.end,
                value: ExpressionAST::BinaryExpression(BinaryExpressionAST {
                    left_expression: Box::new(left),
                    binary_operator: binary_op,
                    right_expression: Box::new(right),
                }),
            }
        }

        Some(left)
    }

    /// Retrieve the precedence of a binary operator.
    fn get_binary_operator_precedence(operator: BinaryOperator) -> usize {
        match operator {
            BinaryOperator::Assignment => 1,
            BinaryOperator::LogicalOr => 2,
            BinaryOperator::LogicalAnd => 3,
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanOrEqual => 4,
            BinaryOperator::Addition
            | BinaryOperator::Subtraction
            | BinaryOperator::CompoundAddition
            | BinaryOperator::CompoundSubtraction => 5,
            BinaryOperator::Multiplication
            | BinaryOperator::Division
            | BinaryOperator::Modulo
            | BinaryOperator::CompoundMultiplication
            | BinaryOperator::CompoundDivision
            | BinaryOperator::CompoundModulo => 6,
        }
    }

    /// Parse the current token pointed by the `current` field as a binary operator.
    fn parse_binary_operator(&mut self) -> Option<PositionWrapper<BinaryOperator>> {
        let starting_index = self.current;

        // move to the next significant token
        self.move_to_significant_token();
        let operator_position_range = self.peek().position_range.clone();

        let bin_op = match self.next().token_type {
            TokenType::Punctuation('+')
            | TokenType::Punctuation('-')
            | TokenType::Punctuation('*')
            | TokenType::Punctuation('/')
            | TokenType::Punctuation('%') => {
                let punctuation = match self.peek_with_offset(-1).token_type {
                    TokenType::Punctuation(c) => c,
                    _ => unreachable!(),
                };

                // check for compound assignment
                match self.peek().token_type {
                    TokenType::Punctuation('=') => {
                        self.next();
                        match punctuation {
                            '+' => BinaryOperator::CompoundAddition,
                            '-' => BinaryOperator::CompoundSubtraction,
                            '*' => BinaryOperator::CompoundMultiplication,
                            '/' => BinaryOperator::CompoundDivision,
                            '%' => BinaryOperator::CompoundModulo,
                            _ => unreachable!(),
                        }
                    }
                    _ => match punctuation {
                        '+' => BinaryOperator::Addition,
                        '-' => BinaryOperator::Subtraction,
                        '*' => BinaryOperator::Multiplication,
                        '/' => BinaryOperator::Division,
                        '%' => BinaryOperator::Modulo,
                        _ => unreachable!(),
                    },
                }
            }
            TokenType::Punctuation('<') | TokenType::Punctuation('>') => {
                let current_punctuation = match self.peek().token_type {
                    TokenType::Punctuation(c) => c,
                    _ => unreachable!(),
                };

                if matches!(self.peek().token_type, TokenType::Punctuation('=')) {
                    self.next();
                    match current_punctuation {
                        '<' => BinaryOperator::LessThanOrEqual,
                        '>' => BinaryOperator::GreaterThanOrEqual,
                        _ => unreachable!(),
                    }
                } else {
                    match current_punctuation {
                        '<' => BinaryOperator::LessThan,
                        '>' => BinaryOperator::GreaterThan,
                        _ => unreachable!(),
                    }
                }
            }
            TokenType::Punctuation('=') => {
                if matches!(self.peek().token_type, TokenType::Punctuation('=')) {
                    self.next();
                    BinaryOperator::Equal
                } else {
                    BinaryOperator::Assignment
                }
            }
            TokenType::Punctuation('!') => {
                if matches!(self.peek().token_type, TokenType::Punctuation('=')) {
                    self.next();
                    BinaryOperator::NotEqual
                } else {
                    self.current = starting_index;
                    return None;
                }
            }
            TokenType::Punctuation('&') => {
                if matches!(self.peek().token_type, TokenType::Punctuation('&')) {
                    self.next();
                    BinaryOperator::LogicalAnd
                } else {
                    self.current = starting_index;
                    return None;
                }
            }
            TokenType::Punctuation('|') => {
                if matches!(self.peek().token_type, TokenType::Punctuation('|')) {
                    self.next();
                    BinaryOperator::LogicalOr
                } else {
                    self.current = starting_index;
                    return None;
                }
            }
            _ => {
                self.current = starting_index;
                return None;
            }
        };

        Some(PositionWrapper {
            position_range: operator_position_range.start
                ..self.peek_with_offset(-1).position_range.end,
            value: bin_op,
        })
    }

    /// Parse the current token pointed by the `current` field as a class instantiation expression.
    fn parse_class_instantiation_expression(
        &mut self,
    ) -> Option<PositionWrapper<ExpressionAST<'src>>>
    where
        'token: 'src,
    {
        // expect the `new` keyword
        let first_token = self.expect_keyword(Keyword::New)?;

        // expect the class name
        let qualified_name = self.parse_qualified_name()?;

        // expect a left brace
        self.expect_punctuation('{')?;

        let field_initializations = self
            .parse_separated_list::<',', '}', PositionWrapper<ClassFiledInitializationAST>>(
                |this| {
                    let identifier = this.expect_identifier()?;
                    this.expect_punctuation(':')?;
                    let expression = this.parse_expression_internal()?;

                    Some(PositionWrapper {
                        position_range: identifier.position_range.start
                            ..expression.position_range.end,
                        value: ClassFiledInitializationAST {
                            identifier,
                            expression,
                        },
                    })
                },
            )?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: ExpressionAST::ClassInstantiationExpression(ClassInstantiationExpressionAST {
                qualified_name,
                field_initializations,
            }),
        })
    }

    /// Parse an expression that starts with an identifier.
    fn parse_identifier_expression(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>>
    where
        'token: 'src,
    {
        let qualified_name = self.parse_qualified_name()?;

        // check if the next token is a left parenthesis
        let pos = self.current;

        if matches!(
            self.peek_significant_token().token_type,
            TokenType::Punctuation('(')
        ) {
            // eat the left parenthesis
            self.next();

            // parse the arguments
            let arguments = self
                .parse_separated_list::<',', ')', PositionWrapper<ExpressionAST<'src>>>(|c| {
                    c.parse_expression_internal()
                })?;

            Some(PositionWrapper {
                position_range: qualified_name.position_range.start
                    ..self.peek().position_range.start,
                value: ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                    qualified_name,
                    arguments,
                }),
            })
        } else {
            self.current = pos;

            // parse a variable expression
            Some(PositionWrapper {
                position_range: qualified_name.position_range,
                value: ExpressionAST::QualifiedNameExpression(QualifiedNameExpressionAST {
                    qualified_name: qualified_name.value,
                }),
            })
        }
    }

    /// Parse the current token pointed by the `current` field as a primary
    /// expression.
    fn parse_primary_expression(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>>
    where
        'token: 'src,
    {
        // move to the next significant token
        self.move_to_significant_token();
        let starting_position = self.peek().position_range.start;

        let mut expression = match self.peek().token_type {
            // parse an expression that starts with an identifier
            TokenType::Identifier(_) => self.parse_identifier_expression()?,

            // parse a literal expression
            TokenType::Literal(literal_expression) => {
                // eat the literal constant
                self.next();

                PositionWrapper {
                    position_range: starting_position..self.peek().position_range.start,
                    value: ExpressionAST::LiteralExpression(LiteralExpressionAST {
                        literal_expression,
                    }),
                }
            }

            // parse a parenthesized expression
            TokenType::Punctuation('(') => {
                let left_parenthesis = self.next();
                let mut expression = self.parse_expression_internal()?;
                let right_parenthesis = self.next();

                self.expect_punctuation(')')?;

                expression.position_range.start = left_parenthesis.position_range.start;
                expression.position_range.end = right_parenthesis.position_range.end;

                expression
            }

            // parse a class instantiation expression
            TokenType::Keyword(Keyword::New) => self.parse_class_instantiation_expression()?,

            // parse a unary expression
            TokenType::Punctuation('+')
            | TokenType::Punctuation('-')
            | TokenType::Punctuation('!') => {
                let unary_operator = self.next();
                let unary_operator = PositionWrapper {
                    position_range: unary_operator.position_range,
                    value: match unary_operator.token_type {
                        TokenType::Punctuation('+') => UnaryOperator::Identity,
                        TokenType::Punctuation('-') => UnaryOperator::Negation,
                        TokenType::Punctuation('!') => UnaryOperator::LogicalNegation,
                        _ => unreachable!(),
                    },
                };
                let expression = self.parse_primary_expression()?;

                PositionWrapper {
                    position_range: unary_operator.position_range.start
                        ..expression.position_range.end,
                    value: ExpressionAST::UnaryExpression(UnaryExpressionAST {
                        unary_operator,
                        expression: Box::new(expression),
                    }),
                }
            }
            _ => {
                // make progress
                let current_token = self.next().position_range;

                return self.create_error(SyntacticError::UnexpectedToken {
                    unexpected_position_range: current_token.start.into()..current_token.end.into(),
                    parsing_context: ParsingContext::Expression,
                });
            }
        };

        loop {
            let current_position = self.current;

            // check if the next token is a dot '.'. If it is, then we are parsing
            // a member access expression.

            if let TokenType::Punctuation('.') = self.next_significant_token().token_type {
                // expect an identifier
                let identifier = self.expect_identifier()?;

                expression = PositionWrapper {
                    position_range: expression.position_range.start..identifier.position_range.end,
                    value: ExpressionAST::MemberFieldAccessExpression(
                        MemberFieldAccessExpressionAST {
                            expression: Box::new(expression),
                            identifier,
                        },
                    ),
                };
            } else {
                self.current = current_position;
                break;
            }
        }

        Some(expression)
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// STATEMENT PARSING FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Parse the current token pointed by the `current` field as a statement.
    fn parse_statement_internal(&mut self) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // move to the next significant token
        match self.peek_significant_token().token_type {
            // parse a variable declaration
            TokenType::Keyword(Keyword::Var) | TokenType::Keyword(Keyword::Let) => {
                self.parse_variable_declaration_statement()
            }
            // parse a block statement
            TokenType::Punctuation('{') => self.parse_block_scope_statement(),
            // parse an if-else statement
            TokenType::Keyword(Keyword::If) => self.parse_if_else_statement(),
            // parse a while loop statement
            TokenType::Keyword(Keyword::While) => self.parse_while_statement(),
            // parse a return statement
            TokenType::Keyword(Keyword::Return) => self.parse_return_statement(),
            // parse break and continue statements
            TokenType::Keyword(Keyword::Break) | TokenType::Keyword(Keyword::Continue) => {
                let token = self.next();
                Some(PositionWrapper {
                    position_range: token.position_range,
                    value: match token.token_type {
                        TokenType::Keyword(Keyword::Break) => StatementAST::BreakStatement,
                        TokenType::Keyword(Keyword::Continue) => StatementAST::ContinueStatement,
                        _ => unreachable!(),
                    },
                })
            }
            _ => {
                // parse an expression statement
                let expr = self.parse_expression_internal()?;

                // expect a semicolon
                self.expect_punctuation(';')?;

                Some(PositionWrapper {
                    position_range: expr.position_range.start..self.peek().position_range.start,
                    value: StatementAST::ExpressionStatement(expr.value),
                })
            }
        }
    }

    /// Parse the current token pointed by the `current` field as a return statement.
    fn parse_return_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // expect the return keyword
        let first_token = self.expect_keyword(Keyword::Return)?;

        if matches!(
            self.peek_significant_token().token_type,
            TokenType::Punctuation(';')
        ) {
            return Some(PositionWrapper {
                position_range: first_token.position_range.start..self.peek().position_range.start,
                value: StatementAST::ReturnStatement(ReturnStatementAST { expression: None }),
            });
        }

        // parse the expression
        let expression = self.parse_expression_internal()?;

        // expect a semicolon
        self.expect_punctuation(';')?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: StatementAST::ReturnStatement(ReturnStatementAST {
                expression: Some(expression),
            }),
        })
    }

    /// Parse the current token pointed by the `current` field as a while loop statement.
    fn parse_while_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // expect a while keyword
        let first_token = self.expect_keyword(Keyword::While)?;

        // expect an opening parenthesis
        self.expect_punctuation('(')?;

        // parse the condition
        let condition = self.parse_expression_internal()?;

        // expect a closing parenthesis
        self.expect_punctuation(')')?;

        // parse the body
        let statement = Box::new(self.parse_statement_internal()?);

        Some(PositionWrapper {
            position_range: first_token.position_range.start..statement.position_range.end,
            value: StatementAST::WhileStatement(WhileStatementAST {
                condition,
                statement,
            }),
        })
    }

    // parse a block statement
    fn parse_block_scope_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // expect an opening brace
        let first_token = self.expect_punctuation('{')?;

        // keep looping until the end of file is reached or the closing brace is
        // found
        let mut statements = Vec::new();

        while !matches!(
            self.peek_significant_token().token_type,
            TokenType::Punctuation('}')
        ) && !matches!(
            self.peek_significant_token().token_type,
            TokenType::EndOfFile
        ) {
            if let Some(statement) = self.parse_statement_internal() {
                statements.push(statement);
            } else {
                // skip to either the separator or the terminator
                self.skip_to(|token| {
                    // skip to either the separator or the terminator
                    matches!(
                        token.token_type,
                        TokenType::Punctuation(c) if c == ';' || c == '}'
                    )

                    // skip to if, while, return, break, continue, var, let
                    || matches!(
                        token.token_type,
                        TokenType::Keyword(k) if k == Keyword::If
                            || k == Keyword::While
                            || k == Keyword::Return
                            || k == Keyword::Break
                            || k == Keyword::Continue
                            || k == Keyword::Let
                            || k == Keyword::Mutable
                    )
                });

                // if the position has skipped to the semicolon, eat it
                if matches!(
                    self.peek_significant_token().token_type,
                    TokenType::Punctuation(';')
                ) {
                    self.next();
                }

                continue;
            }
        }

        self.expect_punctuation('}')?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: StatementAST::BlockScopeStatement(BlockScopeStatementAST { statements }),
        })
    }

    // parse an if-else statemetn
    fn parse_if_else_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // expect the `if` keyword
        let first_token = self.expect_keyword(Keyword::If)?;

        // expect the opening parenthesis
        self.expect_punctuation('(')?;

        // parse the condition
        let condition = self.parse_expression_internal()?;

        // expect the closing parenthesis
        self.expect_punctuation(')')?;

        // expect then statement
        let then_statement = Box::new(self.parse_statement_internal()?);

        // if the next token is an `else` keyword, parse the else expression
        let else_statement = if matches!(
            self.peek_significant_token().token_type,
            TokenType::Keyword(Keyword::Else)
        ) {
            self.next();
            Some(Box::new(self.parse_statement_internal()?))
        } else {
            None
        };

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: StatementAST::IfElseStatement(IfElseStatementAST {
                condition,
                then_statement,
                else_statement,
            }),
        })
    }

    // parse a variable declaration statement
    fn parse_variable_declaration_statement(
        &mut self,
    ) -> Option<PositionWrapper<StatementAST<'src>>>
    where
        'token: 'src,
    {
        // get the first token in variable declaration statement
        // it can either be `let` or `var`
        let first_token = self.next_significant_token();

        let is_mutable = match first_token.token_type {
            TokenType::Keyword(Keyword::Let) => false,
            TokenType::Keyword(Keyword::Var) => true,
            _ => unreachable!("parse_statement should have handled this case"),
        };

        // next, expect an identifier
        let variable_name = self.expect_identifier()?;

        // next, expect an assignment operator
        self.expect_punctuation('=')?;

        // next, expect an expression
        let expression = self.parse_expression_internal()?;

        // finally, expect a semicolon
        self.expect_punctuation(';')?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: StatementAST::VariableDeclarationStatement(VariableDeclarationStatementAST {
                variable_name,
                is_mutable,
                expression,
            }),
        })
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/// DECLARATION PARSING FUNCTIONS
////////////////////////////////////////////////////////////////////////////////////////////////////

impl<'token, 'src> Parser<'token, 'src> {
    /// Parse the current token pointed by the `current` field as a declaration.
    fn parse_declaration_internal(&mut self) -> Option<PositionWrapper<DeclarationAST<'src>>>
    where
        'token: 'src,
    {
        // check if this declaration is exported
        let export_token = if matches!(
            self.peek_significant_token().token_type,
            TokenType::Keyword(Keyword::Export)
        ) {
            Some(self.next())
        } else {
            None
        };

        // parse the declaration
        let mut declaration = {
            let token = self.peek_significant_token().clone();

            match token.token_type {
                TokenType::Keyword(Keyword::Class) => self.parse_class_declaration()?,
                _ => {
                    // make progress
                    self.next();

                    return self.create_error(SyntacticError::UnexpectedToken {
                        unexpected_position_range: token.position_range.clone(),
                        parsing_context: ParsingContext::Declaration,
                    });
                }
            }
        };

        // assign `export`ness to the declaration
        if let Some(export_token) = export_token {
            match &mut declaration.value {
                DeclarationAST::ClassDeclaration(class_decl) => class_decl.export = true,
            }

            // update the position range of the declaration
            declaration.position_range.start = export_token.position_range.start;
        }

        Some(declaration)
    }

    /// Parse the current token pointed by the `current` field as a type unit.
    fn parse_type_unit(&mut self) -> Option<PositionWrapper<TypeUnitAST<'src>>>
    where
        'token: 'src,
    {
        // get the first token
        let first_token = self.peek_significant_token();

        // match the first token to determine the type unit
        let value = match first_token.token_type {
            TokenType::Identifier(_) => {
                TypeUnitAST::QualifiedName(self.parse_qualified_name()?.value)
            }
            TokenType::Keyword(Keyword::Bool) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Bool)
            }
            TokenType::Keyword(Keyword::Void) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Void)
            }
            TokenType::Keyword(Keyword::Int8) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int8)
            }
            TokenType::Keyword(Keyword::Int16) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int16)
            }
            TokenType::Keyword(Keyword::Int32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int32)
            }
            TokenType::Keyword(Keyword::Int64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int64)
            }
            TokenType::Keyword(Keyword::Uint8) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint8)
            }
            TokenType::Keyword(Keyword::Uint16) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint16)
            }
            TokenType::Keyword(Keyword::Uint32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint32)
            }
            TokenType::Keyword(Keyword::Uint64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint64)
            }
            TokenType::Keyword(Keyword::Float32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Float32)
            }
            TokenType::Keyword(Keyword::Float64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Float64)
            }
            _ => {
                return self.create_error(SyntacticError::IdentifierExpected {
                    expected_position: first_token.position_range.start,
                });
            }
        };

        if let TypeUnitAST::PrimitiveTypeUnit(_) = value {
            self.next();
        }

        Some(PositionWrapper {
            position_range: first_token.position_range,
            value,
        })
    }

    /// Parse the current token pointed by the `current` field as a type annotation.
    fn parse_type_annotation(&mut self) -> Option<PositionWrapper<TypeAnnotationAST<'src>>>
    where
        'token: 'src,
    {
        let type_unit = self.parse_type_unit()?;
        Some(PositionWrapper {
            position_range: type_unit.position_range,
            value: TypeAnnotationAST::TypeUnit(type_unit.value),
        })
    }

    /// Parse the current token pointed by the `current` field as a class member declaration.
    fn parse_class_member_declaration(
        &mut self,
    ) -> Option<PositionWrapper<ClassMemberDeclarationAST<'src>>>
    where
        'token: 'src,
    {
        let token = self.peek_significant_token().clone();

        match token.token_type {
            TokenType::Keyword(Keyword::Function) => self.parse_class_method_declaration(),
            TokenType::Identifier(_) => self.parse_class_field_declaration(),
            _ => {
                // make progress
                self.next();

                self.create_error(SyntacticError::UnexpectedToken {
                    unexpected_position_range: token.position_range,
                    parsing_context: ParsingContext::ClassMemberDeclaration,
                })
            }
        }
    }

    /// Parse the current token pointed by the `current` field as a class field declaration.
    fn parse_class_field_declaration(
        &mut self,
    ) -> Option<PositionWrapper<ClassMemberDeclarationAST<'src>>>
    where
        'token: 'src,
    {
        // expect the name of the field
        let identifier = self.expect_identifier()?;

        // expect a colon
        self.expect_punctuation(':')?;

        // expect the type annotation
        let type_annotation = self.parse_type_annotation()?;

        // expect a semicolon
        let semi_colon = self.expect_punctuation(';')?;

        Some(PositionWrapper {
            position_range: identifier.position_range.start..semi_colon.position_range.end,
            value: ClassMemberDeclarationAST::ClassFieldDeclaration(ClassFieldDeclarationAST {
                access_modifier: None, // to be determined
                identifier,
                type_annotation,
            }),
        })
    }

    /// Parse the current token pointed by the `current` field as a class method declaration.
    fn parse_class_method_declaration(
        &mut self,
    ) -> Option<PositionWrapper<ClassMemberDeclarationAST<'src>>>
    where
        'token: 'src,
    {
        // expect `function` keyword
        let first_token = self.expect_keyword(Keyword::Function)?;

        // expect the name of the method
        let identifier = self.expect_identifier()?;

        // expect opening parenthesis
        self.expect_punctuation('(')?;

        // Parse a parameter of a function
        let parameters =
            self.parse_separated_list::<',', ')', PositionWrapper<ParameterAST>>(|this| {
                // if the next token is `mutable`, then the parameter is mutable
                let is_mutable = {
                    if matches!(
                        this.peek_significant_token().token_type,
                        TokenType::Keyword(Keyword::Mutable)
                    ) {
                        this.next();
                        true
                    } else {
                        false
                    }
                };

                // expect the name of the parameter
                let identifier = this.expect_identifier()?;

                // expect a colon
                this.expect_punctuation(':')?;

                // expect the type annotation of the paramter
                let type_annotation = this.parse_type_annotation()?;

                // return the parameter
                Some(PositionWrapper {
                    position_range: identifier.position_range.start
                        ..type_annotation.position_range.end,
                    value: ParameterAST {
                        identifier,
                        qualified_type_annotation: QualifiedTypeAnnotation {
                            type_annotation,
                            is_mutable,
                        },
                    },
                })
            })?;

        // expect a colon
        self.expect_punctuation(':')?;

        // expect the return type
        let return_type_annotation = self.parse_type_annotation()?;

        // expect block scope statement
        let body = {
            // get the statement
            let body_statement = self.parse_block_scope_statement()?;

            // transform the statement into a block statement
            let value = match body_statement.value {
                StatementAST::BlockScopeStatement(block_statement) => block_statement,
                _ => unreachable!("Block scope statement is not a block statement"),
            };

            PositionWrapper {
                position_range: body_statement.position_range,
                value,
            }
        };

        Some(PositionWrapper {
            position_range: first_token.position_range.start..body.position_range.end,
            value: ClassMemberDeclarationAST::ClassMethodDeclaration(ClassMethodDeclarationAST {
                access_modifier: None, // to be determined later
                identifier,
                parameters,
                return_type_annotation,
                body,
            }),
        })
    }

    /// Parse the current token pointed by the `current` field as a class declaration.
    fn parse_class_declaration(&mut self) -> Option<PositionWrapper<DeclarationAST<'src>>>
    where
        'token: 'src,
    {
        // expect `class` keyword
        let first_token = self.expect_keyword(Keyword::Class)?;

        // name of the class
        let identifier = self.expect_identifier()?;

        // expect opening brace
        self.expect_punctuation('{')?;

        // the list of members of the class
        let mut members = Vec::new();

        // parse the members of the class
        while !matches!(
            self.peek_significant_token().token_type,
            TokenType::Punctuation('}')
        ) && !matches!(
            self.peek_significant_token().token_type,
            TokenType::EndOfFile
        ) {
            // check if the member is public or private or not specified
            let access_modifier = if matches!(
                self.peek_significant_token().token_type,
                TokenType::Keyword(Keyword::Public | Keyword::Private)
            ) {
                let token = self.next();
                let access_mod = match token.token_type {
                    TokenType::Keyword(Keyword::Public) => AccessModifier::Public,
                    TokenType::Keyword(Keyword::Private) => AccessModifier::Private,
                    _ => unreachable!(),
                };

                Some(PositionWrapper {
                    position_range: token.position_range,
                    value: access_mod,
                })
            } else {
                None
            };

            // parse the member
            if let Some(mut member) = self.parse_class_member_declaration() {
                // if the member has an access modifier, update the position range of the member
                if let Some(access_modifier) = &access_modifier {
                    member.position_range.start = access_modifier.position_range.start;
                }

                // assign the access modifier to the member
                match &mut member.value {
                    ClassMemberDeclarationAST::ClassMethodDeclaration(method) => {
                        method.access_modifier = access_modifier;
                    }
                    ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                        field.access_modifier = access_modifier;
                    }
                }

                // add the member to the list of members
                members.push(member);
            } else {
                // skip until the next member
                self.skip_to(|token: &Token| {
                    matches!(
                        token.token_type,
                        TokenType::Punctuation('}')
                            | TokenType::Keyword(Keyword::Public)
                            | TokenType::Keyword(Keyword::Private)
                            | TokenType::Keyword(Keyword::Function)
                            | TokenType::Identifier(_)
                    )
                });
            }
        }

        // expect closing brace
        self.expect_punctuation('}')?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..self.peek().position_range.start,
            value: DeclarationAST::ClassDeclaration(ClassDeclarationAST {
                export: false, // to be determined
                identifier,
                members,
            }),
        })
    }

    fn parse_import_module_declaration(&mut self) -> Option<PositionWrapper<ImportModuleAST<'src>>>
    where
        'token: 'src,
    {
        // expect `import` keyword
        let first_token = self.expect_keyword(Keyword::Import)?;

        // expect the qualified name of the module
        let qualified_name = self.parse_qualified_name()?;

        // expect the semicolon
        let semi_colon = self.expect_punctuation(';')?;

        Some(PositionWrapper {
            position_range: first_token.position_range.start..semi_colon.position_range.end,
            value: ImportModuleAST { qualified_name },
        })
    }

    /// Parse all the tokens in the token stream into a [`FileAST`].
    fn parse_file_internal(&mut self) -> Option<FileAST<'src>>
    where
        'token: 'src,
    {
        // flag to indicate if an error has been found
        let mut found_error = false;

        // expect module keyword
        let module_keyword = self.expect_keyword(Keyword::Module)?;

        // expect the qualified name of the module
        let qualified_name = self.parse_qualified_name()?;

        // expect the semicolon
        let semi_colon = self.expect_punctuation(';')?;

        // create the module
        let module = PositionWrapper {
            position_range: module_keyword.position_range.start..semi_colon.position_range.end,
            value: ModuleAST { qualified_name },
        };

        // the list of imports
        let import_modules = {
            let mut import_modules = Vec::new();

            while matches!(
                self.peek_significant_token().token_type,
                TokenType::Keyword(Keyword::Import)
            ) {
                if let Some(import_module_declaration) = self.parse_import_module_declaration() {
                    import_modules.push(import_module_declaration);
                } else {
                    found_error = true;

                    // skip until the next import module declaration
                    self.skip_to(|token| {
                        matches!(
                            token.token_type,
                            TokenType::Keyword(Keyword::Import)
                                | TokenType::Keyword(Keyword::Class)
                                | TokenType::Keyword(Keyword::Export)
                        )
                    });
                }
            }

            import_modules
        };

        // the list of declarations
        let declarations = {
            let mut declarations = Vec::new();

            // the list of declarations
            while !matches!(
                self.peek_significant_token().token_type,
                TokenType::EndOfFile
            ) {
                // parse the declaration
                if let Some(declaration) = self.parse_declaration_internal() {
                    // add the declaration to the list of declarations
                    declarations.push(declaration);
                } else {
                    found_error = true;

                    // skip until the next declaration
                    self.skip_to(|token| {
                        matches!(
                            token.token_type,
                            TokenType::Keyword(Keyword::Class)
                                | TokenType::Keyword(Keyword::Export)
                        )
                    });
                }
            }

            declarations
        };

        if found_error {
            None
        } else {
            Some(FileAST {
                module,
                import_modules,
                declarations,
            })
        }
    }
}

#[cfg(test)]
mod tests;
