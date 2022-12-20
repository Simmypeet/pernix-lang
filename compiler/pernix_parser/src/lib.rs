pub mod abstract_syntax_tree;
pub mod error;

use abstract_syntax_tree::{
    declaration::{
        Declaration, FunctionDeclaration, NamespaceDeclaration, TypeAnnotation,
        UsingDirective,
    },
    expression::{BinaryExpression, Expression, FunctionCallExpression, LiteralExpression},
    statement::{Statement, WhileStatement},
    BinaryOperator, PositionWrapper, UnaryOperator,
};
use error::{Context, Error};
use pernix_lexer::{
    token::{Keyword, Token, TokenKind},
    Lexer,
};
use pernix_project::source_code::SourceCode;

use crate::abstract_syntax_tree::{
    declaration::QualifiedType,
    expression::{UnaryExpression, IdentifierExpression},
    statement::{IfElseStatement, BlockScopeStatement, VariableDeclarationStatement, ReturnStatement},
};

/// Represent a state-machine data structure that is used to parse a Pernix
/// source code file.
pub struct Parser<'a> {
    // The lexer that is used to generate the token stream
    lexer: Lexer<'a>,
    // the accumulated tokens that have been tokenized by the lexer so far
    accumulated_tokens: Vec<Token<'a>>,
    // the accumulated errors that have been found during parsing so far
    accumulated_errors: Vec<Error<'a>>,
    // The current position in the source code
    current_index: usize,
    // Flag that indicates whether the parser should produce errors into the
    // list or not
    produce_errors: bool,
}

/// Represent an AST structure that represents a Pernix source file.
#[derive(Debug, Clone)]
pub struct File<'a> {
    declarations: Vec<PositionWrapper<Declaration<'a>>>,
    using_directives: Vec<PositionWrapper<UsingDirective<'a>>>,
    source_reference: &'a SourceCode,
}

impl<'a> File<'a> {
    /// Return a reference to the declarations of this [`File`].
    pub fn declarations(&self) -> &[PositionWrapper<Declaration>] {
        self.declarations.as_ref()
    }

    /// Return a reference to the using directives of this [`File`].
    pub fn using_directives(&self) -> &[PositionWrapper<UsingDirective>] {
        self.using_directives.as_ref()
    }

    /// Return a reference to the source reference of this [`File`].
    pub fn source_reference(&self) -> &SourceCode {
        self.source_reference
    }
}

macro_rules! parse_unwrap {
    ($sf:ident, $parse:expr, $rollback_index:expr) => {
        match $parse {
            Some(val) => val,
            None => {
                $sf.current_index = $rollback_index;
                return None;
            }
        }
    };
}

macro_rules! parse_setup {
    ($sf:ident, $starting_index:ident, $starting_position:ident) => {
        $sf.move_to_significant();
        let $starting_index = $sf.current_index;
        let $starting_position = $sf.peek().position_range().clone();
    };
}

macro_rules! parse_exit_error {
    ($sf:ident, $error:expr, $rollback_index:expr) => {
        if $sf.produce_errors {
            $sf.accumulated_errors.push($error);
        }
        $sf.current_index = $rollback_index;
        return None;
    };

    ($sf:ident, $error:expr) => {
        if $sf.produce_errors {
            $sf.accumulated_errors.push($error);
        }
        return None;
    };
}

impl<'a> Parser<'a> {
    /// Create a new parser instance targeting the given `source_code`.
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
            current_index: 0,
            produce_errors: true,
        }
    }

    /// Get the source code that is being parsed.
    pub fn source_code(&self) -> &'a SourceCode {
        self.lexer.source_code()
    }

    // Get the current token stream and moves the current position forward
    fn next(&mut self) -> &Token<'a> {
        // need to generate more tokens
        if self.current_index == self.accumulated_tokens.len() - 1 {
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
        else if self.current_index >= self.accumulated_tokens.len() {
            panic!("current position is greater than or equal the length of the accumulated tokens");
        }

        // increment the current position
        self.current_index += 1;

        &self.accumulated_tokens[self.current_index - 1]
    }

    // Move the current position to the next significant token and emits it
    fn next_significant(&mut self) -> &Token<'a> {
        self.move_to_significant();
        self.next()
    }

    // Move the current position to the next significant token
    fn move_to_significant(&mut self) {
        while !self.peek().is_significant_token() {
            self.next();
        }
    }

    // Move the current position to the next significant token and reads it
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

    // Skip the tokens until the predicate returns true. The parser also
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

    // Get the current token without moving the current position forward
    fn peek(&self) -> &Token<'a> {
        &self.accumulated_tokens[self.current_index]
    }

    // Get the token back by one position without moving the current position
    fn peek_back(&self) -> &Token<'a> {
        &self.accumulated_tokens[self.current_index - 1]
    }

    /// Clear the accumulated errors and returns them
    pub fn pop_errors(&mut self) -> Vec<Error<'a>> {
        let mut errors = Vec::new();
        std::mem::swap(&mut errors, &mut self.accumulated_errors);
        errors
    }

    ////////////////////////////////////////////////////////////////////////////
    /// FILE
    ////////////////////////////////////////////////////////////////////////////

    /// Parse all token stream as a file
    ///
    /// File:
    ///     Using_Directive* Declaration*
    pub fn parse_file(&mut self) -> File<'a> {
        let mut file = File {
            declarations: Vec::new(),
            using_directives: Vec::new(),
            source_reference: self.source_code()
        };

        let file_skip_predicate = |token: &Token| {
            matches!(token.token_kind(), TokenKind::Keyword(Keyword::Namespace))
        };

        // loop through all the declarations
        let mut parsing_using = true;

        // loop through all the declarations until closing curly bracket or
        // EOF is found
        while !matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator('}') | TokenKind::EndOfFile
        ) {
            if let TokenKind::Keyword(Keyword::Using) = self.peek().token_kind()
            {
                let using_directive = self.parse_using_directive();

                if let Some(using_directive) = using_directive {
                    file.using_directives.push(using_directive.clone());

                    if !parsing_using {
                        if self.produce_errors {
                            self.accumulated_errors.push(
                             Error::UsingDirectiveMustAppearPriorToAllDeclarations {
                                 using_directive,
                                 
                             }
                         );
                        }
                    }
                } else {
                    // make progress
                    self.next();
                }
                self.skip_to(file_skip_predicate);
            } else if let Some(declaration) = self.parse_declaration() {
                parsing_using = false;
                file.declarations.push(declaration);
            } else {
                // make progress
                self.next();
                self.skip_to(file_skip_predicate);
            }
        }


        file
    }

    ////////////////////////////////////////////////////////////////////////////
    /// DECLARATIONS
    ////////////////////////////////////////////////////////////////////////////

    /// Parse the current token stream as a declaration
    ///
    /// Qualified_Name:
    ///    Identifier ('.' Identifier)*
    ///
    /// Using_Directive:
    ///     'using' Qualified_Name ';'
    ///
    /// Namespace_Declaration:
    ///    'namespace' Qualified_Name '{' Using_Directive* Declaration* '}'
    ///
    /// Declaration:
    ///     Namespace_Declaration
    pub fn parse_declaration(
        &mut self,
    ) -> Option<PositionWrapper<Declaration<'a>>> {
        match self.peek_significant().token_kind() {
            TokenKind::Keyword(Keyword::Namespace) => {
                self.parse_namespace_declaration()
            }
            TokenKind::Identifier => {
                self.parse_function_declaration()
            }
            _ => {
                parse_exit_error!(
                    self,
                    Error::UnexpectedToken {
                        context: Context::Declaration,
                        found_token: self.peek().clone(),
                    }
                );
            }
        }
    }

    // Parses the current token stream as a function declaration
    fn parse_function_declaration(
        &mut self,
    ) -> Option<PositionWrapper<Declaration<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // expect the function declaration
        let return_type = parse_unwrap!(self, self.parse_type_anootation(), starting_index);

        // expect the function name
        let function_name = if matches!(
            self.next_significant().token_kind(),
            TokenKind::Identifier
        ) {
            PositionWrapper {
                position: self.peek_back().position_range().clone(),
                value: self.peek_back().lexeme(),
            }
        } else {
            parse_exit_error!(
                self,
                Error::IdentifierExpected {
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        };

        // expect the opening parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('(')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '(',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // parse the function parameters
        let parameters = {
            let skip_predicate = |token: &Token| {
                matches!(
                    token.token_kind(),
                    TokenKind::Punctuator(')') | TokenKind::Punctuator(',')
                )
            };

            let mut params = Vec::new();
            let mut first_time = true;

            loop {
                // found a closing parenthesis, we're done
                if matches!(
                    self.peek_significant().token_kind(),
                    TokenKind::Punctuator(')')
                ) {
                    break;
                }

                // not first parameter, expect a comma
                if !first_time {
                    // expect a comma
                    if !matches!(
                        self.next_significant().token_kind(),
                        TokenKind::Punctuator(',')
                    ) {
                        if self.produce_errors {
                            self.accumulated_errors.push(
                                Error::PunctuatorExpected {
                                    expected_punctuator: ',',
                                    found_token: self.peek_back().clone(),
                                    
                                },
                            );
                        }

                        // skip to the next comma or closing parenthesis
                        self.skip_to(skip_predicate);

                        continue;
                    }
                }

                // parse the parameter

                // set first_time to false
                first_time = false;


                let is_mutable = {
                    if matches!(
                        self.peek_significant().token_kind(),
                        TokenKind::Keyword(Keyword::Mutable)
                    ) {
                        // eat the mut keyword
                        self.next();
                        true
                    } else {
                        false
                    }
                };
                // parse the type annotation
                let qualified_type_annotation =
                match self.parse_type_anootation() {
                    Some(type_annotation) => QualifiedType {
                        is_mutable,
                        type_annotation,
                    },
                    None => {
                        // make progress
                        self.next();

                        // skip to the next comma or closing parenthesis
                        self.skip_to(skip_predicate);

                        continue;
                    }
                };

                let parameter_starting_position =
                    self.peek().position_range().clone();

                let parameter_name = if matches!(
                    self.next_significant().token_kind(),
                    TokenKind::Identifier
                ) {
                    self.peek_back().lexeme()
                } else {
                    if self.produce_errors {
                        self.accumulated_errors.push(
                            Error::IdentifierExpected {
                                found_token: self.peek_back().clone(),
                                
                            },
                        );
                    }

                    // skip to the next comma or closing parenthesis
                    self.skip_to(skip_predicate);

                    continue;
                };

                params.push(PositionWrapper {
                    position: parameter_starting_position.start
                        ..self.peek_back().position_range().end,
                    value: (qualified_type_annotation, parameter_name),
                });
            }  

            params
        };

        // expect the closing parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(')')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ')',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect a scope
        let scope =
            parse_unwrap!(self, self.parse_block_scope_statement(), starting_index);
        let body = match scope.value {
            Statement::BlockScopeStatement(scope_statement) => PositionWrapper {
                position: scope.position,
                value: scope_statement,
            },
            _ => unreachable!(),
        };

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Declaration::FunctionDeclaration(FunctionDeclaration {
                function_name,
                parameters,
                return_type,
                body,
            }),
        })
    }

    // Parse the current token stream as a qualified name string.
    fn parse_qualified_name(&mut self) -> Option<PositionWrapper<&'a str>> {
        parse_setup!(self, starting_index, starting_position);

        // expect the first identifier
        if !matches!(self.next().token_kind(), TokenKind::Identifier) {
            parse_exit_error!(
                self,
                Error::IdentifierExpected {
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // found additional scopes
        while matches!(self.peek().token_kind(), TokenKind::Punctuator('.')) {
            // eat the dot
            self.next();

            // expect the next identifier
            if !matches!(self.next().token_kind(), TokenKind::Identifier) {
                parse_exit_error!(
                    self,
                    Error::IdentifierExpected {
                        found_token: self.peek_back().clone(),
                        
                    },
                    starting_index
                );
            }
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: &self.source_code().source_code()[starting_position
                .start
                .byte_index
                ..self.peek_back().position_range().end.byte_index],
        })
    }

    // Parse the current token stream as a using_statement.
    fn parse_using_directive(
        &mut self,
    ) -> Option<PositionWrapper<UsingDirective<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // expect the `using` keyword
        if !matches!(
            self.next().token_kind(),
            TokenKind::Keyword(Keyword::Using)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::Using,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        let qualified_name =
            parse_unwrap!(self, self.parse_qualified_name(), starting_index);

        // expect the semicolon
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(';')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ';',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: UsingDirective {
                namespace_name: qualified_name,
            },
        })
    }

    // Parse the current token stream as a type
    fn parse_type_anootation(
        &mut self,
    ) -> Option<PositionWrapper<TypeAnnotation<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // for now, we only support identifiers
        if !matches!(self.next().token_kind(), TokenKind::Identifier) {
            parse_exit_error!(
                self,
                Error::IdentifierExpected {
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: TypeAnnotation::QualifiedName(self.peek_back().lexeme()),
        })
    }

    // Parse the current token stream as a namespace declaration.
    fn parse_namespace_declaration(
        &mut self,
    ) -> Option<PositionWrapper<Declaration<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        let namespace_skip_predicate = |token: &Token| {
            matches!(
                token.token_kind(),
                TokenKind::Keyword(Keyword::Namespace)
                    | TokenKind::Keyword(Keyword::Using)
                    | TokenKind::Punctuator('}')
            )
        };

        // expect the `namespace` keyword
        if !matches!(
            self.next().token_kind(),
            TokenKind::Keyword(Keyword::Namespace)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::Namespace,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        let namespace_name =
            parse_unwrap!(self, self.parse_qualified_name(), starting_index);

        // expect the opening curly bracket
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('{')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '{',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // loop through all the declarations
        let mut declarations = Vec::new();
        let mut using_directives = Vec::new();
        let mut parsing_using = true;

        // loop through all the declarations until closing curly bracket or
        // EOF is found
        while !matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator('}') | TokenKind::EndOfFile
        ) {
            if let TokenKind::Keyword(Keyword::Using) = self.peek().token_kind()
            {
                let using_directive = self.parse_using_directive();

                if let Some(using_directive) = using_directive {
                    using_directives.push(using_directive.clone());

                    if !parsing_using {
                        if self.produce_errors {
                            self.accumulated_errors.push(
                                Error::UsingDirectiveMustAppearPriorToAllDeclarations {
                                    using_directive,
                                    
                                }
                            );
                        }
                    }
                } else {
                    // make progress
                    self.next();
                }
                self.skip_to(namespace_skip_predicate);
            } else if let Some(declaration) = self.parse_declaration() {
                parsing_using = false;
                declarations.push(declaration);
            } else {
                // make progress
                self.next();
                self.skip_to(namespace_skip_predicate);
            }
        }

        // expect the closing curly bracket
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('}')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '}',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Declaration::NamespaceDeclaration(NamespaceDeclaration {
                namespace_name,
                using_directives,
                declarations,
            }),
        })
    }

    ////////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS
    ////////////////////////////////////////////////////////////////////////////

    /// Parse the current token stream as an expression
    ///
    /// Unary_Expression:
    ///     Unary_Operator Primary_Expression
    ///
    /// Identifier_Expression:
    ///     Qualified_Identifier
    ///
    /// Literal_Expression:
    ///     `LiteralToken`
    ///
    /// Function_Call_Expression:
    ///     Identifier_Expression `(` Argument_List? `)`
    ///
    /// Argument_List:
    ///     Expression (`,` Expression)*
    ///
    /// Parenthesized_Expression:
    ///     `(` Expression `)`
    ///
    /// Primary_Expression:
    ///     Identifier_Expression |
    ///     Literal_Expression |
    ///     Function_Call_Expression |
    ///     Parenthesized_Expression
    ///     Unary_Expression
    ///    
    /// Expression:
    ///     Primary_Expression (Binary_Operator Primary_Expression)*
    pub fn parse_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        self.parse_expression_bin(0)
    }

    // parse an expression with the unary operator
    fn parse_unary_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        let operator = match self.next().token_kind() {
            TokenKind::Punctuator('!') => UnaryOperator::LogicalNot,
            TokenKind::Punctuator('-') => UnaryOperator::Minus,
            TokenKind::Punctuator('+') => UnaryOperator::Plus,
            _ => {
                parse_exit_error!(
                    self,
                    Error::UnexpectedToken {
                        context: Context::Expression,
                        found_token: self.peek_back().clone(),
                        
                    },
                    starting_index
                );
            }
        };

        let operand = parse_unwrap!(
            self,
            self.parse_primary_expression(),
            starting_index
        );

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Expression::UnaryExpression(UnaryExpression {
                operator: PositionWrapper {
                    position: starting_position,
                    value: operator,
                },
                operand: Box::new(operand),
            }),
        })
    }

    fn parse_binary_operator_roll_back(
        &mut self,
    ) -> Option<PositionWrapper<BinaryOperator>> {
        let starting_index = self.current_index;

        // move to the next significant token
        self.move_to_significant();
        let operator_position_range = self.peek().position_range().clone();

        let bin_op = match self.next().token_kind() {
            TokenKind::Punctuator('+')
            | TokenKind::Punctuator('-')
            | TokenKind::Punctuator('*')
            | TokenKind::Punctuator('/')
            | TokenKind::Punctuator('%') => {
                let punctuator = match self.peek_back().token_kind() {
                    TokenKind::Punctuator(c) => *c,
                    _ => unreachable!(),
                };

                // check for compound assignment
                match self.peek().token_kind() {
                    TokenKind::Punctuator('=') => {
                        self.next();
                        match punctuator {
                            '+' => BinaryOperator::CompoundAddition,
                            '-' => BinaryOperator::CompoundSubtraction,
                            '*' => BinaryOperator::CompoundMultiplication,
                            '/' => BinaryOperator::CompoundDivision,
                            '%' => BinaryOperator::CompoundRemainder,
                            _ => unreachable!(),
                        }
                    }
                    _ => match punctuator {
                        '+' => BinaryOperator::Add,
                        '-' => BinaryOperator::Subtract,
                        '*' => BinaryOperator::Multiply,
                        '/' => BinaryOperator::Divide,
                        '%' => BinaryOperator::Remainder,
                        _ => unreachable!(),
                    },
                }
            }
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
                    BinaryOperator::Assignment
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
                    self.current_index = starting_index;
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
                    self.current_index = starting_index;
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
                    self.current_index = starting_index;
                    return None;
                }
            }
            _ => {
                self.current_index = starting_index;
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
            BinaryOperator::Assignment => 1,
            BinaryOperator::LogicalOr => 2,
            BinaryOperator::LogicalAnd => 3,
            BinaryOperator::Equal
            | BinaryOperator::NotEqual
            | BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => 4,
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::CompoundAddition
            | BinaryOperator::CompoundSubtraction => 5,
            BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Remainder
            | BinaryOperator::CompoundMultiplication
            | BinaryOperator::CompoundDivision
            | BinaryOperator::CompoundRemainder => 6,
        }
    }

    fn parse_expression_bin(
        &mut self,
        parent_precedence: usize,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        let mut left = self.parse_primary_expression()?;

        loop {
            let starting_index = self.current_index;

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
                self.current_index = starting_index;
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

    // parses a list of arguments
    fn parse_argument_list(
        &mut self,
    ) -> Option<Vec<PositionWrapper<Expression<'a>>>> {
        self.move_to_significant();
        let starting_index = self.current_index;

        // starts with a opening parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('(')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '(',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
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
            let argument = self.parse_expression();

            // if the argument is not None, then we add it to the list
            if let Some(argument) = argument {
                arguments.push(argument);
            } else {
                self.skip_to(|token| {
                    matches!(
                        token.token_kind(),
                        TokenKind::Punctuator(',') | TokenKind::Punctuator(')')
                    )
                });
            }

            match self.next_significant().token_kind() {
                TokenKind::Punctuator(',') => (),
                TokenKind::Punctuator(')') => break,
                _ => {
                    if self.produce_errors {
                        self.accumulated_errors.push(
                            Error::PunctuatorExpected {
                                expected_punctuator: ')',
                                found_token: self.peek_back().clone(),
                                
                            },
                        );
                    }

                    self.skip_to(|token| {
                        matches!(
                            token.token_kind(),
                            TokenKind::Punctuator(')')
                                | TokenKind::Punctuator(',')
                        )
                    });

                    match self.next().token_kind() {
                        TokenKind::Punctuator(')') => break,
                        TokenKind::Punctuator(',') => {
                            self.next();
                        }
                        _ => {
                            self.current_index = starting_index;
                            return None;
                        }
                    }
                }
            };
        }

        Some(arguments)
    }

    // parses an expression that starts with an identifier
    fn parse_identifier_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        let qualified_name =
            parse_unwrap!(self, self.parse_qualified_name(), starting_index);

        match self.peek_significant().token_kind() {
            TokenKind::Punctuator('(') => {
                let arguments = parse_unwrap!(
                    self,
                    self.parse_argument_list(),
                    starting_index
                );

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
                value: Expression::IdentifierExpression(IdentifierExpression { identifier: qualified_name.value} ),
            }),
        }
    }

    // Parses the current token stream as a primary expression
    fn parse_primary_expression(
        &mut self,
    ) -> Option<PositionWrapper<Expression<'a>>> {
        self.move_to_significant();
        let starting_index = self.current_index;

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
                    value: Expression::LiteralExpression(LiteralExpression {
                        literal_expression: val,
                    }),
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
                    parse_exit_error!(
                        self,
                        Error::PunctuatorExpected {
                            expected_punctuator: ')',
                            found_token: self.peek_back().clone(),
                            
                        },
                        starting_index
                    );
                } else {
                    Some(expression)
                }
            }
            _ => {
                parse_exit_error!(
                    self,
                    Error::UnexpectedToken {
                        context: Context::Expression,
                        found_token: self.peek().clone(),
                        
                    },
                    starting_index
                );
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    /// STATEMENTS
    ////////////////////////////////////////////////////////////////////////////

    /// Parses the current token stream as a statement
    ///
    /// Return_Statement:
    ///     `return` Expression? `;`
    ///
    /// Expression_Statement:
    ///     Expression `;`
    ///
    /// Variable_Declaration_Statement:
    ///     (`let` | Type) mutable? Identifier `=` Expression `;`
    ///
    /// Scope:
    ///     `{` Statement* `}`
    ///
    /// If_Statement:
    ///     `if` `(` Expression `)` Statement  
    ///     (`else` Statement)?
    ///
    /// Statement:
    ///     Return_Statement |
    ///     Expression_Statement |
    ///     Variable_Declaration_Statement |
    ///     Scope |
    ///     If_Statement
    pub fn parse_statement(
        &mut self,
    ) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        match self.peek_significant().token_kind() {
            // return statement
            TokenKind::Keyword(Keyword::Return) => {
                return self.parse_return_statement();
            }
            TokenKind::Keyword(Keyword::While) => {
                return self.parse_while_statement();
            }
            TokenKind::Keyword(Keyword::Mutable) |
            TokenKind::Keyword(Keyword::Let) => {
                return self.parse_variable_declaration_statement();
            }
            TokenKind::Punctuator('{') => {
                return self.parse_block_scope_statement();
            }
            TokenKind::Keyword(Keyword::If) => {
                return self.parse_if_else_statement();
            }
            TokenKind::Keyword(Keyword::Break) |
            TokenKind::Keyword(Keyword::Continue) => {
                let statement = match self.peek().token_kind() {
                    TokenKind::Keyword(Keyword::Break) => Statement::BreakStatement,
                    TokenKind::Keyword(Keyword::Continue) => Statement::ContinueStatement,
                    _ => unreachable!(),
                };

                // expect a semi colon
                if !matches!(
                    self.next_significant().token_kind(),
                    TokenKind::Punctuator(';')
                ) {
                    parse_exit_error!(
                        self,
                        Error::PunctuatorExpected {
                            expected_punctuator: ';',
                            found_token: self.peek_back().clone(),
                            
                        },
                        starting_index
                    );
                } else {
                    return Some(PositionWrapper {
                        position: starting_position.start
                            ..self.peek_back().position_range().end,
                        value: statement,
                    });
                }
            },
            _ => {
                // try to parse an expression
                self.produce_errors = false;
                let expression = self.parse_expression();
                self.produce_errors = true;

                match expression {
                    Some(expr) => {
                        // expect a semicolon
                        if !matches!(
                            self.next_significant().token_kind(),
                            TokenKind::Punctuator(';')
                        ) {
                            parse_exit_error!(
                                self,
                                Error::PunctuatorExpected {
                                    expected_punctuator: ';',
                                    found_token: self.peek_back().clone(),
                                    
                                },
                                starting_index
                            );
                        } else {
                            Some(PositionWrapper {
                                position: starting_position.start
                                    ..self.peek_back().position_range().end,
                                value: Statement::ExpressionStatement(expr),
                            })
                        }
                    }
                    None => {
                        parse_exit_error!(
                            self,
                            Error::UnexpectedToken {
                                context: Context::Statement,
                                found_token: self.peek().clone(),
                                
                            },
                            starting_index
                        );
                    }
                }
            }
        }
    }

    // Parse the current token stream as a while loop statement
    fn parse_while_statement(
        &mut self
    ) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);
    
        // expect while keyword
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Keyword(Keyword::While)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::While,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect opening parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('(')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '(',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // parse the condition
        let condition = parse_unwrap!(self, self.parse_expression(), starting_index);

        // expect closing parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(')')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ')',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }
        
        // expect a statement
        let loop_statement = parse_unwrap!(self, self.parse_statement(), starting_index);

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Statement::WhileStatement(WhileStatement {
                condition,
                loop_statement: Box::new(loop_statement)
            }),
        })
    }

    // Parse the current token stream as a return statement
    fn parse_return_statement(
        &mut self,
    ) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // expect a return keyword
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Keyword(Keyword::Return)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::Return,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        if matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator(';')
        ) {
            // eat the semicolon
            self.next();

            return Some(PositionWrapper {
                position: starting_position.start
                    ..self.peek_back().position_range().end,
                value: Statement::ReturnStatement(ReturnStatement {
                    expression: None,
                }),
            });
        }

        let expression = self.parse_expression()?;

        // expect a semicolon
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(';')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ';',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Statement::ReturnStatement(ReturnStatement { expression: Some(expression) }),
        })
    }

    fn parse_variable_declaration_statement(
        &mut self,
    ) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        let mut is_mutable = false;

        if matches!(
            self.peek_significant().token_kind(),
            TokenKind::Keyword(Keyword::Mutable)
        ) {
            // eat the mut keyword
            self.next();

            is_mutable = true;
        }

        // expect a let keyword
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Keyword(Keyword::Let)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::Let,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect an identifier
        let identifier = match self.next_significant().token_kind() {
            TokenKind::Identifier => PositionWrapper {
                position: self.peek_back().position_range().clone(),
                value: self.peek_back().clone().lexeme(),
            },
            _ => {
                parse_exit_error!(
                    self,
                    Error::IdentifierExpected {
                        found_token: self.peek_back().clone(),
                        
                    },
                    starting_index
                );
            }
        };

        // if the next token is a colon, then we have a type annotation
        let type_annotation = if matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator(':')
        ) {
            // eat the colon
            self.next();

            Some(parse_unwrap!(
                self,
                self.parse_type_anootation(),
                starting_index
            ))
        } else {
            None
        };

        // expect an equal sign
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('=')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '=',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        let expression =
            parse_unwrap!(self, self.parse_expression(), starting_index);

        // expect a semicolon
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(';')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ';',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Statement::VariableDeclarationStatement(
                VariableDeclarationStatement {
                    type_annotation,
                    is_mutable,
                    identifier,
                    expression,
                },
            ),
        })
    }

    fn parse_block_scope_statement(
        &mut self,
    ) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // expect a left curly brace
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('{')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '{',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        let mut statements = Vec::new();

        while !matches!(
            self.peek_significant().token_kind(),
            TokenKind::Punctuator('}') | TokenKind::EndOfFile
        ) {
            match self.parse_statement() {
                Some(statement) => {
                    statements.push(statement);
                }
                None => {
                    // make progress
                    self.next();

                    self.skip_to(|token| {
                        matches!(
                            token.token_kind(),
                            TokenKind::Punctuator(';')
                                | TokenKind::Punctuator('}')
                                | TokenKind::Keyword(Keyword::If)
                                | TokenKind::Keyword(Keyword::While)
                                | TokenKind::Keyword(Keyword::Let)
                                | TokenKind::Keyword(Keyword::Mutable)
                        )
                    });

                    if matches!(
                        self.peek_significant().token_kind(),
                        TokenKind::Punctuator(';')
                    ) {
                        // eat the semicolon
                        self.next();
                    }
                }
            }
        }

        // expect a right curly brace
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('}')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '}',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Statement::BlockScopeStatement(BlockScopeStatement { statements }),
        })
    }

    fn parse_if_else_statement(&mut self) -> Option<PositionWrapper<Statement<'a>>> {
        parse_setup!(self, starting_index, starting_position);

        // expect an if keyword
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Keyword(Keyword::If)
        ) {
            parse_exit_error!(
                self,
                Error::KeywordExpected {
                    expected_keyword: Keyword::If,
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect a left parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator('(')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: '(',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect an expression
        let condition =
            parse_unwrap!(self, self.parse_expression(), starting_index);

        // expect a right parenthesis
        if !matches!(
            self.next_significant().token_kind(),
            TokenKind::Punctuator(')')
        ) {
            parse_exit_error!(
                self,
                Error::PunctuatorExpected {
                    expected_punctuator: ')',
                    found_token: self.peek_back().clone(),
                    
                },
                starting_index
            );
        }

        // expect a statement
        let then_statement =
            parse_unwrap!(self, self.parse_statement(), starting_index);

        // if found an else keyword, then parse an else statement
        let else_statement = if matches!(
            self.peek_significant().token_kind(),
            TokenKind::Keyword(Keyword::Else)
        ) {
            // eat the else keyword
            self.next();

            Some(Box::new(parse_unwrap!(
                self,
                self.parse_statement(),
                starting_index
            )))
        } else {
            None
        };

        Some(PositionWrapper {
            position: starting_position.start
                ..self.peek_back().position_range().end,
            value: Statement::IfElseStatement(IfElseStatement {
                condition,
                then_statement: Box::new(then_statement),
                else_statement,
            }),
        })
    }
}

#[cfg(test)]
mod test;
