use pernixc_common::source_file::TextPosition;
use pernixc_lexical_analysis::{
    token::{Keyword, Token, TokenKind},
    token_stream::TokenStream,
};

use crate::{
    abstract_syntax_tree::{
        declaration::{
            AccessModifier, ClassDeclarationAST, ClassFieldDeclarationAST,
            ClassMemberDeclarationAST, ClassMethodDeclarationAST, NamespaceDeclarationAST,
            NamespaceLevelDeclarationAST, ParameterAST, QualifiedTypeAnnotationAST,
            TypeAnnotationAST, UsingDirectiveAST,
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
        PositionWrapper, PrimitiveTypeUnit, TypeUnitAST,
    },
    error::{ParsingContext, SyntacticError},
};

/// A parser that parses a [`TokenStream`] into a [`SyntaxTree`]. The parser is
/// implemented as a recursive descent parser with operator precedence parsing.
pub struct Parser<'src, 'token> {
    token_stream: &'token TokenStream<'src>,
    current: usize,
    accumulated_errors: Vec<SyntacticError>,
}

impl<'src, 'token> Parser<'src, 'token> {
    /// Create a new [`Parser`] from a [`TokenStream`].
    pub fn new(token_stream: &'token TokenStream<'src>) -> Self {
        Self {
            token_stream,
            current: 0,
            accumulated_errors: Vec::new(),
        }
    }

    /// Return a reference to the accumulated errors.
    pub fn errors(&self) -> &[SyntacticError] {
        &self.accumulated_errors
    }

    /// Take the accumulated errors and return them.
    pub fn pop_errors(&mut self) -> Vec<SyntacticError> {
        std::mem::take(&mut self.accumulated_errors)
    }

    /// Move the current token to the next significant token.
    pub fn move_to_significant_token(&mut self) {
        while self.current < self.token_stream.len() {
            let token = &self.token_stream[self.current];

            if token.token_kind.is_significant_token() {
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
    pub fn next(&mut self) -> Token<'src> {
        // if the `current` is the last token, then return the last token
        if self.current == self.token_stream.len() - 1 {
            self.token_stream[self.current].clone()
        } else {
            self.current += 1;
            self.token_stream[self.current - 1].clone()
        }
    }

    /// Peek the current toke and return a reference to it.
    pub fn peek(&self) -> Token<'src> {
        self.token_stream[self.current].clone()
    }

    /// Move the `current` to the next significant token and return a reference to
    /// it. If the `current` is the last token, then return the last token without
    /// moving the `current` field.
    pub fn peek_significant_token(&mut self) -> Token<'src> {
        self.move_to_significant_token();
        self.peek()
    }

    /// Peek the token at the given `offset` and return a reference to it. If the
    /// `offset` is out of bounds, then return the first or last token.
    pub fn peek_with_offset(&self, offset: isize) -> Token<'src> {
        let index = (self.current as isize + offset).clamp(0, self.token_stream.len() as isize - 1)
            as usize;

        self.token_stream[index].clone()
    }

    /// Similar to [`Parser::next`], but the function will move the `current` to the
    /// next significant token before returning the next token.
    pub fn next_significant_token(&mut self) -> Token<'src> {
        self.move_to_significant_token();
        self.next()
    }

    /// Create a new [`Error`] from the given [`SyntacticError`].
    fn create_error<T>(&mut self, error: SyntacticError) -> Option<T> {
        self.accumulated_errors.push(error);
        return None;
    }

    fn parenthesis_delimiter_predicate<const C: char>(token: Token<'src>) -> bool {
        matches!(token.token_kind, TokenKind::Punctuator(c) if c == C)
    }

    // Skip the tokens until the predicate returns true. The parser also
    // skips the tokens inside pairs of brackets and parenthesis
    pub fn skip_to(&mut self, predicate: impl Fn(Token<'src>) -> bool) {
        // keep skipping tokens until the predicate returns true
        while !predicate(self.peek()) && !matches!(self.peek().token_kind, TokenKind::EndOfFile) {
            // if found open bracket or open parenthesis, skip to the closing
            // bracket or parenthesis
            if matches!(
                self.peek().token_kind,
                TokenKind::Punctuator('(')
                    | TokenKind::Punctuator('{')
                    | TokenKind::Punctuator('[')
            ) {
                let predicate = match self.peek().token_kind {
                    TokenKind::Punctuator('(') => Self::parenthesis_delimiter_predicate::<')'>,
                    TokenKind::Punctuator('{') => Self::parenthesis_delimiter_predicate::<'}'>,
                    TokenKind::Punctuator('[') => Self::parenthesis_delimiter_predicate::<']'>,
                    _ => unreachable!(),
                };

                self.next();
                self.skip_to(predicate);
            }
            self.next();
        }
    }

    /////////////////////////////////////////////////////////////////////////////////
    /// EXPECT FUNCTION
    /////////////////////////////////////////////////////////////////////////////////
    fn expect_identifier(&mut self) -> Option<PositionWrapper<&'src str>> {
        // move to the next significant token
        let ident = self.next_significant_token();

        if matches!(ident.token_kind, TokenKind::Identifier) {
            Some(PositionWrapper {
                position: ident.position_range.start.into()..ident.position_range.end.into(),
                value: ident.lexeme,
            })
        } else {
            self.create_error(SyntacticError::IdentifierExpected {
                expected_position: TextPosition {
                    line: ident.position_range.start.line,
                    column: ident.position_range.start.column,
                },
            })
        }
    }

    fn expect_punctuator(&mut self, punctuator: char) -> Option<()> {
        // move to the next significant token
        let punct = self.next_significant_token();

        if !matches!(punct.token_kind, TokenKind::Punctuator(p) if p == punctuator) {
            self.create_error(SyntacticError::PunctuatorExpected {
                expected_punctuator: punctuator,
                expected_position: TextPosition {
                    line: punct.position_range.start.line,
                    column: punct.position_range.start.column,
                },
            })
        } else {
            Some(())
        }
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Option<Token<'src>> {
        // move to the next significant token
        let kw = self.next_significant_token();

        if !matches!(kw.token_kind, TokenKind::Keyword(k) if k == keyword) {
            self.create_error(SyntacticError::KeywordExpected {
                expected_keyword: keyword,
                expected_position: TextPosition {
                    line: kw.position_range.start.line,
                    column: kw.position_range.start.column,
                },
            })
        } else {
            Some(kw)
        }
    }

    /////////////////////////////////////////////////////////////////////////////////
    /// COMMON PARSING
    /////////////////////////////////////////////////////////////////////////////////

    /// Parse the current token pointed by the `current` field as a qualified
    /// name.
    pub fn parse_qualified_name(&mut self) -> Option<PositionWrapper<&'src str>> {
        // move to the next significant token
        self.move_to_significant_token();

        // get the first byte index of the qualified name
        let first_byte_index = self.peek().position_range.start.byte_index;
        let first_position = self.peek().position_range.start;

        self.expect_identifier()?;

        // get the consecutive identifiers
        while matches!(self.peek().token_kind, TokenKind::Punctuator(':'))
            && matches!(
                self.peek_with_offset(1).token_kind,
                TokenKind::Punctuator(':')
            )
        {
            self.next();
            self.next();
            self.expect_identifier()?;
        }

        // get the last byte index of the qualified name
        let last_byte_index = self.peek().position_range.start.byte_index;

        // string slice of the qualified name
        let qualified_name_string =
            &self.token_stream.source_file().source_code()[first_byte_index..last_byte_index];

        Some(PositionWrapper {
            position: first_position.into()..self.peek().position_range.start.into(),
            value: qualified_name_string,
        })
    }

    /// Parse the current token pointed by the `current` field as a type unit.
    pub fn parse_type_unit(&mut self) -> Option<PositionWrapper<TypeUnitAST<'src>>> {
        let next = self.peek_significant_token();

        let type_unit = match next.token_kind {
            TokenKind::Identifier => TypeUnitAST::QualifiedName(self.parse_qualified_name()?.value),
            TokenKind::Keyword(Keyword::Bool) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Bool)
            }
            TokenKind::Keyword(Keyword::Void) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Void)
            }
            TokenKind::Keyword(Keyword::Int8) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int8)
            }
            TokenKind::Keyword(Keyword::Int16) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int16)
            }
            TokenKind::Keyword(Keyword::Int32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int32)
            }
            TokenKind::Keyword(Keyword::Int64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Int64)
            }
            TokenKind::Keyword(Keyword::Uint8) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint8)
            }
            TokenKind::Keyword(Keyword::Uint16) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint16)
            }
            TokenKind::Keyword(Keyword::Uint32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint32)
            }
            TokenKind::Keyword(Keyword::Uint64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Uint64)
            }
            TokenKind::Keyword(Keyword::Float32) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Float32)
            }
            TokenKind::Keyword(Keyword::Float64) => {
                TypeUnitAST::PrimitiveTypeUnit(PrimitiveTypeUnit::Float64)
            }
            _ => {
                return self.create_error(SyntacticError::IdentifierExpected {
                    expected_position: TextPosition {
                        line: next.position_range.start.line,
                        column: next.position_range.start.column,
                    },
                });
            }
        };

        if let TypeUnitAST::PrimitiveTypeUnit(_) = type_unit {
            self.next();
        }

        Some(PositionWrapper {
            position: next.position_range.start.into()..next.position_range.start.into(),
            value: type_unit,
        })
    }

    /// Parse the current token pointed by the `current` field as a type annotation.
    pub fn parse_type_annotation(&mut self) -> Option<PositionWrapper<TypeAnnotationAST<'src>>> {
        let type_unit = self.parse_type_unit()?;
        Some(PositionWrapper {
            position: type_unit.position,
            value: TypeAnnotationAST::TypeUnit(type_unit.value),
        })
    }

    /// Parse the current token pointed by the `current` field as a qualified type
    /// annotation.
    pub fn parse_qualified_type_annotation(
        &mut self,
    ) -> Option<PositionWrapper<QualifiedTypeAnnotationAST<'src>>> {
        let first_token = self.peek_significant_token();
        let mut is_mutable = false;

        // check for `const` keyword
        if let TokenKind::Keyword(Keyword::Mutable) = first_token.token_kind {
            self.next();
            is_mutable = true;
        }

        let type_annotation = self.parse_type_annotation()?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()..type_annotation.position.end,
            value: QualifiedTypeAnnotationAST {
                is_mutable,
                type_annotation,
            },
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
            self.peek_significant_token().token_kind,
            TokenKind::Punctuator(c) if c == TERMINATOR
        ) && !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::EndOfFile
        ) {
            // if this is not the first element, expect a separator
            if !first_element {
                if let None = self.expect_punctuator(SEPARATOR) {
                    found_error = true;

                    // skip to either the separator or the terminator
                    self.skip_to(|token| {
                        matches!(
                            token.token_kind,
                            TokenKind::Punctuator(c) if c == SEPARATOR || c == TERMINATOR
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
                        token.token_kind,
                        TokenKind::Punctuator(c) if c == SEPARATOR || c == TERMINATOR
                    )
                });
                continue;
            }
        }

        // expect closing parenthesis
        self.expect_punctuator(TERMINATOR)?;

        if !found_error {
            Some(elements)
        } else {
            None
        }
    }

    /////////////////////////////////////////////////////////////////////////////////
    /// DECLARATION PARSING
    /////////////////////////////////////////////////////////////////////////////////

    pub(crate) fn parse_file_level_declaration(
        &mut self,
    ) -> (
        Vec<PositionWrapper<UsingDirectiveAST<'src>>>,
        Vec<PositionWrapper<NamespaceDeclarationAST<'src>>>,
    ) {
        let skip_predicate = |token: Token<'src>| {
            matches!(
                token.token_kind,
                TokenKind::EndOfFile
                    | TokenKind::Keyword(Keyword::Namespace)
                    | TokenKind::Keyword(Keyword::Using)
                    | TokenKind::Keyword(Keyword::Class)
            )
        };

        let mut using_directives = Vec::new();
        let mut declarations = Vec::new();

        // set to true when a non-using directive is found
        let mut using_directives_stop = false;

        // keep looping until the end of file is reached
        while !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::EndOfFile
        ) {
            if let TokenKind::Keyword(Keyword::Using) = self.peek_significant_token().token_kind {
                if let Some(using_directive) = self.parse_using_directive() {
                    if using_directives_stop {
                        self.create_error::<i8>(
                            SyntacticError::UsingDirectiveMustBeDeclaredPriorToAnyOtherDeclaration {
                                using_directive_position: using_directive.position.clone()
                            },
                        );
                    } else {
                        using_directives.push(using_directive);
                    }
                    continue;
                }
            } else if let TokenKind::Keyword(Keyword::Namespace) =
                self.peek_significant_token().token_kind
            {
                if let Some(declaration) = self.parse_namespace_declaration() {
                    using_directives_stop = true;
                    declarations.push(PositionWrapper {
                        position: declaration.position,
                        value: declaration.value.to_namespace_declaration().unwrap(),
                    });
                    continue;
                }
            } else {
                // make progress
                let unexpected_token = self.next();

                self.create_error::<i8>(SyntacticError::UnexpectedToken {
                    unexpected_position: unexpected_token.position_range.start.into()
                        ..unexpected_token.position_range.end.into(),
                    parsing_context: ParsingContext::File,
                });
            }

            // skip to the next declaration
            self.skip_to(skip_predicate);
        }

        (using_directives, declarations)
    }

    fn parse_namespace_level_declaration(
        &mut self,
    ) -> (
        Vec<PositionWrapper<UsingDirectiveAST<'src>>>,
        Vec<PositionWrapper<NamespaceLevelDeclarationAST<'src>>>,
    ) {
        let skip_predicate = |token: Token<'src>| {
            matches!(
                token.token_kind,
                TokenKind::EndOfFile
                    | TokenKind::Keyword(Keyword::Namespace)
                    | TokenKind::Keyword(Keyword::Using)
                    | TokenKind::Keyword(Keyword::Class)
                    | TokenKind::Punctuator('}')
            )
        };

        let mut using_directives = Vec::new();
        let mut declarations = Vec::new();

        // set to true when a non-using directive is found
        let mut using_directives_stop = false;

        // keep looping until the end of file is reached or the closing brace is
        // found
        while !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::EndOfFile | TokenKind::Punctuator('}')
        ) {
            if let TokenKind::Keyword(Keyword::Using) = self.peek_significant_token().token_kind {
                if let Some(using_directive) = self.parse_using_directive() {
                    if using_directives_stop {
                        self.create_error::<i8>(
                            SyntacticError::UsingDirectiveMustBeDeclaredPriorToAnyOtherDeclaration {
                                using_directive_position: using_directive.position.clone()
                            },
                        );
                    } else {
                        using_directives.push(using_directive);
                    }
                    continue;
                }
            } else {
                if let Some(declaration) = self.parse_declaration() {
                    using_directives_stop = true;
                    declarations.push(declaration);
                    continue;
                }
            }

            // skip to the next declaration
            self.skip_to(skip_predicate);
        }

        (using_directives, declarations)
    }

    /// Parse the current token pointed by the `current` field as a declaration
    pub fn parse_declaration(
        &mut self,
    ) -> Option<PositionWrapper<NamespaceLevelDeclarationAST<'src>>> {
        match self.peek_significant_token().token_kind {
            TokenKind::Keyword(Keyword::Namespace) => self.parse_namespace_declaration(),
            TokenKind::Keyword(Keyword::Class) => self.parse_class_declaration(),
            _ => {
                self.next(); // make prgress
                self.create_error(SyntacticError::UnexpectedToken {
                    unexpected_position: self.peek().position_range.start.into()
                        ..self.peek().position_range.end.into(),
                    parsing_context: ParsingContext::Declaration,
                })
            }
        }
    }

    /// Parse the current token pointed by the `current` field as an access
    /// modififer.
    fn parse_access_modifier(&mut self) -> Option<PositionWrapper<AccessModifier>> {
        let first_token = self.next_significant_token();
        let access_mod = match first_token.token_kind {
            TokenKind::Keyword(Keyword::Public) => AccessModifier::Public,
            TokenKind::Keyword(Keyword::Private) => AccessModifier::Private,
            _ => {
                return self.create_error(SyntacticError::AccessModifierExpected {
                    expected_position: first_token.position_range.start.into()
                        ..first_token.position_range.end.into(),
                })
            }
        };

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..first_token.position_range.end.into(),
            value: access_mod,
        })
    }

    /// Parse the current token pointed by the `current` field as a class
    /// declaration.
    fn parse_class_declaration(
        &mut self,
    ) -> Option<PositionWrapper<NamespaceLevelDeclarationAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_keyword(Keyword::Class)?;

        // name of the class
        let identifier = self.expect_identifier()?;

        // expect opening brace
        self.expect_punctuator('{')?;

        let skip_predicate = |token: Token<'src>| {
            matches!(
                token.token_kind,
                TokenKind::Punctuator('}')
                    | TokenKind::Keyword(Keyword::Public)
                    | TokenKind::Keyword(Keyword::Private)
            )
        };

        let mut members = Vec::new();

        // parse the members of the class
        while !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::Punctuator('}')
        ) && !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::EndOfFile
        ) {
            // parse the access modifier
            let access_modifier = match self.parse_access_modifier() {
                Some(access_modifier) => access_modifier,
                None => {
                    self.skip_to(skip_predicate);
                    continue;
                }
            };

            // parse the type annotation
            let type_annotation = match self.parse_type_annotation() {
                Some(type_annotation) => type_annotation,
                None => {
                    self.skip_to(skip_predicate);
                    continue;
                }
            };

            // parse the identifier
            let identifier = match self.expect_identifier() {
                Some(identifier) => identifier,
                None => {
                    self.skip_to(skip_predicate);
                    continue;
                }
            };

            // if the next token is a semicolon, then we are parsing a field
            // declaration
            if matches!(
                self.peek_significant_token().token_kind,
                TokenKind::Punctuator(';')
            ) {
                self.next_significant_token();
                members.push(PositionWrapper {
                    position: access_modifier.position.start.into()
                        ..self.peek().position_range.start.into(),
                    value: ClassMemberDeclarationAST::ClassFieldDeclaration(
                        ClassFieldDeclarationAST {
                            access_modifier,
                            type_annotation,
                            identifier,
                        },
                    ),
                })
            }
            // if the next token is an opening parenthesis, then we are parsing
            // a class method
            else if matches!(
                self.peek_significant_token().token_kind,
                TokenKind::Punctuator('(')
            ) {
                self.next_significant_token();

                fn parse_parameter<'src, 'token>(
                    this: &mut Parser<'src, 'token>,
                ) -> Option<PositionWrapper<ParameterAST<'src>>> {
                    let qualified_type_annotation = this.parse_qualified_type_annotation()?;
                    let identifier = this.expect_identifier()?;

                    Some(PositionWrapper {
                        position: qualified_type_annotation.position.start.into()
                            ..identifier.position.end.into(),
                        value: ParameterAST {
                            qualified_type_annotation,
                            identifier,
                        },
                    })
                }

                // list of parameters
                let parameters = self
                    .parse_separated_list::<',', ')', PositionWrapper<ParameterAST>>(|this| {
                        parse_parameter(this)
                    })?;

                // function body
                let body = match self.parse_block_scope_statement() {
                    Some(body) => body,
                    None => {
                        self.skip_to(skip_predicate);
                        continue;
                    }
                };
                let body = match body.value {
                    StatementAST::BlockScopeStatement(block_scope_statement) => PositionWrapper {
                        position: body.position,
                        value: block_scope_statement,
                    },
                    _ => unreachable!(),
                };
                let return_type_annotation = type_annotation;

                members.push(PositionWrapper {
                    position: access_modifier.position.start.into()
                        ..self.peek().position_range.start.into(),
                    value: ClassMemberDeclarationAST::ClassMethodDeclaration(
                        ClassMethodDeclarationAST {
                            access_modifier,
                            return_type_annotation,
                            identifier,
                            parameters,
                            body,
                        },
                    ),
                })
            }
        }

        // expect closing brace
        self.expect_punctuator('}')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: NamespaceLevelDeclarationAST::ClassDeclaration(ClassDeclarationAST {
                identifier,
                members,
            }),
        })
    }

    /// Parse a namespace declaration
    fn parse_namespace_declaration(
        &mut self,
    ) -> Option<PositionWrapper<NamespaceLevelDeclarationAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_keyword(Keyword::Namespace)?;

        let qualified_name = self.parse_qualified_name()?;

        // expect opening brace
        self.expect_punctuator('{')?;

        let (using_directives, declarations) = self.parse_namespace_level_declaration();

        // expect closing brace
        self.expect_punctuator('}')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: NamespaceLevelDeclarationAST::NamespaceDeclaration(NamespaceDeclarationAST {
                qualified_name,
                using_directives,
                declarations,
            }),
        })
    }

    /// Parse the current token pointed by the `current` field as a using directive.
    pub fn parse_using_directive(&mut self) -> Option<PositionWrapper<UsingDirectiveAST<'src>>> {
        let first_token = self.peek_significant_token();

        // expect using keyword
        self.expect_keyword(Keyword::Using)?;

        // expect qualified name
        let qualified_name = self.parse_qualified_name()?;

        // expect semicolon
        self.expect_punctuator(';')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: UsingDirectiveAST { qualified_name },
        })
    }

    /////////////////////////////////////////////////////////////////////////////////
    /// STATEMENT PARSING
    /////////////////////////////////////////////////////////////////////////////////

    /// Parse the current token pointed by the `current` field as a statement.
    pub fn parse_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>> {
        // move to the next significant token
        match self.peek_significant_token().token_kind {
            // parse a variable declaration
            TokenKind::Keyword(Keyword::Mutable) | TokenKind::Keyword(Keyword::Let) => {
                self.parse_variable_declaration_statement()
            }
            // parse a block statement
            TokenKind::Punctuator('{') => self.parse_block_scope_statement(),
            // parse an if-else statement
            TokenKind::Keyword(Keyword::If) => self.parse_if_else_statement(),
            // parse a while loop statement
            TokenKind::Keyword(Keyword::While) => self.parse_while_statement(),
            // parse a return statement
            TokenKind::Keyword(Keyword::Return) => self.parse_return_statement(),
            // parse break and continue statements
            TokenKind::Keyword(Keyword::Break) | TokenKind::Keyword(Keyword::Continue) => {
                let token = self.next();
                Some(PositionWrapper {
                    position: token.position_range.start.into()..token.position_range.end.into(),
                    value: match token.token_kind {
                        TokenKind::Keyword(Keyword::Break) => StatementAST::BreakStatement,
                        TokenKind::Keyword(Keyword::Continue) => StatementAST::ContinueStatement,
                        _ => unreachable!(),
                    },
                })
            }
            _ => {
                // parse an expression statement
                let expr = self.parse_expression()?;

                // expect a semicolon
                self.expect_punctuator(';')?;

                Some(PositionWrapper {
                    position: expr.position.start..self.peek().position_range.start.into(),
                    value: StatementAST::ExpressionStatement(expr.value),
                })
            }
        }
    }

    fn parse_return_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_keyword(Keyword::Return)?;

        let expression = self.parse_expression()?;

        // expect a semicolon
        self.expect_punctuator(';')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: StatementAST::ReturnStatement(ReturnStatementAST { expression }),
        })
    }

    // parse a while loop statement
    fn parse_while_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_keyword(Keyword::While)?;

        // expect an opening parenthesis
        self.expect_punctuator('(')?;

        // parse the condition
        let condition = self.parse_expression()?;

        // expect a closing parenthesis
        self.expect_punctuator(')')?;

        // parse the body
        let statement = Box::new(self.parse_statement()?);

        Some(PositionWrapper {
            position: first_token.position_range.start.into()..statement.position.end,
            value: StatementAST::WhileStatement(WhileStatementAST {
                condition,
                statement,
            }),
        })
    }

    // parse a block statement
    fn parse_block_scope_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_punctuator('{')?;

        // keep looping until the end of file is reached or the closing brace is
        // found
        let mut statements = Vec::new();

        while !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::Punctuator('}')
        ) && !matches!(
            self.peek_significant_token().token_kind,
            TokenKind::EndOfFile
        ) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            } else {
                // skip to either the separator or the terminator
                self.skip_to(|token| {
                    // skip to either the separator or the terminator
                    matches!(
                        token.token_kind,
                        TokenKind::Punctuator(c) if c == ';' || c == '}'
                    )

                    // skip to if, while, return, break, continue, var, let
                    || matches!(
                        token.token_kind,
                        TokenKind::Keyword(k) if k == Keyword::If
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
                    self.peek_significant_token().token_kind,
                    TokenKind::Punctuator(';')
                ) {
                    self.next();
                }

                continue;
            }
        }

        self.expect_punctuator('}')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: StatementAST::BlockScopeStatement(BlockScopeStatementAST { statements }),
        })
    }

    // parse an if-else statemetn
    fn parse_if_else_statement(&mut self) -> Option<PositionWrapper<StatementAST<'src>>> {
        // get the first token in the if-else statement
        let first_token = self.peek_significant_token();

        // expect the `if` keyword
        self.expect_keyword(Keyword::If)?;

        // expect the opening parenthesis
        self.expect_punctuator('(')?;

        // parse the condition
        let condition = self.parse_expression()?;

        // expect the closing parenthesis
        self.expect_punctuator(')')?;

        // expect then statement
        let then_statement = Box::new(self.parse_statement()?);

        // if the next token is an `else` keyword, parse the else expression
        let else_statement = if matches!(
            self.peek_significant_token().token_kind,
            TokenKind::Keyword(Keyword::Else)
        ) {
            self.next();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
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
    ) -> Option<PositionWrapper<StatementAST<'src>>> {
        // get the first token in variable declaration statement
        // it can either be `let` or `var`
        let first_token = self.next_significant_token();

        let is_mutable = match first_token.token_kind {
            TokenKind::Keyword(Keyword::Let) => false,
            TokenKind::Keyword(Keyword::Mutable) => true,
            _ => unreachable!("parse_statement should have handled this case"),
        };

        // next, expect an identifier
        let variable_name = self.expect_identifier()?;

        // next, expect an assignment operator
        self.expect_punctuator('=')?;

        // next, expect an expression
        let expression = self.parse_expression()?;

        // finally, expect a semicolon
        self.expect_punctuator(';')?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: StatementAST::VariableDeclarationStatement(VariableDeclarationStatementAST {
                variable_name,
                is_mutable,
                expression,
            }),
        })
    }

    /////////////////////////////////////////////////////////////////////////////////
    /// EXPRESSION PARSING
    /////////////////////////////////////////////////////////////////////////////////

    /// Parse the current token pointed by the `current` field as an expression.
    pub fn parse_expression(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>> {
        self.parse_expression_bin(0)
    }

    /// Parse the current token pointed by the `current` field as a binary operator.
    fn parse_expression_bin(
        &mut self,
        parent_precedence: usize,
    ) -> Option<PositionWrapper<ExpressionAST<'src>>> {
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
                position: left.position.start..right.position.end,
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

        let bin_op = match self.next().token_kind {
            TokenKind::Punctuator('+')
            | TokenKind::Punctuator('-')
            | TokenKind::Punctuator('*')
            | TokenKind::Punctuator('/')
            | TokenKind::Punctuator('%') => {
                let punctuator = match self.peek_with_offset(-1).token_kind {
                    TokenKind::Punctuator(c) => c,
                    _ => unreachable!(),
                };

                // check for compound assignment
                match self.peek().token_kind {
                    TokenKind::Punctuator('=') => {
                        self.next();
                        match punctuator {
                            '+' => BinaryOperator::CompoundAddition,
                            '-' => BinaryOperator::CompoundSubtraction,
                            '*' => BinaryOperator::CompoundMultiplication,
                            '/' => BinaryOperator::CompoundDivision,
                            '%' => BinaryOperator::CompoundModulo,
                            _ => unreachable!(),
                        }
                    }
                    _ => match punctuator {
                        '+' => BinaryOperator::Addition,
                        '-' => BinaryOperator::Subtraction,
                        '*' => BinaryOperator::Multiplication,
                        '/' => BinaryOperator::Division,
                        '%' => BinaryOperator::Modulo,
                        _ => unreachable!(),
                    },
                }
            }
            TokenKind::Punctuator('<') | TokenKind::Punctuator('>') => {
                let current_punctuator = match self.peek().token_kind {
                    TokenKind::Punctuator(c) => c,
                    _ => unreachable!(),
                };

                if matches!(self.peek().token_kind, TokenKind::Punctuator('=')) {
                    self.next();
                    match current_punctuator {
                        '<' => BinaryOperator::LessThanOrEqual,
                        '>' => BinaryOperator::GreaterThanOrEqual,
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
                if matches!(self.peek().token_kind, TokenKind::Punctuator('=')) {
                    self.next();
                    BinaryOperator::Equal
                } else {
                    BinaryOperator::Assignment
                }
            }
            TokenKind::Punctuator('!') => {
                if matches!(self.peek().token_kind, TokenKind::Punctuator('=')) {
                    self.next();
                    BinaryOperator::NotEqual
                } else {
                    self.current = starting_index;
                    return None;
                }
            }
            TokenKind::Punctuator('&') => {
                if matches!(self.peek().token_kind, TokenKind::Punctuator('&')) {
                    self.next();
                    BinaryOperator::LogicalAnd
                } else {
                    self.current = starting_index;
                    return None;
                }
            }
            TokenKind::Punctuator('|') => {
                if matches!(self.peek().token_kind, TokenKind::Punctuator('|')) {
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
            position: operator_position_range.start.into()
                ..self.peek_with_offset(-1).position_range.end.into(),
            value: bin_op,
        })
    }

    /// Parse the current token pointed by the `current` field as a unary operator.
    fn parse_unary_operator(
        &mut self,
        lookahead: Token<'src>,
    ) -> Option<PositionWrapper<UnaryOperator>> {
        let operator = match lookahead.lexeme {
            "+" | "-" => {
                // two consecutive plus/minus signs are considered as an increment/
                // decrement operator
                if lookahead.lexeme == self.peek().lexeme {
                    // eat the second plus/minus sign
                    self.next();

                    match lookahead.lexeme {
                        "+" => UnaryOperator::Increment,
                        "-" => UnaryOperator::Decrement,
                        _ => unreachable!(),
                    }
                } else {
                    match lookahead.lexeme {
                        "+" => UnaryOperator::Identity,
                        "-" => UnaryOperator::Negation,
                        _ => unreachable!(),
                    }
                }
            }
            "!" => UnaryOperator::LogicalNegation,
            _ => unreachable!(),
        };

        let position =
            lookahead.position_range.start.into()..self.peek().position_range.start.into();

        Some(PositionWrapper {
            position,
            value: operator,
        })
    }

    fn parse_class_instantiation_expression(
        &mut self,
    ) -> Option<PositionWrapper<ExpressionAST<'src>>> {
        let first_token = self.peek_significant_token();
        self.expect_keyword(Keyword::New)?;

        let type_unit = self.parse_type_unit()?;

        // expect a left brace
        self.expect_punctuator('{')?;

        let field_initializations = self
            .parse_separated_list::<',', '}', PositionWrapper<ClassFiledInitializationAST>>(
                |this| {
                    let identifier = this.expect_identifier()?;
                    this.expect_punctuator('=')?;
                    let expression = this.parse_expression()?;

                    Some(PositionWrapper {
                        position: identifier.position.start.into()..expression.position.end.into(),
                        value: ClassFiledInitializationAST {
                            identifier,
                            expression,
                        },
                    })
                },
            )?;

        Some(PositionWrapper {
            position: first_token.position_range.start.into()
                ..self.peek().position_range.start.into(),
            value: ExpressionAST::ClassInstantiationExpression(ClassInstantiationExpressionAST {
                type_unit,
                field_initializations,
            }),
        })
    }

    /// Parse an expression that starts with an identifier.
    fn parse_identifier_expression(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>> {
        let qualified_name = self.parse_qualified_name()?;

        // check if the next token is a left parenthesis
        let pos = self.current;

        if matches!(
            self.peek_significant_token().token_kind,
            TokenKind::Punctuator('(')
        ) {
            // eat the left parenthesis
            self.next();

            // parse the arguments
            let arguments = self
                .parse_separated_list::<',', ')', PositionWrapper<ExpressionAST<'src>>>(|c| {
                    c.parse_expression()
                })?;

            Some(PositionWrapper {
                position: qualified_name.position.start.into()
                    ..self.peek().position_range.start.into(),
                value: ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                    qualified_name: qualified_name,
                    arguments,
                }),
            })
        } else {
            self.current = pos;

            // parse a variable expression
            Some(PositionWrapper {
                position: qualified_name.position,
                value: ExpressionAST::QualifiedNameExpression(QualifiedNameExpressionAST {
                    qualified_name: qualified_name.value,
                }),
            })
        }
    }

    /// Parse the current token pointed by the `current` field as a primary
    /// expression.
    fn parse_primary_expression(&mut self) -> Option<PositionWrapper<ExpressionAST<'src>>> {
        // move to the next significant token
        self.move_to_significant_token();
        let starting_position = self.peek().position_range.start;

        let mut expression = match self.peek().token_kind {
            // parse an expression that starts with an identifier
            TokenKind::Identifier => self.parse_identifier_expression()?,

            // parse a literal expression
            TokenKind::LiteralConstant(literal_expression) => {
                // eat the literal constant
                self.next();

                PositionWrapper {
                    position: starting_position.into()..self.peek().position_range.start.into(),
                    value: ExpressionAST::LiteralExpression(LiteralExpressionAST {
                        literal_expression,
                    }),
                }
            }

            // parse a parenthesized expression
            TokenKind::Punctuator('(') => {
                let left_parenthesis = self.next();
                let mut expression = self.parse_expression()?;
                let right_parenthesis = self.next();

                self.expect_punctuator(')')?;

                expression.position.start = left_parenthesis.position_range.start.into();
                expression.position.end = right_parenthesis.position_range.end.into();

                expression
            }

            // parse a class instantiation expression
            TokenKind::Keyword(Keyword::New) => self.parse_class_instantiation_expression()?,

            // parse a unary expression
            TokenKind::Punctuator('+')
            | TokenKind::Punctuator('-')
            | TokenKind::Punctuator('!') => {
                let next = self.next();
                let unary_operator = self.parse_unary_operator(next)?;
                let expression = self.parse_primary_expression()?;

                PositionWrapper {
                    position: unary_operator.position.start..expression.position.end,
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
                    unexpected_position: current_token.start.into()..current_token.end.into(),
                    parsing_context: ParsingContext::Expression,
                });
            }
        };

        loop {
            let current_position = self.current;

            // check if the next token is a dot '.'. If it is, then we are parsing
            // a member access expression.

            if let TokenKind::Punctuator('.') = self.next_significant_token().token_kind {
                // expect an identifier
                let identifier = self.expect_identifier()?;

                expression = PositionWrapper {
                    position: expression.position.start.into()..identifier.position.end.into(),
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

#[cfg(test)]
mod test;
