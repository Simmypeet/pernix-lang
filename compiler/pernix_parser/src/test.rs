use core::panic;

use pernix_lexer::token::{LiteralConstantType, TokenKind};
use pernix_project::source_code::{SourceCode, SourcePosition};

use crate::{
    abstract_syntax_tree::{
        declaration::{Declaration, NamespaceDeclaration, UsingDirective},
        expression::{
            BinaryExpression, Expression, FunctionCallExpression,
            UnaryExpression,
        },
        BinaryOperator, PositionWrapper, UnaryOperator,
    },
    error::Error,
    Parser,
};

// Checks if the parser can correctly parse expressions
#[test]
fn parse_primary_expression_test() {
    let source = "!true 321 test another(32, test, func())";
    let source_code = SourceCode::new(source.to_string(), String::new());

    let mut parser = Parser::new(&source_code);

    // Expression: !true
    {
        let expression = parser.parse_primary_expression().unwrap();

        assert!(matches!(
            expression,
            PositionWrapper {
                position: _,
                value: Expression::UnaryExpression(UnaryExpression {
                    operator: PositionWrapper {
                        position: _,
                        value: UnaryOperator::LogicalNot
                    },
                    operand
                })
            }
            if matches!(*operand, PositionWrapper {
                position: _,
                value: Expression::LiteralExpression(LiteralConstantType::Boolean(true))
            })
        ))
    }

    // Expression: 321
    {
        let expression = parser.parse_primary_expression().unwrap();

        assert!(matches!(
            expression,
            PositionWrapper {
                position: _,
                value: Expression::LiteralExpression(
                    LiteralConstantType::Integer {
                        value: "321",
                        literal_suffix: None
                    }
                )
            }
        ))
    }

    // Expression: test
    {
        let expression = parser.parse_primary_expression().unwrap();

        assert!(matches!(
            expression,
            PositionWrapper {
                position: _,
                value: Expression::IdentifierExpression("test")
            }
        ))
    }

    // Expression: another(32, test, func())
    {
        let expression = parser.parse_primary_expression().unwrap();

        assert!({
            match expression.value {
                Expression::FunctionCallExpression(
                    FunctionCallExpression {
                        function_name,
                        arguments,
                    },
                ) => {
                    assert_eq!(function_name.value, "another");
                    assert_eq!(arguments.len(), 3);

                    assert!(matches!(
                        arguments[0],
                        PositionWrapper {
                            position: _,
                            value: Expression::LiteralExpression(
                                LiteralConstantType::Integer {
                                    value: "32",
                                    literal_suffix: None
                                }
                            )
                        }
                    ));

                    assert!(matches!(
                        arguments[1],
                        PositionWrapper {
                            position: _,
                            value: Expression::IdentifierExpression("test")
                        }
                    ));

                    assert!(matches!(
                        &arguments[2],
                        PositionWrapper {
                            position: _,
                            value: Expression::FunctionCallExpression(FunctionCallExpression {
                                function_name,
                                arguments
                            })
                        }
                        if function_name.value == "func" && arguments.is_empty()
                    ));
                }
                _ => {}
            }

            true
        })
    }
}

// Checks if the parser can parse a program
#[test]
fn parse_program_test() {
    let source = "using Math; namespace Simmypeet.Program {}";
    let source_code = SourceCode::new(source.to_string(), String::new());
    let mut parser = Parser::new(&source_code);

    let program = parser.parse_program();

    assert!(program.is_some());

    let program = program.unwrap();

    assert_eq!(program.using_directives.len(), 1);
    assert_eq!(program.declarations.len(), 1);

    assert_eq!(
        program.using_directives[0].value.namespace_name.value,
        "Math"
    );

    assert!(matches!(
        &program.declarations[0],
        PositionWrapper {
            position: _,
            value: Declaration::NamespaceDeclaration(NamespaceDeclaration {
                namespace_name,
                using_directives,
                declarations
            })
        }
        if namespace_name.value == "Simmypeet.Program"
        && declarations.is_empty()
        && using_directives.is_empty()
    ));
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
        Declaration::NamespaceDeclaration(NamespaceDeclaration {
            namespace_name,
            using_directives,
            declarations: _,
        }) => {
            assert_eq!(namespace_name.value, "foo");
            assert_eq!(using_directives.len(), 1);

            let using_statement = using_directives.get(0).unwrap();

            assert_eq!(
                using_statement.position.start,
                SourcePosition {
                    line: 1,
                    column: 17,
                    byte_index: 16
                }
            );

            assert_eq!(
                using_statement.position.end,
                SourcePosition {
                    line: 1,
                    column: 27,
                    byte_index: 26
                }
            );

            assert_eq!(using_statement.value.namespace_name.value, "bar");
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
        let using_statement = parser.parse_using_directive().unwrap();

        assert_eq!(
            using_statement.value,
            UsingDirective {
                namespace_name: PositionWrapper {
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
        let using_statement = parser.parse_using_directive().unwrap();

        assert_eq!(
            using_statement.value,
            UsingDirective {
                namespace_name: PositionWrapper {
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
        let using_statement = parser.parse_using_directive();

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

// Check if the parser can parse different kinds of binary operators
#[test]
fn parse_binary_operator_test() {
    let source_code =
        SourceCode::new("+ - == != <= >=".to_string(), String::new());
    let mut parser = Parser::new(&source_code);

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::Add
    );

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::Subtract
    );

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::Equal
    );

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::NotEqual
    );

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::LessThanEqual
    );

    assert_eq!(
        parser.parse_binary_operator_roll_back().unwrap().value,
        BinaryOperator::GreaterThanEqual
    );
}

trait ExpressionGenerator {
    fn generate_value(&self) -> i64;
}

impl ExpressionGenerator for Expression<'_> {
    fn generate_value(&self) -> i64 {
        match self {
            Expression::BinaryExpression(BinaryExpression {
                left,
                operator,
                right,
            }) => {
                let left_value = left.value.generate_value();
                let right_value = right.value.generate_value();

                match operator.value {
                    BinaryOperator::Add => left_value + right_value,
                    BinaryOperator::Subtract => left_value - right_value,
                    BinaryOperator::Asterisk => left_value * right_value,
                    BinaryOperator::Slash => left_value / right_value,
                    BinaryOperator::Percent => left_value % right_value,
                    _ => unimplemented!(),
                }
            }
            Expression::UnaryExpression(UnaryExpression {
                operator,
                operand,
            }) => {
                let operand_value = operand.value.generate_value();

                match operator.value {
                    UnaryOperator::Minus => -operand_value,
                    UnaryOperator::Plus => operand_value,
                    _ => unimplemented!(),
                }
            }
            Expression::LiteralExpression(lit) => match lit {
                LiteralConstantType::Integer {
                    value,
                    literal_suffix: _,
                } => value.parse::<i64>().unwrap(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
}

// Check if the parser can parse different kinds of binary expressions
#[test]
fn parse_binary_expression_test() {
    {
        let source_code =
            SourceCode::new("1 + 2 - 3 * 4 / 5 % 6".to_string(), String::new());
        let mut parser = Parser::new(&source_code);

        let expression = parser.parse_expression().unwrap();

        assert_eq!(expression.value.generate_value(), 1);
    }

    {
        let source_code =
            SourceCode::new("1 + 2 * -3 + -(4 + 3)".to_string(), String::new());
        let mut parser = Parser::new(&source_code);

        let expression = parser.parse_expression().unwrap();

        assert_eq!(expression.value.generate_value(), -12);
    }
}
