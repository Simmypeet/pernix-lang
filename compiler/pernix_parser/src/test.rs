use core::panic;

use pernix_lexer::token::LiteralConstantToken;
use pernix_project::source_code::{SourceCode, SourcePosition};

use crate::{
    abstract_syntax_tree::{
        declaration::{Declaration, TypeAnnotation},
        expression::{
            BinaryExpression, Expression, LiteralExpression, UnaryExpression,
        },
        statement::Statement,
        BinaryOperator, UnaryOperator,
    },
    error::Error,
    Parser,
};

/// Create a new [`SourceCode`] instance from the given code.
fn create_source_code(code: &str) -> SourceCode {
    SourceCode::new(code.to_string(), String::new())
}

// Check if the parser can parse a using directive.
#[test]
fn parse_using_directive_test() {
    let source_code =
        create_source_code("using Simmypeet.Program; using Another; using;");
    let mut parser = Parser::new(&source_code);

    {
        let using_directive = parser.parse_using_directive().unwrap();
        assert_eq!(
            using_directive.value.namespace_name.value,
            "Simmypeet.Program"
        );
        assert_eq!(
            using_directive.position.start,
            SourcePosition {
                line: 1,
                column: 1,
                byte_index: 0
            }
        );
        assert_eq!(
            using_directive.position.end,
            SourcePosition {
                line: 1,
                column: 25,
                byte_index: 24
            }
        );
        assert_eq!(
            using_directive.value.namespace_name.position.start,
            SourcePosition {
                line: 1,
                column: 7,
                byte_index: 6
            }
        );
        assert_eq!(
            using_directive.value.namespace_name.position.end,
            SourcePosition {
                line: 1,
                column: 24,
                byte_index: 23
            }
        );
    }

    {
        let using_directive = parser.parse_using_directive().unwrap();
        assert_eq!(using_directive.value.namespace_name.value, "Another");
        assert_eq!(
            using_directive.position.start,
            SourcePosition {
                line: 1,
                column: 26,
                byte_index: 25
            }
        );
        assert_eq!(
            using_directive.position.end,
            SourcePosition {
                line: 1,
                column: 40,
                byte_index: 39
            }
        );
        assert_eq!(
            using_directive.value.namespace_name.position.start,
            SourcePosition {
                line: 1,
                column: 32,
                byte_index: 31
            }
        );
        assert_eq!(
            using_directive.value.namespace_name.position.end,
            SourcePosition {
                line: 1,
                column: 39,
                byte_index: 38
            }
        );
    }

    {
        assert!(parser.parse_using_directive().is_none());

        let error = parser.pop_errors();

        assert_eq!(error.len(), 1);
        assert!(matches!(&error[0], Error::IdentifierExpected { .. }))
    }
}

// Check if the parser can handle a basic namespace declaration
#[test]
fn parse_namespace_declaration_test() {
    {
        let source_code = create_source_code("namespace Simmypeet.Program { }");
        let mut parser = Parser::new(&source_code);

        let namespace_declaration =
            parser.parse_namespace_declaration().unwrap();

        match namespace_declaration.value {
            Declaration::NamespaceDeclaration(namespace) => {
                assert_eq!(namespace.namespace_name.value, "Simmypeet.Program");
                assert!(namespace.declarations.is_empty());
            }
            _ => panic!("expected a namespace declaration"),
        }
    }

    {
        let source_code = create_source_code(
            "namespace Simmypeet { namespace namespace Another{} }",
        );
        let mut parser = Parser::new(&source_code);

        let namespace_declaration =
            parser.parse_namespace_declaration().unwrap();

        match namespace_declaration.value {
            Declaration::NamespaceDeclaration(namespace) => {
                assert_eq!(namespace.namespace_name.value, "Simmypeet");
                assert_eq!(namespace.declarations.len(), 1);

                assert!(matches!(
                    &namespace.declarations[0].value,
                    Declaration::NamespaceDeclaration(namespace)
                    if namespace.namespace_name.value == "Another"
                ));

                let errors = parser.pop_errors();
                assert_eq!(errors.len(), 1);
                assert!(matches!(&errors[0], Error::IdentifierExpected { .. }));
            }
            Declaration::FunctionDeclaration(_) => {
                panic!("expected a namespace declaration")
            }
        }
    }
}

// Check if the parser can handle a unary expression.
#[test]
fn parse_unary_expression_test() {
    let source_code = create_source_code("!a");
    let mut parser = Parser::new(&source_code);

    match parser.parse_expression().unwrap().value {
        Expression::UnaryExpression(expr) => {
            assert_eq!(expr.operator.value, UnaryOperator::LogicalNot);
            assert!(matches!(
                expr.operand.value,
                Expression::IdentifierExpression(identifier) if identifier.identifier == "a"
            ))
        }
        _ => panic!("expected a unary expression"),
    }
}

// Check if the parser can handle a function call expression.
#[test]
fn parse_function_call_expression_test() {
    let source_code = create_source_code("func(123, test) a()");
    let mut parser = Parser::new(&source_code);

    {
        match parser.parse_expression().unwrap().value {
            Expression::FunctionCallExpression(expr) => {
                assert_eq!(expr.function_name.value, "func");
                assert_eq!(expr.arguments.len(), 2);

                assert!(matches!(
                    &expr.arguments[0].value,
                    Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "123", literal_suffix: None, is_decimal: false } }  )
                ));

                assert!(matches!(
                    &expr.arguments[1].value,
                    Expression::IdentifierExpression(identifier) if identifier.identifier == "test"
                ));
            }
            _ => panic!("expected a function call expression"),
        }
    }

    {
        match parser.parse_expression().unwrap().value {
            Expression::FunctionCallExpression(expr) => {
                assert_eq!(expr.function_name.value, "a");
                assert!(expr.arguments.is_empty());
            }
            _ => panic!("expected a function call expression"),
        }
    }
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
                    BinaryOperator::Multiply => left_value * right_value,
                    BinaryOperator::Divide => left_value / right_value,
                    BinaryOperator::Remainder => left_value % right_value,
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
            Expression::LiteralExpression(lit) => {
                match lit.literal_expression {
                    LiteralConstantToken::Number {
                        value,
                        literal_suffix: _,
                        is_decimal: false,
                    } => value.parse::<i64>().unwrap(),
                    _ => unimplemented!(),
                }
            }
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

// Check if the parse can handle a variable declaration.
#[test]
fn parse_variable_declaration_statement_test() {
    let source_code = create_source_code(
        "
    let a = 32;
    let mutable b = 64;
    let c: int32 = 128;
    let mutable d: int32 = 256;",
    );
    let mut parser = Parser::new(&source_code);

    // let a = 32;
    {
        let statement = parser.parse_statement().unwrap();

        match statement.value {
            Statement::VariableDeclarationStatement(declaration) => {
                assert_eq!(declaration.identifier.value, "a");
                assert!(!declaration.is_mutable);
                assert!(declaration.type_annotation.is_none());

                assert!(matches!(
                    declaration.expression.value,
                    Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "32", literal_suffix: None, is_decimal: false } } )
                ));
            }
            _ => panic!("expected a variable declaration statement"),
        }
    }

    // let mutable b = 64;
    {
        let statement = parser.parse_statement().unwrap();

        match statement.value {
            Statement::VariableDeclarationStatement(declaration) => {
                assert_eq!(declaration.identifier.value, "b");
                assert!(declaration.is_mutable);
                assert!(declaration.type_annotation.is_none());

                assert!(matches!(
                    declaration.expression.value,
                    Expression::LiteralExpression(literal) if matches!(literal,LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "64", literal_suffix: None, is_decimal: false } } )
                ));
            }
            _ => panic!("expected a variable declaration statement"),
        }
    }

    // let c: int32 = 128;
    {
        let statement = parser.parse_statement().unwrap();

        match statement.value {
            Statement::VariableDeclarationStatement(declaration) => {
                assert_eq!(declaration.identifier.value, "c");
                assert!(!declaration.is_mutable);
                assert!(matches!(
                    declaration.type_annotation.unwrap().value,
                    TypeAnnotation::QualifiedName("int32")
                ));

                assert!(matches!(
                    declaration.expression.value,
                    Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "128", literal_suffix: None, is_decimal: false } } )
                ));
            }
            _ => panic!("expected a variable declaration statement"),
        }
    }

    // let mutable d: int32 = 256;
    {
        let statement = parser.parse_statement().unwrap();

        match statement.value {
            Statement::VariableDeclarationStatement(declaration) => {
                assert_eq!(declaration.identifier.value, "d");
                assert!(declaration.is_mutable);
                assert!(matches!(
                    declaration.type_annotation.unwrap().value,
                    TypeAnnotation::QualifiedName("int32")
                ));

                assert!(matches!(
                    declaration.expression.value,
                    Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "256", literal_suffix: None, is_decimal: false } } )
                ));
            }
            _ => panic!("expected a variable declaration statement"),
        }
    }
}

// Check if the parser can handle if statements
#[test]
fn parsing_if_statement_test() {
    let source_code = create_source_code(
        "
        if func()
            return 42;
        else 
            return 69; 
        ",
    );
    let mut parser = Parser::new(&source_code);

    match parser.parse_statement().unwrap().value {
        Statement::IfElseStatement(statement) => {
            assert!(matches!(
                statement.condition.value,
                Expression::FunctionCallExpression(call) if call.function_name.value == "func" && call.arguments.is_empty()
            ));

            assert!(matches!(
                statement.then_statement.value,
                Statement::ReturnStatement(expression) if matches!(expression.expression.clone().unwrap().value, Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "42", literal_suffix: None, is_decimal: false } }))
            ));

            assert!(matches!(
                statement.else_statement.unwrap().value,
                Statement::ReturnStatement(expression) if matches!(expression.expression.clone().unwrap().value, Expression::LiteralExpression(literal) if matches!(literal, LiteralExpression { literal_expression: LiteralConstantToken::Number { value: "69", literal_suffix: None, is_decimal: false } }))
            ));
        }
        _ => panic!("expected an if statement"),
    }
}

// Check if the parser can handle function declaration
#[test]
fn parse_function_declaration() {
    let source_code = create_source_code(
        "
        function add(mutable a: int32, b: int32) : int32 {}
        ",
    );
    let mut parser = Parser::new(&source_code);

    let function_declaration = parser.parse_declaration().unwrap();
    match function_declaration.value {
        Declaration::FunctionDeclaration(function) => {
            assert_eq!(function.function_name.value, "add");
            assert_eq!(function.parameters.len(), 2);
            assert!(matches!(
                function.return_type.unwrap().value,
                TypeAnnotation::QualifiedName("int32")
            ));
            assert_eq!(function.body.value.statements.len(), 0);

            {
                let parameter = &function.parameters[0].value;
                assert_eq!(parameter.1, "a");
                assert!(parameter.0.is_mutable);
                assert!(matches!(
                    parameter.0.type_annotation.value,
                    TypeAnnotation::QualifiedName("int32")
                ));
            }

            {
                let parameter = &function.parameters[1].value;
                assert_eq!(parameter.1, "b");
                assert!(!parameter.0.is_mutable);
                assert!(matches!(
                    parameter.0.type_annotation.value,
                    TypeAnnotation::QualifiedName("int32")
                ));
            }
        }
        _ => panic!("expected a function declaration"),
    }
}
