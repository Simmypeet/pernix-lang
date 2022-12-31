use pernixc_common::source_file::SourceFile;
use pernixc_lexical_analysis::token_stream::TokenStream;

use crate::abstract_syntax_tree::{
    declaration::{
        AccessModifier, ClassMemberDeclarationAST, NamespaceLevelDeclarationAST, TypeAnnotationAST,
    },
    expression::{ExpressionAST, FunctionCallExpressionAST, QualifiedNameExpressionAST},
    statement::StatementAST,
    PrimitiveTypeUnit, TypeUnitAST,
};

use super::Parser;

static QUALIFIED_NAME: &str = "Simmypeet::Program::Test ::Another";
static QUALIFIED_NAME_EXPRESSION: &str = "a::b a() a::b() a::b(a) a::b(a, b)";
static RETURN_STATEMENT: &str = "return 1 + 2;";
static VARIABLE_DECLARATION_STATEMENT: &str = "mutable a = 1 + 2; let b = 1 + 2;";
static IF_ELSE_STATEMENT: &str = "if (a == b) { a = 1; } else { a = 2; } if (a == b) return 1;";
static WHILE_STATEMENT: &str = "while (a == b) return 1;";
static CLASS_INSTANTIATION_EXPRESSION: &str = "new A{} new B{a = 32, b = 65}";
static NAMESPACE_DECLARATION: &str =
    "namespace Outer::Space { using A::B; namespace Inner::Space { } }";
static CLASS_DECLARATION: &str =
    "class Test { public int32 a; private SomeType b; public int32 Main() { return 0; } }";

#[test]
fn test_qualified_name() {
    let source_file = SourceFile::new(QUALIFIED_NAME.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;

    let mut parser = Parser::new(&token_stream);
    let qualified_name = parser.parse_qualified_name().unwrap();

    assert_eq!(qualified_name.value, "Simmypeet::Program::Test");
}

#[test]
fn test_qualified_name_expression() {
    let source_file = SourceFile::new(
        QUALIFIED_NAME_EXPRESSION.to_string(),
        "test.pnx".to_string(),
    );
    let token_stream = TokenStream::tokenize(&source_file).0;

    let mut parser = Parser::new(&token_stream);

    // a.b
    {
        let qualified_name = parser.parse_primary_expression().unwrap();
        assert!(matches!(
            qualified_name.value,
            ExpressionAST::QualifiedNameExpression(QualifiedNameExpressionAST {
                qualified_name: "a::b"
            })
        ));
        assert_eq!(qualified_name.position.start.column, 1);
        assert_eq!(qualified_name.position.start.line, 1);

        assert_eq!(qualified_name.position.end.column, 5);
        assert_eq!(qualified_name.position.end.line, 1);
    }

    // a()
    {
        let qualified_name = parser.parse_primary_expression().unwrap();
        assert!(matches!(
            qualified_name.value,
            ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                qualified_name,
                arguments
            }) if qualified_name.value == "a" && arguments.is_empty()
        ));

        assert_eq!(qualified_name.position.start.column, 6);
        assert_eq!(qualified_name.position.start.line, 1);

        assert_eq!(qualified_name.position.end.column, 9);
        assert_eq!(qualified_name.position.end.line, 1);
    }

    // a::b()
    {
        let qualified_name = parser.parse_primary_expression().unwrap();
        assert!(matches!(
            qualified_name.value,
            ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                qualified_name,
                arguments
            }) if qualified_name.value == "a::b" && arguments.is_empty()
        ));

        assert_eq!(qualified_name.position.start.column, 10);
        assert_eq!(qualified_name.position.start.line, 1);

        assert_eq!(qualified_name.position.end.column, 16);
        assert_eq!(qualified_name.position.end.line, 1);
    }

    // a::b(a)
    {
        let qualified_name = parser.parse_primary_expression().unwrap();
        assert!(matches!(
            qualified_name.value,
            ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                qualified_name,
                arguments
            }) if qualified_name.value == "a::b" && arguments.len() == 1
        ));

        assert_eq!(qualified_name.position.start.column, 17);
        assert_eq!(qualified_name.position.start.line, 1);

        assert_eq!(qualified_name.position.end.column, 24);
        assert_eq!(qualified_name.position.end.line, 1);
    }

    // a::b(a, b)
    {
        let qualified_name = parser.parse_primary_expression().unwrap();
        assert!(matches!(
            qualified_name.value,
            ExpressionAST::FunctionCallExpression(FunctionCallExpressionAST {
                qualified_name,
                arguments
            }) if qualified_name.value == "a::b" && arguments.len() == 2
        ));

        assert_eq!(qualified_name.position.start.column, 25);
        assert_eq!(qualified_name.position.start.line, 1);

        assert_eq!(qualified_name.position.end.column, 35);
        assert_eq!(qualified_name.position.end.line, 1);
    }
}

#[test]
fn test_return_statement() {
    let source_file = SourceFile::new(RETURN_STATEMENT.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;

    let mut parser = Parser::new(&token_stream);
    let return_statement = parser.parse_return_statement().unwrap();

    match return_statement.value {
        StatementAST::ReturnStatement(statement) => {
            assert!(matches!(
                statement.expression.value,
                ExpressionAST::BinaryExpression(_)
            ));

            assert_eq!(return_statement.position.start.column, 1);
            assert_eq!(return_statement.position.start.line, 1);

            assert_eq!(return_statement.position.end.column, 14);
            assert_eq!(return_statement.position.end.line, 1);
        }
        _ => panic!("Expected return statement"),
    }
}

#[test]
fn test_variable_declaration_statement() {
    let source_file = SourceFile::new(
        VARIABLE_DECLARATION_STATEMENT.to_string(),
        "test.pnx".to_string(),
    );
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    // first variable decl: mutable a = 1 + 2;
    {
        let variable_declaration_statement = parser.parse_variable_declaration_statement().unwrap();

        match variable_declaration_statement.value {
            StatementAST::VariableDeclarationStatement(statement) => {
                assert_eq!(statement.variable_name.value, "a");
                assert!(matches!(
                    statement.expression.value,
                    ExpressionAST::BinaryExpression(_)
                ));

                assert_eq!(variable_declaration_statement.position.start.column, 1);
                assert_eq!(variable_declaration_statement.position.start.line, 1);

                assert_eq!(variable_declaration_statement.position.end.column, 19);
                assert_eq!(variable_declaration_statement.position.end.line, 1);

                // is_mutable
                assert_eq!(statement.is_mutable, true);
            }
            _ => panic!("Expected variable declaration statement"),
        }
    }

    // second variable decl: let b = 1 + 2;
    {
        let variable_declaration_statement = parser.parse_variable_declaration_statement().unwrap();

        match variable_declaration_statement.value {
            StatementAST::VariableDeclarationStatement(statement) => {
                assert_eq!(statement.variable_name.value, "b");
                assert!(matches!(
                    statement.expression.value,
                    ExpressionAST::BinaryExpression(_)
                ));

                assert_eq!(variable_declaration_statement.position.start.column, 20);
                assert_eq!(variable_declaration_statement.position.start.line, 1);

                assert_eq!(variable_declaration_statement.position.end.column, 34);
                assert_eq!(variable_declaration_statement.position.end.line, 1);

                // is_mutable
                assert_eq!(statement.is_mutable, false);
            }
            _ => panic!("Expected variable declaration statement"),
        }
    }
}

#[test]
fn if_else_statement_test() {
    let source_file = SourceFile::new(IF_ELSE_STATEMENT.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    // first if-else statement: if with else
    {
        let if_statement = parser.parse_if_else_statement().unwrap();

        match if_statement.value {
            StatementAST::IfElseStatement(statement) => {
                assert!(matches!(
                    statement.condition.value,
                    ExpressionAST::BinaryExpression(_)
                ));

                assert!(matches!(
                    statement.then_statement.value,
                    StatementAST::BlockScopeStatement(_)
                ));

                assert!(matches!(
                    statement.else_statement.unwrap().value,
                    StatementAST::BlockScopeStatement(_)
                ));

                assert_eq!(if_statement.position.start.column, 1);
                assert_eq!(if_statement.position.start.line, 1);

                assert_eq!(if_statement.position.end.column, 39);
                assert_eq!(if_statement.position.end.line, 1);
            }
            _ => panic!("Expected if statement"),
        }
    }

    // second if-else statement: if without else
    {
        let if_statement = parser.parse_if_else_statement().unwrap();

        match if_statement.value {
            StatementAST::IfElseStatement(statement) => {
                assert!(matches!(
                    statement.condition.value,
                    ExpressionAST::BinaryExpression(_)
                ));

                assert!(matches!(
                    statement.then_statement.value,
                    StatementAST::ReturnStatement(_)
                ));

                assert!(statement.else_statement.is_none());

                assert_eq!(if_statement.position.start.column, 40);
                assert_eq!(if_statement.position.start.line, 1);

                assert_eq!(if_statement.position.end.column, 61);
                assert_eq!(if_statement.position.end.line, 1);
            }
            _ => panic!("Expected if statement"),
        }
    }
}

#[test]
fn test_while_statement() {
    let source_file = SourceFile::new(WHILE_STATEMENT.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    let while_statement = parser.parse_while_statement().unwrap();

    match while_statement.value {
        StatementAST::WhileStatement(statement) => {
            assert!(matches!(
                statement.condition.value,
                ExpressionAST::BinaryExpression(_)
            ));

            assert!(matches!(
                statement.statement.value,
                StatementAST::ReturnStatement(_)
            ));

            assert_eq!(while_statement.position.start.column, 1);
            assert_eq!(while_statement.position.start.line, 1);

            assert_eq!(while_statement.position.end.column, 25);
            assert_eq!(while_statement.position.end.line, 1);
        }
        _ => panic!("Expected while statement"),
    }
}

#[test]
fn test_class_instantiation_expression() {
    let source_file = SourceFile::new(
        CLASS_INSTANTIATION_EXPRESSION.to_string(),
        "test.pnx".to_string(),
    );
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    // first instantiation: new A{}
    {
        let class_instantiation_expression = parser.parse_class_instantiation_expression().unwrap();

        match class_instantiation_expression.value {
            ExpressionAST::ClassInstantiationExpression(expression) => {
                match expression.type_unit.value {
                    TypeUnitAST::QualifiedName(name) => {
                        assert_eq!(name, "A");
                    }
                    _ => panic!("Expected qualified name"),
                }

                assert!(expression.field_initializations.is_empty());

                assert_eq!(class_instantiation_expression.position.start.column, 1);
                assert_eq!(class_instantiation_expression.position.start.line, 1);

                assert_eq!(class_instantiation_expression.position.end.column, 8);
                assert_eq!(class_instantiation_expression.position.end.line, 1);
            }
            _ => panic!("Expected class instantiation expression"),
        }
    }

    // second instantiation: new B{a = 32, b = 65}
    {
        let class_instantiation_expression = parser.parse_class_instantiation_expression().unwrap();

        match class_instantiation_expression.value {
            ExpressionAST::ClassInstantiationExpression(expression) => {
                match expression.type_unit.value {
                    TypeUnitAST::QualifiedName(name) => {
                        assert_eq!(name, "B");
                    }
                    _ => panic!("Expected qualified name"),
                }

                assert_eq!(expression.field_initializations.len(), 2);

                // first field initialization: a = 32
                {
                    let field_initialization = &expression.field_initializations[0];

                    assert_eq!(field_initialization.value.identifier.value, "a");

                    assert!(matches!(
                        field_initialization.value.expression.value,
                        ExpressionAST::LiteralExpression(_)
                    ));
                }

                // second field initialization: b = 65
                {
                    let field_initialization = &expression.field_initializations[1];

                    assert_eq!(field_initialization.value.identifier.value, "b");

                    assert!(matches!(
                        field_initialization.value.expression.value,
                        ExpressionAST::LiteralExpression(_)
                    ));
                }

                assert_eq!(class_instantiation_expression.position.start.column, 9);
                assert_eq!(class_instantiation_expression.position.start.line, 1);

                assert_eq!(class_instantiation_expression.position.end.column, 30);
                assert_eq!(class_instantiation_expression.position.end.line, 1);
            }
            _ => panic!("Expected class instantiation expression"),
        }
    }
}

#[test]
fn test_namespace_declaration() {
    let source_file = SourceFile::new(NAMESPACE_DECLARATION.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    let namespace_declaration = parser.parse_namespace_declaration().unwrap();

    match namespace_declaration.value {
        NamespaceLevelDeclarationAST::NamespaceDeclaration(declaration) => {
            assert_eq!(declaration.qualified_name.value, "Outer::Space");

            // using directive: A::B
            {
                assert_eq!(declaration.using_directives.len(), 1);
                let using_directive = &declaration.using_directives[0];

                assert_eq!(using_directive.value.qualified_name.value, "A::B");
            }

            // one declaration: namespace Inner::Space
            {
                assert_eq!(declaration.declarations.len(), 1);
                let declaration = &declaration.declarations[0];

                match &declaration.value {
                    NamespaceLevelDeclarationAST::NamespaceDeclaration(declaration) => {
                        assert_eq!(declaration.qualified_name.value, "Inner::Space");

                        assert!(declaration.using_directives.is_empty());
                        assert!(declaration.declarations.is_empty());
                    }
                    _ => panic!("Expected namespace declaration"),
                }
            }
        }
        _ => panic!("Expected namespace declaration"),
    }
}

#[test]
fn test_class_declaration() {
    let source_file = SourceFile::new(CLASS_DECLARATION.to_string(), "test.pnx".to_string());
    let token_stream = TokenStream::tokenize(&source_file).0;
    let mut parser = Parser::new(&token_stream);

    let class_declaration = parser.parse_class_declaration().unwrap();
    match class_declaration.value {
        NamespaceLevelDeclarationAST::ClassDeclaration(class_declaration) => {
            // class name Test
            assert_eq!(class_declaration.identifier.value, "Test");

            // containing three members
            assert_eq!(class_declaration.members.len(), 3);

            // first member: public int32 a;
            {
                let member = &class_declaration.members[0];

                match &member.value {
                    ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                        assert_eq!(field.identifier.value, "a");

                        assert!(matches!(
                            field.access_modifier.value,
                            AccessModifier::Public
                        ));

                        assert!(matches!(
                            field.type_annotation.value,
                            TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                PrimitiveTypeUnit::Int32
                            ))
                        ));

                        assert_eq!(field.identifier.value, "a");
                    }
                    _ => panic!("Expected class field declaration"),
                }
            }

            // second member: private SomeType b;
            {
                let member = &class_declaration.members[1];

                match &member.value {
                    ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                        assert_eq!(field.identifier.value, "b");

                        assert!(matches!(
                            field.access_modifier.value,
                            AccessModifier::Private
                        ));

                        assert!(matches!(
                            field.type_annotation.value,
                            TypeAnnotationAST::TypeUnit(TypeUnitAST::QualifiedName("SomeType"))
                        ));

                        assert_eq!(field.identifier.value, "b");
                    }
                    _ => panic!("Expected class field declaration"),
                }
            }

            // third member: public int32 Main() { return 0; }
            {
                let member = &class_declaration.members[2];

                match &member.value {
                    ClassMemberDeclarationAST::ClassMethodDeclaration(method) => {
                        assert_eq!(method.identifier.value, "Main");

                        assert!(matches!(
                            method.access_modifier.value,
                            AccessModifier::Public
                        ));

                        assert!(matches!(
                            method.return_type_annotation.value,
                            TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                PrimitiveTypeUnit::Int32
                            ))
                        ));

                        assert!(method.parameters.is_empty());

                        assert_eq!(method.body.value.statements.len(), 1);

                        let statement = &method.body.value.statements[0];

                        match &statement.value {
                            StatementAST::ReturnStatement(statement) => {
                                assert!(matches!(
                                    statement.expression.value,
                                    ExpressionAST::LiteralExpression(_)
                                ));
                            }
                            _ => panic!("Expected return statement"),
                        }
                    }
                    _ => panic!("Expected class method declaration"),
                }
            }
        }
        _ => panic!("Expected class declaration"),
    }
}
