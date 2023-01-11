use pernixc_lexical_analysis::token_stream::TokenStream;

use crate::{
    abstract_syntax_tree::{
        declaration::{
            AccessModifier, ClassMemberDeclarationAST, DeclarationAST, PrimitiveTypeUnit,
            TypeAnnotationAST, TypeUnitAST,
        },
        expression::{ExpressionAST, FunctionCallExpressionAST, QualifiedNameExpressionAST},
        statement::StatementAST,
    },
    parser::Parser,
};

static QUALIFIED_NAME: &str = "Simmypeet::Program::Test ::Another";
static QUALIFIED_NAME_EXPRESSION: &str = "a::b a() a::b() a::b(a) a::b(a, b)";
static RETURN_STATEMENT: &str = "return 1 + 2;";
static VARIABLE_DECLARATION_STATEMENT: &str = "var a = 1 + 2; let b = 1 + 2;";
static IF_ELSE_STATEMENT: &str = "if (a == b) { a = 1; } else { a : 2; } if (a == b) return 1;";
static WHILE_STATEMENT: &str = "while (a == b) return 1;";
static CLASS_INSTANTIATION_EXPRESSION: &str = "new A{} new B{a : 32, b : 65}";
static CLASS_DECLARATION: &str = "
export class A {}
class B {
    public a: int32;
    private b: float32;
    c: float64;

    public function A() : void {}
    private function B(mutable a: int32, b: float32) : float64 {
        return 0;
    }
}
";

static FILE: &str = "
module simmypeet::program::test;

import simmypeet::program;
import another::moduleName;

export class A {}
class B {}

";

#[test]
fn test_qualified_name() {
    let token_stream = TokenStream::tokenize(QUALIFIED_NAME).0;

    let mut parser = Parser::new(&token_stream);
    let qualified_name = parser.parse_qualified_name().unwrap();

    assert_eq!(qualified_name.value, "Simmypeet::Program::Test");
}

#[test]
fn test_qualified_name_expression() {
    let token_stream = TokenStream::tokenize(QUALIFIED_NAME_EXPRESSION).0;

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
        assert_eq!(qualified_name.position_range.start.column, 1);
        assert_eq!(qualified_name.position_range.start.line, 1);

        assert_eq!(qualified_name.position_range.end.column, 5);
        assert_eq!(qualified_name.position_range.end.line, 1);
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

        assert_eq!(qualified_name.position_range.start.column, 6);
        assert_eq!(qualified_name.position_range.start.line, 1);

        assert_eq!(qualified_name.position_range.end.column, 9);
        assert_eq!(qualified_name.position_range.end.line, 1);
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

        assert_eq!(qualified_name.position_range.start.column, 10);
        assert_eq!(qualified_name.position_range.start.line, 1);

        assert_eq!(qualified_name.position_range.end.column, 16);
        assert_eq!(qualified_name.position_range.end.line, 1);
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

        assert_eq!(qualified_name.position_range.start.column, 17);
        assert_eq!(qualified_name.position_range.start.line, 1);

        assert_eq!(qualified_name.position_range.end.column, 24);
        assert_eq!(qualified_name.position_range.end.line, 1);
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

        assert_eq!(qualified_name.position_range.start.column, 25);
        assert_eq!(qualified_name.position_range.start.line, 1);

        assert_eq!(qualified_name.position_range.end.column, 35);
        assert_eq!(qualified_name.position_range.end.line, 1);
    }
}

#[test]
fn test_return_statement() {
    let token_stream = TokenStream::tokenize(RETURN_STATEMENT).0;

    let mut parser = Parser::new(&token_stream);
    let return_statement = parser.parse_return_statement().unwrap();

    match return_statement.value {
        StatementAST::ReturnStatement(statement) => {
            assert!(matches!(
                statement.expression.unwrap().value,
                ExpressionAST::BinaryExpression(_)
            ));

            assert_eq!(return_statement.position_range.start.column, 1);
            assert_eq!(return_statement.position_range.start.line, 1);

            assert_eq!(return_statement.position_range.end.column, 14);
            assert_eq!(return_statement.position_range.end.line, 1);
        }
        _ => panic!("Expected return statement"),
    }
}

#[test]
fn test_variable_declaration_statement() {
    let token_stream = TokenStream::tokenize(VARIABLE_DECLARATION_STATEMENT).0;
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

                assert_eq!(
                    variable_declaration_statement.position_range.start.column,
                    1
                );
                assert_eq!(variable_declaration_statement.position_range.start.line, 1);

                assert_eq!(variable_declaration_statement.position_range.end.column, 15);
                assert_eq!(variable_declaration_statement.position_range.end.line, 1);

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

                assert_eq!(
                    variable_declaration_statement.position_range.start.column,
                    16
                );
                assert_eq!(variable_declaration_statement.position_range.start.line, 1);

                assert_eq!(variable_declaration_statement.position_range.end.column, 30);
                assert_eq!(variable_declaration_statement.position_range.end.line, 1);

                // is_mutable
                assert_eq!(statement.is_mutable, false);
            }
            _ => panic!("Expected variable declaration statement"),
        }
    }
}

#[test]
fn if_else_statement_test() {
    let token_stream = TokenStream::tokenize(IF_ELSE_STATEMENT).0;
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

                assert_eq!(if_statement.position_range.start.column, 1);
                assert_eq!(if_statement.position_range.start.line, 1);

                assert_eq!(if_statement.position_range.end.column, 39);
                assert_eq!(if_statement.position_range.end.line, 1);
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

                assert_eq!(if_statement.position_range.start.column, 40);
                assert_eq!(if_statement.position_range.start.line, 1);

                assert_eq!(if_statement.position_range.end.column, 61);
                assert_eq!(if_statement.position_range.end.line, 1);
            }
            _ => panic!("Expected if statement"),
        }
    }
}

#[test]
fn test_while_statement() {
    let token_stream = TokenStream::tokenize(WHILE_STATEMENT).0;
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

            assert_eq!(while_statement.position_range.start.column, 1);
            assert_eq!(while_statement.position_range.start.line, 1);

            assert_eq!(while_statement.position_range.end.column, 25);
            assert_eq!(while_statement.position_range.end.line, 1);
        }
        _ => panic!("Expected while statement"),
    }
}

#[test]
fn test_class_instantiation_expression() {
    let token_stream = TokenStream::tokenize(CLASS_INSTANTIATION_EXPRESSION).0;
    let mut parser = Parser::new(&token_stream);

    // first instantiation: new A{}
    {
        let class_instantiation_expression = parser.parse_class_instantiation_expression().unwrap();

        match class_instantiation_expression.value {
            ExpressionAST::ClassInstantiationExpression(expression) => {
                assert_eq!(expression.qualified_name.value, "A");

                assert!(expression.field_initializations.is_empty());

                assert_eq!(
                    class_instantiation_expression.position_range.start.column,
                    1
                );
                assert_eq!(class_instantiation_expression.position_range.start.line, 1);

                assert_eq!(class_instantiation_expression.position_range.end.column, 8);
                assert_eq!(class_instantiation_expression.position_range.end.line, 1);
            }
            _ => panic!("Expected class instantiation expression"),
        }
    }

    // second instantiation: new B{a : 32, b : 65}
    {
        let class_instantiation_expression = parser.parse_class_instantiation_expression().unwrap();

        match class_instantiation_expression.value {
            ExpressionAST::ClassInstantiationExpression(expression) => {
                assert_eq!(expression.qualified_name.value, "B");

                assert_eq!(expression.field_initializations.len(), 2);

                // first field initialization: a : 32
                {
                    let field_initialization = &expression.field_initializations[0];

                    assert_eq!(field_initialization.value.identifier.value, "a");

                    assert!(matches!(
                        field_initialization.value.expression.value,
                        ExpressionAST::LiteralExpression(_)
                    ));
                }

                // second field initialization: b : 65
                {
                    let field_initialization = &expression.field_initializations[1];

                    assert_eq!(field_initialization.value.identifier.value, "b");

                    assert!(matches!(
                        field_initialization.value.expression.value,
                        ExpressionAST::LiteralExpression(_)
                    ));
                }

                assert_eq!(
                    class_instantiation_expression.position_range.start.column,
                    9
                );
                assert_eq!(class_instantiation_expression.position_range.start.line, 1);

                assert_eq!(class_instantiation_expression.position_range.end.column, 30);
                assert_eq!(class_instantiation_expression.position_range.end.line, 1);
            }
            _ => panic!("Expected class instantiation expression"),
        }
    }
}

#[test]
fn test_class_declaration() {
    let token_stream = TokenStream::tokenize(CLASS_DECLARATION).0;
    let mut parser = Parser::new(&token_stream);

    // export class A
    {
        let class_declaration = parser.parse_declaration().unwrap();

        match class_declaration.value {
            DeclarationAST::ClassDeclaration(class_decl) => {
                assert!(class_decl.export);
                assert_eq!(class_decl.members.len(), 0);
            }
        }
    }

    // class B {..}
    {
        let class_declaration = parser.parse_declaration().unwrap();

        match class_declaration.value {
            DeclarationAST::ClassDeclaration(class_decl) => {
                assert!(!class_decl.export);
                assert_eq!(class_decl.members.len(), 5);

                // field: public a: int32
                {
                    let field = &class_decl.members[0];

                    match &field.value {
                        ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                            assert!(
                                matches!(&field.access_modifier, Some(access_modifier) if access_modifier.value == AccessModifier::Public),
                            );
                            assert_eq!(field.identifier.value, "a");
                            assert!(matches!(
                                field.type_annotation.value,
                                TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                    PrimitiveTypeUnit::Int32
                                ))
                            ))
                        }
                        _ => panic!("Expected field"),
                    }
                }

                // field: private b: float32
                {
                    let field = &class_decl.members[1];

                    match &field.value {
                        ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                            assert!(
                                matches!(&field.access_modifier, Some(access_modifier) if access_modifier.value == AccessModifier::Private),
                            );
                            assert_eq!(field.identifier.value, "b");
                            assert!(matches!(
                                field.type_annotation.value,
                                TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                    PrimitiveTypeUnit::Float32
                                ))
                            ))
                        }
                        _ => panic!("Expected field"),
                    }
                }

                // field: c: float64
                {
                    let field = &class_decl.members[2];

                    match &field.value {
                        ClassMemberDeclarationAST::ClassFieldDeclaration(field) => {
                            assert!(field.access_modifier.is_none());
                            assert_eq!(field.identifier.value, "c");
                            assert!(matches!(
                                field.type_annotation.value,
                                TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                    PrimitiveTypeUnit::Float64
                                ))
                            ))
                        }
                        _ => panic!("Expected field"),
                    }
                }

                // method: public function A() : void {}
                {
                    let method = &class_decl.members[3];

                    match &method.value {
                        ClassMemberDeclarationAST::ClassMethodDeclaration(method) => {
                            assert!(
                                matches!(&method.access_modifier, Some(access_modifier) if access_modifier.value == AccessModifier::Public),
                            );
                            assert_eq!(method.identifier.value, "A");
                            assert_eq!(method.parameters.len(), 0);
                            assert!(matches!(
                                method.return_type_annotation.value,
                                TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                    PrimitiveTypeUnit::Void
                                ))
                            ));
                            assert_eq!(method.body.value.statements.len(), 0);
                        }
                        _ => panic!("Expected method"),
                    }
                }

                // method: private function B(mutable a: int32, b: float32) : float64 { return 0; }
                {
                    let method = &class_decl.members[4];

                    match &method.value {
                        ClassMemberDeclarationAST::ClassMethodDeclaration(method) => {
                            assert!(
                                matches!(&method.access_modifier, Some(access_modifier) if access_modifier.value == AccessModifier::Private),
                            );
                            assert_eq!(method.identifier.value, "B");
                            assert_eq!(method.parameters.len(), 2);
                            assert!(matches!(
                                method.return_type_annotation.value,
                                TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                    PrimitiveTypeUnit::Float64
                                ))
                            ));

                            // first parameter: mutable a: int32
                            {
                                let parameter = &method.parameters[0];

                                assert!(parameter.value.qualified_type_annotation.is_mutable);
                                assert_eq!(parameter.value.identifier.value, "a");
                                assert!(matches!(
                                    parameter
                                        .value
                                        .qualified_type_annotation
                                        .type_annotation
                                        .value,
                                    TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                        PrimitiveTypeUnit::Int32
                                    ))
                                ));
                            }

                            // second parameter: b: float32
                            {
                                let parameter = &method.parameters[1];

                                assert!(!parameter.value.qualified_type_annotation.is_mutable);
                                assert_eq!(parameter.value.identifier.value, "b");
                                assert!(matches!(
                                    parameter
                                        .value
                                        .qualified_type_annotation
                                        .type_annotation
                                        .value,
                                    TypeAnnotationAST::TypeUnit(TypeUnitAST::PrimitiveTypeUnit(
                                        PrimitiveTypeUnit::Float32
                                    ))
                                ));
                            }

                            // body: return 0;
                            {
                                assert_eq!(method.body.value.statements.len(), 1);

                                let return_statement = &method.body.value.statements[0];

                                match &return_statement.value {
                                    StatementAST::ReturnStatement(return_statement) => {
                                        match &return_statement.expression {
                                            Some(expr) => {
                                                assert!(matches!(
                                                    expr.value,
                                                    ExpressionAST::LiteralExpression(_)
                                                ))
                                            }
                                            None => panic!("Expected expression"),
                                        };
                                    }
                                    _ => panic!("Expected return statement"),
                                }
                            }
                        }
                        _ => panic!("Expected method"),
                    }
                }
            }
        }
    }
}

#[test]
fn test_file() {
    let token_stream = TokenStream::tokenize(FILE);
    let mut parser = Parser::new(&token_stream.0);

    let file = parser.parse_file().unwrap();

    assert_eq!(
        file.module.value.qualified_name.value,
        "simmypeet::program::test"
    );

    assert_eq!(file.import_modules.len(), 2);

    assert_eq!(
        file.import_modules[0].value.qualified_name.value,
        "simmypeet::program"
    );
    assert_eq!(
        file.import_modules[1].value.qualified_name.value,
        "another::moduleName"
    );

    assert_eq!(file.declarations.len(), 2);

    {
        match &file.declarations[0].value {
            DeclarationAST::ClassDeclaration(class_declaration) => {
                assert!(class_declaration.export);
                assert_eq!(class_declaration.identifier.value, "A");
                assert_eq!(class_declaration.members.len(), 0);
            }
        }
    }

    {
        match &file.declarations[1].value {
            DeclarationAST::ClassDeclaration(class_declaration) => {
                assert!(!class_declaration.export);
                assert_eq!(class_declaration.identifier.value, "B");
                assert_eq!(class_declaration.members.len(), 0);
            }
        }
    }
}
