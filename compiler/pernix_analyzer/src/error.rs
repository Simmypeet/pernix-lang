use std::ops::Range;

use pernix_parser::abstract_syntax_tree::{
    declaration::{FunctionDeclaration, QualifiedType},
    expression::{
        BinaryExpression, Expression, FunctionCallExpression,
        IdentifierExpression, LiteralExpression,
    },
    statement::{ReturnStatement, Statement},
    PositionWrapper, UnaryOperator,
};
use pernix_project::source_code::SourcePosition;

use crate::symbol::TypeSymbol;

/// Represent an enumeration containing all the semantic errors
#[derive(Debug)]
pub enum Error<'table, 'ast> {
    FunctionRedeclaration {
        previous_declaration: PositionWrapper<&'ast FunctionDeclaration<'ast>>,
        redeclaration: PositionWrapper<&'ast FunctionDeclaration<'ast>>,
    },
    ParameterRedeclaration {
        function_declaration: PositionWrapper<&'ast FunctionDeclaration<'ast>>,
        previous_declaration:
            &'ast PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
        redeclaration: &'ast PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
    },
    TypeNotFound {
        type_name: PositionWrapper<&'ast str>,
    },
    InvalidUnaryOperation {
        operator: PositionWrapper<UnaryOperator>,
        operand: &'ast PositionWrapper<Expression<'ast>>,
        operand_type: &'table TypeSymbol,
    },
    TypeMismatched {
        expected_type: &'table TypeSymbol,
        expression_type: &'table TypeSymbol,
        expression: &'ast PositionWrapper<Expression<'ast>>,
    },
    InvalidArgumentCount {
        function_declaration: PositionWrapper<&'ast FunctionDeclaration<'ast>>,
        function_call_expression:
            PositionWrapper<&'ast FunctionCallExpression<'ast>>,
        expected_argument_count: usize,
        supplied_argument_count: usize,
    },
    UndefinedVariable {
        variable_name: PositionWrapper<&'ast IdentifierExpression<'ast>>,
    },
    UndefinedFunctionName {
        function_call_expression:
            PositionWrapper<&'ast FunctionCallExpression<'ast>>,
    },
    UndefinedLiteralSuffix {
        literal_expression: PositionWrapper<&'ast LiteralExpression<'ast>>,
    },
    RValueAssignment {
        rvalue_expression: &'ast PositionWrapper<Expression<'ast>>,
    },
    InvalidBinaryOperation {
        binary_expression: PositionWrapper<&'ast BinaryExpression<'ast>>,
    },
    ReturnStatementMustReturnAValue {
        return_statement: PositionWrapper<&'ast ReturnStatement<'ast>>,
    },
    DefinedVoidVariable {
        position: Range<SourcePosition>,
    },
    IsNotMutable {
        lvalue_expression: &'ast PositionWrapper<Expression<'ast>>,
        mutate_expression: &'ast PositionWrapper<Expression<'ast>>,
    },
    InvalidLoopFlowStatement {
        invalid_statement: &'ast PositionWrapper<Statement<'ast>>,
    },
}
