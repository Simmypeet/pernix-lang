use std::ops::Range;

use pernix_parser::abstract_syntax_tree::{
    declaration::{FunctionDeclaration, QualifiedType},
    expression::{
        BinaryExpression, Expression, FunctionCallExpression,
        IdentifierExpression, LiteralExpression,
    },
    statement::ReturnStatement,
    PositionWrapper, UnaryOperator,
};
use pernix_project::source_code::SourcePosition;

use crate::symbol::TypeSymbol;

/// Represent an enumeration containing all the semantic errors
#[derive(Debug)]
pub enum Error<'table, 'parser, 'ast> {
    FunctionRedeclaration {
        previous_declaration:
            PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        redeclaration: PositionWrapper<&'parser FunctionDeclaration<'ast>>,
    },
    ParameterRedeclaration {
        function_declaration:
            PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        previous_declaration:
            &'parser PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
        redeclaration:
            &'parser PositionWrapper<(QualifiedType<'ast>, &'ast str)>,
    },
    TypeNotFound {
        type_name: PositionWrapper<&'ast str>,
    },
    InvalidUnaryOperation {
        operator: PositionWrapper<UnaryOperator>,
        operand: &'parser PositionWrapper<Expression<'ast>>,
        operand_type: &'table TypeSymbol,
    },
    TypeMismatched {
        expected_type: &'table TypeSymbol,
        expression_type: &'table TypeSymbol,
        expression: &'parser PositionWrapper<Expression<'ast>>,
    },
    InvalidArgumentCount {
        function_declaration:
            PositionWrapper<&'parser FunctionDeclaration<'ast>>,
        function_call_expression:
            PositionWrapper<&'parser FunctionCallExpression<'ast>>,
        expected_argument_count: usize,
        supplied_argument_count: usize,
    },
    UndefinedVariable {
        variable_name: PositionWrapper<&'parser IdentifierExpression<'ast>>,
    },
    UndefinedFunctionName {
        function_call_expression:
            PositionWrapper<&'parser FunctionCallExpression<'ast>>,
    },
    UndefinedLiteralSuffix {
        literal_expression: PositionWrapper<&'parser LiteralExpression<'ast>>,
    },
    RValueAssignment {
        rvalue_expression: &'parser PositionWrapper<Expression<'ast>>,
    },
    InvalidBinaryOperation {
        binary_expression: PositionWrapper<&'parser BinaryExpression<'ast>>,
    },
    ReturnStatementMustReturnAValue {
        return_statement: PositionWrapper<&'parser ReturnStatement<'ast>>,
    },
    DefinedVoidVariable {
        position: Range<SourcePosition>,
    },
    IsNotMutable {
        lvalue_expression: &'parser PositionWrapper<Expression<'ast>>,
        mutate_expression: &'parser PositionWrapper<Expression<'ast>>,
    },
}
