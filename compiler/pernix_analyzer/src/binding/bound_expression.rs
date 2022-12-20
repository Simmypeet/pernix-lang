use std::sync::Arc;

use pernix_parser::abstract_syntax_tree::{
    expression::{
        BinaryExpression, Expression, FunctionCallExpression,
        IdentifierExpression, LiteralExpression, UnaryExpression,
    },
    PositionWrapper,
};

use crate::symbol::{FunctionSymbol, TypeSymbol, VariableSymbol};

/// Represent an enumeration containing all the possible bound expressions.
#[derive(Clone, Debug)]
pub enum BoundExpression<'table, 'ast> {
    BoundUnaryExpression(BoundUnaryExpression<'table, 'ast>),
    BoundBinaryExpression(BoundBinaryExpression<'table, 'ast>),
    BoundIdentifierExpression(BoundIdentifierExpression<'table, 'ast>),
    BoundLiteralExpression(BoundLiteralExpression<'table, 'ast>),
    BoundFunctionCallExpression(BoundFunctionCallExpression<'table, 'ast>),
    BoundCastExpression(BoundCastExpression<'table, 'ast>),
}

/// Represent a bound version of an [`UnaryExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundUnaryExpression<'table, 'ast> {
    pub ast: PositionWrapper<&'ast UnaryExpression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`BinaryExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundBinaryExpression<'table, 'ast> {
    pub ast: PositionWrapper<&'ast BinaryExpression<'ast>>,
    pub left: Box<BoundExpression<'table, 'ast>>,
    pub right: Box<BoundExpression<'table, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`Expression::LiteralExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundLiteralExpression<'table, 'ast> {
    pub ast: PositionWrapper<&'ast LiteralExpression<'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of an [`Expression::IdentifierExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundIdentifierExpression<'table, 'ast> {
    pub ast: PositionWrapper<&'ast IdentifierExpression<'ast>>,
    pub variable_symbol: Arc<VariableSymbol<'table, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`FunctionCallExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundFunctionCallExpression<'table, 'ast> {
    pub ast: PositionWrapper<&'ast FunctionCallExpression<'ast>>,
    pub arguments: Vec<BoundExpression<'table, 'ast>>,
    pub function: &'table FunctionSymbol<'table, 'ast>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a structure containing the type of an expression and its category.
#[derive(Clone, Debug)]
pub struct ExpressionType<'table> {
    pub type_symbol: &'table TypeSymbol,
    pub category: ExpressionCategory,
}

/// Represent a bound expression that performs casting.
#[derive(Clone, Debug)]
pub struct BoundCastExpression<'table, 'ast> {
    pub ast: &'ast PositionWrapper<Expression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent an enumeration containing all the possible categories of an
/// expression.
#[derive(Clone, Copy, Debug)]
pub enum ExpressionCategory {
    /// Represent an expression that doesn't have the memory storage.
    RValue,

    /// Represent an expression that has the memory storage.
    LValue { is_mutable: bool },
}

impl<'table, 'ast> BoundExpression<'table, 'ast> {
    /// Get the type of the expression.
    pub fn get_type(&self) -> &ExpressionType<'table> {
        match &self {
            BoundExpression::BoundUnaryExpression(unary_expression) => {
                &unary_expression.expression_type
            }
            BoundExpression::BoundBinaryExpression(binary_expression) => {
                &binary_expression.expression_type
            }
            BoundExpression::BoundIdentifierExpression(
                identifier_expression,
            ) => &identifier_expression.expression_type,
            BoundExpression::BoundLiteralExpression(literal_expression) => {
                &literal_expression.expression_type
            }
            BoundExpression::BoundFunctionCallExpression(
                function_call_expression,
            ) => &function_call_expression.expression_type,
            BoundExpression::BoundCastExpression(implicit_cast_expression) => {
                &implicit_cast_expression.expression_type
            }
        }
    }
}
