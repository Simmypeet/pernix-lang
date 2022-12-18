use pernix_parser::abstract_syntax_tree::{
    expression::{
        BinaryExpression, Expression, FunctionCallExpression,
        IdentifierExpression, LiteralExpression, UnaryExpression,
    },
    PositionWrapper,
};

use crate::symbol::{FunctionSymbol, TypeSymbol};

/// Represent an enumeration containing all the possible bound expressions.
#[derive(Clone, Debug)]
pub enum BoundExpression<'table, 'parser, 'ast> {
    BoundUnaryExpression(BoundUnaryExpression<'table, 'parser, 'ast>),
    BoundBinaryExpression(BoundBinaryExpression<'table, 'parser, 'ast>),
    BoundIdentifierExpression(BoundIdentifierExpression<'table, 'parser, 'ast>),
    BoundLiteralExpression(BoundLiteralExpression<'table, 'parser, 'ast>),
    BoundFunctionCallExpression(
        BoundFunctionCallExpression<'table, 'parser, 'ast>,
    ),
    BoundCastExpression(BoundCastExpression<'table, 'parser, 'ast>),
}

/// Represent a bound version of an [`UnaryExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundUnaryExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser UnaryExpression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`BinaryExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundBinaryExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser BinaryExpression<'ast>>,
    pub left: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub right: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`Expression::LiteralExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundLiteralExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser LiteralExpression<'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of an [`Expression::IdentifierExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundIdentifierExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser IdentifierExpression<'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`FunctionCallExpression`] AST.
#[derive(Clone, Debug)]
pub struct BoundFunctionCallExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser FunctionCallExpression<'ast>>,
    pub arguments: Vec<BoundExpression<'table, 'parser, 'ast>>,
    pub function: &'table FunctionSymbol<'table, 'parser, 'ast>,
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
pub struct BoundCastExpression<'table, 'parser, 'ast> {
    pub ast: &'parser PositionWrapper<Expression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'parser, 'ast>>,
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

impl<'table, 'parser, 'ast> BoundExpression<'table, 'parser, 'ast> {
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
