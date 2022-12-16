use pernix_lexer::token::LiteralConstantType;
use pernix_parser::abstract_syntax_tree::{
    expression::{
        BinaryExpression, Expression, FunctionCallExpression, UnaryExpression,
    },
    PositionWrapper,
};

use crate::symbol::TypeSymbol;

/// Represent an enumeration containing all the possible bound expressions.
pub enum BoundExpression<'table, 'parser, 'ast> {
    BoundUnaryExpression(BoundUnaryExpression<'table, 'parser, 'ast>),
    BoundBinaryExpression(BoundBinaryExpression<'table, 'parser, 'ast>),
    BoundIdentifierExpression(BoundIdentifierExpression<'table, 'parser, 'ast>),
    BoundLiteralExpression(BoundLiteralExpression<'table, 'parser, 'ast>),
    BoundFunctionCallExpression(
        BoundFunctionCallExpression<'table, 'parser, 'ast>,
    ),
    BoundImplicitCastExpression(
        BoundImplicitCastExpression<'table, 'parser, 'ast>,
    ),
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
            BoundExpression::BoundImplicitCastExpression(
                implicit_cast_expression,
            ) => &implicit_cast_expression.expression_type,
        }
    }
}

/// Represent a bound version of an [`UnaryExpression`] AST.
pub struct BoundUnaryExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser UnaryExpression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`BinaryExpression`] AST.
pub struct BoundBinaryExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser BinaryExpression<'ast>>,
    pub left: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub right: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`Expression::LiteralExpression`] AST.
pub struct BoundLiteralExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser LiteralConstantType<'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of an [`Expression::IdentifierExpression`] AST.
pub struct BoundIdentifierExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser &'ast str>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a bound version of a [`FunctionCallExpression`] AST.
pub struct BoundFunctionCallExpression<'table, 'parser, 'ast> {
    pub ast: PositionWrapper<&'parser FunctionCallExpression<'ast>>,
    pub arguments: Vec<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent a structure containing the type of an expression and its category.
pub struct ExpressionType<'table> {
    pub type_symbol: &'table TypeSymbol,
    pub category: ExpressionCategory,
}

/// Represent a bound expression that performs casting.
pub struct BoundImplicitCastExpression<'table, 'parser, 'ast> {
    pub ast: &'parser PositionWrapper<Expression<'ast>>,
    pub operand: Box<BoundExpression<'table, 'parser, 'ast>>,
    pub expression_type: ExpressionType<'table>,
}

/// Represent an enumeration containing all the possible categories of an
/// expression.
pub enum ExpressionCategory {
    /// Represent an expression that doesn't have the memory storage.
    RValue,

    /// Represent an expression that has the memory storage.
    LValue { is_mutable: bool },
}
