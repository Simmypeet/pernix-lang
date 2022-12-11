use pernix_lexer::token::LiteralConstantType;

use super::{BinaryOperator, PositionWrapper, UnaryOperator};

/// Represent an expression that is composed of two expressions and a binary
/// operator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpression<'a> {
    pub left: Box<PositionWrapper<Expression<'a>>>,
    pub operator: PositionWrapper<BinaryOperator>,
    pub right: Box<PositionWrapper<Expression<'a>>>,
}

/// Represent an expression that is composed of an unary operator and an operand
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpression<'a> {
    pub operator: PositionWrapper<UnaryOperator>,
    pub operand: Box<PositionWrapper<Expression<'a>>>,
}

/// Represent an expression that is produced by a function call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCallExpression<'a> {
    pub function_name: PositionWrapper<&'a str>,
    pub arguments: Vec<PositionWrapper<Expression<'a>>>,
}

/// Represent an enumeration containing all possible expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression<'a> {
    BinaryExpression(BinaryExpression<'a>),
    UnaryExpression(UnaryExpression<'a>),
    LiteralExpression(LiteralConstantType<'a>),
    IdentifierExpression(&'a str),
    FunctionCallExpression(FunctionCallExpression<'a>),
}
