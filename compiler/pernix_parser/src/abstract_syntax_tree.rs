use std::ops::Range;

use pernix_project::source_code::SourcePosition;

pub mod declaration;
pub mod expression;
pub mod statement;

/// Describe a wrapper around a value that also contains the position of the value in the
/// source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PositionWrapper<T> {
    pub position: Range<SourcePosition>,
    pub value: T,
}

/// Represent an enumeration containing all possible binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Asterisk,
    Slash,
    Percent,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    Equal,
    NotEqual,
    LogicalAnd,
    LogicalOr,
}

/// List of all available unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
}
