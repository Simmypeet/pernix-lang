use std::ops::Range;

use pernix_project::source_code::SourcePosition;

pub mod declaration;
pub mod expression;

/// A wrapper around a value that also contains the position of the value in the
/// source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PositiionWrapper<T> {
    pub position: Range<SourcePosition>,
    pub value: T,
}

/// List of all available binary operators
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
    And,
    Or,
}

/// List of all available unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
}

/// An enumeration containing all kinds of types referenced in the program
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Identifier(String),
    Array {
        element_type: Box<Type>,
        size: usize,
    },
}
