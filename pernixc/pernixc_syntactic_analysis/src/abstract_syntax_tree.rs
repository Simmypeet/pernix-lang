use std::ops::Range;

use pernixc_common::source_file::TextPosition;

pub mod declaration;
pub mod expression;
pub mod statement;

/// Is a wrapper around a value that also contains the position of the value in the
/// source file.
#[derive(Debug, Clone)]
pub struct PositionWrapper<T> {
    pub position: Range<TextPosition>,
    pub value: T,
}

/// Represent an enumeration containing all type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// TypeUnit:
///     PrimitiveTypeUnit
///     | QualifiedName;
/// ```
#[derive(Debug, Clone)]
pub enum TypeUnit<'src> {
    PrimitiveTypeUnit(PrimitiveTypeUnit),
    QualifiedName(&'src str),
}

/// Represent an enumeration containing all primitive type units.
///
/// ANTLR4 grammar:
///
/// ``` txt
/// PrimitiveTypeUnit:
///     'void'
///     | 'bool'
///     | 'int8'
///     | 'int16'
///     | 'int32'
///     | 'int64'
///     | 'uint8'
///     | 'uint16'
///     | 'uint32'
///     | 'uint64'
///     | 'float32'
///     | 'float64'
/// ```
#[derive(Debug, Clone, Copy)]
pub enum PrimitiveTypeUnit {
    Void,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
}
