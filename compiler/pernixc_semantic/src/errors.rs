use enum_as_inner::EnumAsInner;
use thiserror::Error;

use crate::{
    binding::expression::{BinaryOperator, ExpressionType, PrefixOperator},
    symbol::{ty::Type, AccessModifier},
    SourceSpan,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccessibilityLeaking {
    pub symbol_span: SourceSpan,
    pub symbol_access_modifier: AccessModifier,
    pub parent_access_modifier: AccessModifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantRedefinition {
    pub new_variant_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldRedefinition {
    pub new_field_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRedifinition {
    pub available_symbol_index: usize,
    pub new_symbol_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotAccessible {
    pub referencing_site: SourceSpan,
    pub symbol_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolNotFound {
    pub referencing_site: SourceSpan,
    pub in_scope: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeExpected {
    pub span: SourceSpan,
    pub symbol_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterRedifinition {
    pub new_parameter_span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidNumericLiteralSuffix {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OutOfRangeNumericLiteral {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMismatch {
    pub expected: Type,
    pub actual: Type,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToRValue {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToImmutable {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOperation {
    pub lhs_type: ExpressionType,
    pub operator: BinaryOperator,
    pub rhs_type: ExpressionType,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AmbiguousConversionInBinaryOperation {
    pub lhs_type: ExpressionType,
    pub operator: BinaryOperator,
    pub rhs_type: ExpressionType,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidPrefixOperation {
    pub operator: PrefixOperator,
    pub operand_type: ExpressionType,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpressionExpected {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgumentCountMismatch {
    pub expected: usize,
    pub found: usize,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotCallable {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructExpected {
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Error, EnumAsInner)]
#[error("Encountered a semantic error while constructing the symbol table.")]
pub enum SemanticError {
    AccessibilityLeaking(AccessibilityLeaking),
    SymbolRedifinition(SymbolRedifinition),
    SymbolIsNotAccessible(SymbolIsNotAccessible),
    SymbolNotFound(SymbolNotFound),
    TypeExpected(TypeExpected),
    ParameterRedifinition(ParameterRedifinition),
    FieldRedefinition(FieldRedefinition),
    InvalidNumericLiteralSuffix(InvalidNumericLiteralSuffix),
    OutOfRangeNumericLiteral(OutOfRangeNumericLiteral),
    TypeMismatch(TypeMismatch),
    AssignToRValue(AssignToRValue),
    AssignToImmutable(AssignToImmutable),
    InvalidBinaryOperation(InvalidBinaryOperation),
    AmbiguousConversionInBinaryOperation(AmbiguousConversionInBinaryOperation),
    InvalidPrefixOperation(InvalidPrefixOperation),
    EnumVariantRedefinition(EnumVariantRedefinition),
    ExpressionExpected(ExpressionExpected),
    ArgumentCountMismatch(ArgumentCountMismatch),
    SymbolIsNotCallable(SymbolIsNotCallable),
    StructExpected(StructExpected),
}
