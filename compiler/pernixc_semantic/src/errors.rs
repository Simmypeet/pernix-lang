//! Contains all kinds of errors that can be encountered during the semantic analysis phase.

use enum_as_inner::EnumAsInner;
use thiserror::Error;

use crate::{
    binding::expression::{BinaryOperator, ExpressionType, PrefixOperator},
    symbol::{ty::Type, AccessModifier},
    SourceSpan,
};

/// The access modifier of a symbol is leaking or not consistent.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AccessibilityLeaking {
    /// The span of the symbol that is leaking.
    pub symbol_span: SourceSpan,

    /// The access modifier of the symbol that is leaking.
    pub symbol_access_modifier: AccessModifier,

    /// The access modifier of the parent symbol.
    pub parent_access_modifier: AccessModifier,
}

/// Enum variant redefinition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariantRedefinition {
    /// The span of the variant that redefines another variant.
    pub new_variant_span: SourceSpan,
}

/// Field redefinition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldRedefinition {
    /// The span of the field that redefines another field.
    pub new_field_span: SourceSpan,
}

/// Symbol redefinition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolRedifinition {
    /// The index of the symbol that is being redefined.
    pub available_symbol_index: usize,

    /// The span of the symbol that redefines another symbol.
    pub new_symbol_span: SourceSpan,
}

/// The symbol cannot be accessed from the current scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotAccessible {
    /// The span of the path that tries to access the symbol.
    pub referencing_site: SourceSpan,

    /// The index of the symbol that is not accessible.
    pub symbol_index: usize,
}

/// The symbol can not be found in a particular scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolNotFound {
    /// The span of the path that tries to access the symbol.
    pub referencing_site: SourceSpan,

    /// The location where the symbol was expected to be found.
    pub in_scope: Option<usize>,
}

/// Expected the symbol to be a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeExpected {
    /// The span of the symbol that is not a type.
    pub span: SourceSpan,

    /// The index of the symbol that is not a type.
    pub symbol_index: usize,
}

/// Parameter redefinition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParameterRedifinition {
    /// The span of the parameter that redefines another parameter.
    pub new_parameter_span: SourceSpan,
}

/// The numeric literal has an invalid suffix.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidNumericLiteralSuffix {
    /// The span of the numeric literal suffix.
    pub span: SourceSpan,
}

/// The numeric literal is out of range.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OutOfRangeNumericLiteral {
    /// The span of the numeric literal.
    pub span: SourceSpan,
}

/// Mismatch between the expected and the type of the expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMismatch {
    /// The expected type.
    pub expected: Type,

    /// The type of the expression.
    pub actual: Type,

    /// The span of the expression.
    pub span: SourceSpan,
}

/// Assignment to an rvalue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToRValue {
    /// The span of the binary expression that performs the assignment.
    pub span: SourceSpan,
}

/// Assignment to an immutable l-value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToImmutable {
    /// The span of the binary expression that performs the assignment.
    pub span: SourceSpan,
}

/// The binary expression has an invalid combination of operands and operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOperation {
    /// The type of the left-hand side operand.
    pub lhs_type: ExpressionType,

    /// The operator.
    pub operator: BinaryOperator,

    /// The type of the right-hand side operand.
    pub rhs_type: ExpressionType,

    /// The span of the binary expression.
    pub span: SourceSpan,
}

/// The operands of the binary expression have an ambiguous conversion.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AmbiguousConversionInBinaryOperation {
    /// The type of the left-hand side operand.
    pub lhs_type: ExpressionType,

    /// The operator.
    pub operator: BinaryOperator,

    /// The type of the right-hand side operand.
    pub rhs_type: ExpressionType,

    /// The span of the binary expression.
    pub span: SourceSpan,
}

/// The prefix expression has an invalid combination of operands and operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidPrefixOperation {
    /// The operator.
    pub operator: PrefixOperator,

    /// The type of the operand.
    pub operand_type: ExpressionType,

    /// The span of the prefix expression.
    pub span: SourceSpan,
}

/// The symbol does not produce a value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpressionExpected {
    /// The span of the symbol referencing.
    pub span: SourceSpan,
}

/// The number of arguments passed to a function call does not match the number of parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgumentCountMismatch {
    /// The number of parameters expected.
    pub expected: usize,

    /// The number of arguments passed.
    pub found: usize,

    /// The span of the function call.
    pub span: SourceSpan,
}

/// The symbol is not callable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotCallable {
    /// The span of the function call.
    pub span: SourceSpan,
}

/// The symbol is not a struct for struct literal construction.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructExpected {
    /// The span of the struct literal.
    pub span: SourceSpan,
}

/// Is an enumeration of all kinds of errors that can occur during semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error, EnumAsInner)]
#[error("Encountered a semantic error while analyzing the program")]
#[allow(missing_docs)]
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
