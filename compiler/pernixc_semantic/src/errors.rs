//! Contains all kinds of errors that can be encountered during the semantic analysis phase.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use thiserror::Error;

use crate::{
    hir::value::{BinaryOperator, InferrableType, PrefixOperator},
    symbol::{AccessModifier, FieldID, StructID, ID},
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

/// The prefix operation cannot be applied to the operand type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidPrefixOperation {
    /// The span of the prefix operation.
    pub source_span: SourceSpan,

    /// The prefix operator.
    pub prefix_operator: PrefixOperator,

    /// The type of the operand.
    pub operand_type: InferrableType,
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
    pub available_symbol_id: ID,

    /// The span of the symbol that redefines another symbol.
    pub new_symbol_span: SourceSpan,
}

/// The symbol cannot be accessed from the current scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotAccessible {
    /// The span of the path that tries to access the symbol.
    pub referencing_site: SourceSpan,

    /// The index of the symbol that is not accessible.
    pub symbol_id: ID,
}

/// The symbol can not be found in a particular scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolNotFound {
    /// The span of the path that tries to access the symbol.
    pub referencing_site: SourceSpan,

    /// The location where the symbol was expected to be found.
    pub in_scope: Option<ID>,
}

/// The given symbol cannot be used as an expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpressionExpected {
    /// The span of the symbol that is not an expression.
    pub source_span: SourceSpan,
}

/// Expected the symbol to be a type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeExpected {
    /// The span of the symbol that is not a type.
    pub source_span: SourceSpan,

    /// The index of the symbol that is not a type.
    pub symbol_id: ID,
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
    pub source_span: SourceSpan,
}

/// The numeric literal is out of range.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OutOfRangeNumericLiteral {
    /// The span of the numeric literal.
    pub source_span: SourceSpan,
}

/// Mismatch between the expected and the type of the expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMismatch {
    /// The expected type.
    pub expected: InferrableType,

    /// The type of the expression.
    pub actual: InferrableType,

    /// The span of the expression.
    pub source_span: SourceSpan,
}

/// Assignment to an rvalue.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToRValue {
    /// The span of the binary expression that performs the assignment.
    pub source_span: SourceSpan,
}

/// Assignment to an immutable l-value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssignToImmutable {
    /// The span of the binary expression that performs the assignment.
    pub source_span: SourceSpan,
}

/// The binary expression has an invalid combination of operands and operator.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidBinaryOperation {
    /// The type of the left-hand side operand.
    pub left_operand_type: InferrableType,

    /// The binary operator.
    pub binary_operator: BinaryOperator,

    /// The type of the right-hand side operand.
    pub right_operand_type: InferrableType,

    /// The span of the binary expression.
    pub source_span: SourceSpan,
}

/// The number of arguments passed to a function call does not match the number of parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ArgumentCountMismatch {
    /// The number of parameters expected.
    pub expected: usize,

    /// The number of arguments passed.
    pub found: usize,

    /// The span of the function call.
    pub source_span: SourceSpan,
}

/// The symbol is not callable.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolIsNotCallable {
    /// The span of the function call.
    pub source_span: SourceSpan,
}

/// The symbol is not a struct for struct literal construction.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructExpected {
    /// The span of the struct literal.
    pub source_span: SourceSpan,
}

/// The field with the given could not be found in the struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldNotFound {
    /// The span of the field name.
    pub source_span: SourceSpan,

    /// The ID of the struct that does not contain the field.
    pub struct_id: StructID,
}

/// The field is already initialized in the struct literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DuplicateFieldInitialization {
    /// The span of the duplicate field initialization.
    pub source_span: SourceSpan,
}

/// The struct member is not accessible.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberNotAccessible {
    /// The span of the field name.
    pub source_span: SourceSpan,

    /// The ID of the struct symbol that does not contain the field.
    pub struct_id: StructID,

    /// The ID of the field in the struct.
    pub field_id: FieldID,
}

/// The field is not accessible.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldIsNotAccessible {
    /// The span of the field name.
    pub source_span: SourceSpan,

    /// The ID of the struct that contains the inaccessible field.
    pub struct_id: StructID,

    /// The ID of the field in the struct.
    pub field_id: FieldID,
}

/// Field access on a non-struct expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberAccessOnNonStruct {
    /// The span of the field name.
    pub source_span: SourceSpan,
}

/// Loop control is used outside of a loop.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoopControlOutsideLoop {
    /// The span of the loop control.
    pub span: SourceSpan,
}

/// The label does not refer to any block or loop.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelNotFound {
    /// The span of the label name.
    pub source_span: SourceSpan,
}

/// The `express` expression is used outside of a block.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExpressOutsideBlock {
    /// The span of the expression.
    pub span: SourceSpan,
}

/// The return requires an expression but the expression is missing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReturnRequiredValue {
    /// The span of the return statement.
    pub source_span: SourceSpan,
}

/// The struct literal doesn't initialize all fields.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IncompleteFieldInitialization {
    /// The span of the field name.
    pub source_span: SourceSpan,

    /// The ID of the struct that contains the field.
    pub struct_id: StructID,
}

/// Not all paths in the block expression express the value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NotAllPathExpressTheValue {
    /// Specifies the location of the block expression.
    pub source_span: SourceSpan,
}

/// Is an enumeration of all kinds of errors that can occur during semantic analysis.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error, EnumAsInner, From)]
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
    NotAllPathExpressTheValue(NotAllPathExpressTheValue),
    /*
    AmbiguousConversionInBinaryOperation(AmbiguousConversionInBinaryOperation),
    */
    ExpressionExpected(ExpressionExpected),
    InvalidPrefixOperation(InvalidPrefixOperation),
    EnumVariantRedefinition(EnumVariantRedefinition),
    ArgumentCountMismatch(ArgumentCountMismatch),
    SymbolIsNotCallable(SymbolIsNotCallable),
    StructExpected(StructExpected),
    FieldNotFound(FieldNotFound),
    DuplicateFieldInitialization(DuplicateFieldInitialization),
    MemberNotAccessible(MemberNotAccessible),
    FieldIsNotAccessible(FieldIsNotAccessible),
    MemberAccessOnNonStruct(MemberAccessOnNonStruct),
    LoopControlOutsideLoop(LoopControlOutsideLoop),
    LabelNotFound(LabelNotFound),
    ExpressOutsideBlock(ExpressOutsideBlock),
    IncompleteFieldInitialization(IncompleteFieldInitialization),
    ReturnRequiredValue(ReturnRequiredValue),
}
