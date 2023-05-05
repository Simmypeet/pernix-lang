//! Contains the definitions of the semantic errors related to the binding/HIR building phase.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;

use crate::{
    cfg::BasicBlockID,
    infer::InferableType,
    symbol::{FieldID, GlobalID, OverloadID, OverloadSetID, ScopedID, StructID},
};

/// Is a semantic error that occurs during the binding/HIR building phase.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Error {
    InvalidNumericLiteralSuffix(InvalidNumericLiteralSuffix),
    FloatingPointLiteralHasIntegralSuffix(FloatingPointLiteralHasIntegralSuffix),
    SymbolNotCallable(SymbolNotCallable),
    NoAccessibleOverload(NoAccessibleOverload),
    NoOverloadWithMatchingNumberOfArguments(NoOverloadWithMatchingNumberOfArguments),
    NoOverloadWithMatchingArgumentTypes(NoOverloadWithMatchingArgumentTypes),
    TypeMismatch(TypeMismatch),
    AmbiguousFunctionCall(AmbiguousFunctionCall),
    ValueExpected(ValueExpected),
    UninitializedFields(UninitializedFields),
    FieldInaccessible(FieldInaccessible),
    DuplicateFieldInitialization(DuplicateFieldInitialization),
    StructExpected(StructExpected),
    UnknownField(UnknownField),
    NoFieldOnType(NoFieldOnType),
    LValueExpected(LValueExpected),
    MutableLValueExpected(MutableLValueExpected),
    ExpressOutsideBlock(ExpressOutsideBlock),
    NoBlockWithGivenLabelFound(NoBlockWithGivenLabelFound),
    NotAllFlowPathExpressValue(NotAllFlowPathExpressValue),
}

/// The numeric literal suffix is not applicable to the literal's type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct InvalidNumericLiteralSuffix {
    /// Specifies the location of the numeric literal suffix.
    #[get = "pub"]
    pub(super) suffix_span: Span,
}

/// The numeric literal suffix is for integral types, but it's applied to a floating point literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct FloatingPointLiteralHasIntegralSuffix {
    /// Specifies the location of the numeric literal suffix.
    #[get = "pub"]
    pub(super) floating_point_span: Span,
}

/// The symbol is not callable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct SymbolNotCallable {
    /// The symbol that was attempted to be called.
    #[get_copy = "pub"]
    pub(super) found_id: GlobalID,

    /// Specifies the location where the symbol was attempted to be called.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

/// No overload accessible for resolving the call.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoAccessibleOverload {
    /// The overload set that contains all inaccessible overloads.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// Specifies the location where the symbol was attempted to be accessed.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

/// No overload with matching number of arguments found.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoOverloadWithMatchingNumberOfArguments {
    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// The number of arguments that were passed to the overload set.
    #[get_copy = "pub"]
    pub(super) argument_count: usize,

    /// Specifies the location where the symbol was attempted to be resolved.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

/// No overload with matching argument types found.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoOverloadWithMatchingArgumentTypes {
    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,

    /// Specifes the location where the symbol was attempted to be resolved.
    #[get = "pub"]
    pub(super) symbol_span: Span,

    /// The types of the arguments that were passed to the overload set.
    #[get = "pub"]
    pub(super) argument_types: Vec<InferableType>,
}

/// Multiple overloads are available to be called.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct AmbiguousFunctionCall {
    /// The span of the function call.
    #[get = "pub"]
    pub(super) function_call_span: Span,

    /// The list of overloads that are available to be called.
    #[get = "pub"]
    pub(super) candidate_overloads: Vec<OverloadID>,

    /// The overload set that the function call was attempted to resolve to.
    #[get_copy = "pub"]
    pub(super) overload_set_id: OverloadSetID,
}

/// The type of an expression does not match the expected type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct TypeMismatch {
    /// The span of the expression.
    #[get = "pub"]
    pub(super) expression_span: Span,

    /// The expected type.
    #[get_copy = "pub"]
    pub(super) expect: InferableType,

    /// The type that was found.
    #[get_copy = "pub"]
    pub(super) found: InferableType,
}

/// Expected a value from symbol resolution, but got a non-value symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct ValueExpected {
    /// The span of the expression.
    #[get = "pub"]
    pub(super) expression_span: Span,

    /// The found symbol that was not a value.
    #[get_copy = "pub"]
    pub(super) found_symbol: GlobalID,
}

/// Not all fields of a struct literal were initialized.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct UninitializedFields {
    /// The span of the struct literal.
    #[get = "pub"]
    pub(super) struct_literal_span: Span,

    /// The struct that was not fully initialized.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The fields that were not initialized.
    #[get = "pub"]
    pub(super) uninitialized_fields: Vec<FieldID>,
}

/// A field of a struct was initialized multiple times.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct DuplicateFieldInitialization {
    /// The span of the duplicate initialization.
    #[get = "pub"]
    pub(super) duplicate_initialization_span: Span,

    /// The span of the previous initialization.
    #[get = "pub"]
    pub(super) previous_initialization_span: Span,

    /// The ID of the field that was initialized multiple times.
    #[get_copy = "pub"]
    pub(super) field_id: FieldID,

    /// The ID of the struct that the field belongs to.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,
}

/// The field of the struct is not accessible from the current scope.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct FieldInaccessible {
    /// The ID of the field that was not accessible.
    #[get_copy = "pub"]
    pub(super) field_id: FieldID,

    /// The ID of the struct that the field belongs to.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The span of the field access.
    #[get = "pub"]
    pub(super) field_span: Span,

    /// The ID of the scope that the field was accessed from.
    #[get_copy = "pub"]
    pub(super) current_scope: ScopedID,
}

/// The symbol found in the struct literal syntax is not a struct symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct StructExpected {
    /// The ID of the symbol that was found in the struct literal syntax.
    #[get_copy = "pub"]
    pub(super) found_id: GlobalID,

    /// The span of the symbol that was found in the struct literal syntax.
    #[get = "pub"]
    pub(super) symbol_span: Span,
}

/// The struct does not have a field with the given name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct UnknownField {
    /// The ID of the struct that does not have the field.
    #[get_copy = "pub"]
    pub(super) struct_id: StructID,

    /// The name of the field that was not found.
    #[get = "pub"]
    pub(super) field_name_span: Span,
}

/// The expression cannot be accessed as a field.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NoFieldOnType {
    /// The span of the expression that was used to access the field.
    #[get = "pub"]
    pub(super) operand_span: Span,

    /// The type of the expression that was used to access the field.
    #[get_copy = "pub"]
    pub(super) operand_type: InferableType,
}

/// Expected an lvalue, but got an rvalue.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct LValueExpected {
    /// The span of the expression that was used as an lvalue.
    #[get = "pub"]
    pub(super) expression_span: Span,
}

/// The given lvalue is not mutable.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct MutableLValueExpected {
    /// The span of the expression that was used as an lvalue.
    #[get = "pub"]
    pub(super) expression_span: Span,
}

/// `express` expression has a label that refers to a block that does not exist.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NoBlockWithGivenLabelFound {
    /// The span of the label in the `express` expression.
    #[get = "pub"]
    pub(super) label_span: Span,
}

/// `express` expression was used outside the block.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct ExpressOutsideBlock {
    /// The span of the invalid `express` expression.
    #[get = "pub"]
    pub(super) express_span: Span,
}

///  Not all flow paths in the `block` express a value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct NotAllFlowPathExpressValue {
    /// The span of the `block` expression.
    #[get = "pub"]
    pub(super) block_span: Span,

    /// List of the successors of the `block` expression that do not express a value.
    #[get = "pub"]
    pub(super) missing_value_basic_blocks: Vec<BasicBlockID>,
}
