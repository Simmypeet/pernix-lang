//! Contains the definitions of the semantic errors related to the binding/HIR building phase.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;

use crate::{
    infer::InferableType,
    symbol::{GlobalID, OverloadID, OverloadSetID},
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
