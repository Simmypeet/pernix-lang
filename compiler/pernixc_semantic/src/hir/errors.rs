use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_common::source_file::Span;

use crate::{
    infer::{Constraint, InferableType},
    symbol::{errors::SymbolError, ty::Type, GlobalID, OverloadID, OverloadSetID},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum SemanticError {
    SymbolError(SymbolError),
    InvalidNumericLiteralSuffix(InvalidNumericLiteralSuffix),
    FloatingPointLiteralHasIntegralSuffix(FloatingPointLiteralHasIntegralSuffix),
    NoOverloadAccessible(NoAccessibleOverload),
    NoOverloadWithMatchingNumberOfArguments(NoOverloadWithMatchingNumberOfArguments),
    NoOverloadWithMatchingArgumentTypes(NoOverloadWithMatchingArgumentTypes),
    AmbiguousFunctionCall(AmbiguousFunctionCall),
    SymbolNotCallable(SymbolNotCallable),
    TypeMismatch(TypeMismatch),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InvalidNumericLiteralSuffix {
    pub(super) suffix_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatingPointLiteralHasIntegralSuffix {
    pub(super) floating_point_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolNotCallable {
    pub(super) found_id: GlobalID,
    pub(super) symbol_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NoAccessibleOverload {
    pub(super) overload_set_id: OverloadSetID,
    pub(super) symbol_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NoOverloadWithMatchingNumberOfArguments {
    pub(super) overload_set_id: OverloadSetID,
    pub(super) argument_count: usize,
    pub(super) symbol_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NoOverloadWithMatchingArgumentTypes {
    pub(super) overload_set_id: OverloadSetID,
    pub(super) symbol_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AmbiguousFunctionCall {
    pub(super) function_call_span: Span,
    pub(super) candidate_overloads: Vec<OverloadID>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeMismatch {
    pub(super) expression_span: Span,
    pub(super) expected: InferableType,
    pub(super) found: InferableType,
}
