use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::source_file::Span;
use pernixc_syntax::syntax_tree::expression::PrefixOperator;

use super::{Hir, Inspectable, Reachability, SsaValue, ValueTypeID};
use crate::symbol::OverloadID;

/// Represents a bound
/// [`ExpressionSyntaxTree`](pernixc_syntax::syntax_tree::expression::Expression) (except for
/// numeric and boolean literals).
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, EnumAsInner)]
pub enum Binding {
    FunctionCall(FunctionCall),
    Prefix(Prefix),
}

impl Inspectable<Binding> for Hir {
    fn get_value_type_id(&self, value: &Binding) -> ValueTypeID {
        match value {
            Binding::FunctionCall(function_call) => self.get_value_type_id(function_call),
            Binding::Prefix(prefix) => self.get_value_type_id(prefix),
        }
    }

    fn get_reachability(&self, value: &Binding) -> Reachability {
        match value {
            Binding::FunctionCall(function_call) => self.get_reachability(function_call),
            Binding::Prefix(prefix) => self.get_reachability(prefix),
        }
    }

    fn get_span(&self, value: &Binding) -> Span {
        match value {
            Binding::FunctionCall(function_call) => self.get_span(function_call),
            Binding::Prefix(prefix) => self.get_span(prefix),
        }
    }
}

/// Represents a bound
/// [`FunctionCallSyntaxTree`](pernixc_syntax::syntax_tree::expression::FunctionCall).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct FunctionCall {
    pub(super) span: Span,

    /// The ID of the overload that was bound to the function call.
    #[get_copy = "pub"]
    pub(super) overload_id: OverloadID,

    /// List of arguments that were passed to the function call.
    #[get = "pub"]
    pub(super) arguments: Vec<SsaValue>,
}

impl Inspectable<FunctionCall> for Hir {
    fn get_value_type_id(&self, value: &FunctionCall) -> ValueTypeID {
        ValueTypeID::Type(self.table[value.overload_id].return_type())
    }

    fn get_reachability(&self, _: &FunctionCall) -> Reachability { Reachability::Reachable }

    fn get_span(&self, value: &FunctionCall) -> Span { value.span.clone() }
}

/// Represents a bound
/// [`PrefixExpressionSyntaxTree`](pernixc_syntax::syntax_tree::expression::Prefix).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct Prefix {
    /// The prefix operator that was applied.
    #[get = "pub"]
    pub(super) prefix_operator: PrefixOperator,

    /// The operand that the prefix operator was applied to.
    #[get = "pub"]
    pub(super) operand: SsaValue,

    /// The span of the prefix operator.
    pub(super) span: Span,
}

impl Inspectable<Prefix> for Hir {
    fn get_value_type_id(&self, value: &Prefix) -> ValueTypeID {
        self.get_value_type_id(&value.operand)
    }

    fn get_reachability(&self, value: &Prefix) -> Reachability {
        self.get_reachability(&value.operand)
    }

    fn get_span(&self, value: &Prefix) -> Span { value.span.clone() }
}
