//! Contains all the definitions of bound syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;

use super::{TypeSystem, Value};
use crate::{
    hir::{Container, InvalidValueError, Reachability, ValueInspect},
    symbol::OverloadID,
};

/// Represents a bound syntax tree.
///
/// The bound syntax tree is attached with type information and additional semantics.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Binding<T: TypeSystem> {
    FunctionCall(FunctionCall<T>),
}

impl<T: TypeSystem> ValueInspect<T, Binding<T>> for Container<T> {
    fn get_type(&self, value: &Binding<T>) -> Result<T, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_type(value),
        }
    }

    fn get_span(&self, value: &Binding<T>) -> Result<pernixc_source::Span, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_span(value),
        }
    }

    fn get_reachability(&self, value: &Binding<T>) -> Result<Reachability, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_reachability(value),
        }
    }
}

/// Represents a bound [`FunctionCall`](pernixc_syntax::syntax_tree::expression::FunctionCall).
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct FunctionCall<T: TypeSystem> {
    /// Specifies the location of the function call.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The ID of the function overload that is being called.
    #[get_copy = "pub"]
    pub(in crate::hir) overload_id: OverloadID,

    /// The arguments that are passed to the function.
    #[get = "pub"]
    pub(in crate::hir) arguments: Vec<Value<T>>,
}

impl<T: TypeSystem> ValueInspect<T, FunctionCall<T>> for Container<T> {
    fn get_type(&self, value: &FunctionCall<T>) -> Result<T, InvalidValueError> {
        let overload = self
            .table
            .get_overload(value.overload_id)
            .map_err(|_| InvalidValueError)?;

        Ok(T::from_type(overload.return_type()))
    }

    fn get_span(&self, value: &FunctionCall<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(
        &self,
        _value: &FunctionCall<T>,
    ) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}
