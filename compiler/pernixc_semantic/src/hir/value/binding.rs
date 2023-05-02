//! Contains all the definitions of bound syntax trees.

use derive_more::From;
use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_source::Span;
use pernixc_syntax::syntax_tree::expression::PrefixOperator;

use super::{Address, TypeSystem, Value};
use crate::{
    hir::{Container, InvalidValueError, Reachability, ValueInspect},
    symbol::{ty::Type, FieldID, OverloadID, StructID},
};

/// Represents a bound syntax tree.
///
/// The bound syntax tree is attached with type information and additional semantics.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
#[allow(missing_docs)]
pub enum Binding<T: TypeSystem> {
    FunctionCall(FunctionCall<T>),
    Prefix(Prefix<T>),
    NamedLoad(NamedLoad),
    StructLiteral(StructLiteral<T>),
}

impl<T: TypeSystem> ValueInspect<T, Binding<T>> for Container<T> {
    fn get_type(&self, value: &Binding<T>) -> Result<T, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_type(value),
            Binding::Prefix(value) => self.get_type(value),
            Binding::NamedLoad(value) => self.get_type(value),
            Binding::StructLiteral(value) => self.get_type(value),
        }
    }

    fn get_span(&self, value: &Binding<T>) -> Result<pernixc_source::Span, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_span(value),
            Binding::Prefix(value) => self.get_span(value),
            Binding::NamedLoad(value) => self.get_span(value),
            Binding::StructLiteral(value) => self.get_span(value),
        }
    }

    fn get_reachability(&self, value: &Binding<T>) -> Result<Reachability, InvalidValueError> {
        match value {
            Binding::FunctionCall(value) => self.get_reachability(value),
            Binding::Prefix(value) => self.get_reachability(value),
            Binding::NamedLoad(value) => self.get_reachability(value),
            Binding::StructLiteral(value) => self.get_reachability(value),
        }
    }
}

/// Represents a bound [`FunctionCall`](pernixc_syntax::syntax_tree::expression::FunctionCall)
/// syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Getters, Hash, CopyGetters)]
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

    fn get_reachability(&self, _: &FunctionCall<T>) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Represents a bound [`Prefix`](pernixc_syntax::syntax_tree::expression::Prefix) syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters)]
pub struct Prefix<T: TypeSystem> {
    /// Specifies the location of the prefix expression.
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// The operator that is applied to the operand.
    #[get = "pub"]
    pub(in crate::hir) prefix_operator: PrefixOperator,

    /// The operand that is being operated on.
    #[get = "pub"]
    pub(in crate::hir) operand: Value<T>,
}

impl<T: TypeSystem> ValueInspect<T, Prefix<T>> for Container<T> {
    fn get_type(&self, value: &Prefix<T>) -> Result<T, InvalidValueError> {
        self.get_type(&value.operand)
    }

    fn get_span(&self, value: &Prefix<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _: &Prefix<T>) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Specifies how the [`NamedLoad`] loads the value from the address.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum LoadType {
    /// The value is moved from the address.
    Move,

    /// The value is copied from the address.
    Copy,
}

/// Represents a bound [`Named`](pernixc_syntax::syntax_tree::expression::Named) syntax tree.
///
/// This struct represents the [`Named`](pernixc_syntax::syntax_tree::expression::Named) syntax
/// tree that loads the value from the address.
/// [`Named`](pernixc_syntax::syntax_tree::expression::Named) can also represent the
/// [`EnumLiteral`](super::EnumLiteral) value as well.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct NamedLoad {
    /// The span of the [`NamedLoad`].
    #[get = "pub"]
    pub(in crate::hir) span: Span,

    /// Determines how the value is loaded from the address.
    #[get_copy = "pub"]
    pub(in crate::hir) load_type: LoadType,

    /// The address of the value.
    #[get = "pub"]
    pub(in crate::hir) address: Address,
}

impl<T: TypeSystem> ValueInspect<T, NamedLoad> for Container<T> {
    fn get_type(&self, value: &NamedLoad) -> Result<T, InvalidValueError> {
        match &value.address {
            Address::AllocaID(id) => Ok(self.allocas.get(*id).map_err(|_| InvalidValueError)?.ty()),
            Address::ParameterID(id) => Ok(T::from_type(
                self.table
                    .get_parameter(*id)
                    .map_err(|_| InvalidValueError)?
                    .type_binding()
                    .ty,
            )),
            Address::FieldAddress(address) => Ok(T::from_type(
                self.table
                    .get_field(address.field_id)
                    .map_err(|_| InvalidValueError)?
                    .ty(),
            )),
        }
    }

    fn get_span(&self, value: &NamedLoad) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _: &NamedLoad) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}

/// Represents a bound
/// [`StructLiteral`](pernixc_syntax::syntax_tree::expression::StructLiteral) syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Getters, CopyGetters)]
pub struct StructLiteral<T: TypeSystem> {
    /// The span of the struct literal.
    #[get = "pub "]
    pub(in crate::hir) span: Span,

    /// The struct ID that this literal instantiates.
    #[get_copy = "pub"]
    pub(in crate::hir) struct_id: StructID,

    /// Is a list of tuple pairs of field IDs and the value to initialize them with.
    #[get = "pub"]
    pub(in crate::hir) initialization: Vec<(FieldID, Value<T>)>,
}

impl<T: TypeSystem> ValueInspect<T, StructLiteral<T>> for Container<T> {
    fn get_type(&self, value: &StructLiteral<T>) -> Result<T, InvalidValueError> {
        Ok(T::from_type(Type::TypedID(value.struct_id.into())))
    }

    fn get_span(&self, value: &StructLiteral<T>) -> Result<Span, InvalidValueError> {
        Ok(value.span.clone())
    }

    fn get_reachability(&self, _: &StructLiteral<T>) -> Result<Reachability, InvalidValueError> {
        Ok(Reachability::Reachable)
    }
}
