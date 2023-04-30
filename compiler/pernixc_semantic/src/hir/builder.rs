//! Contains the definition of [`Builder`] -- the main interface for building the HIR.

use std::sync::Arc;

use derive_more::From;
use enum_as_inner::EnumAsInner;
use pernixc_source::Span;
use pernixc_syntax::syntax_tree::expression::{
    Expression as ExpressionSyntaxTree, Functional as FunctionalSyntaxTree,
    Imperative as ImperativeSyntaxTree,
};
use pernixc_system::arena::InvalidIDError;
use thiserror::Error;

use super::{
    instruction::Backend,
    value::{Address, Value},
    BindingErrorHandler, Hir, TypeSystem,
};
use crate::{
    cfg::ControlFlowGraph,
    infer::InferenceID,
    symbol::{table::Table, ty::Type, ModuleID, OverloadID, OverloadSetID},
};

/// Is an enumeration flag that specifies how the builder should bind the syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum BindingTarget {
    /// Binds the syntax tree for a value.
    ///
    /// This is the default binding target.
    #[default]
    ForValue,

    /// Binds the syntax tree for the underlying address of the value.
    ///
    /// This is used for obtaining the address of r-values.
    ForAddress,

    /// Specifes that the expression is bound at a statement level.
    ///
    /// This is useful for avoiding allocating a register for some expressions.
    ForStatement,
}

/// Is a data passed to every `bind_*` method that specifies how the binding should be done.
///
/// This struct is just a request to the builder, and the builder may ignore it if it is not
/// applicable. The caller should check for the result of the binding and see if it satisfies the
/// needs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BindingOption {
    /// Specifies the binding target.
    bind_kind: BindingTarget,
}

/// Represents a context in which values are created and used.
pub trait ValueContext<T: TypeSystem, V> {
    /// Gets the type of the value.
    ///
    /// # Errors
    /// If the given value was not created in this context, an [`InvalidValueError`] is returned.
    fn get_type(&self, value: &V) -> Result<T, InvalidValueError>;

    /// Gets the span of the value.
    ///
    /// # Errors
    /// If the given value was not created in this context, an [`InvalidValueError`] is returned.
    fn get_span(&self, value: &V) -> Result<Span, InvalidValueError>;
}

/// Is a [`TypeSystem`] used for building the [`Hir`].
///
/// While building the [`Hir`], the type of the value might not be known right away. Therefore, the
/// builder uses this [`TypeID`] to represent the type of the value that might be inferred later.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner, From)]
pub enum TypeID {
    /// The type might be inferred later by the inference context.
    InferenceID(InferenceID),

    /// The type is determined right away at the time of the creation of the value.
    Type(Type),
}

impl TypeSystem for TypeID {
    fn from_type(ty: Type) -> Self { ty.into() }
}

/// Occurs when a value is used in a context where it wasn't created from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[error("The value is used in a context where it wasn't created from.")]
pub struct InvalidValueError;

/// Is a builder that builds the [`Hir`] by inputting the various
/// [`StatementSyntaxTree`](pernixc_syntax::syntax_tree::statement::Statement) to it.
#[derive(Debug)]
pub struct Builder {
    control_flow_graph: ControlFlowGraph<Backend<TypeID>>,
    table: Arc<Table>,
    overload_id: OverloadID,
    overload_set_id: OverloadSetID,
    parent_module_id: ModuleID,
}

impl Builder {
    /// Creates a new [`Builder`].
    ///
    /// # Parameters
    /// - `table`: The [`Table`] that will be used for symbol resolution and various lookups.
    /// - `overload_id`: The context in which the [`Builder`] will be building the [`Hir`].
    ///
    /// # Errors
    /// - [`InvalidIDError`] if the `overload_id` is invalid for the `table`.
    pub fn new(table: Arc<Table>, overload_id: OverloadID) -> Result<Self, InvalidIDError> {
        let overload_set_id = table.get_overload(overload_id)?.parent_overload_set_id();
        let parent_module_id = table.get_overload_set(overload_set_id)?.parent_module_id();

        Ok(Self {
            control_flow_graph: ControlFlowGraph::new(),
            table,
            overload_id,
            overload_set_id,
            parent_module_id,
        })
    }

    /// Finishes the building process and returns the [`Hir`].
    pub fn build(self, handler: &impl BindingErrorHandler) -> Hir { todo!() }
}

/// Is an error occurred during the binding process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum BindingError {
    #[error("Encountered a fatal semantic error that cause the binding process to terminate.")]
    FatalSemanticError,
}

/// Is the result of various `bind_*` functions.
#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner, From)]
pub enum BindingResult {
    /// The binding process returns a value.
    Value(Value<TypeID>),

    /// The binding process returns an address to a value.
    Address(Address),

    /// The binding process doesn't return anything but does update the control flow graph.
    None,
}

impl Builder {
    /// Binds the given [`ExpressionSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_expression(
        &mut self,
        syntax_tree: &ExpressionSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            ExpressionSyntaxTree::Functional(syn) => {
                self.bind_functional(syn, binding_option, handler)
            }
            ExpressionSyntaxTree::Imperative(syn) => {
                self.bind_imperative(syn, binding_option, handler)
            }
        }
    }

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_functional(
        &mut self,
        syntax_tree: &FunctionalSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        match syntax_tree {
            FunctionalSyntaxTree::NumericLiteral(_) => todo!(),
            FunctionalSyntaxTree::BooleanLiteral(_) => todo!(),
            FunctionalSyntaxTree::Binary(_) => todo!(),
            FunctionalSyntaxTree::Prefix(_) => todo!(),
            FunctionalSyntaxTree::Named(_) => todo!(),
            FunctionalSyntaxTree::FunctionCall(_) => todo!(),
            FunctionalSyntaxTree::Parenthesized(_) => todo!(),
            FunctionalSyntaxTree::StructLiteral(_) => todo!(),
            FunctionalSyntaxTree::MemberAccess(_) => todo!(),
            FunctionalSyntaxTree::Continue(_) => todo!(),
            FunctionalSyntaxTree::Break(_) => todo!(),
            FunctionalSyntaxTree::Return(_) => todo!(),
            FunctionalSyntaxTree::Express(_) => todo!(),
            FunctionalSyntaxTree::Cast(_) => todo!(),
        }
    }

    /// Binds the given [`ImperativeSyntaxTree`] and returns the [`BindingResult`].
    ///
    /// # Errors
    /// - If the binding process encounters a fatal semantic error.
    pub fn bind_imperative(
        &mut self,
        syntax_tree: &ImperativeSyntaxTree,
        binding_option: &BindingOption,
        handler: &impl BindingErrorHandler,
    ) -> Result<BindingResult, BindingError> {
        todo!()
    }
}
