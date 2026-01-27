//! Defines the [`IR`] and [`IRMap`] structs.

use getset::{CopyGetters, Getters, MutGetters};
use pernixc_arena::{Arena, ID};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use qbice::{Decode, Encode, StableHash};
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

use crate::{
    alloca::Alloca,
    control_flow_graph::ControlFlowGraph,
    function_ir::IRContext,
    scope,
    transform::{self, Transformer, TypeTermSource},
    value::{Value, register::Register},
};

/// Composes of the [`IR`] and its associated [`IRContext`].
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Encode,
    Decode,
    Getters,
    MutGetters,
    CopyGetters,
)]
pub struct IRWithContext {
    /// The intermediate representation (IR).
    #[get = "pub"]
    #[get_mut = "pub"]
    ir: IR,

    /// The context in which the IR is used.
    #[get_copy = "pub"]
    context: IRContext,
}

impl IRWithContext {
    /// Creates a new [`IRWithContext`] with the given IR and context.
    #[must_use]
    pub const fn new(ir: IR, context: IRContext) -> Self {
        Self { ir, context }
    }

    /// Creates a new [`IRWithContext`] representing a root IR.
    #[must_use]
    pub const fn new_root_ir(ir: IR) -> Self {
        Self { ir, context: IRContext::Root }
    }
}

/// A collection of intermediate representations (IRs) used in a paritcular
/// functino.
///
/// Typically, each function could have multiple IRs representing different ///
/// semantics such as closures, effect handlers, etc.
///
/// But ultimately, there is a single root IR that represents the main body of
/// the function.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    StableHash,
    Encode,
    Decode,
    derive_more::Index,
    derive_more::IndexMut,
)]
pub struct IRMap {
    #[index]
    #[index_mut]
    irs: Arena<IRWithContext>,
}

impl transform::Element for IRMap {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) {
        for (_, ir_with_context) in &mut self.irs {
            ir_with_context.ir.transform(transformer, engine).await;
        }
    }
}

impl Default for IRMap {
    fn default() -> Self { Self::new() }
}

impl IRMap {
    /// Creates a new empty collection of IRs with a root IR.
    #[must_use]
    pub fn new() -> Self { Self { irs: Arena::new() } }

    /// Inserts a new IR into the collection and returns its ID.
    #[must_use]
    pub fn new_ir(&mut self, ir: IRWithContext) -> ID<IRWithContext> {
        self.irs.insert(ir)
    }

    /// Returns an iterator over all IRs with their IDs.
    #[must_use]
    pub fn ir_with_contexts(
        &self,
    ) -> impl ExactSizeIterator<Item = (ID<IRWithContext>, &'_ IRWithContext)> + '_
    {
        self.irs.iter()
    }

    /// Returns a mutable iterator over all IRs with their IDs.
    #[must_use]
    pub fn ir_with_contexts_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (ID<IRWithContext>, &'_ mut IRWithContext)> + '_
    {
        self.irs.iter_mut()
    }
}

/// Contains all the registers and allocas used in the program.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Encode, Decode, StableHash,
)]
pub struct Values {
    /// Contains all the registers used in the program.
    pub registers: Arena<Register>,

    /// Contains all the allocas used in the program.
    pub allocas: Arena<Alloca>,
}

impl Values {
    /// Gets the span of the given value, if it has one.
    #[must_use]
    pub fn span_of_value<'s>(&'s self, value: &'s Value) -> &'s RelativeSpan {
        match value {
            Value::Register(id) => &self.registers[*id].span,
            Value::Literal(lit) => lit.span(),
        }
    }
}

/// An intermediate representation of a particular procedure.
///
/// It can be used as a body of a function, closure, effect handler,
/// compile-time constant, etc.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Encode, Decode, StableHash,
)]
pub struct IR {
    /// Contains the registers and allocas used in the program.
    pub values: Values,

    /// The control flow graph of the program.
    pub control_flow_graph: ControlFlowGraph,

    /// The tree of scopes in the program.
    pub scope_tree: scope::Tree,
}

impl transform::Element for IR {
    async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        transformer: &mut T,
        engine: &TrackedEngine,
    ) {
        self.control_flow_graph.transform(transformer, engine).await;

        for (_, register) in &mut self.values.registers {
            register.transform(transformer, engine).await;
        }

        for (&alloca_id, alloca) in &mut self.values.allocas {
            transformer
                .transform(
                    &mut alloca.r#type,
                    TypeTermSource::Alloca(alloca_id),
                    alloca.span,
                )
                .await;
        }
    }
}
