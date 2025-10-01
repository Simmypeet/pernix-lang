//! Contains all the definitions related to the intermediate representation of
//! the function body.

use std::sync::Arc;

use alloca::Alloca;
use control_flow_graph::ControlFlowGraph;
use pernixc_arena::Arena;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;
use pernixc_term::{constant::Constant, lifetime::Lifetime, r#type::Type};

use crate::{
    closure::{Capture, Closure},
    transform::{Transformer, TypeTermSource},
    value::register::Register,
};

pub mod address;
pub mod alloca;
pub mod closure;
pub mod control_flow_graph;
pub mod instruction;
pub mod pattern;
pub mod scope;
pub mod transform;
pub mod value;

/// Contains all the registers and allocas used in the program.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, StableHash,
)]
pub struct Values {
    /// Contains all the registers used in the program.
    pub registers: Arena<Register>,

    /// Contains all the allocas used in the program.
    pub allocas: Arena<Alloca>,

    /// Contains all the closures defined in the program.
    pub closures: Arena<Closure>,

    /// The list of all captured memories (variables) from the parent IR if
    /// the current IR is a closure.
    pub captures: Arena<Capture>,
}

impl Values {
    /// Gets the span of the given value, if it has one.
    #[must_use]
    pub fn span_of_value<'s>(
        &'s self,
        value: &'s value::Value,
    ) -> Option<&'s RelativeSpan> {
        match value {
            value::Value::Register(id) => {
                self.registers.get(*id).unwrap().span.as_ref()
            }
            value::Value::Literal(lit) => lit.span(),
        }
    }
}

/// An intermediate representation of the program.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    StableHash,
    pernixc_query::Value,
)]
#[id(Global<pernixc_symbol::ID>)]
#[value(Arc<IR>)]
#[extend(method(get_ir))]
pub struct IR {
    /// Contains the registers and allocas used in the program.
    pub values: Values,

    /// The control flow graph of the program.
    pub control_flow_graph: ControlFlowGraph,

    /// The tree of scopes in the program.
    pub scope_tree: scope::Tree,
}

impl IR {
    /// Applies the given transformer to all types, lifetimes, and constants in
    /// the IR.
    pub async fn transform<
        T: Transformer<Lifetime> + Transformer<Type> + Transformer<Constant>,
    >(
        &mut self,
        tracked_engine: &TrackedEngine,
        transformer: &mut T,
    ) -> Result<(), CyclicError> {
        self.control_flow_graph.transform(transformer).await?;

        for (_, register) in &mut self.values.registers {
            register.transform(transformer, tracked_engine).await?;
        }

        self.control_flow_graph.transform(transformer).await?;

        for (&alloca_id, alloca) in &mut self.values.allocas {
            transformer
                .transform(
                    &mut alloca.r#type,
                    TypeTermSource::Alloca(alloca_id),
                    alloca.span,
                )
                .await?;
        }

        Ok(())
    }
}
