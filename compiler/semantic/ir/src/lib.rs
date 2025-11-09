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
    effect_handler::HandlerGroups,
    transform::{Transformer, TypeTermSource},
    value::register::Register,
};

pub mod address;
pub mod alloca;
pub mod capture;
pub mod closure_parameters;
pub mod control_flow_graph;
pub mod effect_handler;
pub mod instruction;
pub mod pattern;
pub mod scope;
pub mod transform;
pub mod typer;
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

/// An intermediate representation of a particular procedure.
///
/// It can be used as a body of a function, closure, effect handler,
/// compile-time constant, etc.
#[derive(
    Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, StableHash,
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
    ) -> Result<(), CyclicError> {
        self.control_flow_graph.transform(transformer, engine).await?;

        for (_, register) in &mut self.values.registers {
            register.transform(transformer, engine).await?;
        }

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

/// An intermediate representation of a function.
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
#[value(Arc<FunctionIR>)]
#[extend(method(get_ir))]
pub struct FunctionIR {
    /// The IR representing the body of the function.
    pub ir: IR,

    /// The collection of all handler groups defined in the function body.
    pub handler_groups: HandlerGroups,
}
