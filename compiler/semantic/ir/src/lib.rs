//! Contains all the definitions related to the intermediate representation of
//! the function body.

use std::sync::Arc;

use alloca::Alloca;
use control_flow_graph::ControlFlowGraph;
use pernixc_arena::Arena;
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;
use pernixc_target::Global;

use crate::value::register::Register;

pub mod address;
pub mod alloca;
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
