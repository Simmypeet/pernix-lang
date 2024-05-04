//! Contains the definition of [`Representation`].

use super::{control_flow_graph::ControlFlowGraph, value::register::Register};
use crate::{arena::Arena, ir::alloca::Alloca};

pub(crate) mod pattern;
pub mod sub_value;

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Representation {
    control_flow_graph: ControlFlowGraph,
    registers: Arena<Register>,
    allocas: Arena<Alloca>,
}
