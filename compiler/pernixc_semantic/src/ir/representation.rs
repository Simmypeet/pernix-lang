//! Contains the definition of [`Representation`].

use super::{control_flow_graph::ControlFlowGraph, value::register::Register};
use crate::{arena::Arena, ir::alloca::Alloca};

pub(crate) mod pattern;

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Representation<T> {
    control_flow_graph: ControlFlowGraph,
    registers: Arena<Register>,
    allocas: Arena<Alloca>,

    _marker: std::marker::PhantomData<T>,
}
