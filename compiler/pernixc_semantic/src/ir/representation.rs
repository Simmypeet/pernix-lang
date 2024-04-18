use super::{alloca::Alloca, cfg::ControlFlowGraph, value::register::Register};
use crate::arena::Arena;

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Representation<T> {
    control_flow_graph: ControlFlowGraph,
    registers: Arena<Register>,
    allocas: Arena<Alloca>,

    _marker: std::marker::PhantomData<T>,
}
