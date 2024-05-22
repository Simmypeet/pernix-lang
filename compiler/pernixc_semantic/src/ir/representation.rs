//! Contains the definition of [`Representation`].

use getset::Getters;

use super::{control_flow_graph::ControlFlowGraph, value::register::Register};
use crate::{arena::Arena, ir::alloca::Alloca, semantic::model::Model};

pub(crate) mod building;
pub mod sub_value;

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Representation<M: Model> {
    /// The control flow graph containing the instructions of the program.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<M>,

    /// Contains all the registers used in the program.
    #[get = "pub"]
    registers: Arena<Register<M>>,

    /// Contains all the alloca used in the program.
    #[get = "pub"]
    allocas: Arena<Alloca<M>>,
}
