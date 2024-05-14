//! Contains the definition of [`Representation`].

use getset::Getters;

use super::{
    control_flow_graph::ControlFlowGraph, value::register::Register, State,
};
use crate::{arena::Arena, ir::alloca::Alloca};

// pub(crate) mod pattern;
pub mod sub_value;

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Representation<T: State> {
    /// The control flow graph containing the instructions of the program.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<T>,

    /// Contains all the registers used in the program.
    #[get = "pub"]
    registers: Arena<Register<T>>,

    /// Contains all the alloca used in the program.
    #[get = "pub"]
    allocas: Arena<Alloca<T>>,
}
