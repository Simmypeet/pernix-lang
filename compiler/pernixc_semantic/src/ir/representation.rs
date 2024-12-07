//! Contains the definition of [`Representation`].

use getset::Getters;

use super::{
    control_flow_graph::ControlFlowGraph, scope, value::register::Register,
};
use crate::{arena::Arena, ir::alloca::Alloca, type_system::model::Model};

pub mod binding;
pub mod borrow;

/// Contains all the registers and allocas used in the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Values<M: Model> {
    /// Contains all the registers used in the program.
    #[get = "pub"]
    registers: Arena<Register<M>>,
    /// Contains all the allocas used in the program.
    #[get = "pub"]
    allocas: Arena<Alloca<M>>,
}

/// An intermediate representation of the program.
#[derive(Debug, Clone, PartialEq, Eq, Default, Getters)]
pub struct Representation<M: Model> {
    /// Contains the registers and allocas used in the program.
    #[get = "pub"]
    values: Values<M>,

    /// The control flow graph of the program.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<M>,

    /// The tree of scopes in the program.
    #[get = "pub"]
    scope_tree: scope::Tree,
}
