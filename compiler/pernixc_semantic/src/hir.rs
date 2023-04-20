//! This module is responsible for building the high-level intermediate representation of the
//! program.
//!
//! The high-level intermediate representation is responsible for desconstructing the program
//! control statement/expression (i.e if-else, loop, etc.) into a control flow graph and most of the
//! semantic checks are done here, such as type checking, type inference, symbol resolution, etc.

use std::collections::HashMap;

use getset::Getters;

use self::{
    instruction::Backend,
    value::{Variable, VariableID},
};
use crate::{cfg::ControlFlowGraph, symbol::ty::Type};

mod builder;
pub mod instruction;
pub mod value;

/// The high-level intermediate representation of the function.
#[derive(Debug, Clone, Getters)]
pub struct HIR {
    /// The control flow graph of the function.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<Backend<Type>>,

    /// The list of local variables used in the function.
    #[get = "pub"]
    local_variables: HashMap<VariableID, Variable<Type>>,
}
