//! This module is responsible for building the high-level intermediate representation of the
//! program.
//!
//! The HIR is a first semantically analyzed representation of the program -- every semantic checks
//! are performed at this level. It's in the form of a control flow graph that has an abstraction
//! level similar to that of C language. Most of the high-level constructs are lowered into a
//! sequence of C-like instructions.
//!
//! The HIR can be later used in a varius ways, such as:
//! - Transpiling to a lower-level language such as C.
//! - Breaking down into a lower-level representation, MIR.

use std::collections::HashMap;

use getset::Getters;

use self::{
    instruction::Instruction,
    value::{Value, Variable, VariableID},
};
use crate::{control_flow_graph::ControlFlowGraph, symbol::ty::Type};

pub mod builder;
pub mod instruction;
pub mod value;

/// The high-level intermediate representation of the function.
#[derive(Debug, Clone, Getters)]
pub struct HIR {
    /// The control flow graph of the function.
    #[get = "pub"]
    control_flow_graph: ControlFlowGraph<Instruction<Type>, Value<Type>>,

    /// The list of local variables used in the function.
    #[get = "pub"]
    local_variables: HashMap<VariableID, Variable<Type>>,
}