//! Contains all the definitions related to the intermediate representation of
//! the function body.

pub mod address;
pub mod alloca;
pub mod capture;
pub mod closure_parameters;
pub mod control_flow_graph;
pub mod function_ir;
pub mod handling_scope;
pub mod instruction;
pub mod ir;
pub mod pattern;
pub mod scope;
pub mod transform;
pub mod typer;
pub mod value;
pub mod visitor;

// re-exports
pub use function_ir::{FunctionIR, Key, get_ir};
pub use ir::{IRWithContext, Values};
