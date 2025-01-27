//! Implements various builders for each components of the symbols.

pub mod builder;
pub mod fields;
pub mod function;
pub mod generic_parameters;
pub mod implementation;
pub mod reflector;
pub mod type_alias;
pub mod type_system;
pub mod variant;
pub mod where_clause;
pub mod variance_map;

pub(crate) mod occurrences;

mod build;

pub use build::{build, Compilation, ComponentCallback, SymbolCallback};
