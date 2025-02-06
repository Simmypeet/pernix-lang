//! Implements various builders for each components of the symbols.

pub mod builder;
pub mod fields;
pub mod function;
pub mod generic_parameters;
pub mod implementation;
pub mod implementation_coherence;
pub mod ir;
pub mod occurrences;
pub mod reflector;
pub mod type_alias;
pub mod type_system;
pub mod utility;
pub mod variance_map;
pub mod variant;
pub mod where_clause;

mod build;

pub use build::{build, Compilation, ComponentCallback, SymbolCallback};
