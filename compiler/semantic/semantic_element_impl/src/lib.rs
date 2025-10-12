//! Defines the executors for various term queries.

use crate::build::Build;

pub mod diagnostic;

mod build;
mod do_effect;
mod fields;
mod function_signature;
mod generic_parameters;
mod implemented;
mod implements_qualified_identifier;
mod import;
mod ir;
mod occurrences;
mod r#unsafe;
mod type_alias;
mod variance;
mod variant;
mod wf_check;
mod where_clause;
