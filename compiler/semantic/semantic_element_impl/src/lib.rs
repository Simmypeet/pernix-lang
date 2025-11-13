//! Defines the executors for various term queries.

use crate::build::Build;

pub mod diagnostic;

mod build;
mod effect_annotation;
mod fields;
mod function_ir;
mod function_signature;
mod generic_parameters;
mod implemented;
mod implements_qualified_identifier;
mod import;
mod occurrences;
mod type_alias;
mod variance;
mod variant;
mod wf_check;
mod where_clause;
