//! Defines the executors for various term queries.

use crate::build::Build;

pub mod diagnostic;

mod build;
mod effect_annotation;
mod fields;
mod function_signature;
mod generic_parameters;
mod global_instances;
mod implemented;
mod implements_qualified_identifier;
mod import;
mod instance_associated_value;
mod instance_coherence;
mod occurrences;
mod recursive_adt_check;
mod trait_ref;
mod type_alias;
mod variance;
mod variant;
mod wf_check;
mod where_clause;
