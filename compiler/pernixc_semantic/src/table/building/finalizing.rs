//! This module contains the finalizing logic for the semantic analysis.
//!
//! The finalizing phase is the last phase and follows after the drafting phase.
//! It builds the symbol table to its final form and having all the correct
//! information.

mod check;
mod finalize;
mod finalizer;
mod generic_parameters;
mod occurrences;
mod variance;
mod where_caluse;

pub use finalizer::Finalizer;
