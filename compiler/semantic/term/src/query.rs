//! Module contains the definition of various query components that can be
//! retrieved via the [`pernixc_query`] crate.
//!
//! The queries defined here are simply the query component definitions, it
//! doesn't contain the actual query logic, which will be defined in the
//! another crate.

pub mod generic_parameters;
pub mod where_clause;
