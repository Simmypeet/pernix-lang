//! Contains the definition of all semantic elements of each symbol.
//!
//! This crate purely contains the declaration (a Key with Value), without any
//! executor implementations.
//!
//! This crate also doesn't contain the IR of the function, it's defined in
//! `pernixc_ir` crate.

pub mod elided_lifetimes;
pub mod fields;
pub mod implemented;
pub mod implements;
pub mod implements_arguments;
pub mod implied_predicate;
pub mod late_bound_lifetimes;
pub mod parameter;
pub mod return_type;
pub mod type_alias;
pub mod variance;
pub mod variant;
pub mod where_clause;
