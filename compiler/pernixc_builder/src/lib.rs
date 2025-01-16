//! Implements various builders for each components of the symbols.

pub mod diagnostic;
pub mod generic_parameters;
pub mod reflector;

pub(crate) mod occurrences;

mod build;

pub use build::build;

/// Builder for all the components of the symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Builder;
