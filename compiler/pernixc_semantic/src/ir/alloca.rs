//! Contains the definition of the [`Alloca`] struct.

use pernixc_base::source_file::Span;

use crate::semantic::term::r#type::Type;

/// Represents a stack memory allocation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alloca {
    /// The type of the value being allocated.
    pub r#type: Type,

    /// The span of the allocation.
    pub span: Option<Span>,
}
