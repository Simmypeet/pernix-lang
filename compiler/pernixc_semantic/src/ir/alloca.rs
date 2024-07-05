//! Contains the definition of the [`Alloca`] struct.

use pernixc_base::source_file::Span;

use crate::type_system::{model::Model, term::r#type::Type};

/// Represents a stack memory allocation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alloca<M: Model> {
    /// The type of the value being allocated.
    pub r#type: Type<M>,

    /// The span of the allocation.
    pub span: Option<Span>,
}
