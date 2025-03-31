//! Contains the definition of the [`Alloca`] struct.

use pernixc_arena::ID;
use pernixc_semantic::term::{self, r#type::Type};
use pernixc_source_file::Span;
use serde::{Deserialize, Serialize};

use crate::{model::Transform, scope};

/// Represents a stack memory allocation.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct Alloca<M: term::Model> {
    /// The type of the value being allocated.
    pub r#type: Type<M>,

    /// The scope in which this alloca is declared.
    pub declared_in_scope_id: ID<scope::Scope>,

    /// The order in which this alloca is declared in the scope (starting from
    /// 0).
    pub declaration_order: usize,

    /// The span of the allocation.
    #[serde(skip)]
    pub span: Option<Span>,
}

impl<M: term::Model> Alloca<M> {
    /// Transforms the address to another model using the given transformer.
    #[allow(clippy::missing_errors_doc)]
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Result<Alloca<T::Target>, T::Error> {
        Ok(Alloca {
            r#type: transformer.transform(self.r#type, self.span.as_ref())?,
            declared_in_scope_id: self.declared_in_scope_id,
            declaration_order: self.declaration_order,
            span: self.span,
        })
    }
}
