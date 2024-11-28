//! Contains the definition of the [`Alloca`] struct.

use pernixc_base::source_file::Span;

use super::scope;
use crate::{
    arena::ID,
    type_system::{
        model::{Model, Transform},
        term::r#type::Type,
    },
};

/// Represents a stack memory allocation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Alloca<M: Model> {
    /// The type of the value being allocated.
    pub r#type: Type<M>,

    /// The scope in which this alloca is declared.
    pub declared_in_scope_id: ID<scope::Scope>,

    /// The order in which this alloca is declared in the scope (starting from
    /// 0).
    pub declaration_order: usize,

    /// The span of the allocation.
    pub span: Option<Span>,
}

impl<M: Model> Alloca<M> {
    /// Transforms the address to another model using the given transformer.
    pub fn transform_model<T: Transform<Type<M>>>(
        self,
        transformer: &mut T,
    ) -> Alloca<T::Target> {
        Alloca {
            r#type: transformer.transform(self.r#type, self.span.clone()),
            declared_in_scope_id: self.declared_in_scope_id,
            declaration_order: self.declaration_order,
            span: self.span,
        }
    }
}
