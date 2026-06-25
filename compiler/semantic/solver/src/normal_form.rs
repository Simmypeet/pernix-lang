use pernixc_type::r#type::{Type, context::TyContext, kind::TyKind};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    /// Transforms the given type into its normal form, returning the lifetime
    /// constraints that need to be satisfied for the normalization to hold.
    ///
    /// Returns `None` if the resulting normal form contains a type or instance
    /// inference variable.
    pub async fn normal_form(
        &mut self,
        ty: Interned<Type>,
    ) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
        let (normalized, constraints) = self
            .reduce_type(ty.clone())
            .await?
            .unwrap_or((ty, Constraints::default()));

        let contains_type_or_instance_inference = normalized
            .as_ref()
            .contains_inference_variable_matching(|variable| {
                matches!(
                    self.get_inference_variable_kind(&variable),
                    TyKind::Type | TyKind::Instance
                )
            });

        Ok((!contains_type_or_instance_inference)
            .then_some((normalized, constraints)))
    }
}

#[cfg(test)]
mod test;
