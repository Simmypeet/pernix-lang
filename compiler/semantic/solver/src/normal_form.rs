use pernixc_type::r#type::{Type2, kind::TyKind};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    /// Transforms the given type into its normal form, returning the lifetime
    /// constraints that need to be satisfied for the normalization to hold.
    ///
    /// Returns `None` if the resulting normal form contains a type, instance,
    /// effect-signature, or effect-row inference variable.
    pub async fn normal_form(
        &mut self,
        ty: Interned<Type2>,
    ) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
        let (normalized, constraints) = self
            .reduce_type(ty.clone())
            .await?
            .unwrap_or((ty, Constraints::default()));

        let contains_non_lifetime_inference = normalized
            .as_ref()
            .contains_inference_variable_matching(|variable| {
                matches!(
                    variable.kind(),
                    TyKind::Type
                        | TyKind::Instance
                        | TyKind::EffectSignature
                        | TyKind::EffectRow
                )
            });

        Ok((!contains_non_lifetime_inference)
            .then_some((normalized, constraints)))
    }
}

#[cfg(test)]
mod test;
