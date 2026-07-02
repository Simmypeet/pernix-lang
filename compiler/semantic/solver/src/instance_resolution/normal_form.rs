use pernixc_type::symbol::TraitRef2;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    /// Transforms every generic argument in `trait_ref` into its closed normal
    /// form, returning the constraints required for the normalization to hold.
    ///
    /// Returns `None` if any normalized argument contains a type or instance
    /// inference variable.
    pub async fn trait_ref_normal_form(
        &mut self,
        trait_ref: TraitRef2,
    ) -> Result<Option<(TraitRef2, Constraints)>, OverflowError> {
        let mut normalized_arguments =
            Vec::with_capacity(trait_ref.generic_arguments().len());
        let mut constraints = Constraints::default();

        for argument in trait_ref.generic_arguments().iter() {
            let Some((normalized, new_constraints)) =
                self.normal_form(argument.clone()).await?
            else {
                return Ok(None);
            };

            normalized_arguments.push(normalized);
            constraints.extend(new_constraints);
        }

        Ok(Some((
            TraitRef2::new(
                trait_ref.trait_id(),
                self.engine().intern_unsized(normalized_arguments),
                trait_ref.binder().clone(),
            ),
            constraints,
        )))
    }
}
