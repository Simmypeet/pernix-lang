use pernixc_type::symbol::{Symbol2, TraitRef2};

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

impl Solver<'_> {
    /// Determines whether the polymorphic `target` trait reference can be used
    /// in place of `request`.
    ///
    /// The bound variables in `target` are instantiated with fresh inference
    /// variables before its generic arguments are unified with those in
    /// `request`.
    pub async fn match_trait_ref(
        &mut self,
        target: TraitRef2,
        request: Symbol2,
    ) -> Result<Option<Constraints>, OverflowError> {
        if target.trait_id() != request.symbol_id()
            || target.generic_arguments().len()
                != request.generic_arguments().len()
        {
            return Ok(None);
        }

        let instantiations =
            self.create_inference_instantiations(target.binder().kinds());

        let target = target.instantiate(&instantiations, self.engine());

        // NOTE: we use match here because we expect that the `target` is fully
        // reduced and of course the `request` is also fully reduced
        let Some((_, constrs)) = self
            .match_types(
                target
                    .generic_arguments()
                    .iter()
                    .cloned()
                    .zip(request.generic_arguments().iter().cloned()),
            )
            .await
        else {
            return Ok(None);
        };

        Ok(Some(constrs))
    }
}
