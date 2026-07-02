use pernixc_type::symbol::{Symbol2, TraitRef2};

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
    unify::Unify,
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

        let unifications = target
            .generic_arguments()
            .iter()
            .zip(request.generic_arguments().iter())
            .map(|(target, request)| {
                Unify::new(target.clone(), request.clone())
            })
            .collect();

        let (_, residual_unifications, constrs) =
            self.resolve_unification_constraints(unifications).await?;

        Ok(residual_unifications.is_empty().then_some(constrs))
    }
}
