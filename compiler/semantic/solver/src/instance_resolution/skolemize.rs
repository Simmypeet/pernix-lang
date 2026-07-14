use pernixc_type::symbol::{Symbol2, TraitRef2};

use crate::solver::{BoundInstantiation, Solver};

impl Solver<'_> {
    /// Instantiates the bound variables in `trait_ref` with fresh skolemized
    /// variables from the current universe.
    pub fn skolemize_trait_ref(
        &mut self,
        trait_ref: &TraitRef2,
    ) -> (Symbol2, BoundInstantiation) {
        let new_universe = trait_ref.max_universe().next();
        let instantiations = self.create_skolem_instantiations_in_universe(
            trait_ref.binder().kinds(),
            new_universe,
        );

        (trait_ref.instantiate(&instantiations, self.engine()), instantiations)
    }
}
