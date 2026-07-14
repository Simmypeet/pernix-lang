use pernixc_type::{substitution::Substitution, symbol::Symbol2};

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
    type_relation::TypeRelation,
};

impl Solver<'_> {
    /// Unifies two binder-free trait references.
    ///
    /// Returns the resulting substitution and constraints, or `None` when the
    /// references cannot be unified.
    pub async fn unify_trait_ref(
        &mut self,
        left: Symbol2,
        right: Symbol2,
    ) -> Result<Option<(Substitution, Constraints)>, OverflowError> {
        if left.symbol_id() != right.symbol_id()
            || left.generic_arguments().len() != right.generic_arguments().len()
        {
            return Ok(None);
        }

        let type_relations = left
            .generic_arguments()
            .iter()
            .zip(right.generic_arguments().iter())
            .map(|(left, right)| {
                TypeRelation::invariant(left.clone(), right.clone())
            })
            .collect();

        let (substitution, residual_relations, constraints) =
            self.resolve_type_relations(type_relations).await?;

        if residual_relations.is_empty() {
            Ok(Some((substitution, constraints)))
        } else {
            Ok(None)
        }
    }
}
