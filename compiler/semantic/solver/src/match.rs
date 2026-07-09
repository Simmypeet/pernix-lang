use pernixc_type::{substitution::Substitution, r#type::Type2};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints, solver::Solver, type_relation::TypeRelation,
};

#[cfg(test)]
mod test;

impl Solver<'_> {
    /// Computes a substitution `S` such that `S(head) == subject)`, if one
    /// exists, and the associated lifetime constraints.
    ///
    /// Lifetime constraints are generated if two lifetimes mismatch, for
    /// example, lifetime `a` and `b` such that `a != b` would generate the
    /// constraint `a: 'b` and `b: 'a`.
    ///
    /// Note that using match operation, it doesn't attempt to reduce the types
    /// at all. This is because `reduce` operation defined in the solver
    /// requires the call to this function. Therefore, if we call `reduce` here,
    /// it will cause a circular call and end up in overflowing the stack.
    pub async fn match_type(
        &mut self,
        head: &Interned<Type2>,
        subject: &Interned<Type2>,
    ) -> Option<(Substitution, Constraints)> {
        // quickly check for syntactic equality
        if head == subject {
            return Some((Substitution::new(), Constraints::default()));
        }

        self.match_types([(head.clone(), subject.clone())]).await
    }

    /// Matches each pair of types as invariant type relations.
    pub async fn match_types(
        &mut self,
        pairs: impl IntoIterator<Item = (Interned<Type2>, Interned<Type2>)>,
    ) -> Option<(Substitution, Constraints)> {
        let type_relations = pairs
            .into_iter()
            .map(|(head, subject)| TypeRelation::new_matching(head, subject))
            .collect();

        let (substitution, residual_relations, constraints) =
            self.resolve_type_relations(type_relations).await.ok()?;

        if residual_relations.is_empty() {
            Some((substitution, constraints))
        } else {
            None
        }
    }
}
