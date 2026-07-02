use pernixc_type::{
    substitution::{Substitutable, Substitution},
    r#type::{Type2, constructor::DestructureOptions},
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{DoOccurCheck, Solver},
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

        match (&**head, &**subject) {
            (Type2::InferenceVariable(infer_var), x)
                if !x.is_bound_variable() =>
            {
                if !self
                    .can_bind_inference_variable_to_type(
                        *infer_var,
                        subject,
                        DoOccurCheck::No,
                    )
                    .await
                {
                    return None;
                }

                Some((
                    Substitution::singleton(*infer_var, subject.clone()),
                    Constraints::default(),
                ))
            }

            (Type2::Application(left_a), Type2::Application(right_a)) => {
                let iter = left_a.destructure(
                    right_a,
                    DestructureOptions::require_equal_binders(),
                    self.engine(),
                )?;

                Box::pin(self.match_types(iter)).await
            }

            _ => {
                // if both are lifetime kinds and none of them are bound
                // variables, return invariant constraints
                if !head.is_bound_variable()
                    && !subject.is_bound_variable()
                    && self.kind_of(head).await.is_lifetime()
                    && self.kind_of(subject).await.is_lifetime()
                {
                    Some((
                        Substitution::default(),
                        Constraints::lifetimes_eq(
                            head.clone(),
                            subject.clone(),
                        ),
                    ))
                } else {
                    None
                }
            }
        }
    }

    /// Iterates through each pair of types and call [`Self::match_type`]
    /// on them.
    pub async fn match_types(
        &mut self,
        pairs: impl IntoIterator<Item = (Interned<Type2>, Interned<Type2>)>,
    ) -> Option<(Substitution, Constraints)> {
        let mut subst = Substitution::new();
        let mut constraints = Constraints::default();

        for (head, subject) in pairs {
            let head = head.apply_or_clone(&subst, self.engine());
            let subject = subject.apply_or_clone(&subst, self.engine());

            let (new_subst, new_constraints) =
                self.match_type(&head, &subject).await?;

            subst.merge(&new_subst);
            constraints.extend(new_constraints);
        }

        Some((subst, constraints))
    }
}
