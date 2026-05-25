use pernixc_type::{substitution::Substitution, r#type::Type};
use qbice::storage::intern::Interned;

use crate::{constraints::Constraints, solver::Solver};

impl Solver<'_> {
    /// Computes a substitution `S` such that `S(head) == subject)`, if one
    /// exists, and the associated lifetime constraints.
    ///
    /// Lifetime constraints are generated if two lifetimes mismatch, for
    /// example, lifetime `a` and `b` such that `a != b` would generate the
    /// constraint `a: 'b` and `b: 'a`.
    pub async fn match_types(
        &mut self,
        head: &Interned<Type>,
        subject: &Interned<Type>,
    ) -> Option<(Substitution, Constraints)> {
        // quickly check for syntactic equality
        if head == subject {
            return Some((Substitution::new(), Constraints::default()));
        }

        match (&**head, &**subject) {
            (Type::InferenceVariable(infer_var), x)
                if !x.is_bound_variable() =>
            {
                let var_kind = self.kind_of(head).await;
                let subject_kind = self.kind_of(subject).await;

                if var_kind != subject_kind {
                    return None;
                }

                let var_uni = self.max_universe_index(head);
                let subject_uni = self.max_universe_index(subject);

                if var_uni < subject_uni {
                    return None;
                }

                Some((
                    Substitution::singleton(*infer_var, subject.clone()),
                    Constraints::default(),
                ))
            }

            (Type::Application(left_a), Type::Application(right_a)) => {
                let iter = self.destructure(left_a, right_a)?;

                let mut subst = Substitution::new();
                let mut constraint = Constraints::default();

                Box::pin(async move {
                    for (head_ty, subject_ty) in iter {
                        let head_ty = self.apply(&subst, &head_ty);
                        let subject_ty = self.apply(&subst, &subject_ty);

                        let (new_subst, new_constraints) =
                            self.match_types(&head_ty, &subject_ty).await?;

                        subst.merge(&new_subst);
                        constraint.extend(new_constraints);
                    }

                    Some((subst, constraint))
                })
                .await
            }

            _ => {
                // if both are lifetime kinds and none of them are bound
                // variables, return invariant constraints
                if self.kind_of(head).await.is_lifetime()
                    && self.kind_of(subject).await.is_lifetime()
                    && !head.is_bound_variable()
                    && !subject.is_bound_variable()
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
}
