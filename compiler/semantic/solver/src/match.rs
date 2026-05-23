use pernixc_type::{
    substitution::Substitution,
    r#type::{Type, kind::TyKind},
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{Agree, OverflowError, Solve, Solver},
};

/// One way unification, finding a substitution "S" such that S(head) = subject.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Match {
    head: Interned<Type>,
    subject: Interned<Type>,
}

impl Agree for (Substitution, Constraints) {
    fn agree(&self, other: &Self) -> bool {
        self.0 == other.0 && self.1 == other.1
    }
}

impl Solve for Match {
    type Result = Option<(Substitution, Constraints)>;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        // syntactically equal types
        if self.head == self.subject {
            return Ok(Some((Substitution::default(), Constraints::default())));
        }

        match (&*self.head, &*self.subject) {
            (Type::InferenceVariable(var), ty) => {
                if !solver.map_check(var, ty).await {
                    return Ok(None);
                }

                Ok(Some((
                    Substitution::singleton(*var, self.subject.clone()),
                    Constraints::default(),
                )))
            }

            (Type::Application(left), Type::Application(right)) => {
                // destructure the applications and unify their constructors and
                // arguments pairwise
                solver
                    .new_universe(async |solver| {
                        let Some(destructure) = solver.destructure(left, right)
                        else {
                            return Ok(None);
                        };

                        todo!()
                    })
                    .await
            }

            (left, right) => {
                // if left and right are both lifetimes, we can unify them by
                // adding an outlives constraint
                let left_kind = solver.kind_of(left).await;
                let right_kind = solver.kind_of(right).await;

                if left_kind == right_kind && left_kind == TyKind::Lifetime {
                    return Ok(Some((
                        Substitution::default(),
                        Constraints::lifetimes_eq(
                            self.head.clone(),
                            self.subject.clone(),
                        ),
                    )));
                }

                Ok(None)
            }
        }
    }

    fn provisional_result(&self) -> Self::Result { todo!() }
}
