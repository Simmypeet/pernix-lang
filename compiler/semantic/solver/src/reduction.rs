use pernixc_type::{
    predicate::{Equality, Predicate},
    r#type::{Type, rewrite::rewrite_application},
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Provisional, Solve, Solver},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reduction(Interned<Type>);

impl Solver<'_> {
    /// Reduces the given type as much as possible using the reduction rules,
    /// and returns the reduced type along with any constraints that need to be
    /// satisfied for the reduction to hold.
    pub async fn reduce_type(
        &mut self,
        ty: Interned<Type>,
    ) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
        self.solve(&Reduction(ty)).await
    }

    // due to the weird cyclic query error in rustc, we have to define the
    // implementation with `+ Send` bounds
    #[allow(clippy::manual_async_fn)]
    fn reduce_type_impl(
        &mut self,
        ty: &Interned<Type>,
    ) -> impl Future<
        Output = Result<Option<(Interned<Type>, Constraints)>, OverflowError>,
    > + Send {
        async move {
            if let Some(result) =
                recurse_from_result(rewrite_inner(ty, self).await?, self)
                    .await?
            {
                return Ok(Some(result));
            }

            if let Some(result) =
                recurse_from_result(rewrite_step(ty, self).await?, self).await?
            {
                return Ok(Some(result));
            }

            recurse_from_result(rewrite_from_eq(ty, self).await?, self).await
        }
    }
}

impl Solve for Reduction {
    type Result = Option<(Interned<Type>, Constraints)>;

    async fn solve(
        &self,
        solver: &mut Solver<'_>,
    ) -> Result<Self::Result, OverflowError> {
        solver.reduce_type_impl(&self.0).await
    }

    fn provisional_result(&self) -> Provisional<Self::Result> {
        // A recursive reduction query means a rule can keep reintroducing the
        // same reducible type, for example `T = (bool, T)`. Returning `None`
        // would hide that non-termination, while returning the original type
        // would let the recursive reducer keep expanding around it. Treat the
        // cycle as overflow instead.
        Provisional::Bail
    }
}

async fn rewrite_step(
    ty: &Interned<Type>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    let Type::Application(ap) = &**ty else {
        return Ok(None);
    };

    Ok(solver.reduce_step(ap).await.map(|x| (x, Constraints::default())))
}

async fn rewrite_inner(
    ty: &Interned<Type>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    let Type::Application(ap) = &**ty else {
        return Ok(None);
    };

    let mut constrs = Constraints::default();

    Ok(rewrite_application(ap, async |arg| {
        let reduced = Box::pin(solver.solve(&Reduction(arg.clone()))).await?;

        if let Some((reduced_ty, new_constrs)) = reduced {
            constrs.extend(new_constrs);
            Ok(Some(reduced_ty))
        } else {
            Ok(None)
        }
    })
    .await?
    .map(|x| (solver.intern(Type::Application(x)), constrs)))
}

async fn recurse_from_result(
    rule_result: Option<(Interned<Type>, Constraints)>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    if let Some((reduced_ty, constrs)) = rule_result {
        let further_reduced =
            Box::pin(solver.solve(&Reduction(reduced_ty.clone()))).await?;

        if let Some((further_reduced_ty, further_constrs)) = further_reduced {
            let mut all_constrs = constrs;
            all_constrs.extend(further_constrs);
            Ok(Some((further_reduced_ty, all_constrs)))
        } else {
            Ok(Some((reduced_ty, constrs)))
        }
    } else {
        Ok(None)
    }
}

async fn try_match_eq(
    ty: &Interned<Type>,
    eq: &Equality,
    solver: &mut Solver<'_>,
) -> Option<(Interned<Type>, Constraints)> {
    let fresh_instantiation =
        solver.create_inference_instantiations(eq.binder().kinds());

    let instantiated_lhs = solver.instantiate(eq.left(), &fresh_instantiation);

    let (subst, constrs) = solver.match_types(&instantiated_lhs, ty).await?;

    let instantiated_rhs = solver.instantiate(eq.right(), &fresh_instantiation);

    Some((solver.apply_or_self(&subst, instantiated_rhs), constrs))
}

async fn rewrite_from_eq(
    ty: &Interned<Type>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type>, Constraints)>, OverflowError> {
    for pred in solver.premise_predicates() {
        let Predicate::Equality(eq) = pred else {
            continue;
        };

        if let Some((reduced_ty, constraints)) = solver
            .new_universe(async |solver| try_match_eq(ty, eq, solver).await)
            .await
        {
            return Ok(Some((reduced_ty, constraints)));
        }
    }

    Ok(None)
}

#[cfg(test)]
mod test;
