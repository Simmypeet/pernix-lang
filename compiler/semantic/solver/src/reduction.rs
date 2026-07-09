use pernixc_type::{
    predicate::{Equality, Predicate2},
    substitution::Substitutable,
    r#type::{Type2, bound::Instantiate, rewrite::rewrite_application},
};
use qbice::storage::intern::Interned;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Provisional, Solve, Solver},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Reduction(Interned<Type2>);

impl Solver<'_> {
    /// Reduces the given type as much as possible using the reduction rules,
    /// and returns the reduced type along with any constraints that need to be
    /// satisfied for the reduction to hold.
    pub async fn reduce_type(
        &mut self,
        ty: Interned<Type2>,
    ) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
        self.solve(&Reduction(ty)).await
    }

    // due to the weird cyclic query error in rustc, we have to define the
    // implementation with `+ Send` bounds
    #[allow(clippy::manual_async_fn)]
    fn reduce_type_impl(
        &mut self,
        ty: &Interned<Type2>,
    ) -> impl Future<
        Output = Result<Option<(Interned<Type2>, Constraints)>, OverflowError>,
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
    type Result = Option<(Interned<Type2>, Constraints)>;

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
    ty: &Interned<Type2>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
    let Type2::Application(ap) = &**ty else {
        return Ok(None);
    };

    Ok(ap.reduce(solver.engine()).await.map(|x| (x, Constraints::default())))
}

async fn rewrite_inner(
    ty: &Interned<Type2>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
    let Type2::Application(ap) = &**ty else {
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
    .map(|x| (solver.intern(Type2::Application(x)), constrs)))
}

async fn recurse_from_result(
    rule_result: Option<(Interned<Type2>, Constraints)>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
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
    ty: &Interned<Type2>,
    eq: &Equality,
    solver: &mut Solver<'_>,
) -> Option<(Interned<Type2>, Constraints)> {
    let max_universe = ty.universe_index();
    let fresh_instantiation = solver
        .create_inference_instantiations_in_universe(
            eq.binder().kinds(),
            max_universe,
        );

    let instantiated_lhs =
        eq.left().instantiate(&fresh_instantiation, solver.engine());

    let (subst, constrs) = solver.match_type(&instantiated_lhs, ty).await?;

    let instantiated_rhs =
        eq.right().instantiate(&fresh_instantiation, solver.engine());

    Some((instantiated_rhs.apply_or_self(&subst, solver.engine()), constrs))
}

async fn rewrite_from_eq(
    ty: &Interned<Type2>,
    solver: &mut Solver<'_>,
) -> Result<Option<(Interned<Type2>, Constraints)>, OverflowError> {
    for pred in solver.premise_predicates() {
        let Predicate2::Equality(eq) = pred else {
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
