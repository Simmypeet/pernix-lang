use pernixc_type::{
    substitution::{Substitutable, Substitution},
    r#type::{Type2, bound::Instantiate, constructor::Application},
    variance::Variance2,
};
use qbice::storage::intern::Interned;

use super::ResolveStrategy;
use crate::{
    constraints::Constraints,
    hrtb::HrtbVariables,
    solver::{OverflowError, Solver},
    subtype::Step,
};

#[derive(Debug, Clone, Copy)]
enum HrtbInstantiation {
    // The subtype side is existential and the supertype side is universal:
    // `for<a> T[a] <: for<b> U[b]` becomes `T[?a] <: U[!b]`.
    LesserInferenceGreaterSkolem,

    // Used for contravariant positions and the second invariant pass, where
    // the subtype/supertype roles are observed through the flipped variance.
    LesserSkolemGreaterInference,
}

struct HrtbRun {
    substitution: Substitution,
    constraints: Constraints,
    variables: HrtbVariables,
}

impl Solver<'_> {
    pub(super) async fn handle_hrtb_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        variance: Variance2,
    ) -> Result<Option<Step>, OverflowError> {
        match variance {
            // for the contravariant and covariant cases, a single run with
            // appropriate instantiation is sufficient.
            Variance2::Covariant | Variance2::Contravariant => {
                let instantiation = match variance {
                    Variance2::Covariant => {
                        HrtbInstantiation::LesserInferenceGreaterSkolem
                    }
                    Variance2::Contravariant => {
                        HrtbInstantiation::LesserSkolemGreaterInference
                    }
                    Variance2::Invariant | Variance2::Bivariant => {
                        unreachable!(
                            "invariant and bivariant are handled separately"
                        )
                    }
                };

                let Some(run) = self
                    .handle_hrtb_application_run(
                        lesser_ap,
                        greater_ap,
                        arguments,
                        variance,
                        instantiation,
                    )
                    .await?
                else {
                    return Ok(None);
                };

                Ok(self.clean_hrtb_step(
                    run.substitution,
                    &run.constraints,
                    &run.variables,
                ))
            }

            Variance2::Invariant => {
                Box::pin(self.handle_invariant_hrtb_application(
                    lesser_ap, greater_ap, arguments,
                ))
                .await
            }

            Variance2::Bivariant => Ok(Some((
                Substitution::new(),
                Vec::new(),
                Constraints::default(),
            ))),
        }
    }

    /// Runs higher-ranked subtyping proof for Invariant ambient variance. This
    /// requires proving both directions of the subtyping relationship by
    /// running `handle_hrtb_application_run` twice with opposite instantiation
    /// strategies, then combining the results.
    async fn handle_invariant_hrtb_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
    ) -> Result<Option<Step>, OverflowError> {
        // Invariant HRTB must prove both directions, but each proof is still an
        // invariant argument solve. Only binder polarity is swapped between the
        // two runs.
        let Some(first_run) = Box::pin(self.handle_hrtb_application_run(
            lesser_ap,
            greater_ap,
            arguments,
            Variance2::Invariant,
            HrtbInstantiation::LesserInferenceGreaterSkolem,
        ))
        .await?
        else {
            return Ok(None);
        };

        let engine = self.engine();
        let first_substitution = first_run.substitution;

        let substituted_arguments = arguments
            .iter()
            .map(|(lesser, greater)| {
                (
                    lesser.apply_or_clone(&first_substitution, engine),
                    greater.apply_or_clone(&first_substitution, engine),
                )
            })
            .collect::<Vec<_>>();

        let Some(second_run) = Box::pin(self.handle_hrtb_application_run(
            lesser_ap,
            greater_ap,
            &substituted_arguments,
            Variance2::Invariant,
            HrtbInstantiation::LesserSkolemGreaterInference,
        ))
        .await?
        else {
            return Ok(None);
        };

        // Residual subtypes are guaranteed to be empty
        let mut second_substitution = second_run.substitution;

        second_substitution.compose(first_substitution, self.engine());

        let variables = first_run.variables.union_into(second_run.variables);
        let constraints =
            first_run.constraints.union_into(second_run.constraints);

        Ok(self.clean_hrtb_step(second_substitution, &constraints, &variables))
    }

    /// Runs the higher-ranked subtyping proof for the given application and
    /// arguments. Depending on the `instantiation` strategy, lesser/greater
    /// binders are instantiated with either inference variables or skolem
    /// variables. Then the set of subtypes is solved with the instantiated
    /// arguments with the result as [`HrtbRun`] if successful.
    async fn handle_hrtb_application_run(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        variance: Variance2,
        instantiation: HrtbInstantiation,
    ) -> Result<Option<HrtbRun>, OverflowError> {
        self.new_universe(async |solver| {
            let (lesser_inst, greater_inst) = match instantiation {
                HrtbInstantiation::LesserInferenceGreaterSkolem => (
                    lesser_ap.binder().map(|x| {
                        solver.create_inference_instantiations(x.kinds())
                    }),
                    greater_ap.binder().map(|x| {
                        solver.create_skolem_instantiations(x.kinds())
                    }),
                ),
                HrtbInstantiation::LesserSkolemGreaterInference => (
                    lesser_ap.binder().map(|x| {
                        solver.create_skolem_instantiations(x.kinds())
                    }),
                    greater_ap.binder().map(|x| {
                        solver.create_inference_instantiations(x.kinds())
                    }),
                ),
            };
            let variables = solver.hrtb_variables_from_instantiations(
                lesser_inst
                    .iter()
                    .flatten()
                    .chain(greater_inst.iter().flatten()),
            );

            let engine = solver.engine();

            let step = Box::pin(solver.handle_application_arguments(
                lesser_ap,
                arguments.iter().map(|(l, g)| {
                    (
                        lesser_inst.as_ref().map_or_else(
                            || l.clone(),
                            |insts| l.instantiate(insts, engine),
                        ),
                        greater_inst.as_ref().map_or_else(
                            || g.clone(),
                            |insts| g.instantiate(insts, engine),
                        ),
                    )
                }),
                variance,
                ResolveStrategy::ResolveImmediately,
            ))
            .await?;

            let Some((substitution, residual, constraints)) = step else {
                return Ok(None);
            };

            if !residual.is_empty() {
                return Ok(None);
            }

            Ok(Some(HrtbRun { substitution, constraints, variables }))
        })
        .await
    }

    /// Run leak check and clean up the resulting constraints from the HRTB
    /// proof run
    fn clean_hrtb_step(
        &mut self,
        substitution: Substitution,
        constraints: &Constraints,
        variables: &HrtbVariables,
    ) -> Option<Step> {
        // NOTE: You might think that we should apply the substitutions on the
        // constraints before the leack check first, but we actually don't need
        // it because:
        //
        // 1a. Currently, in **Subtyping Relation**, lifetimes can never be
        //    mapped by substitution. However, in other relations like **Match**
        //    still does, but it's not relevant here.
        // 2a. The `Constraints` generated here can only be in the form of
        //    `lifetime: lifetime` and since (1a) holds, the substitution won't
        //    change the shape of constraints.
        //
        // Proof? Trust me bro :-)
        let constraints =
            self.check_and_clean_hrtb_constraints(constraints, variables)?;

        // NOTE: here we directly return the original substitution without
        // eliminating the internal variables, because:
        //
        // 1b. We currently assume that all higher-ranked variables are
        //    lifetimes, even though we have infrastructure to support other
        //    kinds. If we happens to change this assumption in the future, we
        //    might need to revisit this decision.
        // 2b. The internal higher-ranked variables will never be in the
        //    codomain of the substitution, because:
        //    2.1b Since (1b) and (1a) holds, it means that all domains of the
        //      substitution will always have root universe.
        //    2.2b Therefore, internal higher-ranked variables that are created
        //      in a higher universe will never be substituted due to universe
        //      checks.
        // 3b. Therefore, the subsitution will never mention any internal
        //    higher-ranked variables, and thus we can safely return it without
        //    eliminating them.

        Some((substitution, Vec::new(), constraints))
    }
}
