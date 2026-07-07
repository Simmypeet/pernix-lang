use pernixc_type::{
    substitution::{Substitutable, Substitution, Variable},
    r#type::{Type2, bound::Instantiate, constructor::Application},
    variance::Variance2,
};
use qbice::storage::intern::Interned;

use super::ResolveStrategy;
use crate::{
    constraints::Constraints,
    hrtb::HrtbVariables,
    solver::{OverflowError, Solver},
    type_relation::Step,
};

#[derive(Debug, Clone, Copy)]
enum HrtbInstantiation {
    // The left side is existential and the right side is universal:
    // `for<a> T[a] <: for<b> U[b]` becomes `T[?a] <: U[!b]`.
    LesserInferenceGreaterSkolem,

    // Used for contravariant positions and the second invariant pass, where
    // the left/right roles are observed through the flipped variance.
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
                    run.constraints,
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

    /// Runs higher-ranked relation proof for Invariant ambient variance. This
    /// requires proving both directions of the relationship by running
    /// `handle_hrtb_application_run` twice with opposite instantiation
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

        let constraints = first_run
            .constraints
            // apply the substitution from the second run to the constraints
            // from the first run as well
            .apply_or_self(&second_run.substitution, self.engine())
            .union_into(second_run.constraints);

        let mut second_substitution = second_run.substitution;

        second_substitution.compose(first_substitution, self.engine());

        let variables = first_run.variables.union_into(second_run.variables);

        Ok(self.clean_hrtb_step(second_substitution, constraints, &variables))
    }

    /// Runs the higher-ranked subtyping proof for the given application and
    /// arguments. Depending on the `instantiation` strategy, lesser/greater
    /// binders are instantiated with either inference variables or skolem
    /// variables. Then the set of relations is solved with the instantiated
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
        mut substitution: Substitution,
        constraints: Constraints,
        variables: &HrtbVariables,
    ) -> Option<Step> {
        // HRTB constraints are generated from instantiated lifetime variables.
        // They are cleaned against the proof-local variables before any of
        // those variables can escape to callers.
        let constraints =
            self.check_and_clean_hrtb_constraints(constraints, variables)?;

        // Invariant type relations can bind internal HRTB inference lifetimes
        // while proving the local relation. Those variables are proof-local,
        // so their substitutions must not escape the HRTB run.
        substitution.retain(|variable, _| match variable {
            Variable::Inference(inference_variable) => {
                !variables.is_internal_inference_variable(inference_variable)
            }
            Variable::Generic(_) => true,
        });

        Some((substitution, Vec::new(), constraints))
    }
}
