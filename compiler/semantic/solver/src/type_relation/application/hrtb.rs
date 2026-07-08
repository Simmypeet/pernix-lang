use pernixc_type::{
    substitution::{Substitutable, Substitution, Variable},
    r#type::{
        Type2, bound::Instantiate, constructor::Application,
        universe::UniverseIndex,
    },
    variance::Variance2,
};
use qbice::storage::intern::Interned;

use super::ResolveStrategy;
use crate::{
    constraints::Constraints,
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
}

impl Solver<'_> {
    pub(super) async fn handle_hrtb_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        variance: Variance2,
    ) -> Result<Option<Step>, OverflowError> {
        self.new_universe(async |solver| {
            let closing_universe = solver.current_universe();

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
                                "invariant and bivariant are handled \
                                 separately"
                            )
                        }
                    };

                    let Some(run) = solver
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

                    Ok(solver.clean_hrtb_step(
                        run.substitution,
                        run.constraints,
                        closing_universe,
                    ))
                }

                Variance2::Invariant => {
                    Box::pin(solver.handle_invariant_hrtb_application(
                        lesser_ap,
                        greater_ap,
                        arguments,
                        closing_universe,
                    ))
                    .await
                }

                Variance2::Bivariant => Ok(Some((
                    Substitution::new(),
                    Vec::new(),
                    Constraints::default(),
                ))),
            }
        })
        .await
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
        closing_universe: UniverseIndex,
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

        let first_substitution = first_run.substitution;
        let engine = self.engine();

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

        Ok(self.clean_hrtb_step(
            second_substitution,
            constraints,
            closing_universe,
        ))
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
        let (lesser_inst, greater_inst) = match instantiation {
            HrtbInstantiation::LesserInferenceGreaterSkolem => (
                lesser_ap
                    .binder()
                    .map(|x| self.create_inference_instantiations(x.kinds())),
                greater_ap
                    .binder()
                    .map(|x| self.create_skolem_instantiations(x.kinds())),
            ),
            HrtbInstantiation::LesserSkolemGreaterInference => (
                lesser_ap
                    .binder()
                    .map(|x| self.create_skolem_instantiations(x.kinds())),
                greater_ap
                    .binder()
                    .map(|x| self.create_inference_instantiations(x.kinds())),
            ),
        };
        let engine = self.engine();

        let step = Box::pin(self.handle_application_arguments(
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

        Ok(Some(HrtbRun { substitution, constraints }))
    }

    /// Run leak check and clean up the resulting constraints from the HRTB
    /// proof run
    fn clean_hrtb_step(
        &mut self,
        mut substitution: Substitution,
        constraints: Constraints,
        closing_universe: UniverseIndex,
    ) -> Option<Step> {
        // HRTB constraints are generated from instantiated lifetime variables.
        // They are cleaned against the proof-local variables before any of
        // those variables can escape to callers.
        let constraints = self
            .check_and_clean_hrtb_constraints(constraints, closing_universe)?;

        // Inference variables created in the closing universe are proof-local,
        // including both binder instantiations and variables introduced while
        // solving the local relation. Their substitutions must not escape the
        // HRTB run.
        substitution.retain(|variable, _| match variable {
            Variable::Inference(inference_variable) => {
                inference_variable.universe_index() != closing_universe
            }
            Variable::Generic(_) => true,
        });

        Some((substitution, Vec::new(), constraints))
    }
}

#[cfg(test)]
mod test {
    use pernixc_qbice::create_minimal_engine as create_engine;
    use pernixc_type::r#type::{
        constructor::Primitive, inference::InferenceVariable, kind::TyKind,
        universe::UniverseIndex,
    };

    use super::*;
    use crate::premise::Premise;

    // input: HRTB cleanup with ?T@U1 := bool and ?U@U0 := bool
    // premise: U1 is the closing universe
    // output: ?U@U0 := bool
    #[tokio::test]
    async fn hrtb_cleanup_removes_substitutions_keyed_by_closing_universe() {
        let engine = create_engine().await;
        let closing_universe = UniverseIndex::root().next();
        let closing_variable =
            InferenceVariable::new(0, TyKind::Type, closing_universe);
        let external_variable =
            InferenceVariable::new(1, TyKind::Type, UniverseIndex::root());
        let bool_type = Type2::new_primitive(Primitive::Bool, &engine);

        let mut substitution =
            Substitution::singleton(closing_variable, bool_type.clone());
        substitution.merge(&Substitution::singleton(
            external_variable,
            bool_type.clone(),
        ));

        let (substitution, residual_relations, constraints) =
            Solver::new(&Premise::default(), &engine)
                .clean_hrtb_step(
                    substitution,
                    Constraints::default(),
                    closing_universe,
                )
                .unwrap();

        assert_eq!(
            substitution,
            Substitution::singleton(external_variable, bool_type)
        );
        assert_eq!(residual_relations, Vec::new());
        assert_eq!(constraints, Constraints::default());
    }
}
