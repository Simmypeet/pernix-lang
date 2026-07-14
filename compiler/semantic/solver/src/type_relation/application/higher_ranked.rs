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
    type_relation::{RelationFlags, Step},
};

#[derive(Debug, Clone, Copy)]
enum HigherRankedInstantiation {
    // The left side is existential and the right side is universal:
    // `for<a> T[a] <: for<b> U[b]` becomes `T[?a] <: U[!b]`.
    LesserInferenceGreaterSkolem,

    // Used for contravariant positions and the second invariant pass, where
    // the left/right roles are observed through the flipped variance.
    LesserSkolemGreaterInference,
}

struct HigherRankedRun {
    substitution: Substitution,
    constraints: Constraints,
}

impl Solver<'_> {
    pub(super) async fn handle_higher_ranked_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        flags: RelationFlags,
    ) -> Result<Option<Step>, OverflowError> {
        let closing_universe =
            lesser_ap.max_universe().max(greater_ap.max_universe()).next();

        match flags.variance() {
            // for the contravariant and covariant cases, a single run with
            // appropriate instantiation is sufficient.
            Variance2::Covariant | Variance2::Contravariant => {
                let instantiation = match flags.variance() {
                    Variance2::Covariant => {
                        HigherRankedInstantiation::LesserInferenceGreaterSkolem
                    }
                    Variance2::Contravariant => {
                        HigherRankedInstantiation::LesserSkolemGreaterInference
                    }
                    Variance2::Invariant | Variance2::Bivariant => {
                        unreachable!(
                            "invariant and bivariant are handled separately"
                        )
                    }
                };

                let Some(run) = self
                    .handle_higher_ranked_application_run(
                        lesser_ap,
                        greater_ap,
                        arguments,
                        flags,
                        closing_universe,
                        instantiation,
                    )
                    .await?
                else {
                    return Ok(None);
                };

                Ok(self.clean_higher_ranked_step(
                    run.substitution,
                    run.constraints,
                    closing_universe,
                ))
            }

            Variance2::Invariant => {
                Box::pin(self.handle_invariant_higher_ranked_application(
                    lesser_ap,
                    greater_ap,
                    arguments,
                    closing_universe,
                    flags,
                ))
                .await
            }

            Variance2::Bivariant => Ok(Some((
                Substitution::new(),
                self.engine().intern_unsized([]),
                Constraints::default(),
            ))),
        }
    }

    /// Runs higher-ranked relation proof for Invariant ambient variance. This
    /// requires proving both directions of the relationship by running
    /// `handle_higher_ranked_application_run` twice with opposite instantiation
    /// strategies, then combining the results.
    async fn handle_invariant_higher_ranked_application(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        closing_universe: UniverseIndex,
        flags: RelationFlags,
    ) -> Result<Option<Step>, OverflowError> {
        let invariant_flags = flags.with_variance(Variance2::Invariant);

        // Invariant higher-ranked relations must prove both directions, but
        // each proof is still an invariant argument solve. Only binder polarity
        // is swapped between the two runs.
        let Some(first_run) =
            Box::pin(self.handle_higher_ranked_application_run(
                lesser_ap,
                greater_ap,
                arguments,
                invariant_flags,
                closing_universe,
                HigherRankedInstantiation::LesserInferenceGreaterSkolem,
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

        let Some(second_run) =
            Box::pin(self.handle_higher_ranked_application_run(
                lesser_ap,
                greater_ap,
                &substituted_arguments,
                invariant_flags,
                closing_universe,
                HigherRankedInstantiation::LesserSkolemGreaterInference,
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

        Ok(self.clean_higher_ranked_step(
            second_substitution,
            constraints,
            closing_universe,
        ))
    }

    /// Runs the higher-ranked subtyping proof for the given application and
    /// arguments. Depending on the `instantiation` strategy, lesser/greater
    /// binders are instantiated with either inference variables or skolem
    /// variables. Then the set of relations is solved with the instantiated
    /// arguments with the result as [`HigherRankedRun`] if successful.
    async fn handle_higher_ranked_application_run(
        &mut self,
        lesser_ap: &Application,
        greater_ap: &Application,
        arguments: &[(Interned<Type2>, Interned<Type2>)],
        flags: RelationFlags,
        closing_universe: UniverseIndex,
        instantiation: HigherRankedInstantiation,
    ) -> Result<Option<HigherRankedRun>, OverflowError> {
        let (lesser_inst, greater_inst) = match instantiation {
            HigherRankedInstantiation::LesserInferenceGreaterSkolem => (
                lesser_ap.binder().map(|x| {
                    self.create_inference_instantiations_in_universe(
                        x.kinds(),
                        closing_universe,
                    )
                }),
                greater_ap.binder().map(|x| {
                    self.create_skolem_instantiations_in_universe(
                        x.kinds(),
                        closing_universe,
                    )
                }),
            ),
            HigherRankedInstantiation::LesserSkolemGreaterInference => (
                lesser_ap.binder().map(|x| {
                    self.create_skolem_instantiations_in_universe(
                        x.kinds(),
                        closing_universe,
                    )
                }),
                greater_ap.binder().map(|x| {
                    self.create_inference_instantiations_in_universe(
                        x.kinds(),
                        closing_universe,
                    )
                }),
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
            flags,
            ResolveStrategy::ResolveImmediately,
        ))
        .await?;

        let Some((substitution, residual, constraints)) = step else {
            return Ok(None);
        };

        if !residual.is_empty() {
            return Ok(None);
        }

        Ok(Some(HigherRankedRun { substitution, constraints }))
    }

    /// Run leak check and clean up the resulting constraints from the
    /// higher-ranked proof run.
    fn clean_higher_ranked_step(
        &mut self,
        mut substitution: Substitution,
        constraints: Constraints,
        closing_universe: UniverseIndex,
    ) -> Option<Step> {
        // Higher-ranked constraints are generated from instantiated lifetime
        // variables. They are cleaned against the proof-local variables before
        // any of those variables can escape to callers.
        let constraints = self.check_and_clean_higher_ranked_constraints(
            constraints,
            closing_universe,
        )?;

        // Inference variables created in the closing universe are proof-local,
        // including both binder instantiations and variables introduced while
        // solving the local relation. Their substitutions must not escape the
        // higher-ranked run.
        substitution.retain(|variable, _| match variable {
            Variable::Inference(inference_variable) => {
                inference_variable.universe_index() != closing_universe
            }
            Variable::Generic(_) => true,
        });

        Some((substitution, self.engine().intern_unsized([]), constraints))
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

    // input: higher-ranked cleanup with ?T@U1 := bool and ?U@U0 := bool
    // premise: U1 is the closing universe
    // output: ?U@U0 := bool
    #[tokio::test]
    async fn higher_ranked_cleanup_removes_substitutions_keyed_by_closing_universe()
     {
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
                .clean_higher_ranked_step(
                    substitution,
                    Constraints::default(),
                    closing_universe,
                )
                .unwrap();

        assert_eq!(
            substitution,
            Substitution::singleton(external_variable, bool_type)
        );
        assert!(residual_relations.is_empty());
        assert_eq!(constraints, Constraints::default());
    }
}
