use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::FunctionCall;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    do_effect::get_do_effects, parameter::get_parameters, variance::Variance,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::{get_parent, get_parent_global},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    predicate::{PositiveTrait, Predicate},
};
use pernixc_type_system::{
    lifetime_constraint::LifetimeConstraint, normalizer::Normalizer,
    UnrecoverableError,
};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_effect_operation(
        &self,
        function_call: &FunctionCall,
        span: &RelativeSpan,
        lifetime_constraints: &mut BTreeSet<LifetimeConstraint>,
    ) -> Result<(), UnrecoverableError> {
        let parent_effect_id = self
            .tracked_engine()
            .get_parent_global(function_call.callable_id)
            .await
            .unwrap();

        let parent_generic_parameters = self
            .tracked_engine()
            .get_generic_parameters(parent_effect_id)
            .await?;

        let effect_operation = Symbol {
            id: parent_effect_id,
            generic_arguments: function_call
                .instantiation
                .create_generic_arguments(
                    parent_effect_id,
                    &parent_generic_parameters,
                ),
        };

        let do_effects =
            self.tracked_engine().get_do_effects(self.current_site()).await?;

        let mut found = false;
        for available_cap in &do_effects.effects {
            // must be the same effect
            if available_cap.id != effect_operation.id {
                continue;
            }

            let Some(subtypable) = self
                .environment()
                .subtypes_generic_arguments(
                    &effect_operation.generic_arguments,
                    &available_cap.generic_arguments,
                )
                .await
                .map_err(|e| {
                    e.report_as_type_calculating_overflow(
                        *span,
                        &self.handler(),
                    )
                })?
            else {
                continue;
            };

            assert!(subtypable.result.forall_lifetime_errors.is_empty());

            lifetime_constraints.extend(subtypable.constraints.iter().cloned());

            found = true;
            break;
        }

        assert!(
            found,
            "in borrow checking, all effect operations should be valid"
        );

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_function_call(
        &self,
        function_call: &FunctionCall,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let function_signature = self
            .tracked_engine()
            .get_parameters(function_call.callable_id)
            .await?;

        let mut lifetime_constraints = BTreeSet::new();

        for (parameter, argument) in function_signature
            .parameter_order
            .iter()
            .copied()
            .map(|x| function_signature.parameters.get(x).unwrap())
            .zip(&function_call.arguments)
        {
            /*
            The c-varargs will break this assertion
            assert_eq!(
                function_signature.parameter_order.len(),
                function_call.arguments.len()
            );
            */

            let mut parameter_ty = parameter.r#type.clone();
            function_call.instantiation.instantiate(&mut parameter_ty);

            self.subtypes_value(
                parameter_ty,
                argument,
                Variance::Covariant,
                &mut lifetime_constraints,
            )
            .await?;
        }

        lifetime_constraints.extend(
            self.environment()
                .wf_check(
                    function_call.callable_id,
                    *span,
                    &function_call.instantiation,
                    false,
                    &self.handler(),
                )
                .await?,
        );

        let kind =
            self.tracked_engine().get_kind(function_call.callable_id).await;

        match kind {
            Kind::Function | Kind::ExternFunction => {}

            Kind::TraitFunction => {
                // parent trait requirement
                let parent_trait_id = Global::new(
                    function_call.callable_id.target_id,
                    self.tracked_engine()
                        .get_parent(function_call.callable_id)
                        .await
                        .unwrap(),
                );

                let trait_generic_params = self
                    .tracked_engine()
                    .get_generic_parameters(parent_trait_id)
                    .await?;

                let trait_arguments =
                    function_call.instantiation.create_generic_arguments(
                        parent_trait_id,
                        &trait_generic_params,
                    );

                // check extra trait satisfiability
                lifetime_constraints.extend(
                    self.environment()
                        .predicate_satisfied(
                            Predicate::PositiveTrait(PositiveTrait {
                                trait_id: parent_trait_id,
                                is_const: false,
                                generic_arguments: trait_arguments,
                            }),
                            *span,
                            None,
                            false,
                            &self.handler(),
                        )
                        .await?,
                );
            }

            Kind::ImplementationFunction => {
                let parent_implementation_id = Global::new(
                    function_call.callable_id.target_id,
                    self.tracked_engine()
                        .get_parent(function_call.callable_id)
                        .await
                        .unwrap(),
                );

                lifetime_constraints.extend(
                    self.environment()
                        .wf_check(
                            parent_implementation_id,
                            *span,
                            &function_call.instantiation,
                            false,
                            &self.handler(),
                        )
                        .await?,
                );
            }

            Kind::EffectOperation => {
                lifetime_constraints.extend(
                    self.environment()
                        .wf_check(
                            function_call.callable_id,
                            *span,
                            &function_call.instantiation,
                            false,
                            &self.handler(),
                        )
                        .await?,
                );

                self.get_changes_of_effect_operation(
                    function_call,
                    span,
                    &mut lifetime_constraints,
                )
                .await?;
            }

            _ => unreachable!(
                "function call to non-function kind: {}",
                self.tracked_engine()
                    .get_kind(function_call.callable_id)
                    .await
                    .kind_str()
            ),
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, *span))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::default(),
        })
    }
}
