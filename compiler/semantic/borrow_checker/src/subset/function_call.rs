use std::collections::BTreeSet;

use pernixc_arena::ID;
use pernixc_hash::{HashMap, HashSet};
use pernixc_ir::value::register::{EffectHandlerArgument, FunctionCall};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    capability::get_capabilities, parameter::get_parameters, variance::Variance,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    effect,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    predicate::{PositiveTrait, Predicate},
};
use pernixc_type_system::{
    lifetime_constraint::LifetimeConstraint, normalizer::Normalizer,
    UnrecoverableError,
};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_subset_of_effect_operations(
        &self,
        capability_arguments: &HashMap<ID<effect::Unit>, EffectHandlerArgument>,
        instantiation: &Instantiation,
        callled_id: Global<pernixc_symbol::ID>,
        span: &RelativeSpan,
        lifetime_constraints: &mut BTreeSet<LifetimeConstraint>,
    ) -> Result<(), UnrecoverableError> {
        let called_capabilities =
            self.tracked_engine().get_capabilities(callled_id).await?;

        let current_capabilities =
            self.tracked_engine().get_capabilities(self.current_site()).await?;

        for (required_id, argument) in capability_arguments {
            let mut required_capability =
                called_capabilities[*required_id].clone();

            // instantiate the generic arguments of the required capability
            required_capability.generic_arguments.instantiate(instantiation);

            match argument {
                EffectHandlerArgument::FromPassedCapability(
                    capability_unit,
                ) => {
                    // no need to instantiate, as the capability unit is
                    // already instantiated from the call site
                    let available_capability =
                        &current_capabilities[*capability_unit];

                    let subtypable = self
                        .type_environment()
                        .subtypes_generic_arguments(
                            &required_capability.generic_arguments,
                            &available_capability.generic_arguments,
                        )
                        .await
                        .map_err(|x| {
                            x.report_as_type_check_overflow(
                                *span,
                                &self.handler(),
                            )
                        })?;

                    let subtypable =
                        subtypable.expect("should've been checked");

                    assert!(subtypable
                        .result
                        .forall_lifetime_errors
                        .is_empty());

                    lifetime_constraints
                        .extend(subtypable.constraints.iter().cloned());
                }

                #[allow(clippy::match_same_arms)]
                EffectHandlerArgument::FromEffectHandler(_) => {
                    // TODO: extract the lifetimme constraints
                }

                EffectHandlerArgument::Unhandled => {
                    // error should've been reported
                }
            }
        }

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
            self.type_environment()
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
                    self.type_environment()
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
                    self.type_environment()
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
                    self.type_environment()
                        .wf_check(
                            function_call.callable_id,
                            *span,
                            &function_call.instantiation,
                            false,
                            &self.handler(),
                        )
                        .await?,
                );
            }

            _ => unreachable!(
                "function call to non-function kind: {}",
                self.tracked_engine()
                    .get_kind(function_call.callable_id)
                    .await
                    .kind_str()
            ),
        }

        self.get_subset_of_effect_operations(
            &function_call.capability_arguments,
            &function_call.instantiation,
            function_call.callable_id,
            span,
            &mut lifetime_constraints,
        )
        .await?;

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
