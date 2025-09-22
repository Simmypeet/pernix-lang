use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::FunctionCall;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{parameter::get_parameters, variance::Variance};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::get_generic_parameters,
    predicate::{PositiveTrait, Predicate},
};
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
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
            );
        }

        lifetime_constraints.extend(
            self.environment()
                .wf_check(
                    function_call.callable_id,
                    span.clone(),
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
                            span.clone(),
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
                            span.clone(),
                            &function_call.instantiation,
                            false,
                            &self.handler(),
                        )
                        .await?,
                );
            }

            _ => {
                panic!("Unexpected symbol kind encountered")
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::default(),
        })
    }
}
