use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    value::{
        register::{self, Register},
        TypeOf, Value,
    },
    IR,
};
use pernixc_symbol::{
    kind::{get_kind, Kind},
    name::get_by_qualified_name,
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    generic_parameters::get_generic_parameters,
    instantiation::get_instantiation,
    predicate::{PositiveMarker, PositiveTrait, Predicate},
    r#type::Qualifier,
};
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::{
    binder::{report_as_type_calculating_overflow, UnrecoverableError},
    diagnostic::Diagnostic,
};

#[allow(clippy::too_many_lines)]
async fn check_register_assignment<N: Normalizer>(
    ir: &IR,
    register_id: ID<Register>,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    let register =
        ir.values.registers.get(register_id).expect("Register not found");

    match &register.assignment {
        register::Assignment::Struct(st) => {
            let instantiation = environment
                .tracked_engine()
                .get_instantiation(st.struct_id, st.generic_arguments.clone())
                .await?
                .expect("failed to get instantiation");

            environment
                .wf_check(
                    st.struct_id,
                    register.span.unwrap(),
                    &instantiation,
                    false,
                    &handler,
                )
                .await?;

            Ok(())
        }
        register::Assignment::Variant(variant) => {
            let enum_id = Global::new(
                variant.variant_id.target_id,
                environment
                    .tracked_engine()
                    .get_parent(variant.variant_id)
                    .await
                    .unwrap(),
            );

            let instantiation = environment
                .tracked_engine()
                .get_instantiation(enum_id, variant.generic_arguments.clone())
                .await?
                .expect("failed to get instantiation");

            environment
                .wf_check(
                    enum_id,
                    register.span.unwrap(),
                    &instantiation,
                    false,
                    &handler,
                )
                .await?;

            Ok(())
        }
        register::Assignment::FunctionCall(function_call) => {
            let symbol_kind = environment
                .tracked_engine()
                .get_kind(function_call.callable_id)
                .await;

            match symbol_kind {
                Kind::Function | Kind::ExternFunction => {
                    environment
                        .wf_check(
                            function_call.callable_id,
                            register.span.unwrap(),
                            &function_call.instantiation,
                            false,
                            &handler,
                        )
                        .await?;

                    Ok(())
                }

                Kind::TraitFunction => {
                    // parent trait requirement
                    let parent_trait_id = Global::new(
                        function_call.callable_id.target_id,
                        environment
                            .tracked_engine()
                            .get_parent(function_call.callable_id)
                            .await
                            .unwrap(),
                    );

                    let trait_arguments =
                        function_call.instantiation.create_generic_arguments(
                            parent_trait_id,
                            &environment
                                .tracked_engine()
                                .get_generic_parameters(parent_trait_id)
                                .await
                                .unwrap(),
                        );

                    // check extra trait satisfiability
                    environment
                        .predicate_satisfied(
                            Predicate::PositiveTrait(PositiveTrait {
                                trait_id: parent_trait_id,
                                is_const: false, /* TODO: reflect the
                                                  * actual valuec */
                                generic_arguments: trait_arguments,
                            }),
                            register.span.unwrap(),
                            None,
                            false,
                            &handler,
                        )
                        .await?;

                    environment
                        .wf_check(
                            function_call.callable_id,
                            register.span.unwrap(),
                            &function_call.instantiation,
                            false,
                            &handler,
                        )
                        .await?;

                    Ok(())
                }

                Kind::ImplementationFunction => {
                    let parent_implementation_id = Global::new(
                        function_call.callable_id.target_id,
                        environment
                            .tracked_engine()
                            .get_parent(function_call.callable_id)
                            .await
                            .unwrap(),
                    );

                    environment
                        .wf_check(
                            parent_implementation_id,
                            register.span.unwrap(),
                            &function_call.instantiation,
                            false,
                            &handler,
                        )
                        .await?;

                    environment
                        .wf_check(
                            function_call.callable_id,
                            register.span.unwrap(),
                            &function_call.instantiation,
                            false,
                            &handler,
                        )
                        .await?;

                    Ok(())
                }

                _ => unreachable!(),
            }
        }

        // check for move behind shared reference on non-copy type
        register::Assignment::Load(load) => {
            if load.address.get_reference_qualifier()
                == Some(Qualifier::Immutable)
                || load.address.is_behind_index()
            {
                let ty = ir
                    .values
                    .type_of(&load.address, current_site, environment)
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            *ir.values
                                .span_of_value(&Value::Register(register_id))
                                .unwrap(),
                            handler,
                        )
                    })?;

                let copy_marker = environment
                    .tracked_engine()
                    .get_by_qualified_name(
                        pernixc_corelib::copy::MARKER_SEQUENCE,
                    )
                    .await
                    .unwrap();

                let predicate = Predicate::PositiveMarker(PositiveMarker::new(
                    copy_marker,
                    GenericArguments {
                        lifetimes: Vec::new(),
                        types: vec![ty.result.clone()],
                        constants: Vec::new(),
                    },
                ));

                environment
                    .predicate_satisfied(
                        predicate,
                        register.span.unwrap(),
                        None,
                        false,
                        &handler,
                    )
                    .await?;
            }

            Ok(())
        }

        // tuple unpacking
        register::Assignment::Tuple(tuple) => {
            for element in tuple.elements.iter().filter(|x| x.is_unpacked) {
                let ty = ir
                    .values
                    .type_of(&element.value, current_site, environment)
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            *ir.values.span_of_value(&element.value).unwrap(),
                            handler,
                        )
                    })?;

                let predicate = Predicate::TupleType(
                    pernixc_term::predicate::Tuple(ty.result.clone()),
                );

                environment
                    .predicate_satisfied(
                        predicate,
                        *ir.values.span_of_value(&element.value).unwrap(),
                        None,
                        false,
                        &handler,
                    )
                    .await?;
            }

            Ok(())
        }

        register::Assignment::Borrow(_)
        | register::Assignment::Prefix(_)
        | register::Assignment::Binary(_)
        | register::Assignment::Array(_)
        | register::Assignment::Phi(_)
        | register::Assignment::Cast(_)
        | register::Assignment::VariantNumber(_) => Ok(()),
    }
}

pub(super) async fn check<N: Normalizer>(
    ir: &IR,
    current_site: Global<pernixc_symbol::ID>,
    environment: &Environment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    for register_id in ir
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_register_assignment().map(|x| x.id))
    {
        check_register_assignment(
            ir,
            register_id,
            current_site,
            environment,
            handler,
        )
        .await?;
    }

    Ok(())
}
