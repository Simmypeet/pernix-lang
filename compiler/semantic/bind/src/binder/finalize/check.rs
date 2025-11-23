use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    IR,
    value::{
        Environment as ValueEnvironment, TypeOf, Value,
        register::{self, Register},
    },
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
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
use pernixc_type_system::normalizer::Normalizer;

use crate::{binder::UnrecoverableError, diagnostic::Diagnostic};

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
async fn check_register_assignment<N: Normalizer>(
    ir: &IR,
    register_id: ID<Register>,
    value_environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    let register =
        ir.values.registers.get(register_id).expect("Register not found");

    match &register.assignment {
        register::Assignment::Struct(st) => {
            let instantiation = value_environment
                .tracked_engine()
                .get_instantiation(st.struct_id, st.generic_arguments.clone())
                .await?
                .expect("failed to get instantiation");

            value_environment
                .type_environment
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
                value_environment
                    .tracked_engine()
                    .get_parent(variant.variant_id)
                    .await
                    .unwrap(),
            );

            let instantiation = value_environment
                .tracked_engine()
                .get_instantiation(enum_id, variant.generic_arguments.clone())
                .await?
                .expect("failed to get instantiation");

            value_environment
                .type_environment
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
            let symbol_kind = value_environment
                .tracked_engine()
                .get_kind(function_call.callable_id)
                .await;

            match symbol_kind {
                Kind::TraitFunction => {
                    // parent trait requirement
                    let parent_trait_id = Global::new(
                        function_call.callable_id.target_id,
                        value_environment
                            .tracked_engine()
                            .get_parent(function_call.callable_id)
                            .await
                            .unwrap(),
                    );

                    let trait_arguments =
                        function_call.instantiation.create_generic_arguments(
                            parent_trait_id,
                            &value_environment
                                .tracked_engine()
                                .get_generic_parameters(parent_trait_id)
                                .await
                                .unwrap(),
                        );

                    // check extra trait satisfiability
                    value_environment
                        .type_environment
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

                    value_environment
                        .type_environment
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
                        value_environment
                            .tracked_engine()
                            .get_parent(function_call.callable_id)
                            .await
                            .unwrap(),
                    );

                    value_environment
                        .type_environment
                        .wf_check(
                            parent_implementation_id,
                            register.span.unwrap(),
                            &function_call.instantiation,
                            false,
                            &handler,
                        )
                        .await?;

                    value_environment
                        .type_environment
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

                Kind::Function
                | Kind::ExternFunction
                | Kind::EffectOperation => {
                    value_environment
                        .type_environment
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
            if load.address().get_reference_qualifier()
                == Some(Qualifier::Immutable)
                || load.address().is_behind_index()
            {
                let ty = ir
                    .values
                    .type_of(load.address(), value_environment)
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            *ir.values
                                .span_of_value(&Value::Register(register_id))
                                .unwrap(),
                            &handler,
                        )
                    })?;

                let copy_marker = value_environment
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

                value_environment
                    .type_environment
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
                    .type_of(&element.value, value_environment)
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            *ir.values.span_of_value(&element.value).unwrap(),
                            &handler,
                        )
                    })?;

                let predicate = Predicate::TupleType(
                    pernixc_term::predicate::Tuple(ty.result.clone()),
                );

                value_environment
                    .type_environment
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
        | register::Assignment::Do(_)
        | register::Assignment::VariantNumber(_) => Ok(()),
    }
}

pub(super) async fn check_recursive<N: Normalizer>(
    ir: &IR,
    value_environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    for do_ir in
        ir.values.registers.items().filter_map(|x| x.assignment.as_do())
    {
        let (do_closure_ir, do_captures) = do_ir.do_closure();

        let inner_value_environment = ValueEnvironment::builder()
            .captures(do_captures)
            .current_site(value_environment.current_site)
            .type_environment(value_environment.type_environment)
            .build();

        // recursively performs memory check in nested closures
        Box::pin(check(do_closure_ir, &inner_value_environment, handler))
            .await?;

        let (with_captures, with_irs) = do_ir.with_closures();

        // recursively performs memory check in nested closures
        for (with_ir, with_closure_parameters) in with_irs {
            let inner_value_environment = ValueEnvironment::builder()
                .captures(with_captures)
                .current_site(value_environment.current_site)
                .type_environment(value_environment.type_environment)
                .closure_parameters(with_closure_parameters)
                .build();

            Box::pin(check(with_ir, &inner_value_environment, handler)).await?;
        }
    }

    Ok(())
}

pub(super) async fn check<N: Normalizer>(
    ir: &IR,
    value_environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    for register_id in ir
        .control_flow_graph
        .traverse()
        .flat_map(|x| x.1.instructions())
        .filter_map(|x| x.as_register_assignment().map(|x| x.id))
    {
        check_register_assignment(ir, register_id, value_environment, handler)
            .await?;
    }

    Box::pin(check_recursive(ir, value_environment, handler)).await?;

    Ok(())
}
