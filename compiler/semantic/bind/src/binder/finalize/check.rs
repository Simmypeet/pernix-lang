use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    FunctionIR,
    ir::IR,
    value::{
        Environment as ValueEnvironment, TypeOf, Value,
        register::{self, Register},
    },
};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    name::get_by_qualified_name,
    parent::get_parent_global,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{PositiveMarker, Predicate},
    r#type::Qualifier,
};
use pernixc_type_system::{
    environment::Environment as TypeSystemEnvironment, normalizer::Normalizer,
};

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
            let instantiation = st
                .create_instantiation(value_environment.tracked_engine())
                .await;

            value_environment
                .type_environment
                .wf_check_instantiation(
                    st.struct_id(),
                    &register.span,
                    &instantiation,
                    &handler,
                )
                .await?;

            Ok(())
        }
        register::Assignment::Variant(variant) => {
            let enum_id = variant
                .parent_enum_id(value_environment.tracked_engine())
                .await;

            let inst = variant
                .create_instantiation(value_environment.tracked_engine())
                .await;

            value_environment
                .type_environment
                .wf_check_instantiation(
                    enum_id,
                    &register.span,
                    &inst,
                    &handler,
                )
                .await?;

            Ok(())
        }
        register::Assignment::FunctionCall(function_call) => {
            let symbol_kind = value_environment
                .tracked_engine()
                .get_kind(function_call.callee_symbol_id())
                .await;

            let Some(inst) = function_call
                .create_instantiation(value_environment.tracked_engine())
                .await
            else {
                return Ok(());
            };

            match symbol_kind {
                Kind::ImplementationAssociatedFunction => {
                    let parent_implementation_id = value_environment
                        .tracked_engine()
                        .get_parent_global(function_call.callee_symbol_id())
                        .await
                        .unwrap();

                    value_environment
                        .type_environment
                        .wf_check_instantiation(
                            parent_implementation_id,
                            &register.span,
                            &inst,
                            &handler,
                        )
                        .await?;

                    value_environment
                        .type_environment
                        .wf_check_instantiation(
                            parent_implementation_id,
                            &register.span,
                            &inst,
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
                        .wf_check_instantiation(
                            function_call.callee_symbol_id(),
                            &register.span,
                            &inst,
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
                                .span_of_value(&Value::Register(register_id)),
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
                    GenericArguments::new(
                        Vec::new(),
                        vec![ty.result.clone()],
                        Vec::new(),
                        Vec::new(),
                    ),
                ));

                value_environment
                    .type_environment
                    .predicate_satisfied(
                        predicate,
                        &register.span,
                        None,
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
                            *ir.values.span_of_value(&element.value),
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
                        ir.values.span_of_value(&element.value),
                        None,
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
        | register::Assignment::ResumeCall(_)
        | register::Assignment::VariantNumber(_) => Ok(()),
    }
}

pub(super) async fn check_all<N: Normalizer>(
    function_ir: &FunctionIR,
    ty_environment: &TypeSystemEnvironment<'_, N>,
    current_site: Global<pernixc_symbol::ID>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    for (_, ir, value_environment) in
        function_ir.ir_with_value_environments(ty_environment, current_site)
    {
        check(ir.ir(), &value_environment, handler).await?;
    }

    Ok(())
}

async fn check<N: Normalizer>(
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

    Ok(())
}
