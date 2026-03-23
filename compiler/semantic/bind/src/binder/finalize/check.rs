use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    FunctionIR,
    ir::IR,
    resolution_visitor::{
        Abort, IntoResolutionWithSpan, RecursiveSymbolicResolutionVisitor,
        SymbolicResolution, accept_recursive_symbolic_resolution_visitor,
    },
    value::{
        Environment as ValueEnvironment, TypeOf, Value,
        register::{self, Register},
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_symbol::name::get_by_qualified_name;
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

/// A visitor that performs well-formedness checks on all symbolic resolutions.
struct WfCheckVisitor<'a, 'b, N> {
    value_environment: &'a ValueEnvironment<'b, N>,
    handler: &'a dyn Handler<Diagnostic>,
}

impl<N: Normalizer> RecursiveSymbolicResolutionVisitor
    for WfCheckVisitor<'_, '_, N>
{
    async fn visit(
        &mut self,
        resolution: SymbolicResolution<'_>,
        span: RelativeSpan,
    ) -> Result<(), Abort> {
        let engine = self.value_environment.tracked_engine();

        // Create instantiation, skip if it fails
        let Some(instantiation) = resolution.create_instantiation(engine).await
        else {
            return Ok(());
        };

        // Get all symbol IDs that need wf_check
        for symbol_id in resolution.get_symbol_ids_for_wf_check(engine).await {
            self.value_environment
                .type_environment
                .wf_check_instantiation(
                    symbol_id,
                    &span,
                    &instantiation,
                    &self.handler,
                )
                .await
                .map_err(|_| Abort)?;
        }

        Ok(())
    }
}

#[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
async fn check_register_assignment<N: Normalizer>(
    ir: &IR,
    register_id: ID<Register>,
    value_environment: &ValueEnvironment<'_, N>,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    let register =
        ir.values.registers.get(register_id).expect("Register not found");

    let mut wf_check_visitor = WfCheckVisitor { value_environment, handler };

    match &register.assignment {
        register::Assignment::Struct(st) => {
            // Recursively check all symbols within the struct
            let visitable =
                IntoResolutionWithSpan::new(st.symbol(), register.span);

            accept_recursive_symbolic_resolution_visitor(
                &visitable,
                &mut wf_check_visitor,
            )
            .await
            .map_err(|_| UnrecoverableError::Reported)?;

            Ok(())
        }
        register::Assignment::Variant(variant) => {
            // Recursively check all symbols within the variant
            let visitable =
                IntoResolutionWithSpan::new(variant.symbol(), register.span);

            accept_recursive_symbolic_resolution_visitor(
                &visitable,
                &mut wf_check_visitor,
            )
            .await
            .map_err(|_| UnrecoverableError::Reported)?;

            Ok(())
        }
        register::Assignment::FunctionCall(function_call) => {
            // Recursively check all symbols within the function call callee
            let visitable = IntoResolutionWithSpan::new(
                function_call.callee(),
                register.span,
            );

            accept_recursive_symbolic_resolution_visitor(
                &visitable,
                &mut wf_check_visitor,
            )
            .await
            .map_err(|_| UnrecoverableError::Reported)?;

            Ok(())
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

        register::Assignment::Do(do_with) => {
            for handler_clause_id in do_with.handler_clause_ids() {
                let handler_clause =
                    value_environment.get_handler_clause(handler_clause_id);

                let inst = handler_clause
                    .create_instantiation(value_environment.tracked_engine())
                    .await;

                let effect_id = handler_clause.effect_id();

                value_environment
                    .type_environment
                    .wf_check_instantiation(
                        effect_id,
                        handler_clause.qualified_identifier_span(),
                        &inst,
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
