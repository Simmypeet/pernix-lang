use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{
    FunctionIR,
    ir::IR,
    value::{
        Environment as ValueEnvironment,
        register::{self, Register},
    },
};
use pernixc_target::Global;
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
            // Use the new wf_check method from pernixc_ir
            st.wf_check(value_environment, register.span, handler).await?;

            Ok(())
        }
        register::Assignment::Variant(variant) => {
            // Use the new wf_check method from pernixc_ir
            variant.wf_check(value_environment, register.span, handler).await?;

            Ok(())
        }
        register::Assignment::FunctionCall(function_call) => {
            // Use the new wf_check method from pernixc_ir
            function_call
                .wf_check(value_environment, register.span, handler)
                .await?;

            Ok(())
        }

        // check for move behind shared reference on non-copy type
        register::Assignment::Load(load) => {
            // Use the new wf_check method from pernixc_ir
            load.wf_check(
                value_environment,
                &ir.values,
                register.span,
                handler,
            )
            .await?;

            Ok(())
        }

        register::Assignment::Tuple(tuple) => {
            // Use the new wf_check method from pernixc_ir
            tuple.wf_check(value_environment, &ir.values, handler).await?;

            Ok(())
        }

        register::Assignment::Do(do_with) => {
            // Use the new wf_check method from pernixc_ir
            do_with.wf_check(value_environment, handler).await?;

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
