use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_ir::{
    FunctionIR, IRWithContext,
    instruction::{Instruction, ScopePop},
};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_symbol::kind::get_kind;
use pernixc_target::get_ir_verification;
use pernixc_term::r#type::Type;
use pernixc_type_system::environment::Environment as TyEnvironment;

use crate::{
    binder::{Binder, UnrecoverableError},
    diagnostic::{Diagnostic, NotAllFlowPathsReturnAValue},
};

mod check;
mod transform_inference;
mod verify;

impl Binder<'_> {
    /// Performs sanity checks on the IR and finalizes it.
    pub(super) fn tidy_ir(&mut self) {
        self.block_context.assert_empty();
        self.loop_context.assert_empty();

        // TODO: we might need to implements some debug verification logic here
        // to ensure the IR is valid. For example, all scope push should have
        // corresponding scope pop.

        let root_scope_id = self.ir.scope_tree.root_scope_id();
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(root_scope_id)));

        // remove all unreachable blocks
        self.ir.control_flow_graph.remove_unreachable_blocks();

        // remove all unreachable registers
        for reg_id in self.unreachable_register_ids.iter().copied() {
            assert!(self.ir.values.registers.remove(reg_id).is_some());
        }
    }

    /// Finalizes the binding process, performing necessary checks and
    /// transformations on the IR.
    pub async fn finalize_function_ir(
        mut self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<FunctionIR, UnrecoverableError> {
        // all the effect handler stacks should've been all popped
        self.effect_handler_context.assert_empty();

        self.tidy_ir();

        // we're in the function, check if all paths return the value
        'out: {
            let symbol_kind = self.engine.get_kind(self.current_site()).await;
            if symbol_kind.has_function_body() {
                let return_ty =
                    self.engine.get_return_type(self.current_site()).await;

                // no checking need
                if *return_ty == Type::unit() {
                    break 'out;
                }

                // check if all paths return the value
                if self
                    .ir
                    .control_flow_graph
                    .traverse()
                    .any(|(_, x)| x.terminator().is_none())
                {
                    handler.receive(
                        NotAllFlowPathsReturnAValue {
                            callable_id: self.current_site(),
                        }
                        .into(),
                    );
                }
            }
        }

        // transform inference types
        self.transform_inference(handler).await?;

        let ty_env = TyEnvironment::new(
            Cow::Borrowed(&self.environment.premise),
            Cow::Borrowed(self.engine),
            &self.inference_context,
        );

        let root_ir_id =
            self.ir_map.new_ir(IRWithContext::new_root_ir(self.ir));

        let function_ir = FunctionIR::new(
            self.effect_handler_context.into_handler_groups(),
            self.ir_map,
            self.captures_map,
            root_ir_id,
        );

        check::check_all(&function_ir, &ty_env, handler).await?;

        if self.engine.get_ir_verification(current_site.target_id).await {
            verify::verify_function_ir(&function_ir).await;
        }

        Ok(function_ir)
    }
}
