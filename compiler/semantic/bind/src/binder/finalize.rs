use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{Instruction, ScopePop},
    IR,
};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_symbol::kind::get_kind;
use pernixc_term::r#type::Type;

use crate::{
    binder::{Binder, UnrecoverableError},
    diagnostic::{Diagnostic, NotAllFlowPathsReturnAValue},
};

mod check;
mod transform_inference;

impl Binder<'_> {
    /// Finalizes the binding process, performing necessary checks and
    /// transformations on the IR.
    pub async fn finalize(
        mut self,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<IR, UnrecoverableError> {
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

        // we're in the function, check if all paths return the value
        'out: {
            let symbol_kind = self.engine.get_kind(self.current_site).await;
            if symbol_kind.has_function_body() {
                let return_ty =
                    self.engine.get_return_type(self.current_site).await?;

                // no checking need
                if *return_ty
                    == Type::Tuple(pernixc_term::tuple::Tuple {
                        elements: Vec::new(),
                    })
                {
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
                            callable_id: self.current_site,
                        }
                        .into(),
                    );
                }
            }
        }

        // transform inference types
        self.transform_inference(handler).await?;
        let env = self.create_environment();

        check::check(&self.ir, self.current_site, &env, handler).await?;

        Ok(self.ir)
    }
}
