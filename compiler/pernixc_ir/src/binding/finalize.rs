//! Contains the "binder-finalization" logic.

use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_semantic::{
    component::{
        derived::function_signature::FunctionSignature, input::SymbolKind,
    },
    diagnostic::Diagnostic,
    term::{self, r#type::Type},
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer,
};

use super::{diagnostic::NotAllFlowPathsReturnAValue, Abort, Binder};
use crate::{
    instruction::{Instruction, ScopePop},
    IR,
};

mod check;
mod transform_inference;

impl Binder<'_> {
    /// Finalizes the current binder and returns the IR
    ///
    /// # Errors
    ///
    /// Return [`Err`] with [`IR`] of [`Suboptimal`] if any semantic errors
    /// encountered during binding and finalization.
    #[allow(clippy::result_large_err)]
    pub fn finalize(
        mut self,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<IR, Abort> {
        let root_scope_id =
            self.intermediate_representation.scope_tree.root_scope_id();
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(root_scope_id)));
        self.inference_context.fill_default_inferences();

        // we're in the function, check if all paths return the value
        'out: {
            let symbol_kind = *self.table.get::<SymbolKind>(self.current_site);
            if symbol_kind.has_function_body() {
                let function_signature =
                    self.table.query::<FunctionSignature>(self.current_site)?;

                // no checking need
                if function_signature.return_type
                    == Type::Tuple(term::Tuple { elements: Vec::new() })
                {
                    break 'out;
                }

                // check if all paths return the value
                if self
                    .intermediate_representation
                    .control_flow_graph
                    .traverse()
                    .any(|(_, x)| x.terminator().is_none())
                {
                    handler.receive(Box::new(NotAllFlowPathsReturnAValue {
                        callable_id: self.current_site,
                    }));
                }
            }
        }

        let transformed_ir = transform_inference::transform_inference(
            self.intermediate_representation,
            &self.inference_context,
            self.table,
            handler,
        )?;

        let environment = Environment::new(
            Cow::Owned(self.table.get_active_premise(self.current_site)),
            self.table,
            normalizer::NO_OP,
        );

        transformed_ir.check(self.current_site, &environment, handler)?;

        Ok(transformed_ir)
    }
}

#[cfg(test)]
mod test;
