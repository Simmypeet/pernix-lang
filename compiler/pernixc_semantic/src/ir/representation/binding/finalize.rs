//! Contains the "binder-finalization" logic.

use pernixc_base::handler::Handler;

use super::{infer, Binder};
use crate::{
    error::{self, NotAllFlowPathsReturnAValue},
    ir::{self, Suboptimal, Success, IR},
    symbol::{
        table::{self, resolution},
        CallableID,
    },
    type_system::{
        self,
        environment::Environment,
        normalizer,
        term::{r#type::Type, Tuple},
    },
};

mod check;
mod transform_inference;

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Finalizes the current binder and returns the IR
    ///
    /// # Errors
    ///
    /// Return [`Err`] with [`IR`] of [`Suboptimal`] if any semantic errors
    /// encountered during binding and finalization.
    #[allow(clippy::result_large_err)]
    pub fn finalize(
        mut self,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<IR<Success>, IR<Suboptimal>> {
        self.inference_context.fill_default_inferences();

        // we're in the function, check if all paths return the value
        'out: {
            if let Ok(callable_id) = CallableID::try_from(self.current_site) {
                let callable = self.table.get_callable(callable_id).unwrap();
                let return_type = callable.return_type();

                // no checking need
                if *return_type == Type::Tuple(Tuple { elements: Vec::new() }) {
                    break 'out;
                }

                // check if all paths return the value
                if self
                    .intermediate_representation
                    .control_flow_graph
                    .traverse()
                    .any(|(_, x)| x.terminator().is_none())
                {
                    self.create_handler_wrapper(handler).receive(Box::new(
                        NotAllFlowPathsReturnAValue { callable_id },
                    ));
                }
            }
        }

        let handler_wrapper = self.create_handler_wrapper(handler);
        let transformed_ir = transform_inference::transform_inference(
            self.intermediate_representation,
            &self.inference_context,
            self.table,
            &handler_wrapper,
        );

        // stop now, it will produce useless errors
        if *handler_wrapper.suboptimal.read() {
            return Err(IR {
                representation: transformed_ir,
                state: Suboptimal,
            });
        }

        let premise = self
            .table
            .get_active_premise::<ir::Model>(self.current_site)
            .unwrap();
        let (environment, _) = Environment::new_with(
            premise,
            self.table,
            normalizer::NO_OP,
            &self.type_system_observer,
        );

        // perform the well-formedness check
        transformed_ir.check(&environment, &handler_wrapper);

        Ok(IR { representation: transformed_ir, state: Success(()) })
    }
}

#[cfg(test)]
mod test;
