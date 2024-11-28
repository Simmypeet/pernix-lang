//! Contains the "binder-finalization" logic.

use pernixc_base::handler::Handler;

use super::{infer, Binder};
use crate::{
    error::{self, NotAllFlowPathsReturnAValue},
    symbol::{
        table::{self, resolution},
        CallableID,
    },
    type_system::{
        self,
        term::{r#type::Type, Tuple},
    },
};

mod transform_inference;

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Finalizes the current binder and returns the IR
    pub fn finalize(mut self, handler: &dyn Handler<Box<dyn error::Error>>) {
        self.inference_context.fill_default_inferences();

        // we're in the function, check if all paths return the value
        'out: {
            if let Ok(callable_id) = CallableID::try_from(self.current_site) {
                let callable = self.table.get_callable(callable_id).unwrap();
                let return_type = dbg!(callable.return_type());

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
        let _transformed_ir = transform_inference::transform_inference(
            self.intermediate_representation,
            &self.inference_context,
            self.table,
            &handler_wrapper,
        );
    }
}

#[cfg(test)]
mod test;
