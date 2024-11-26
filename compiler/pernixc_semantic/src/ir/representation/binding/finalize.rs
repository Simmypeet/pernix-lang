//! Contains the "binder-finalization" logic.

use std::collections::HashSet;

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

        // remove all the unreachable blocks
        self.intermediate_representation
            .control_flow_graph
            .remove_unerachable_blocks();

        let used_register_assignments = self
            .intermediate_representation
            .control_flow_graph
            .traverse()
            .flat_map(|x| x.1.instructions())
            .flat_map(|x| x.as_register_assignment().map(|x| x.id))
            .collect::<HashSet<_>>();
        let unused_registers = self
            .intermediate_representation
            .registers
            .ids()
            .filter(|x| !used_register_assignments.contains(x))
            .collect::<Vec<_>>();

        for register in unused_registers {
            self.intermediate_representation
                .registers
                .remove(register)
                .unwrap();
        }
    }
}

#[cfg(test)]
mod test;
