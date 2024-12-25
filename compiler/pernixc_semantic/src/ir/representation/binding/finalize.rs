//! Contains the "binder-finalization" logic.

use pernixc_base::handler::Handler;

use super::{infer, Binder};
use crate::{
    error::{self, NotAllFlowPathsReturnAValue, TypeSystemOverflow},
    ir::{
        self,
        instruction::{Instruction, ScopePop},
        representation::borrow::Model as BorrowModel,
        Suboptimal, Success, IR,
    },
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

mod borrow;
mod check;
mod memory;
mod simplify_drop;
mod transform_inference;

#[derive(Debug, Clone, PartialEq, Eq, derive_more::From)]
#[allow(clippy::large_enum_variant)]
pub enum Error {
    TypeSystemOverflow(TypeSystemOverflow<ir::Model>),
    Suboptimal(IR<Suboptimal>),
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<
                ir::representation::borrow::Model,
                S,
            >,
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
    ) -> Result<IR<Success>, Error> {
        let root_scope_id =
            self.intermediate_representation.scope_tree.root_scope_id();
        let _ = self
            .current_block_mut()
            .add_instruction(Instruction::ScopePop(ScopePop(root_scope_id)));
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
        let mut transformed_ir = transform_inference::transform_inference(
            self.intermediate_representation,
            &self.inference_context,
            self.table,
            &handler_wrapper,
        )?;

        // stop now, it will produce useless errors
        if *handler_wrapper.suboptimal.read() {
            return Err(Error::Suboptimal(IR {
                representation: transformed_ir,
                state: Suboptimal,
            }));
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

        transformed_ir.check(self.current_site, &environment, &handler_wrapper);
        transformed_ir.memory_check(
            self.current_site,
            &environment,
            &handler_wrapper,
        )?;

        // create new environment for borrow checking
        let premise = self
            .table
            .get_active_premise::<BorrowModel>(self.current_site)
            .unwrap();
        let (environment, _) = Environment::new_with(
            premise,
            self.table,
            normalizer::NO_OP,
            &self.type_system_observer,
        );

        transformed_ir.borrow_check(
            self.current_site,
            &environment,
            &handler_wrapper,
        )?;

        if *handler_wrapper.suboptimal.as_ref().read() {
            Err(Error::Suboptimal(IR {
                representation: transformed_ir,
                state: Suboptimal,
            }))
        } else {
            Ok(IR { representation: transformed_ir, state: Success(()) })
        }
    }
}

#[cfg(test)]
mod test;
