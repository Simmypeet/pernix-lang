use pernixc_handler::Handler;
#[allow(unused_imports)]
use pernixc_hash::HashSet;
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

#[cfg(debug_assertions)]
fn check_all_register_assigned(ir: &IR) {
    let mut assigned = HashSet::default();
    for (_, block) in ir.control_flow_graph.blocks().iter() {
        for inst in block.instructions() {
            if let Some(reg) = inst.as_register_assignment() {
                assert!(
                    assigned.insert(reg.id),
                    "register assigned more than once"
                );
            }
        }
    }

    let unassigned_registers = ir
        .values
        .registers
        .ids()
        .filter(|id| !assigned.contains(id))
        .collect::<HashSet<_>>();

    if !unassigned_registers.is_empty() {
        for reg in unassigned_registers {
            let assignment = ir.values.registers.get(reg).unwrap();

            tracing::error!(
                "register `ID({reg:?}) = {assignment:?}` is never assigned in \
                 cfg"
            );
        }

        panic!("some registers are never assigned in cfg");
    }
}

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

        // remove all unreachable registers
        for reg_id in self.unreachable_register_ids.iter().copied() {
            assert!(self.ir.values.registers.remove(reg_id).is_some());
        }

        #[cfg(debug_assertions)]
        check_all_register_assigned(&self.ir);

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
