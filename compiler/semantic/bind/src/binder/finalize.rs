use std::borrow::Cow;

use pernixc_handler::Handler;
use pernixc_hash::HashSet;
use pernixc_ir::{
    FunctionIR, IRWithContext,
    instruction::{Instruction, ScopePop},
    ir::IR,
};
use pernixc_semantic_element::return_type::get_return_type;
use pernixc_symbol::kind::get_kind;
use pernixc_term::r#type::Type;
use pernixc_type_system::environment::Environment as TyEnvironment;

use crate::{
    binder::{Binder, UnrecoverableError},
    diagnostic::{Diagnostic, NotAllFlowPathsReturnAValue},
};

mod check;
mod transform_inference;

#[allow(dead_code)]
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

        #[cfg(debug_assertions)]
        check_all_register_assigned(&self.ir);
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
                            callable_id: self.current_site(),
                        }
                        .into(),
                    );
                }
            }
        }

        // transform inference types
        self.transform_inference(handler).await;

        let current_site = self.current_site();
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
            self.closure_parameters_map,
            self.captures_map,
            root_ir_id,
        );

        check::check_all(&function_ir, &ty_env, current_site, handler).await?;

        Ok(function_ir)
    }
}
