use pernixc_arena::ID;
use pernixc_hash::FxHashSet;
use pernixc_ir::{IRWithContext, ir::IR};

pub(super) fn verify_register_assignments(
    ir_id: ID<IRWithContext>,
    ir: &IR,
) -> usize {
    let mut assigned_registers = FxHashSet::default();
    let mut violations = 0;

    for (_, block) in ir.control_flow_graph.traverse() {
        for instruction in block.instructions() {
            let Some(register_assignment) =
                instruction.as_register_assignment()
            else {
                continue;
            };

            assigned_registers.insert(register_assignment.id);
        }
    }

    for register_id in ir.values.registers.ids() {
        if assigned_registers.contains(&register_id) {
            continue;
        }

        let register = &ir.values.registers[register_id];

        tracing::error!(
            "IR verification failed for {ir_id:?}: register {register_id:?} = \
             {register:?} is never assigned in the CFG",
        );
        violations += 1;
    }

    violations
}

#[cfg(test)]
mod test {
    use pernixc_arena::ID;

    use super::verify_register_assignments;
    use crate::binder::finalize::verify::test::{
        assert_verification_panics, empty_ir, phi_register, unit_return,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn missing_assignment_panics() {
        let mut ir = empty_ir();

        let _register_id = ir.values.registers.insert(phi_register());

        assert!(ir.control_flow_graph.insert_terminator(
            ir.control_flow_graph.entry_block_id(),
            unit_return(),
        ));

        assert_verification_panics(ir).await;
    }

    #[test]
    fn direct_pass_reports_single_violation() {
        let mut ir = empty_ir();
        let _register_id = ir.values.registers.insert(phi_register());

        assert!(ir.control_flow_graph.insert_terminator(
            ir.control_flow_graph.entry_block_id(),
            unit_return(),
        ));

        assert_eq!(verify_register_assignments(ID::new(0), &ir), 1);
    }
}
