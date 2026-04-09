use pernixc_arena::ID;
use pernixc_ir::{
    IRWithContext, control_flow_graph::Block, instruction::Terminator,
};

pub(super) fn verify_critical_edges(
    ir_id: ID<IRWithContext>,
    ir: &pernixc_ir::ir::IR,
) -> usize {
    let mut violations = 0;

    for (block_id, block) in ir.control_flow_graph.traverse() {
        let successor_targets = successor_targets(block);

        if successor_targets.len() <= 1 {
            continue;
        }

        for target_block_id in successor_targets {
            let predecessor_count =
                ir.control_flow_graph[target_block_id].predecessors().len();

            if predecessor_count <= 1 {
                continue;
            }

            tracing::error!(
                "IR verification failed for {ir_id:?}: critical edge from \
                 block {block_id:?} to block {target_block_id:?}",
            );
            violations += 1;
        }
    }

    violations
}

fn successor_targets(block: &Block) -> Vec<ID<Block>> {
    let Some(Terminator::Jump(jump)) = block.terminator() else {
        return Vec::new();
    };

    jump.jump_targets()
}

#[cfg(test)]
mod test {
    use pernixc_arena::ID;
    use pernixc_ir::{
        instruction::{ConditionalJump, Jump, Terminator, UnconditionalJump},
        value::Value,
    };

    use super::verify_critical_edges;
    use crate::binder::finalize::verify::test::{
        assert_verification_panics, empty_ir, test_span, unit_return,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn critical_edge_panics() {
        let mut ir = empty_ir();

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let left_block_id = ir.control_flow_graph.new_block();
        let right_block_id = ir.control_flow_graph.new_block();
        let join_block_id = ir.control_flow_graph.new_block();
        let exit_block_id = ir.control_flow_graph.new_block();

        assert!(ir.control_flow_graph.insert_terminator(
            entry_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: left_block_id,
                false_target: right_block_id,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            left_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join_block_id,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            right_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: join_block_id,
                false_target: exit_block_id,
            })),
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );
        assert!(
            ir.control_flow_graph
                .insert_terminator(exit_block_id, unit_return())
        );

        assert_verification_panics(ir).await;
    }

    #[test]
    fn direct_pass_reports_single_violation() {
        let mut ir = empty_ir();

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let left_block_id = ir.control_flow_graph.new_block();
        let right_block_id = ir.control_flow_graph.new_block();
        let join_block_id = ir.control_flow_graph.new_block();
        let exit_block_id = ir.control_flow_graph.new_block();

        assert!(ir.control_flow_graph.insert_terminator(
            entry_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: left_block_id,
                false_target: right_block_id,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            left_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join_block_id,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            right_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: join_block_id,
                false_target: exit_block_id,
            })),
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );
        assert!(
            ir.control_flow_graph
                .insert_terminator(exit_block_id, unit_return())
        );

        assert_eq!(verify_critical_edges(ID::new(0), &ir), 1);
    }
}
