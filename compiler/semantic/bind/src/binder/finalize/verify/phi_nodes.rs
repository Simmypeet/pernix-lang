use pernixc_arena::ID;
use pernixc_ir::{IRWithContext, ir::IR, value::register::Assignment};

pub(super) fn verify_phi_nodes(ir_id: ID<IRWithContext>, ir: &IR) -> usize {
    let mut violations = 0;

    for (block_id, block) in ir.control_flow_graph.traverse() {
        for instruction in block.instructions() {
            let Some(register_assignment) =
                instruction.as_register_assignment()
            else {
                continue;
            };

            let register = &ir.values.registers[register_assignment.id];
            let Assignment::Phi(phi) = &register.assignment else {
                continue;
            };

            for incoming_block_id in phi.incoming_values.keys() {
                if block.predecessors().contains(incoming_block_id) {
                    continue;
                }

                tracing::error!(
                    "IR verification failed for {ir_id:?}: phi register \
                     {register_id:?} in block {block_id:?} references \
                     incoming block {incoming_block_id:?} that is not a \
                     predecessor",
                    incoming_block_id = *incoming_block_id,
                    register_id = register_assignment.id,
                );
                violations += 1;
            }
        }
    }

    violations
}

#[cfg(test)]
mod test {
    use pernixc_arena::ID;
    use pernixc_ir::{
        instruction::{
            ConditionalJump, Instruction, Jump, RegisterAssignment, Terminator,
            UnconditionalJump,
        },
        value::{
            Value,
            register::{Assignment, Phi, Register},
        },
    };

    use super::verify_phi_nodes;
    use crate::binder::finalize::verify::test::{
        assert_verification_panics, empty_ir, test_span, unit_return,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn predecessor_integrity_panics() {
        let mut ir = empty_ir();

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let left_block_id = ir.control_flow_graph.new_block();
        let right_block_id = ir.control_flow_graph.new_block();
        let join_block_id = ir.control_flow_graph.new_block();
        let foreign_block_id = ir.control_flow_graph.new_block();

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
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join_block_id,
            })),
        ));

        let register_id = ir.values.registers.insert(Register {
            assignment: Assignment::Phi(Phi {
                incoming_values: [
                    (left_block_id, Value::unit(test_span())),
                    (foreign_block_id, Value::unit(test_span())),
                ]
                .into_iter()
                .collect(),
                r#type: pernixc_term::r#type::Type::unit(),
            }),
            span: test_span(),
        });

        assert!(ir.control_flow_graph[join_block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id,
            }),
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );

        assert_verification_panics(ir).await;
    }

    #[test]
    fn direct_pass_reports_single_violation() {
        let mut ir = empty_ir();

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let join_block_id = ir.control_flow_graph.new_block();

        assert!(ir.control_flow_graph.insert_terminator(
            entry_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join_block_id,
            })),
        ));

        let register_id = ir.values.registers.insert(Register {
            assignment: Assignment::Phi(Phi {
                incoming_values: std::iter::once((
                    entry_block_id,
                    Value::unit(test_span()),
                ))
                .collect(),
                r#type: pernixc_term::r#type::Type::unit(),
            }),
            span: test_span(),
        });

        assert!(ir.control_flow_graph[join_block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id,
            }),
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );

        let foreign_block_id = ir.control_flow_graph.new_block();
        if let Assignment::Phi(phi) =
            &mut ir.values.registers[register_id].assignment
        {
            phi.incoming_values
                .insert(foreign_block_id, Value::unit(test_span()));
        }

        assert_eq!(verify_phi_nodes(ID::new(0), &ir), 1);
    }
}
