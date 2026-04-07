use pernixc_arena::ID;
use pernixc_hash::FxHashSet;
use pernixc_ir::{
    FunctionIR, IRWithContext,
    control_flow_graph::Block,
    instruction::{Jump, Terminator},
    ir::IR,
    value::register::Assignment,
};

pub(super) fn verify_function_ir(function_ir: &FunctionIR) {
    let violation_count = function_ir
        .ir_with_contexts()
        .map(|(ir_id, ir_with_context)| verify_ir(ir_id, ir_with_context.ir()))
        .sum::<usize>();

    assert!(
        violation_count == 0,
        "found {violation_count} IR verification violation(s)"
    );
}

fn verify_ir(ir_id: ID<IRWithContext>, ir: &IR) -> usize {
    verify_phi_nodes(ir_id, ir)
        + verify_register_assignments(ir_id, ir)
        + verify_critical_edges(ir_id, ir)
}

fn verify_phi_nodes(ir_id: ID<IRWithContext>, ir: &IR) -> usize {
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

fn verify_register_assignments(ir_id: ID<IRWithContext>, ir: &IR) -> usize {
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

fn verify_critical_edges(ir_id: ID<IRWithContext>, ir: &IR) -> usize {
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
    use pernixc_hash::FxHashMap;
    use pernixc_ir::{
        FunctionIR, IRWithContext, Values,
        capture::CapturesMap,
        closure_parameters::ClosureParametersMap,
        control_flow_graph::ControlFlowGraph,
        function_ir::IRContext,
        handling_scope::HandlingScopes,
        instruction::{
            ConditionalJump, Instruction, Jump, RegisterAssignment, Return,
            Terminator, UnconditionalJump,
        },
        ir::{IR, IRMap},
        scope,
        value::{
            Value,
            register::{Assignment, Phi, Register},
        },
    };
    use pernixc_lexical::tree::{
        OffsetMode, ROOT_BRANCH_ID, RelativeLocation, RelativeSpan,
    };
    use pernixc_source_file::{LocalSourceID, Span};
    use pernixc_target::TargetID;
    use pernixc_term::r#type::Type;

    use super::verify_function_ir;

    fn test_span() -> RelativeSpan {
        let source_id = TargetID::TEST.make_global(LocalSourceID::new(0, 0));

        Span::new(
            RelativeLocation {
                offset: 0,
                mode: OffsetMode::Start,
                relative_to: ROOT_BRANCH_ID,
            },
            RelativeLocation {
                offset: 0,
                mode: OffsetMode::Start,
                relative_to: ROOT_BRANCH_ID,
            },
            source_id,
        )
    }

    fn unit_return() -> Terminator {
        let span = test_span();

        Terminator::Return(Return { value: Value::unit(span), span })
    }

    fn phi_register() -> Register {
        Register {
            assignment: Assignment::Phi(Phi {
                incoming_values: FxHashMap::default(),
                r#type: Type::unit(),
            }),
            span: test_span(),
        }
    }

    fn function_ir(ir: IR) -> FunctionIR {
        let mut ir_map = IRMap::new();
        let root_ir_id = ir_map.new_ir(IRWithContext::new(ir, IRContext::Root));

        FunctionIR::new(
            HandlingScopes::default(),
            ir_map,
            ClosureParametersMap::default(),
            CapturesMap::default(),
            root_ir_id,
        )
    }

    #[test]
    fn missing_register_assignment_panics() {
        let mut ir = IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        };

        let _register_id = ir.values.registers.insert(phi_register());

        assert!(ir.control_flow_graph.insert_terminator(
            ir.control_flow_graph.entry_block_id(),
            unit_return()
        ));

        let panic = std::panic::catch_unwind(|| {
            verify_function_ir(&function_ir(ir));
        });

        assert!(panic.is_err(), "expected missing register assignment");
    }

    #[test]
    fn phi_predecessor_integrity_panics() {
        let mut ir = IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        };

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
                r#type: Type::unit(),
            }),
            span: test_span(),
        });

        assert!(ir.control_flow_graph[join_block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id,
            })
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );

        let panic = std::panic::catch_unwind(|| {
            verify_function_ir(&function_ir(ir));
        });

        assert!(panic.is_err(), "expected phi predecessor integrity failure");
    }

    #[test]
    fn critical_edge_panics() {
        let mut ir = IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        };

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

        let panic = std::panic::catch_unwind(|| {
            verify_function_ir(&function_ir(ir));
        });

        assert!(panic.is_err(), "expected critical edge failure");
    }

    #[test]
    fn valid_ir_passes_verification() {
        let mut ir = IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        };

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let left_block_id = ir.control_flow_graph.new_block();
        let right_block_id = ir.control_flow_graph.new_block();
        let split_block_id = ir.control_flow_graph.new_block();
        let join_block_id = ir.control_flow_graph.new_block();

        let register_id = ir.values.registers.insert(phi_register());

        assert!(ir.control_flow_graph[entry_block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id
            }),
        ));

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
                target: split_block_id,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            split_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join_block_id,
            })),
        ));
        assert!(
            ir.control_flow_graph
                .insert_terminator(join_block_id, unit_return())
        );

        verify_function_ir(&function_ir(ir));
    }
}
