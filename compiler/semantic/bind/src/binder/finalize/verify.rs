use pernixc_ir::FunctionIR;

mod critical_edges;
mod phi_nodes;
mod register_assignments;
mod scope_stack;

pub(super) async fn verify_function_ir(function_ir: &FunctionIR) {
    let mut violation_count = 0;

    for (ir_id, ir_with_context) in function_ir.ir_with_contexts() {
        let ir = ir_with_context.ir();

        violation_count += phi_nodes::verify_phi_nodes(ir_id, ir);
        violation_count +=
            register_assignments::verify_register_assignments(ir_id, ir);
        violation_count += critical_edges::verify_critical_edges(ir_id, ir);
        violation_count += scope_stack::verify_scope_stack(ir_id, ir).await;
    }

    assert!(
        violation_count == 0,
        "found {violation_count} IR verification violation(s)"
    );
}

#[cfg(test)]
pub(super) mod test {
    use std::num::NonZeroUsize;

    use pernixc_arena::ID;
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
            ScopePop, ScopePush, Terminator, UnconditionalJump,
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

    pub(super) fn test_span() -> RelativeSpan {
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

    pub(super) fn unit_return() -> Terminator {
        let span = test_span();

        Terminator::Return(Return { value: Value::unit(span), span })
    }

    pub(super) fn phi_register() -> Register {
        Register {
            assignment: Assignment::Phi(Phi {
                incoming_values: FxHashMap::default(),
                r#type: Type::unit(),
            }),
            span: test_span(),
        }
    }

    pub(super) fn empty_ir() -> IR {
        IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        }
    }

    pub(super) fn function_ir(ir: IR) -> FunctionIR {
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

    pub(super) fn new_scope(ir: &mut IR) -> ID<scope::Scope> {
        ir.scope_tree
            .new_child_branch(
                ir.scope_tree.root_scope_id(),
                NonZeroUsize::new(1).unwrap(),
            )
            .unwrap()[0]
    }

    pub(super) async fn assert_verification_panics(ir: IR) {
        let handle = tokio::spawn(async move {
            let function_ir = function_ir(ir);
            verify_function_ir(&function_ir).await;
        });

        let join_error = handle.await.expect_err("expected verification panic");
        assert!(join_error.is_panic(), "expected verification panic");
    }

    pub(super) async fn assert_verification_passes(ir: IR) {
        verify_function_ir(&function_ir(ir)).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn valid_ir_passes_verification() {
        let mut ir = empty_ir();

        let entry_block_id = ir.control_flow_graph.entry_block_id();
        let left_block_id = ir.control_flow_graph.new_block();
        let right_block_id = ir.control_flow_graph.new_block();
        let split_block_id = ir.control_flow_graph.new_block();
        let join_block_id = ir.control_flow_graph.new_block();

        let register_id = ir.values.registers.insert(phi_register());
        let scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph[entry_block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id
            }),
        ));
        assert!(
            ir.control_flow_graph[entry_block_id]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph[entry_block_id]
                .add_instruction(Instruction::ScopePop(ScopePop(scope_id)),)
        );

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

        assert_verification_passes(ir).await;
    }
}
