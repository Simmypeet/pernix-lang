use std::convert::Infallible;

use pernixc_arena::ID;
use pernixc_hash::FxHashSet;
use pernixc_ir::{
    IRWithContext,
    control_flow_graph::{Block, ControlFlowEdge, Point},
    dataflow::{self, DataflowProblem, Direction, JoinLattice},
    instruction::{Instruction, ScopePop, ScopePush, Terminator},
    ir::IR,
    scope::Scope,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeStackFact {
    Valid(Vec<ID<Scope>>),
    Invalid,
}

impl ScopeStackFact {
    fn valid_stack(&self) -> Option<&[ID<Scope>]> {
        match self {
            Self::Valid(stack) => Some(stack),
            Self::Invalid => None,
        }
    }
}

impl JoinLattice for ScopeStackFact {
    async fn join(&mut self, other: &Self) -> bool {
        match (self.clone(), other) {
            (Self::Invalid, _) => false,
            (_, Self::Invalid) => {
                *self = Self::Invalid;
                true
            }
            (Self::Valid(this), Self::Valid(other)) => {
                if this == *other {
                    false
                } else {
                    *self = Self::Invalid;
                    true
                }
            }
        }
    }
}

impl ScopeStackProblem<'_> {
    fn report_duplicate_push(&mut self, point: Point, scope_id: ID<Scope>) {
        if self.push_pop_violations.insert(point) {
            tracing::error!(
                "IR verification failed for {ir_id:?}: duplicate scope push \
                 of {scope_id:?} at {point:?}",
                ir_id = self.ir_id,
            );
        }
    }

    fn report_pop_mismatch(
        &mut self,
        point: Point,
        expected: Option<ID<Scope>>,
        found: ID<Scope>,
    ) {
        if self.push_pop_violations.insert(point) {
            tracing::error!(
                "IR verification failed for {ir_id:?}: scope pop at {point:?} \
                 expected top {expected:?}, found {found:?}",
                ir_id = self.ir_id,
            );
        }
    }

    fn report_non_empty_return(
        &mut self,
        block_id: ID<Block>,
        stack: &[ID<Scope>],
    ) {
        if self.return_violations.insert(block_id) {
            tracing::error!(
                "IR verification failed for {ir_id:?}: return block \
                 {block_id:?} exits with non-empty scope stack {stack:?}",
                ir_id = self.ir_id,
            );
        }
    }

    fn apply_instruction(
        &mut self,
        point: Point,
        instruction: &Instruction,
        state: &mut Option<ScopeStackFact>,
    ) {
        let Some(state) = state else {
            return;
        };

        match instruction {
            Instruction::ScopePush(ScopePush(scope_id)) => {
                let mut invalid = false;

                if let ScopeStackFact::Valid(stack) = state {
                    if stack.contains(scope_id) {
                        invalid = true;
                    } else {
                        stack.push(*scope_id);
                    }
                }

                if invalid {
                    self.report_duplicate_push(point, *scope_id);
                    *state = ScopeStackFact::Invalid;
                }
            }

            Instruction::ScopePop(ScopePop(scope_id)) => {
                let mut expected = None;
                let mut invalid = false;

                if let ScopeStackFact::Valid(stack) = state {
                    if stack.last().copied() == Some(*scope_id) {
                        stack.pop();
                    } else {
                        expected = stack.last().copied();
                        invalid = true;
                    }
                }

                if invalid {
                    self.report_pop_mismatch(point, expected, *scope_id);
                    *state = ScopeStackFact::Invalid;
                }
            }

            _ => {}
        }
    }

    fn apply_terminator(
        &mut self,
        block_id: ID<Block>,
        terminator: &Terminator,
        state: &mut Option<ScopeStackFact>,
    ) {
        if let Terminator::Return(_) = terminator
            && let Some(ScopeStackFact::Valid(stack)) = state
            && !stack.is_empty()
        {
            self.report_non_empty_return(block_id, stack);
        }
    }
}

#[derive(Debug)]
struct ScopeStackProblem<'a> {
    ir_id: ID<IRWithContext>,
    push_pop_violations: FxHashSet<Point>,
    return_violations: FxHashSet<ID<Block>>,
    _ir: &'a IR,
}

impl DataflowProblem for ScopeStackProblem<'_> {
    type JoinLattice = Option<ScopeStackFact>;
    type Error = Infallible;

    const DIRECTION: Direction = Direction::Forward;
    const EDGE_SENSITIVE: bool = false;

    async fn bottom(
        &mut self,
        _: ID<Block>,
    ) -> Result<Self::JoinLattice, Self::Error> {
        Ok(None)
    }

    async fn boundary_facts(
        &mut self,
        _: ID<Block>,
    ) -> Result<Self::JoinLattice, Self::Error> {
        Ok(Some(ScopeStackFact::Valid(Vec::new())))
    }

    async fn transfer_instruction(
        &mut self,
        point: Point,
        instruction: &Instruction,
        state: &mut Self::JoinLattice,
    ) -> Result<(), Self::Error> {
        self.apply_instruction(point, instruction, state);
        Ok(())
    }

    async fn transfer_terminator(
        &mut self,
        block_id: ID<Block>,
        terminator: &Terminator,
        state: &mut Self::JoinLattice,
    ) -> Result<(), Self::Error> {
        self.apply_terminator(block_id, terminator, state);
        Ok(())
    }

    async fn transfer_edge(
        &mut self,
        _: &ControlFlowEdge,
        _: &mut Self::JoinLattice,
    ) -> Result<(), Self::Error> {
        unreachable!(
            "scope stack verification does not use edge-sensitive transfer"
        );
    }
}

pub(super) async fn verify_scope_stack(
    ir_id: ID<IRWithContext>,
    ir: &IR,
) -> usize {
    let mut problem = ScopeStackProblem {
        ir_id,
        push_pop_violations: FxHashSet::default(),
        return_violations: FxHashSet::default(),
        _ir: ir,
    };

    let solution =
        dataflow::solve(&mut problem, &ir.control_flow_graph).await.unwrap();

    let mut merge_violations = FxHashSet::default();

    for (block_id, block) in ir.control_flow_graph.traverse() {
        if solution.block_entry(block_id).is_none()
            || block.predecessors().len() <= 1
        {
            continue;
        }

        let mut seen_valid_stacks = FxHashSet::default();

        for predecessor_id in block.predecessors().iter().copied() {
            let Some(state) = solution.block_exit(predecessor_id) else {
                continue;
            };

            let Some(stack) =
                state.as_ref().and_then(ScopeStackFact::valid_stack)
            else {
                continue;
            };

            if !seen_valid_stacks.insert(stack.to_vec())
                || seen_valid_stacks.len() <= 1
            {
                continue;
            }

            if merge_violations.insert(block_id) {
                tracing::error!(
                    "IR verification failed for {ir_id:?}: merge block \
                     {block_id:?} receives incompatible scope stacks",
                );
            }

            break;
        }
    }

    problem.push_pop_violations.len()
        + problem.return_violations.len()
        + merge_violations.len()
}

#[cfg(test)]
mod test {
    use pernixc_arena::ID;
    use pernixc_ir::{
        instruction::{
            ConditionalJump, Instruction, Jump, ScopePop, ScopePush,
            Terminator, UnconditionalJump,
        },
        value::Value,
    };

    use super::verify_scope_stack;
    use crate::binder::finalize::verify::test::{
        assert_verification_panics, assert_verification_passes, empty_ir,
        new_scope, test_span, unit_return,
    };

    #[tokio::test(flavor = "current_thread")]
    async fn balanced_push_pop_return_passes() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePop(ScopePop(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(entry, unit_return()));

        assert_verification_passes(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn pop_wrong_scope_id_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let first_scope_id = new_scope(&mut ir);
        let second_scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph[entry].add_instruction(
            Instruction::ScopePush(ScopePush(first_scope_id)),
        ));
        assert!(
            ir.control_flow_graph[entry].add_instruction(
                Instruction::ScopePop(ScopePop(second_scope_id)),
            )
        );
        assert!(ir.control_flow_graph.insert_terminator(entry, unit_return()));

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn pop_empty_stack_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePop(ScopePop(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(entry, unit_return()));

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn duplicate_push_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph.insert_terminator(entry, Terminator::Panic)
        );

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn merge_different_predecessor_stacks_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let left = ir.control_flow_graph.new_block();
        let right = ir.control_flow_graph.new_block();
        let merge = ir.control_flow_graph.new_block();
        let scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: left,
                false_target: right,
            })),
        ));
        assert!(
            ir.control_flow_graph[left]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(
            left,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            right,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(
            ir.control_flow_graph.insert_terminator(merge, Terminator::Panic)
        );

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn merge_equal_predecessor_stacks_passes() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let left = ir.control_flow_graph.new_block();
        let right = ir.control_flow_graph.new_block();
        let merge = ir.control_flow_graph.new_block();
        let scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: left,
                false_target: right,
            })),
        ));

        for block_id in [left, right] {
            assert!(
                ir.control_flow_graph[block_id].add_instruction(
                    Instruction::ScopePush(ScopePush(scope_id)),
                )
            );
            assert!(ir.control_flow_graph[block_id].add_instruction(
                Instruction::ScopePop(ScopePop(scope_id)),
            ));
            assert!(ir.control_flow_graph.insert_terminator(
                block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: merge,
                })),
            ));
        }

        assert!(ir.control_flow_graph.insert_terminator(merge, unit_return()));

        assert_verification_passes(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn return_with_non_empty_stack_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(entry, unit_return()));

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn panic_with_non_empty_stack_passes() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph.insert_terminator(entry, Terminator::Panic)
        );

        assert_verification_passes(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn loop_reentry_after_matching_pop_passes() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let header = ir.control_flow_graph.new_block();
        let body = ir.control_flow_graph.new_block();
        let exit = ir.control_flow_graph.new_block();
        let scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: header,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(
            header,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: body,
                false_target: exit,
            })),
        ));
        assert!(
            ir.control_flow_graph[body]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(
            ir.control_flow_graph[body]
                .add_instruction(Instruction::ScopePop(ScopePop(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(
            body,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: header,
            })),
        ));
        assert!(ir.control_flow_graph.insert_terminator(exit, unit_return()));

        assert_verification_passes(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn loop_with_unbounded_scope_growth_panics() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let looping = ir.control_flow_graph.new_block();
        let exit = ir.control_flow_graph.new_block();
        let scope_id = new_scope(&mut ir);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: looping,
            })),
        ));
        assert!(
            ir.control_flow_graph[looping]
                .add_instruction(Instruction::ScopePush(ScopePush(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(
            looping,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: looping,
                false_target: exit,
            })),
        ));
        assert!(
            ir.control_flow_graph.insert_terminator(exit, Terminator::Panic)
        );

        assert_verification_panics(ir).await;
    }

    #[tokio::test(flavor = "current_thread")]
    async fn direct_pass_reports_single_violation() {
        let mut ir = empty_ir();
        let entry = ir.control_flow_graph.entry_block_id();
        let scope_id = new_scope(&mut ir);

        assert!(
            ir.control_flow_graph[entry]
                .add_instruction(Instruction::ScopePop(ScopePop(scope_id)),)
        );
        assert!(ir.control_flow_graph.insert_terminator(entry, unit_return()));

        assert_eq!(verify_scope_stack(ID::new(0), &ir).await, 1);
    }
}
