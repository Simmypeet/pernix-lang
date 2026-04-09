//! A reusable monotone data-flow solver for IR control-flow graphs.

use std::collections::VecDeque;

use pernixc_arena::ID;
use pernixc_hash::{FxHashMap, FxHashSet};

use crate::{
    IR,
    control_flow_graph::{Block, Point},
    instruction::{Instruction, Terminator},
};

/// The direction that the data-flow analysis propagates facts through the CFG.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnalysisDirection {
    /// Facts flow from predecessors to successors.
    Forward,

    /// Facts flow from successors to predecessors.
    Backward,
}

/// Describes a specific monotone data-flow analysis.
pub trait DataflowAnalysis {
    /// The fact propagated through the CFG.
    type Fact: Clone + Eq;

    /// An error produced by the analysis.
    type Error;

    /// The direction that the analysis flows through the CFG.
    const DIRECTION: AnalysisDirection;

    /// Returns the lattice bottom for this analysis.
    fn bottom(&self, ir: &IR) -> Self::Fact;

    /// Returns the boundary fact used to seed the analysis.
    fn boundary(&self, ir: &IR) -> Self::Fact;

    /// Merges `incoming` into `into`.
    ///
    /// Returns `true` if `into` changed as part of the merge.
    fn merge(
        &mut self,
        into: &mut Self::Fact,
        incoming: &Self::Fact,
        block_id: ID<Block>,
    ) -> Result<bool, Self::Error>;

    /// Applies the transfer function of a single instruction.
    fn transfer_instruction(
        &mut self,
        fact: &mut Self::Fact,
        point: Point,
        instruction: &Instruction,
    ) -> Result<(), Self::Error>;

    /// Applies the transfer function of a block terminator.
    fn transfer_terminator(
        &mut self,
        fact: &mut Self::Fact,
        block_id: ID<Block>,
        terminator: Option<&Terminator>,
    ) -> Result<(), Self::Error>;
}

/// The solved block entry and exit facts for a reachable CFG.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Solution<Fact> {
    reachable_block_ids: Vec<ID<Block>>,
    entry_facts_by_block_id: FxHashMap<ID<Block>, Fact>,
    exit_facts_by_block_id: FxHashMap<ID<Block>, Fact>,
}

impl<Fact> Solution<Fact> {
    /// Returns the reachable block IDs in stable traversal order.
    #[must_use]
    pub fn reachable_block_ids(&self) -> &[ID<Block>] {
        &self.reachable_block_ids
    }

    /// Returns the entry fact for the given reachable block.
    #[must_use]
    pub fn entry_fact(&self, block_id: ID<Block>) -> Option<&Fact> {
        self.entry_facts_by_block_id.get(&block_id)
    }

    /// Returns the exit fact for the given reachable block.
    #[must_use]
    pub fn exit_fact(&self, block_id: ID<Block>) -> Option<&Fact> {
        self.exit_facts_by_block_id.get(&block_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BlockFacts<Fact> {
    entry: Option<Fact>,
    exit: Option<Fact>,
}

/// Solves the given data-flow analysis on a reachable subset of the CFG.
pub fn solve<A: DataflowAnalysis>(
    analysis: &mut A,
    ir: &IR,
) -> Result<Solution<A::Fact>, A::Error> {
    let reachable_block_ids = ir.control_flow_graph.reachable_block_ids();

    let mut facts = reachable_block_ids
        .iter()
        .copied()
        .map(|block_id| (block_id, BlockFacts { entry: None, exit: None }))
        .collect::<FxHashMap<_, _>>();

    let reachable =
        reachable_block_ids.iter().copied().collect::<FxHashSet<_>>();

    match A::DIRECTION {
        AnalysisDirection::Forward => solve_forward(
            analysis,
            ir,
            &reachable_block_ids,
            &reachable,
            &mut facts,
        )?,
        AnalysisDirection::Backward => solve_backward(
            analysis,
            ir,
            &reachable_block_ids,
            &reachable,
            &mut facts,
        )?,
    }

    let bottom = analysis.bottom(ir);
    let mut entry_facts_by_block_id = FxHashMap::default();
    let mut exit_facts_by_block_id = FxHashMap::default();

    for block_id in &reachable_block_ids {
        let block_facts = facts
            .get(block_id)
            .expect("reachable blocks must have dataflow state");

        entry_facts_by_block_id.insert(
            *block_id,
            block_facts.entry.clone().unwrap_or_else(|| bottom.clone()),
        );
        exit_facts_by_block_id.insert(
            *block_id,
            block_facts.exit.clone().unwrap_or_else(|| bottom.clone()),
        );
    }

    Ok(Solution {
        reachable_block_ids,
        entry_facts_by_block_id,
        exit_facts_by_block_id,
    })
}

fn solve_forward<A: DataflowAnalysis>(
    analysis: &mut A,
    ir: &IR,
    _reachable_block_ids: &[ID<Block>],
    reachable: &FxHashSet<ID<Block>>,
    facts: &mut FxHashMap<ID<Block>, BlockFacts<A::Fact>>,
) -> Result<(), A::Error> {
    let mut queued = FxHashSet::default();
    let mut worklist = VecDeque::new();
    let entry_block_id = ir.control_flow_graph.entry_block_id();

    worklist.push_back(entry_block_id);
    queued.insert(entry_block_id);

    while let Some(block_id) = worklist.pop_front() {
        queued.remove(&block_id);

        let Some(block_facts) = facts.get(&block_id) else {
            continue;
        };

        let block = &ir.control_flow_graph[block_id];

        let Some(entry_fact) =
            compute_forward_entry_fact(analysis, ir, facts, block_id, block)?
        else {
            continue;
        };

        let mut exit_fact = entry_fact.clone();

        for (instruction_index, instruction) in
            block.instructions().iter().enumerate()
        {
            analysis.transfer_instruction(
                &mut exit_fact,
                Point { instruction_index, block_id },
                instruction,
            )?;
        }

        analysis.transfer_terminator(
            &mut exit_fact,
            block_id,
            block.terminator(),
        )?;

        let entry_changed = block_facts.entry.as_ref() != Some(&entry_fact);
        let exit_changed = block_facts.exit.as_ref() != Some(&exit_fact);

        if let Some(block_facts) = facts.get_mut(&block_id) {
            block_facts.entry = Some(entry_fact);
            block_facts.exit = Some(exit_fact);
        }

        if !(entry_changed || exit_changed) {
            continue;
        }

        for successor_block_id in
            ir.control_flow_graph.successor_block_ids(block_id)
        {
            if !reachable.contains(&successor_block_id)
                || !queued.insert(successor_block_id)
            {
                continue;
            }

            worklist.push_back(successor_block_id);
        }
    }
    Ok(())
}

fn solve_backward<A: DataflowAnalysis>(
    analysis: &mut A,
    ir: &IR,
    reachable_block_ids: &[ID<Block>],
    reachable: &FxHashSet<ID<Block>>,
    facts: &mut FxHashMap<ID<Block>, BlockFacts<A::Fact>>,
) -> Result<(), A::Error> {
    let mut queued = FxHashSet::default();
    let mut worklist = VecDeque::new();

    for block_id in reachable_block_ids
        .iter()
        .copied()
        .filter(|block_id| is_terminal_block(&ir.control_flow_graph[*block_id]))
    {
        worklist.push_back(block_id);
        queued.insert(block_id);
    }

    while let Some(block_id) = worklist.pop_front() {
        queued.remove(&block_id);

        let Some(block_facts) = facts.get(&block_id) else {
            continue;
        };

        let block = &ir.control_flow_graph[block_id];

        let Some(exit_fact) =
            compute_backward_exit_fact(analysis, ir, facts, block_id, block)?
        else {
            continue;
        };

        let mut entry_fact = exit_fact.clone();
        analysis.transfer_terminator(
            &mut entry_fact,
            block_id,
            block.terminator(),
        )?;

        for (instruction_index, instruction) in
            block.instructions().iter().enumerate().rev()
        {
            analysis.transfer_instruction(
                &mut entry_fact,
                Point { instruction_index, block_id },
                instruction,
            )?;
        }

        let entry_changed = block_facts.entry.as_ref() != Some(&entry_fact);
        let exit_changed = block_facts.exit.as_ref() != Some(&exit_fact);

        if let Some(block_facts) = facts.get_mut(&block_id) {
            block_facts.entry = Some(entry_fact);
            block_facts.exit = Some(exit_fact);
        }

        if !(entry_changed || exit_changed) {
            continue;
        }

        for predecessor_block_id in block.predecessors() {
            if !reachable.contains(predecessor_block_id)
                || !queued.insert(*predecessor_block_id)
            {
                continue;
            }

            worklist.push_back(*predecessor_block_id);
        }
    }

    Ok(())
}

fn compute_forward_entry_fact<A: DataflowAnalysis>(
    analysis: &mut A,
    ir: &IR,
    facts: &FxHashMap<ID<Block>, BlockFacts<A::Fact>>,
    block_id: ID<Block>,
    block: &Block,
) -> Result<Option<A::Fact>, A::Error> {
    if block.is_entry() {
        return Ok(Some(analysis.boundary(ir)));
    }

    let mut entry_fact = None;

    for predecessor_block_id in block.predecessors() {
        let Some(incoming_fact) = facts
            .get(predecessor_block_id)
            .and_then(|facts| facts.exit.as_ref())
        else {
            continue;
        };

        if let Some(existing) = &mut entry_fact {
            analysis.merge(existing, incoming_fact, block_id)?;
        } else {
            entry_fact = Some(incoming_fact.clone());
        }
    }

    Ok(entry_fact)
}

fn compute_backward_exit_fact<A: DataflowAnalysis>(
    analysis: &mut A,
    ir: &IR,
    facts: &FxHashMap<ID<Block>, BlockFacts<A::Fact>>,
    block_id: ID<Block>,
    block: &Block,
) -> Result<Option<A::Fact>, A::Error> {
    if is_terminal_block(block) {
        return Ok(Some(analysis.boundary(ir)));
    }

    let mut exit_fact = None;

    for successor_block_id in
        ir.control_flow_graph.successor_block_ids(block_id)
    {
        let Some(incoming_fact) = facts
            .get(&successor_block_id)
            .and_then(|facts| facts.entry.as_ref())
        else {
            continue;
        };

        if let Some(existing) = &mut exit_fact {
            analysis.merge(existing, incoming_fact, block_id)?;
        } else {
            exit_fact = Some(incoming_fact.clone());
        }
    }

    Ok(exit_fact)
}

const fn is_terminal_block(block: &Block) -> bool {
    matches!(
        block.terminator(),
        None | Some(Terminator::Return(_) | Terminator::Panic)
    )
}

#[cfg(test)]
mod test {
    use std::convert::Infallible;

    use pernixc_arena::ID;
    use pernixc_hash::FxHashMap;
    use pernixc_lexical::tree::{
        OffsetMode, ROOT_BRANCH_ID, RelativeLocation, RelativeSpan,
    };
    use pernixc_source_file::{LocalSourceID, Span};
    use pernixc_target::TargetID;
    use pernixc_term::r#type::Type;

    use super::{AnalysisDirection, DataflowAnalysis, solve};
    use crate::{
        Values,
        control_flow_graph::{Block, ControlFlowGraph, Point},
        instruction::{
            ConditionalJump, Instruction, Jump, RegisterAssignment, Return,
            Terminator, UnconditionalJump,
        },
        ir::IR,
        scope,
        value::{
            Value,
            register::{Assignment, Phi, Register},
        },
    };

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

    fn empty_ir() -> IR {
        IR {
            values: Values::default(),
            control_flow_graph: ControlFlowGraph::default(),
            scope_tree: scope::Tree::default(),
        }
    }

    #[derive(Debug, Default)]
    struct ForwardToyAnalysis;

    impl DataflowAnalysis for ForwardToyAnalysis {
        type Fact = usize;
        type Error = Infallible;

        const DIRECTION: AnalysisDirection = AnalysisDirection::Forward;

        fn bottom(&self, _: &IR) -> Self::Fact { 0 }

        fn boundary(&self, _: &IR) -> Self::Fact { 0 }

        fn merge(
            &mut self,
            into: &mut Self::Fact,
            incoming: &Self::Fact,
            _: ID<Block>,
        ) -> Result<bool, Self::Error> {
            let merged = (*into).max(*incoming);
            let changed = merged != *into;
            *into = merged;
            Ok(changed)
        }

        fn transfer_instruction(
            &mut self,
            fact: &mut Self::Fact,
            _: Point,
            _: &Instruction,
        ) -> Result<(), Self::Error> {
            *fact += 1;
            Ok(())
        }

        fn transfer_terminator(
            &mut self,
            _: &mut Self::Fact,
            _: ID<Block>,
            _: Option<&Terminator>,
        ) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[derive(Debug, Default)]
    struct BackwardToyAnalysis;

    impl DataflowAnalysis for BackwardToyAnalysis {
        type Fact = Vec<&'static str>;
        type Error = Infallible;

        const DIRECTION: AnalysisDirection = AnalysisDirection::Backward;

        fn bottom(&self, _: &IR) -> Self::Fact { Vec::new() }

        fn boundary(&self, _: &IR) -> Self::Fact { vec!["boundary"] }

        fn merge(
            &mut self,
            into: &mut Self::Fact,
            incoming: &Self::Fact,
            _: ID<Block>,
        ) -> Result<bool, Self::Error> {
            if *into == *incoming {
                return Ok(false);
            }

            if into.len() >= incoming.len() {
                return Ok(false);
            }

            *into = incoming.clone();
            Ok(true)
        }

        fn transfer_instruction(
            &mut self,
            fact: &mut Self::Fact,
            point: Point,
            _: &Instruction,
        ) -> Result<(), Self::Error> {
            if point.instruction_index == 0 {
                fact.push("inst0");
            } else {
                fact.push("inst1");
            }

            Ok(())
        }

        fn transfer_terminator(
            &mut self,
            fact: &mut Self::Fact,
            _: ID<Block>,
            terminator: Option<&Terminator>,
        ) -> Result<(), Self::Error> {
            if terminator.is_some() {
                fact.push("term");
            }

            Ok(())
        }
    }

    #[derive(Debug, Default)]
    struct MergeChangeAnalysis {
        transfer_counts: FxHashMap<ID<Block>, usize>,
    }

    impl DataflowAnalysis for MergeChangeAnalysis {
        type Fact = usize;
        type Error = Infallible;

        const DIRECTION: AnalysisDirection = AnalysisDirection::Forward;

        fn bottom(&self, _: &IR) -> Self::Fact { 0 }

        fn boundary(&self, _: &IR) -> Self::Fact { 1 }

        fn merge(
            &mut self,
            into: &mut Self::Fact,
            incoming: &Self::Fact,
            _: ID<Block>,
        ) -> Result<bool, Self::Error> {
            let changed = *into != *incoming;
            *into = *incoming;
            Ok(changed)
        }

        fn transfer_instruction(
            &mut self,
            _: &mut Self::Fact,
            point: Point,
            _: &Instruction,
        ) -> Result<(), Self::Error> {
            *self.transfer_counts.entry(point.block_id).or_default() += 1;
            Ok(())
        }

        fn transfer_terminator(
            &mut self,
            _: &mut Self::Fact,
            _: ID<Block>,
            _: Option<&Terminator>,
        ) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    fn add_instruction(ir: &mut IR, block_id: ID<Block>) {
        let register_id = ir.values.registers.insert(Register {
            assignment: Assignment::Phi(Phi {
                incoming_values: FxHashMap::default(),
                r#type: Type::unit(),
            }),
            span: test_span(),
        });

        assert!(ir.control_flow_graph[block_id].add_instruction(
            Instruction::RegisterAssignment(RegisterAssignment {
                id: register_id
            }),
        ));
    }

    #[test]
    fn forward_analysis_propagates_and_merges() {
        let mut ir = empty_ir();

        let entry = ir.control_flow_graph.entry_block_id();
        let left = ir.control_flow_graph.new_block();
        let right = ir.control_flow_graph.new_block();
        let join = ir.control_flow_graph.new_block();

        add_instruction(&mut ir, entry);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: Value::unit(test_span()),
                true_target: left,
                false_target: right,
            })),
        ));
        add_instruction(&mut ir, left);
        assert!(ir.control_flow_graph.insert_terminator(
            left,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join,
            })),
        ));
        add_instruction(&mut ir, right);
        assert!(ir.control_flow_graph.insert_terminator(
            right,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: join,
            })),
        ));
        add_instruction(&mut ir, join);
        assert!(ir.control_flow_graph.insert_terminator(join, unit_return()));

        let solution = solve(&mut ForwardToyAnalysis, &ir).unwrap();

        assert_eq!(solution.entry_fact(entry), Some(&0));
        assert_eq!(solution.exit_fact(entry), Some(&1));
        assert_eq!(solution.entry_fact(left), Some(&1));
        assert_eq!(solution.exit_fact(left), Some(&2));
        assert_eq!(solution.entry_fact(right), Some(&1));
        assert_eq!(solution.exit_fact(right), Some(&2));
        assert_eq!(solution.entry_fact(join), Some(&2));
        assert_eq!(solution.exit_fact(join), Some(&3));
    }

    #[test]
    fn backward_analysis_runs_terminator_before_reverse_instructions() {
        let mut ir = empty_ir();

        let entry = ir.control_flow_graph.entry_block_id();
        add_instruction(&mut ir, entry);
        add_instruction(&mut ir, entry);

        assert!(
            ir.control_flow_graph.insert_terminator(entry, Terminator::Panic)
        );

        let solution = solve(&mut BackwardToyAnalysis, &ir).unwrap();

        assert_eq!(solution.exit_fact(entry), Some(&vec!["boundary"]));
        assert_eq!(
            solution.entry_fact(entry),
            Some(&vec!["boundary", "term", "inst1", "inst0"]),
        );
    }

    #[test]
    fn worklist_reenqueues_only_when_facts_change() {
        let mut ir = empty_ir();

        let entry = ir.control_flow_graph.entry_block_id();
        let loop_body = ir.control_flow_graph.new_block();

        add_instruction(&mut ir, entry);

        assert!(ir.control_flow_graph.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: loop_body,
            })),
        ));
        add_instruction(&mut ir, loop_body);
        assert!(ir.control_flow_graph.insert_terminator(
            loop_body,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: loop_body,
            })),
        ));

        let mut analysis = MergeChangeAnalysis::default();
        let solution = solve(&mut analysis, &ir).unwrap();

        assert_eq!(solution.entry_fact(entry), Some(&1));
        assert_eq!(solution.exit_fact(entry), Some(&1));
        assert_eq!(solution.entry_fact(loop_body), Some(&1));
        assert_eq!(solution.exit_fact(loop_body), Some(&1));
        assert_eq!(analysis.transfer_counts.get(&entry), Some(&1));
        assert_eq!(analysis.transfer_counts.get(&loop_body), Some(&2));
    }
}
