//! Generic dataflow solving utilities over the control flow graph.

use std::{collections::VecDeque, future::Future};

use pernixc_arena::ID;
use pernixc_hash::{FxHashMap, FxHashSet};

use crate::{
    control_flow_graph::{Block, ControlFlowEdge, ControlFlowGraph, Point},
    instruction::{Instruction, Terminator},
};

/// Represents the direction in which a dataflow problem propagates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    /// Facts flow from block entry to block exit.
    Forward,

    /// Facts flow from block exit to block entry.
    Backward,
}

/// A join-semilattice used by the dataflow solver.
pub trait JoinLattice: Clone + Eq {
    /// Joins `other` into `self`, returning whether `self` changed.
    fn join<'a>(
        &'a mut self,
        other: &'a Self,
    ) -> impl Future<Output = bool> + Send + use<'a, Self>;
}

impl<T: JoinLattice + Send + Sync> JoinLattice for Option<T> {
    #[allow(clippy::manual_async_fn)]
    fn join<'a>(
        &'a mut self,
        other: &'a Self,
    ) -> impl Future<Output = bool> + Send + use<'a, T> {
        async move {
            match other {
                None => false,

                Some(other) => match self {
                    None => {
                        *self = Some(other.clone());
                        true
                    }

                    Some(this) => this.join(other).await,
                },
            }
        }
    }
}

/// Describes a dataflow problem that can be solved over a CFG.
pub trait DataflowProblem {
    /// The lattice carried through the analysis.
    type JoinLattice: JoinLattice;

    /// The error returned by transfer or initialization routines.
    type Error;

    /// The propagation direction of the analysis.
    const DIRECTION: Direction;

    /// Determines whether the solver tracks distinct facts for each CFG edge.
    ///
    /// When this is `false`, facts flow directly between neighboring blocks
    /// without invoking [`Self::transfer_edge`], and edge states are omitted
    /// from the final solution to reduce memory usage.
    const EDGE_SENSITIVE: bool;

    /// Creates the lattice bottom used to initialize the given block.
    fn bottom(
        &self,
        block_id: ID<Block>,
    ) -> impl Future<Output = Result<Self::JoinLattice, Self::Error>>
    + Send
    + use<'_, Self>;

    /// Creates the facts used to initialize boundary blocks.
    fn boundary_facts(
        &self,
        block_id: ID<Block>,
    ) -> impl Future<Output = Result<Self::JoinLattice, Self::Error>>
    + Send
    + use<'_, Self>;

    /// Applies the instruction transfer function in-place.
    fn transfer_instruction<'a>(
        &'a self,
        point: Point,
        instruction: &'a Instruction,
        state: &'a mut Self::JoinLattice,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + use<'a, Self>;

    /// Applies the terminator transfer function in-place.
    fn transfer_terminator<'a>(
        &'a self,
        block_id: ID<Block>,
        terminator: &'a Terminator,
        state: &'a mut Self::JoinLattice,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + use<'a, Self>;

    /// Applies the edge transfer function in-place.
    ///
    /// This is invoked only when [`Self::EDGE_SENSITIVE`] is `true`.
    fn transfer_edge<'a>(
        &'a self,
        edge: &'a ControlFlowEdge,
        state: &'a mut Self::JoinLattice,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + use<'a, Self>;
}

/// Stores the solved dataflow facts for reachable CFG blocks and edges.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataflowSolution<L> {
    edge_sensitive: bool,
    reachable_blocks: Vec<ID<Block>>,
    edges: Vec<ControlFlowEdge>,
    block_entries: FxHashMap<ID<Block>, L>,
    block_exits: FxHashMap<ID<Block>, L>,
    edge_states: Option<FxHashMap<ControlFlowEdge, L>>,
}

impl<L> DataflowSolution<L> {
    /// Returns the solved entry fact for the given block.
    #[must_use]
    pub fn block_entry(&self, block_id: ID<Block>) -> Option<&L> {
        self.block_entries.get(&block_id)
    }

    /// Returns the solved exit fact for the given block.
    #[must_use]
    pub fn block_exit(&self, block_id: ID<Block>) -> Option<&L> {
        self.block_exits.get(&block_id)
    }

    /// Returns the solved state for the given control-flow edge.
    ///
    /// # Panics
    ///
    /// Panics if edge sensitivity was disabled for the solved problem.
    #[must_use]
    pub fn edge_state(&self, edge: &ControlFlowEdge) -> Option<&L> {
        assert!(
            self.edge_sensitive,
            "edge states are unavailable when edge sensitivity is disabled",
        );

        self.edge_states.as_ref().unwrap().get(edge)
    }

    /// Returns the reachable blocks that were solved.
    #[must_use]
    pub fn reachable_blocks(
        &self,
    ) -> impl ExactSizeIterator<Item = ID<Block>> + '_ {
        self.reachable_blocks.iter().copied()
    }

    /// Returns the reachable edges that were solved.
    #[must_use]
    pub fn edges(
        &self,
    ) -> impl ExactSizeIterator<Item = &'_ ControlFlowEdge> + '_ {
        self.edges.iter()
    }
}

fn enqueue_block(
    worklist: &mut VecDeque<ID<Block>>,
    queued_blocks: &mut FxHashSet<ID<Block>>,
    block_id: ID<Block>,
) {
    if queued_blocks.insert(block_id) {
        worklist.push_back(block_id);
    }
}

/// Solves a dataflow problem to a fixpoint using Kildall's algorithm.
#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
pub async fn solve<P: DataflowProblem>(
    problem: &P,
    cfg: &ControlFlowGraph,
) -> Result<DataflowSolution<P::JoinLattice>, P::Error> {
    let reachable_blocks = cfg.reachable_block_ids();
    let mut block_entries = FxHashMap::default();
    let mut block_exits = FxHashMap::default();

    let mut edge_states = P::EDGE_SENSITIVE
        .then(FxHashMap::<ControlFlowEdge, P::JoinLattice>::default);
    let mut edges = Vec::new();

    for block_id in reachable_blocks.iter().copied() {
        let bottom = problem.bottom(block_id).await?;
        block_entries.insert(block_id, bottom.clone());
        block_exits.insert(block_id, bottom);
    }

    for block_id in reachable_blocks.iter().copied() {
        for edge in cfg.outgoing_edges(block_id).unwrap() {
            if let Some(edge_states) = &mut edge_states {
                let edge_bottom = match P::DIRECTION {
                    Direction::Forward => {
                        block_exits.get(&edge.source).unwrap().clone()
                    }
                    Direction::Backward => {
                        block_entries.get(&edge.target).unwrap().clone()
                    }
                };

                edge_states.insert(edge, edge_bottom);
            }

            edges.push(edge);
        }
    }

    edges.sort_unstable();

    let mut worklist = VecDeque::new();
    let mut queued_blocks = FxHashSet::default();

    for block_id in cfg.boundary_block_ids(P::DIRECTION) {
        let boundary_facts = problem.boundary_facts(block_id).await?;

        match P::DIRECTION {
            Direction::Forward => {
                *block_entries.get_mut(&block_id).unwrap() = boundary_facts;
            }
            Direction::Backward => {
                *block_exits.get_mut(&block_id).unwrap() = boundary_facts;
            }
        }

        enqueue_block(&mut worklist, &mut queued_blocks, block_id);
    }

    while let Some(block_id) = worklist.pop_front() {
        queued_blocks.remove(&block_id);

        match P::DIRECTION {
            Direction::Forward => {
                let mut candidate_state =
                    block_entries.get(&block_id).unwrap().clone();

                for (point, instruction) in
                    cfg.instructions_with_points(block_id).unwrap()
                {
                    problem
                        .transfer_instruction(
                            point,
                            instruction,
                            &mut candidate_state,
                        )
                        .await?;
                }

                if let Some(terminator) =
                    cfg.get_block(block_id).unwrap().terminator()
                {
                    problem
                        .transfer_terminator(
                            block_id,
                            terminator,
                            &mut candidate_state,
                        )
                        .await?;
                }

                let block_exit = block_exits.get_mut(&block_id).unwrap();
                let block_changed = *block_exit != candidate_state;

                if block_changed {
                    *block_exit = candidate_state.clone();
                }

                if !block_changed {
                    continue;
                }

                for edge in cfg.outgoing_edges(block_id).unwrap() {
                    if let Some(edge_states) = &mut edge_states {
                        let mut edge_state = candidate_state.clone();
                        problem.transfer_edge(&edge, &mut edge_state).await?;

                        let stored_edge_state =
                            edge_states.get_mut(&edge).unwrap();
                        if *stored_edge_state != edge_state {
                            *stored_edge_state = edge_state.clone();

                            if block_entries
                                .get_mut(&edge.target)
                                .unwrap()
                                .join(&edge_state)
                                .await
                            {
                                enqueue_block(
                                    &mut worklist,
                                    &mut queued_blocks,
                                    edge.target,
                                );
                            }
                        }
                    } else if block_entries
                        .get_mut(&edge.target)
                        .unwrap()
                        .join(&candidate_state)
                        .await
                    {
                        enqueue_block(
                            &mut worklist,
                            &mut queued_blocks,
                            edge.target,
                        );
                    }
                }
            }

            Direction::Backward => {
                let mut candidate_state =
                    block_exits.get(&block_id).unwrap().clone();

                if let Some(terminator) =
                    cfg.get_block(block_id).unwrap().terminator()
                {
                    problem
                        .transfer_terminator(
                            block_id,
                            terminator,
                            &mut candidate_state,
                        )
                        .await?;
                }

                for (point, instruction) in
                    cfg.instructions_with_points_rev(block_id).unwrap()
                {
                    problem
                        .transfer_instruction(
                            point,
                            instruction,
                            &mut candidate_state,
                        )
                        .await?;
                }

                let block_entry = block_entries.get_mut(&block_id).unwrap();
                let block_changed = *block_entry != candidate_state;

                if block_changed {
                    *block_entry = candidate_state.clone();
                }

                if !block_changed {
                    continue;
                }

                for edge in cfg.incoming_edges(block_id).unwrap() {
                    if let Some(edge_states) = &mut edge_states {
                        let mut edge_state = candidate_state.clone();
                        problem.transfer_edge(&edge, &mut edge_state).await?;

                        let stored_edge_state =
                            edge_states.get_mut(&edge).unwrap();
                        if *stored_edge_state != edge_state {
                            *stored_edge_state = edge_state.clone();

                            if block_exits
                                .get_mut(&edge.source)
                                .unwrap()
                                .join(&edge_state)
                                .await
                            {
                                enqueue_block(
                                    &mut worklist,
                                    &mut queued_blocks,
                                    edge.source,
                                );
                            }
                        }
                    } else if block_exits
                        .get_mut(&edge.source)
                        .unwrap()
                        .join(&candidate_state)
                        .await
                    {
                        enqueue_block(
                            &mut worklist,
                            &mut queued_blocks,
                            edge.source,
                        );
                    }
                }
            }
        }
    }

    Ok(DataflowSolution {
        edge_sensitive: P::EDGE_SENSITIVE,
        reachable_blocks,
        edges,
        block_entries,
        block_exits,
        edge_states,
    })
}

#[cfg(test)]
#[allow(
    clippy::manual_async_fn,
    clippy::needless_lifetimes,
    clippy::elidable_lifetime_names
)]
mod test {
    use pernixc_arena::ID;
    use pernixc_hash::FxHashSet;
    use pernixc_lexical::tree::{OffsetMode, RelativeLocation, RelativeSpan};
    use pernixc_source_file::LocalSourceID;
    use pernixc_target::{Global, TargetID};

    use super::{
        DataflowProblem, DataflowSolution, Direction, JoinLattice, solve,
    };
    use crate::{
        control_flow_graph::{
            Block, ControlFlowEdge, ControlFlowEdgeKind, ControlFlowGraph,
            Point,
        },
        instruction::{
            ConditionalJump, Instruction, Jump, ScopePush, Terminator,
            UnconditionalJump,
        },
        value::{Value, literal},
    };

    #[derive(Debug, Clone, PartialEq, Eq, Default)]
    struct Facts(FxHashSet<String>);

    impl Facts {
        fn from_labels(labels: impl IntoIterator<Item = &'static str>) -> Self {
            Self(labels.into_iter().map(str::to_string).collect())
        }

        fn contains(&self, label: &str) -> bool { self.0.contains(label) }
    }

    impl JoinLattice for Facts {
        fn join<'a>(
            &'a mut self,
            other: &'a Self,
        ) -> impl std::future::Future<Output = bool> + Send {
            async move {
                let original_len = self.0.len();
                self.0.extend(other.0.iter().cloned());
                self.0.len() != original_len
            }
        }
    }

    fn dummy_span() -> RelativeSpan {
        RelativeSpan::new(
            RelativeLocation {
                offset: 0,
                mode: OffsetMode::Start,
                relative_to: ID::new(0),
            },
            RelativeLocation {
                offset: 0,
                mode: OffsetMode::Start,
                relative_to: ID::new(0),
            },
            Global { target_id: TargetID::TEST, id: LocalSourceID::new(0, 0) },
        )
    }

    fn dummy_condition() -> Value {
        Value::Literal(literal::Literal::Boolean(literal::Boolean {
            value: true,
            span: dummy_span(),
        }))
    }

    fn single_scope_instruction() -> Instruction {
        Instruction::ScopePush(ScopePush(ID::new(1)))
    }

    fn add_instruction(cfg: &mut ControlFlowGraph, block_id: ID<Block>) {
        cfg.get_block_mut(block_id)
            .unwrap()
            .insert_instructions(0, [single_scope_instruction()]);
    }

    struct ForwardBranchProblem {
        entry: ID<Block>,
        left: ID<Block>,
        right: ID<Block>,
        merge: ID<Block>,
    }

    impl DataflowProblem for ForwardBranchProblem {
        type JoinLattice = Facts;
        type Error = ();

        const DIRECTION: Direction = Direction::Forward;
        const EDGE_SENSITIVE: bool = true;

        fn bottom(
            &self,
            _: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move { Ok(Facts::default()) }
        }

        fn boundary_facts<'a>(
            &'a self,
            _: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move { Ok(Facts::from_labels(["seed"])) }
        }

        fn transfer_instruction<'a>(
            &'a self,
            point: Point,
            _: &'a Instruction,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if point.block_id == self.entry {
                    "entry_instruction"
                } else if point.block_id == self.left {
                    "left_instruction"
                } else if point.block_id == self.right {
                    "right_instruction"
                } else if point.block_id == self.merge {
                    "merge_instruction"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_terminator<'a>(
            &'a self,
            block_id: ID<Block>,
            _: &'a Terminator,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if block_id == self.entry {
                    "entry_terminator"
                } else if block_id == self.left {
                    "left_terminator"
                } else if block_id == self.right {
                    "right_terminator"
                } else if block_id == self.merge {
                    "merge_terminator"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_edge<'a>(
            &'a self,
            edge: &'a ControlFlowEdge,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = match edge.kind {
                    ControlFlowEdgeKind::ConditionalTrue => "true_edge",
                    ControlFlowEdgeKind::ConditionalFalse => "false_edge",
                    ControlFlowEdgeKind::Unconditional => "unconditional_edge",
                    ControlFlowEdgeKind::SwitchCase(_) => "switch_case_edge",
                    ControlFlowEdgeKind::SwitchOtherwise => "switch_otherwise",
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }
    }

    struct BackwardProblem {
        entry: ID<Block>,
        middle: ID<Block>,
        terminal: ID<Block>,
    }

    impl DataflowProblem for BackwardProblem {
        type JoinLattice = Facts;
        type Error = ();

        const DIRECTION: Direction = Direction::Backward;
        const EDGE_SENSITIVE: bool = true;

        fn bottom(
            &self,
            _: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move { Ok(Facts::default()) }
        }

        fn boundary_facts<'a>(
            &'a self,
            block_id: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move {
                assert_eq!(block_id, self.terminal);
                Ok(Facts::from_labels(["seed"]))
            }
        }

        fn transfer_instruction<'a>(
            &'a self,
            point: Point,
            _: &'a Instruction,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if point.block_id == self.entry {
                    "entry_instruction"
                } else if point.block_id == self.middle {
                    "middle_instruction"
                } else if point.block_id == self.terminal {
                    "terminal_instruction"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_terminator<'a>(
            &'a self,
            block_id: ID<Block>,
            _: &'a Terminator,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if block_id == self.entry {
                    "entry_terminator"
                } else if block_id == self.middle {
                    "middle_terminator"
                } else if block_id == self.terminal {
                    "terminal_terminator"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_edge<'a>(
            &'a self,
            _: &'a ControlFlowEdge,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                state.0.insert("backward_edge".to_string());
                Ok(())
            }
        }
    }

    fn expect_solution<'a>(
        solution: &'a DataflowSolution<Facts>,
        block_id: ID<Block>,
    ) -> (&'a Facts, &'a Facts) {
        (
            solution.block_entry(block_id).unwrap(),
            solution.block_exit(block_id).unwrap(),
        )
    }

    #[tokio::test(flavor = "current_thread")]
    async fn option_join_lattice_treats_none_as_bottom() {
        let mut state = None::<Facts>;
        assert!(state.join(&Some(Facts::from_labels(["a"]))).await);
        assert_eq!(state, Some(Facts::from_labels(["a"])));

        assert!(!state.join(&None).await);
        assert!(state.join(&Some(Facts::from_labels(["b"]))).await);
        assert_eq!(state, Some(Facts::from_labels(["a", "b"])));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn forward_branch_keeps_edge_states_path_sensitive() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let left = cfg.new_block();
        let right = cfg.new_block();
        let merge = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, left);
        add_instruction(&mut cfg, right);
        add_instruction(&mut cfg, merge);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: dummy_condition(),
                true_target: left,
                false_target: right,
            })),
        ));
        assert!(cfg.insert_terminator(
            left,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(cfg.insert_terminator(
            right,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(cfg.insert_terminator(merge, Terminator::Panic));

        let solution =
            solve(&ForwardBranchProblem { entry, left, right, merge }, &cfg)
                .await
                .unwrap();

        let entry_edges =
            cfg.outgoing_edges(entry).unwrap().collect::<Vec<_>>();
        assert_eq!(entry_edges.len(), 2);

        let true_edge_state = solution.edge_state(&entry_edges[0]).unwrap();
        let false_edge_state = solution.edge_state(&entry_edges[1]).unwrap();
        assert!(true_edge_state.contains("true_edge"));
        assert!(!true_edge_state.contains("false_edge"));
        assert!(false_edge_state.contains("false_edge"));
        assert!(!false_edge_state.contains("true_edge"));

        let (merge_entry, merge_exit) = expect_solution(&solution, merge);
        assert!(merge_entry.contains("left_instruction"));
        assert!(merge_entry.contains("right_instruction"));
        assert!(merge_entry.contains("true_edge"));
        assert!(merge_entry.contains("false_edge"));
        assert!(merge_exit.contains("merge_instruction"));
        assert!(merge_exit.contains("merge_terminator"));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn backward_solver_reports_entry_and_exit_in_cfg_order() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let middle = cfg.new_block();
        let terminal = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, middle);
        add_instruction(&mut cfg, terminal);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: middle,
            })),
        ));
        assert!(cfg.insert_terminator(
            middle,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: terminal,
            })),
        ));
        assert!(cfg.insert_terminator(terminal, Terminator::Panic));

        let solution =
            solve(&BackwardProblem { entry, middle, terminal }, &cfg)
                .await
                .unwrap();

        let (entry_entry, entry_exit) = expect_solution(&solution, entry);
        let (middle_entry, middle_exit) = expect_solution(&solution, middle);
        let (terminal_entry, terminal_exit) =
            expect_solution(&solution, terminal);

        assert!(terminal_exit.contains("seed"));
        assert!(terminal_entry.contains("terminal_instruction"));
        assert!(terminal_entry.contains("terminal_terminator"));

        assert!(middle_exit.contains("terminal_instruction"));
        assert!(middle_exit.contains("backward_edge"));
        assert!(!middle_exit.contains("middle_instruction"));
        assert!(middle_entry.contains("middle_instruction"));
        assert!(middle_entry.contains("middle_terminator"));

        assert!(entry_exit.contains("middle_instruction"));
        assert!(!entry_exit.contains("entry_instruction"));
        assert!(entry_entry.contains("entry_instruction"));
        assert!(entry_entry.contains("entry_terminator"));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn forward_solver_converges_on_loop_carried_facts() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let header = cfg.new_block();
        let body = cfg.new_block();
        let exit = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, header);
        add_instruction(&mut cfg, body);
        add_instruction(&mut cfg, exit);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: header,
            })),
        ));
        assert!(cfg.insert_terminator(
            header,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: dummy_condition(),
                true_target: body,
                false_target: exit,
            })),
        ));
        assert!(cfg.insert_terminator(
            body,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: header,
            })),
        ));
        assert!(cfg.insert_terminator(exit, Terminator::Panic));

        let solution = solve(
            &ForwardBranchProblem {
                entry,
                left: header,
                right: body,
                merge: exit,
            },
            &cfg,
        )
        .await
        .unwrap();

        let (header_entry, header_exit) = expect_solution(&solution, header);
        assert!(header_entry.contains("right_instruction"));
        assert!(header_exit.contains("left_instruction"));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn edge_identity_distinguishes_shared_targets() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let merge = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, merge);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: dummy_condition(),
                true_target: merge,
                false_target: merge,
            })),
        ));
        assert!(cfg.insert_terminator(merge, Terminator::Panic));

        let solution = solve(
            &ForwardBranchProblem { entry, left: merge, right: merge, merge },
            &cfg,
        )
        .await
        .unwrap();

        let edges = cfg.outgoing_edges(entry).unwrap().collect::<Vec<_>>();
        assert_eq!(edges.len(), 2);
        assert_ne!(edges[0], edges[1]);

        let first = solution.edge_state(&edges[0]).unwrap();
        let second = solution.edge_state(&edges[1]).unwrap();

        assert_ne!(first, second);
        assert!(first.contains("true_edge") || first.contains("false_edge"));
        assert!(second.contains("true_edge") || second.contains("false_edge"));
    }

    #[tokio::test(flavor = "current_thread")]
    async fn unreachable_blocks_are_omitted_from_the_solution() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let reachable = cfg.new_block();
        let unreachable = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, reachable);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: reachable,
            })),
        ));
        assert!(cfg.insert_terminator(reachable, Terminator::Panic));

        let solution = solve(
            &ForwardBranchProblem {
                entry,
                left: reachable,
                right: reachable,
                merge: reachable,
            },
            &cfg,
        )
        .await
        .unwrap();

        assert!(solution.block_entry(unreachable).is_none());
        assert!(solution.block_exit(unreachable).is_none());
    }

    struct NonEdgeSensitiveProblem {
        entry: ID<Block>,
        left: ID<Block>,
        right: ID<Block>,
        merge: ID<Block>,
    }

    impl DataflowProblem for NonEdgeSensitiveProblem {
        type JoinLattice = Facts;
        type Error = ();

        const DIRECTION: Direction = Direction::Forward;
        const EDGE_SENSITIVE: bool = false;

        fn bottom(
            &self,
            _: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move { Ok(Facts::default()) }
        }

        fn boundary_facts<'a>(
            &'a self,
            _: ID<Block>,
        ) -> impl std::future::Future<
            Output = Result<Self::JoinLattice, Self::Error>,
        > + Send {
            async move { Ok(Facts::from_labels(["seed"])) }
        }

        fn transfer_instruction<'a>(
            &'a self,
            point: Point,
            _: &'a Instruction,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if point.block_id == self.entry {
                    "entry_instruction"
                } else if point.block_id == self.left {
                    "left_instruction"
                } else if point.block_id == self.right {
                    "right_instruction"
                } else if point.block_id == self.merge {
                    "merge_instruction"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_terminator<'a>(
            &'a self,
            block_id: ID<Block>,
            _: &'a Terminator,
            state: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                let label = if block_id == self.entry {
                    "entry_terminator"
                } else if block_id == self.left {
                    "left_terminator"
                } else if block_id == self.right {
                    "right_terminator"
                } else if block_id == self.merge {
                    "merge_terminator"
                } else {
                    unreachable!()
                };

                state.0.insert(label.to_string());
                Ok(())
            }
        }

        fn transfer_edge<'a>(
            &'a self,
            _: &'a ControlFlowEdge,
            _: &'a mut Self::JoinLattice,
        ) -> impl std::future::Future<Output = Result<(), Self::Error>> + Send
        {
            async move {
                panic!(
                    "transfer_edge must not be called when edge sensitivity \
                     is disabled"
                );
            }
        }
    }

    #[tokio::test(flavor = "current_thread")]
    async fn disabling_edge_sensitivity_skips_edge_transfer() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let left = cfg.new_block();
        let right = cfg.new_block();
        let merge = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, left);
        add_instruction(&mut cfg, right);
        add_instruction(&mut cfg, merge);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition: dummy_condition(),
                true_target: left,
                false_target: right,
            })),
        ));
        assert!(cfg.insert_terminator(
            left,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(cfg.insert_terminator(
            right,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: merge,
            })),
        ));
        assert!(cfg.insert_terminator(merge, Terminator::Panic));

        let solution =
            solve(&NonEdgeSensitiveProblem { entry, left, right, merge }, &cfg)
                .await
                .unwrap();

        let (merge_entry, _) = expect_solution(&solution, merge);
        assert!(merge_entry.contains("left_instruction"));
        assert!(merge_entry.contains("right_instruction"));
        assert!(!merge_entry.contains("true_edge"));
        assert!(!merge_entry.contains("false_edge"));
    }

    #[tokio::test(flavor = "current_thread")]
    #[should_panic(expected = "edge states are unavailable when edge \
                               sensitivity is disabled")]
    async fn edge_state_panics_when_edge_sensitivity_is_disabled() {
        let mut cfg = ControlFlowGraph::default();
        let entry = cfg.entry_block_id();
        let exit = cfg.new_block();

        add_instruction(&mut cfg, entry);
        add_instruction(&mut cfg, exit);

        assert!(cfg.insert_terminator(
            entry,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: exit,
            })),
        ));
        assert!(cfg.insert_terminator(exit, Terminator::Panic));

        let solution = solve(
            &NonEdgeSensitiveProblem {
                entry,
                left: exit,
                right: exit,
                merge: exit,
            },
            &cfg,
        )
        .await
        .unwrap();

        let edge = cfg.outgoing_edges(entry).unwrap().next().unwrap();
        let _ = solution.edge_state(&edge);
    }
}
