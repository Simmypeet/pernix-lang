use crate::{
    cfg::{BasicBlockID, ControlFlowGraph},
    hir::{instruction::Backend, value::IntermediateType},
};

pub(super) struct ControlFlowGraphManager {
    control_flow_graph: ControlFlowGraph<Backend<IntermediateType>>,
    current_basic_block: BasicBlockID,
}

impl ControlFlowGraphManager {
    pub(super) fn new() -> Self {
        let control_flow_graph = ControlFlowGraph::new();
        let entry_block = control_flow_graph.entry_block();

        Self {
            control_flow_graph,
            current_basic_block: entry_block,
        }
    }
}
