//! Manages the state of loops during binding.

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use pernixc_arena::ID;
use pernixc_hash::HashMap;
use pernixc_ir::{control_flow_graph::Block, scope::Scope, value::Value};
use pernixc_term::r#type::Type;

use crate::binder::Binder;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
enum LoopKindState {
    While,
    Loop {
        incoming_values: HashMap<ID<Block>, Value>,
        break_type: Option<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoopState {
    label: Option<SharedStr>,
    loop_block_id: ID<Block>,
    exit_block_id: ID<Block>,
    kind_state: LoopKindState,
}

/// Manages the context of loops during binding. This remembers the state
/// required to correctly handle `break` and `continue` statements.
#[derive(Debug, Default)]
pub struct Context {
    loop_states_by_scope_id: HashMap<ID<Scope>, LoopState>,
}

impl Context {
    pub(crate) fn assert_empty(&self) {
        assert!(self.loop_states_by_scope_id.is_empty());
    }
}

/// Enumerates the kinds of loops.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum LoopKind {
    While,
    Loop,
}

impl Binder<'_> {
    /// Pushes a new loop state onto the loop context.
    pub fn push_loop_state(
        &mut self,
        scope_id: ID<Scope>,
        label: Option<SharedStr>,
        loop_block_id: ID<Block>,
        exit_block_id: ID<Block>,
        kind: LoopKind,
    ) {
        let kind_state = match kind {
            LoopKind::While => LoopKindState::While,
            LoopKind::Loop => LoopKindState::Loop {
                incoming_values: HashMap::default(),
                break_type: None,
            },
        };

        let loop_state =
            LoopState { label, loop_block_id, exit_block_id, kind_state };

        assert!(self
            .loop_context
            .loop_states_by_scope_id
            .insert(scope_id, loop_state)
            .is_none());
    }

    /// Pops the loop state for the given scope ID from the loop context.
    pub fn pop_loop_state(&mut self, scope_id: ID<Scope>) {
        assert!(self
            .loop_context
            .loop_states_by_scope_id
            .remove(&scope_id)
            .is_some());
    }
}
