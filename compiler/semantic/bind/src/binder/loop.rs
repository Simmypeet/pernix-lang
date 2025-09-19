//! Manages the state of loops during binding.

use enum_as_inner::EnumAsInner;
use flexstr::SharedStr;
use getset::CopyGetters;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    control_flow_graph::Block,
    scope::Scope,
    value::{
        literal::{self, Literal, Unreachable},
        register::{Assignment, Phi},
        Value,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::r#type::Type;

use super::Error;
use crate::{
    bind::{Bind, Expression, Guidance},
    binder::Binder,
    diagnostic::Diagnostic,
    inference_context::constraint,
};

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
enum LoopKindState {
    While,
    Loop {
        incoming_values: HashMap<ID<Block>, Value>,
        break_type: Option<Type>,
    },
}

/// Represents the state of a loop during binding. It may contain additional
/// `break` values and their types, which are used to construct the final
/// value of the loop expression.
#[derive(Debug, Clone, PartialEq, Eq, CopyGetters)]
pub struct LoopState {
    label: Option<SharedStr>,
    /// The block ID of the loop header. This is where `continue` statements
    /// will jump to.
    #[get_copy = "pub"]
    loop_block_id: ID<Block>,

    /// The block ID to jump to when exiting the loop. This is where `break`
    /// statements will jump to.
    #[get_copy = "pub"]
    exit_block_id: ID<Block>,
    kind_state: LoopKindState,
    whole_span: RelativeSpan,
    keyword_span: RelativeSpan,
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
    #[allow(clippy::too_many_arguments)]
    pub fn push_loop_state(
        &mut self,
        scope_id: ID<Scope>,
        label: Option<SharedStr>,
        loop_block_id: ID<Block>,
        exit_block_id: ID<Block>,
        kind: LoopKind,
        whole_span: RelativeSpan,
        keyword_span: RelativeSpan,
    ) {
        let kind_state = match kind {
            LoopKind::While => LoopKindState::While,
            LoopKind::Loop => LoopKindState::Loop {
                incoming_values: HashMap::default(),
                break_type: None,
            },
        };

        let loop_state = LoopState {
            label,
            loop_block_id,
            exit_block_id,
            kind_state,
            whole_span,
            keyword_span,
        };

        assert!(self
            .loop_context
            .loop_states_by_scope_id
            .insert(scope_id, loop_state)
            .is_none());
    }

    /// Pops the loop state for the given scope ID from the loop context.
    pub fn pop_loop_state(&mut self, scope_id: ID<Scope>) -> LoopState {
        self.loop_context.loop_states_by_scope_id.remove(&scope_id).unwrap()
    }
}

impl Bind<LoopState> for Binder<'_> {
    async fn bind(
        &mut self,
        loop_state: LoopState,
        _: &Guidance<'_>,
        _: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        assert_eq!(self.current_block_id(), loop_state.exit_block_id);

        match loop_state.kind_state {
            LoopKindState::While => {
                let value = if self.current_block().predecessors().is_empty() {
                    Value::Literal(Literal::Unreachable(Unreachable {
                        r#type: Type::Inference(self.create_type_inference(
                            constraint::Type::All(true),
                        )),
                        span: Some(loop_state.whole_span),
                    }))
                } else {
                    Value::Literal(Literal::Unit(literal::Unit {
                        span: Some(loop_state.whole_span),
                    }))
                };

                Ok(Expression::RValue(value))
            }

            LoopKindState::Loop { mut incoming_values, break_type } => {
                // set the current block to the exit block
                let value = if let Some(break_type) = break_type {
                    assert!(self
                        .current_block()
                        .predecessors()
                        .iter()
                        .copied()
                        .all(
                            |block_id| incoming_values.contains_key(&block_id)
                        ));

                    // filter out the incoming values that are unreachable
                    #[allow(clippy::needless_collect)]
                    for remove in incoming_values
                        .keys()
                        .copied()
                        .filter(|x| {
                            !self.current_block().predecessors().contains(x)
                        })
                        .collect::<Vec<_>>()
                    {
                        incoming_values.remove(&remove);
                    }

                    match incoming_values.len() {
                        0 => {
                            Value::Literal(Literal::Unreachable(Unreachable {
                                r#type: break_type,
                                span: Some(loop_state.whole_span),
                            }))
                        }

                        // only one incoming value, just return it
                        1 => incoming_values.into_iter().next().unwrap().1,

                        // multiple incoming values, create a phi node
                        _ => {
                            let phi_register_id = self
                                .create_register_assignment(
                                    Assignment::Phi(Phi {
                                        r#type: break_type,
                                        incoming_values,
                                    }),
                                    loop_state.whole_span,
                                );

                            Value::Register(phi_register_id)
                        }
                    }
                } else {
                    assert!(incoming_values.is_empty());
                    assert!(self
                        .current_block()
                        .is_unreachable_or_terminated());

                    Value::Literal(Literal::Unreachable(Unreachable {
                        r#type: {
                            Type::Inference(self.create_type_inference(
                                constraint::Type::All(true),
                            ))
                        },
                        span: Some(loop_state.whole_span),
                    }))
                };

                Ok(Expression::RValue(value))
            }
        }
    }
}
