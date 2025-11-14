//! Manages the state of loops during binding.

use std::collections::hash_map::Entry;

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
    binder::{Binder, BindingError, UnrecoverableError},
    diagnostic::{
        Diagnostic, LoopControlFlow, LoopControlFlowOutsideLoop,
        LoopWithGivenLabelNameNotFound,
    },
    infer::constraint,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct LoopTracking {
    incoming_values: HashMap<ID<Block>, Value>,
    break_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
enum LoopKindState {
    While,
    Loop(LoopTracking),
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

impl LoopState {
    /// Gets the expected type of the loop expression based on its kind.
    /// For `while` loops, this is always the unit type. For `loop`
    /// loops, this is the type of the values passed to `break` statements,
    /// or `None` if no `break` statements have been encountered.
    #[must_use]
    pub fn get_expected_type(&self) -> Option<Type> {
        match &self.kind_state {
            LoopKindState::While => Some(Type::unit()),
            LoopKindState::Loop(tracking) => tracking.break_type.clone(),
        }
    }
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
            LoopKind::Loop => LoopKindState::Loop(LoopTracking {
                incoming_values: HashMap::default(),
                break_type: None,
            }),
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

    /// Finds a [`ID<Scope>`] to operate a control flow on based on the
    /// location and label.
    pub fn find_loop_scope_id(
        &self,
        control_flow: LoopControlFlow,
        label: Option<pernixc_syntax::Identifier>,
        syntax_tree_span: RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<ID<Scope>, Error> {
        let mut loop_scope_id = None;

        // find the loop state
        for scope in self.stack.scopes().iter().rev() {
            let Some(get_loop_state) = self
                .loop_context
                .loop_states_by_scope_id
                .get(&scope.scope_id())
            else {
                continue;
            };

            if let Some(label) = label.as_ref() {
                if get_loop_state.label.as_deref() != Some(&label.kind) {
                    continue;
                }
            }

            loop_scope_id = Some(scope.scope_id());
            break;
        }

        // loop state not found report the error
        let Some(loop_scope_id) = loop_scope_id else {
            if let Some(label) = label {
                handler.receive(Diagnostic::LoopWithGivenLabelNameNotFound(
                    LoopWithGivenLabelNameNotFound { span: label.span },
                ));
            } else {
                handler.receive(Diagnostic::LoopControlFlowOutsideLoop(
                    LoopControlFlowOutsideLoop {
                        span: syntax_tree_span,
                        control_flow,
                    },
                ));
            }

            return Err(Error::Binding(BindingError(syntax_tree_span)));
        };

        Ok(loop_scope_id)
    }

    /// Gets the loop state for the given scope ID
    #[must_use]
    pub fn get_loop_state(&self, scope_id: ID<Scope>) -> &LoopState {
        self.loop_context.loop_states_by_scope_id.get(&scope_id).unwrap()
    }

    /// Records a `break` value for the loop identified by `loop_scope_id`.
    /// If the loop does not yet have a break type, it is set to the type
    /// of the provided value. If it already has a break type, the type of
    /// the provided value must unify with the existing break type.
    pub async fn break_value(
        &mut self,
        loop_scope_id: ID<Scope>,
        value: Value,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let ty = self.type_of_value(&value, handler).await?;
        let loop_state = self
            .loop_context
            .loop_states_by_scope_id
            .get_mut(&loop_scope_id)
            .unwrap();

        match &mut loop_state.kind_state {
            LoopKindState::While => {
                // do nothing, can only be unit type
                // assuming the type has been checked already
            }

            LoopKindState::Loop(LoopTracking {
                incoming_values,
                break_type,
            }) => {
                if break_type.is_none() {
                    *break_type = Some(ty);
                }

                if let Entry::Vacant(entry) =
                    incoming_values.entry(self.current_block_id)
                {
                    entry.insert(value);
                }
            }
        }

        Ok(())
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

            LoopKindState::Loop(LoopTracking {
                mut incoming_values,
                break_type,
            }) => {
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
