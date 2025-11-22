//! Binding logic for block expressions.

use std::collections::hash_map::Entry;

use bon::bon;
use flexstr::SharedStr;
use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    control_flow_graph::Block,
    instruction::{
        Instruction, Jump, ScopePop, ScopePush, Terminator, UnconditionalJump,
    },
    scope::Scope,
    value::{
        Value,
        literal::{self, Literal, Unit, Unreachable},
        register::{Assignment, Phi},
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_syntax::Label;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression},
    binder::{Binder, Error, UnrecoverableError},
    diagnostic::{Diagnostic, NotAllFlowPathsExpressValue},
    infer::constraint,
};

#[derive(Debug, Default)]
pub(super) struct Context {
    block_states_by_scope_id: HashMap<ID<Scope>, BlockState>,
}

impl Context {
    pub(crate) fn assert_empty(&self) {
        assert!(self.block_states_by_scope_id.is_empty());
    }
}

/// Represents the block value after binding a block. It may holds the value
/// of the `express` expression if it exists.
#[derive(Debug, Clone, PartialEq, Eq, Getters, CopyGetters)]
pub struct BlockState {
    label: Option<SharedStr>,
    incoming_values: HashMap<ID<Block>, Value>,

    /// The block ID to jump to after the block is finished executing.
    #[get_copy = "pub"]
    successor_block_id: ID<Block>,

    /// The type of the `express` expression.
    ///
    /// If `None`, the block hasn't expressed a value.
    #[get = "pub"]
    express_type: Option<Type>,

    span: RelativeSpan,
}

#[bon]
impl Binder<'_> {
    /// Binds a block expression to the IR, returning the block state that
    /// can be later used to obtain the value of the `express` expression if
    /// it exists.
    #[builder]
    pub async fn bind_block<
        'a,
        T: IntoIterator<Item = &'a pernixc_syntax::statement::Statement>,
    >(
        &mut self,
        /// Optional label for the `express` expression to target
        label: Option<&Label>,
        /// The list of statements inside the block that will be bound
        /// immediately after
        statements: T,
        /// The span of the entire block
        span: RelativeSpan,
        /// The scope ID that this block is defined in. The binder will push
        /// a new scope for the block and pop it when finished binding all
        /// statements
        scope_id: ID<Scope>,
        /// The block ID to jump to after the block is finished executing
        successor_block_id: ID<Block>,

        /// Whether the block is marked as unsafe
        #[builder(default = true)]
        is_unsafe: bool,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<BlockState, UnrecoverableError> {
        self.push_instruction(Instruction::ScopePush(ScopePush(scope_id)));

        assert!(
            self.block_context
                .block_states_by_scope_id
                .insert(scope_id, BlockState {
                    label: label
                        .and_then(pernixc_syntax::Label::identifier)
                        .map(|x| x.kind.0),
                    incoming_values: HashMap::default(),
                    successor_block_id,
                    express_type: None,
                    span,
                })
                .is_none(),
            "attempted to bind a block in a scope that already has a block \
             bound to it"
        );

        // push a new scope for the block
        self.stack.push_scope(scope_id, is_unsafe);

        // bind all statements in the block
        for statement in statements {
            self.bind_statement(statement, handler).await?;
        }

        // pop the scope for the block
        assert_eq!(
            self.stack.pop_scope().map(|x| x.scope_id()),
            Some(scope_id),
            "scope stack is corrupted"
        );

        // pop the scope for the block
        self.push_instruction(Instruction::ScopePop(ScopePop(scope_id)));

        // jump to the successor block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: successor_block_id },
        )));

        self.current_block_id = successor_block_id;

        // remove the block state for the scope
        Ok(self
            .block_context
            .block_states_by_scope_id
            .remove(&scope_id)
            .expect("block state should exist"))
    }
}

impl Binder<'_> {
    /// Searches for a [`BlockState`] in the current scope stack that matches
    /// the given optional label. If a label is provided, it will only return
    /// a block state that matches the label. If no label is provided, it will
    /// return the nearest block state in the scope stack.
    #[must_use]
    pub fn search_block_scope_id(
        &self,
        label_str: Option<&str>,
    ) -> Option<ID<Scope>> {
        for scope_id in
            self.stack.scopes().iter().rev().map(super::stack::Scope::scope_id)
        {
            let Some(block_state) =
                self.block_context.block_states_by_scope_id.get(&scope_id)
            else {
                continue;
            };

            if let Some(label_str) = label_str
                && block_state.label.as_deref() != Some(label_str)
            {
                continue;
            }

            return Some(scope_id);
        }

        None
    }

    /// Gets the block state associated with the given scope ID, if it exists.
    #[must_use]
    pub fn get_block_state_from_scope_id(
        &self,
        scope_id: ID<Scope>,
    ) -> Option<&BlockState> {
        self.block_context.block_states_by_scope_id.get(&scope_id)
    }

    /// Expresses a value to a block associated with the given scope ID.
    pub async fn express_value(
        &mut self,
        scope_id: ID<Scope>,
        value: Value,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let ty = self.type_of_value(&value, handler).await?;
        let block_state = self
            .block_context
            .block_states_by_scope_id
            .get_mut(&scope_id)
            .unwrap();

        // we'll assume the type of the value is the same
        if block_state.express_type.is_none() {
            block_state.express_type = Some(ty);
        }

        // insert the incoming value from the current block
        if let Entry::Vacant(entry) =
            block_state.incoming_values.entry(self.current_block_id)
        {
            entry.insert(value);
        }

        Ok(())
    }
}

impl Bind<BlockState> for Binder<'_> {
    async fn bind(
        &mut self,
        mut block_state: BlockState,
        _: &crate::bind::Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        // should be operating in the block's successor block
        assert_eq!(
            block_state.successor_block_id, self.current_block_id,
            "block state's successor block ID does not match the current \
             block ID"
        );

        let value = if let Some(express_type) = block_state.express_type {
            // the block id that doesn't have incoming values
            let missing_value_block_ids = self
                .current_block()
                .predecessors()
                .iter()
                .copied()
                .filter(|block_id| {
                    !block_state.incoming_values.contains_key(block_id)
                })
                .collect::<Vec<_>>();

            // filter out the incoming values that are unreachable
            #[allow(clippy::needless_collect)]
            for remove in block_state
                .incoming_values
                .keys()
                .copied()
                .filter(|x| !self.current_block().predecessors().contains(x))
                .collect::<Vec<_>>()
            {
                block_state.incoming_values.remove(&remove);
            }

            // add the missing values
            for block_id in missing_value_block_ids.iter().copied() {
                assert!(
                    block_state
                        .incoming_values
                        .insert(
                            block_id,
                            Value::Literal(Literal::Error(literal::Error {
                                r#type: express_type.clone(),
                                span: Some(block_state.span),
                            })),
                        )
                        .is_none()
                );
            }

            if !missing_value_block_ids.is_empty() {
                handler.receive(Diagnostic::NotAllFlowPathsExpressValue(
                    NotAllFlowPathsExpressValue { span: block_state.span },
                ));
            }

            match block_state.incoming_values.len() {
                // no incoming values, unreachable
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: express_type,
                    span: Some(block_state.span),
                })),

                // only one incoming value, just return it
                1 => block_state.incoming_values.into_iter().next().unwrap().1,

                // multiple incoming values, create a phi node
                _ => {
                    let phi_register_id = self.create_register_assignment(
                        Assignment::Phi(Phi {
                            r#type: express_type,
                            incoming_values: block_state.incoming_values,
                        }),
                        block_state.span,
                    );

                    Value::Register(phi_register_id)
                }
            }
        } else {
            // should have had no incoming values
            assert!(block_state.incoming_values.is_empty());

            if self.current_block().is_unreachable_or_terminated() {
                Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: Type::Inference(
                        self.create_type_inference(constraint::Type::All(true)),
                    ),
                    span: Some(block_state.span),
                }))
            } else {
                Value::Literal(Literal::Unit(Unit {
                    span: Some(block_state.span),
                }))
            }
        };

        Ok(Expression::RValue(value))
    }
}
