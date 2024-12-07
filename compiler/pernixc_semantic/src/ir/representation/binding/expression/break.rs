use std::collections::hash_map::Entry;

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, Label};

use super::{Bind, Config, Expression};
use crate::{
    error::{self, LoopControlFlow},
    ir::{
        self,
        control_flow_graph::InsertTerminatorError,
        instruction::{
            Instruction, Jump, ScopePop, Terminator, UnconditionalJump,
        },
        representation::{
            binding::{
                infer::{self, InferenceVariable},
                stack::Scope,
                Binder, Error, LoopKind,
            },
            borrow,
        },
        value::{
            literal::{self, Literal, Unreachable},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::{
            self,
            r#type::{Constraint, Expected, Type},
        },
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Break> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Break,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = syntax_tree
            .binary()
            .as_ref()
            .map(|x| self.bind_value_or_error(x, handler))
            .transpose()?;

        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Break,
            syntax_tree.label().as_ref().map(Label::identifier),
            syntax_tree.span(),
            handler,
        )?;

        let value_type = value.as_ref().map_or_else(
            || Ok(Type::Tuple(term::Tuple { elements: Vec::new() })),
            |x| self.type_of_value(x),
        )?;

        match &self.loop_states_by_scope_id.get(&loop_scope_id).unwrap().kind {
            // can only be unit type
            LoopKind::While => {
                let _ = self.type_check(
                    &value_type,
                    Expected::Known(Type::Tuple(term::Tuple {
                        elements: Vec::new(),
                    })),
                    syntax_tree.binary().as_ref().map_or_else(
                        || syntax_tree.span(),
                        SourceElement::span,
                    ),
                    true,
                    handler,
                )?;
            }

            // can use any type
            LoopKind::Loop { break_type, .. } => {
                if let Some(break_type) = break_type {
                    let _ = self.type_check(
                        &value_type,
                        Expected::Known(break_type.clone()),
                        syntax_tree.span(),
                        true,
                        handler,
                    )?;
                } else {
                    *self
                        .loop_states_by_scope_id
                        .get_mut(&loop_scope_id)
                        .unwrap()
                        .kind
                        .as_loop_mut()
                        .unwrap()
                        .1 = Some(value_type);
                }

                // insert incoming value
                if let Entry::Vacant(entry) = self
                    .loop_states_by_scope_id
                    .get_mut(&loop_scope_id)
                    .unwrap()
                    .kind
                    .as_loop_mut()
                    .unwrap()
                    .0
                    .entry(self.current_block_id)
                {
                    entry.insert(value.unwrap_or(Value::Literal(
                        Literal::Unit(literal::Unit {
                            span: (syntax_tree.span()),
                        }),
                    )));
                }
            }
        }

        // pop all the needed scopes
        for popping_scope in self
            .stack
            .scopes()
            .iter()
            .rev()
            .map(Scope::scope_id)
            .take_while(|x| *x != loop_scope_id)
            .chain(std::iter::once(loop_scope_id))
        {
            let _ = self
                .intermediate_representation
                .control_flow_graph
                .get_block_mut(self.current_block_id)
                .unwrap()
                .insert_instruction(Instruction::ScopePop(ScopePop(
                    popping_scope,
                )));
        }

        // jump to the exit block
        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: self
                        .loop_states_by_scope_id
                        .get(&loop_scope_id)
                        .unwrap()
                        .exit_block_id,
                })),
            )
        {
            panic!("invalid block id");
        }

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: {
                let inference = InferenceVariable::new();

                assert!(self
                    .inference_context
                    .register(inference, Constraint::All(true)));

                Type::Inference(inference)
            },
            span: (syntax_tree.span()),
        }));

        Ok(Expression::RValue(value))
    }
}
