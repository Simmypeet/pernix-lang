use std::{collections::HashMap, num::NonZeroUsize};

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, expression::BlockOrIfElse};

use super::{Bind, Config, Expression};
use crate::{
    error::{self},
    ir::{
        self,
        control_flow_graph::InsertTerminatorError,
        instruction::{Jump, Terminator, UnconditionalJump},
        representation::binding::{
            infer::{self, InferenceVariable},
            Binder, Error,
        },
        value::{
            literal::{self, Literal, Unreachable},
            register::{Assignment, Phi},
            Value,
        },
    },
    symbol::table::{
        self,
        resolution::{self},
    },
    type_system::{
        self,
        term::r#type::{self, Constraint, Expected, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::IfElse> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::IfElse,
        _: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let condition =
            self.bind_value_or_error(syntax_tree.parenthesized(), handler)?;

        // expect the type boolean
        let _ = self.type_check(
            &self.type_of_value(&condition)?,
            Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.parenthesized().span(),
            true,
            handler,
        )?;

        let then_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let else_block_id =
            self.intermediate_representation.control_flow_graph.new_block();
        let if_else_successor_block_id =
            self.intermediate_representation.control_flow_graph.new_block();

        let (then_scope_id, else_scope_id) = {
            let scopes = self
                .intermediate_representation
                .scope_tree
                .new_child_branch(
                    self.stack.current_scope().scope_id(),
                    NonZeroUsize::new(2).unwrap(),
                )
                .unwrap();

            (scopes[0], scopes[1])
        };

        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                self.current_block_id,
                Terminator::Jump(Jump::Conditional(
                    crate::ir::instruction::ConditionalJump {
                        condition,
                        true_target: then_block_id,
                        false_target: else_block_id,
                    },
                )),
            )
        {
            panic!("invalid block id");
        }

        // bind the then block
        self.current_block_id = then_block_id;
        let (then_value, successor_then_block_id) = {
            let successor_then_block_id =
                self.intermediate_representation.control_flow_graph.new_block();

            let block_state = self.bind_block(
                syntax_tree.then_expression(),
                then_scope_id,
                successor_then_block_id,
                handler,
            )?;

            let value = self.bind_value_or_error(block_state, handler)?;

            if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
                .intermediate_representation
                .control_flow_graph
                .insert_terminator(
                    successor_then_block_id,
                    Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                        target: if_else_successor_block_id,
                    })),
                )
            {
                panic!("invalid id")
            }

            (value, successor_then_block_id)
        };

        // bind the else block
        self.current_block_id = else_block_id;
        let (else_value, successor_else_block_id) = match syntax_tree
            .else_expression()
            .as_ref()
            .map(|x| &**x.expression())
        {
            Some(BlockOrIfElse::Block(block)) => {
                let successor_else_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let block_state = self.bind_block(
                    block,
                    else_scope_id,
                    successor_else_block_id,
                    handler,
                )?;

                let value = self.bind_value_or_error(block_state, handler)?;

                (value, successor_else_block_id)
            }
            Some(BlockOrIfElse::IfElse(if_else)) => {
                let expression = self.bind_value_or_error(
                    if_else,
                    &self.create_handler_wrapper(handler),
                )?;

                (expression, self.current_block_id)
            }
            None => (
                Value::Literal(Literal::Unit(literal::Unit {
                    span: (syntax_tree.span()),
                })),
                self.current_block_id,
            ),
        };

        if let Err(InsertTerminatorError::InvalidBlockID(_)) = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(
                successor_else_block_id,
                Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                    target: if_else_successor_block_id,
                })),
            )
        {
            panic!("invalid id")
        }

        // change the current block to the if else successor block
        self.current_block_id = if_else_successor_block_id;

        let value = match self.current_block().predecessors().len() {
            0 => Value::Literal(Literal::Unreachable(Unreachable {
                r#type: {
                    let inference = InferenceVariable::new();
                    assert!(self
                        .inference_context
                        .register(inference, Constraint::All(true)));

                    Type::Inference(inference)
                },
                span: (syntax_tree.span()),
            })),
            1 => {
                if self
                    .current_block()
                    .predecessors()
                    .contains(&successor_then_block_id)
                {
                    then_value
                } else {
                    assert!(self
                        .current_block()
                        .predecessors()
                        .contains(&successor_else_block_id));
                    else_value
                }
            }
            2 => {
                assert!(
                    self.current_block()
                        .predecessors()
                        .contains(&successor_else_block_id)
                        && self
                            .current_block()
                            .predecessors()
                            .contains(&successor_then_block_id)
                );

                let then_type = self.type_of_value(&then_value)?;
                let else_type = self.type_of_value(&else_value)?;

                if syntax_tree.else_expression().is_some() {
                    let _ = self.type_check(
                        &else_type,
                        Expected::Known(then_type.clone()),
                        syntax_tree
                            .else_expression()
                            .as_ref()
                            .map_or(syntax_tree.span(), SourceElement::span),
                        true,
                        handler,
                    )?;
                } else {
                    let _ = self.type_check(
                        &then_type,
                        Expected::Known(else_type),
                        syntax_tree.span(),
                        true,
                        handler,
                    )?;
                }

                let phi_register_id = self.create_register_assignmnet(
                    Assignment::Phi(Phi {
                        r#type: then_type,
                        incoming_values: {
                            let mut incoming_values = HashMap::new();
                            incoming_values
                                .insert(successor_then_block_id, then_value);
                            incoming_values
                                .insert(successor_else_block_id, else_value);

                            incoming_values
                        },
                    }),
                    syntax_tree.span(),
                );

                Value::Register(phi_register_id)
            }
            _ => unreachable!(),
        };

        Ok(Expression::RValue(value))
    }
}
