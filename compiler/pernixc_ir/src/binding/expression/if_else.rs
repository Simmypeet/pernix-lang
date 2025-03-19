use std::{collections::HashMap, num::NonZeroUsize};

use pernixc_abort::Abort;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    self,
    expression::block::{Group, GroupOrIfElse},
};
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::{Primitive, Type};

use super::{Bind, Config, Expression, Target};
use crate::{
    binding::{
        self,
        infer::{self, Expected, InferenceVariable},
        Binder, Error,
    },
    control_flow_graph::Block,
    instruction::{ConditionalJump, Jump, Terminator, UnconditionalJump},
    model::Constraint,
    value::{
        literal::{self, Literal, Unreachable},
        register::{Assignment, Phi},
        Value,
    },
};

impl Binder<'_> {
    #[allow(clippy::type_complexity)]
    fn bind_group_for_if_else(
        &mut self,
        expression: &Group,
        allocated_scope_id: ID<binding::scope::Scope>,
        if_else_success_block_id: ID<Block<infer::Model>>,
        is_statement: bool,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(Option<Value<infer::Model>>, ID<Block<infer::Model>>), Abort>
    {
        let (value, group_sucessor_block_id) = match &expression {
            Group::Indented(indented_group) => {
                let success_sub_block_id = self
                    .intermediate_representation
                    .control_flow_graph
                    .new_block();

                let block_state = self.bind_block(
                    indented_group.label.as_ref(),
                    indented_group
                        .statements
                        .statements
                        .iter()
                        .filter_map(|x| x.as_option()),
                    indented_group.span(),
                    allocated_scope_id,
                    success_sub_block_id,
                    indented_group.unsafe_keyword.is_some(),
                    handler,
                )?;

                let value = self.bind_value_or_error(block_state, handler)?;

                (Some(value), success_sub_block_id)
            }

            Group::Inline(inline_expression) => {
                self.push_scope_with(allocated_scope_id, false);

                let value = if is_statement {
                    match self.bind(
                        &*inline_expression.expression,
                        Config { target: Target::Statement },
                        handler,
                    ) {
                        Ok(_) | Err(Error::Binding(_)) => {}
                        Err(Error::Unrecoverable(abort)) => return Err(abort),
                    };

                    None
                } else {
                    Some(self.bind_value_or_error(
                        &*inline_expression.expression,
                        handler,
                    )?)
                };

                self.pop_scope(allocated_scope_id);

                (value, self.current_block_id)
            }
        };

        self.intermediate_representation.control_flow_graph.insert_terminator(
            group_sucessor_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: if_else_success_block_id,
            })),
        );

        Ok((value, group_sucessor_block_id))
    }
}

impl Bind<&syntax_tree::expression::block::IfElse> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::block::IfElse,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let condition =
            self.bind_value_or_error(&*syntax_tree.binary, handler)?;

        let is_statement_level = match config.target {
            Target::RValue | Target::LValue => false,

            Target::Statement => true,
        };

        // expect the type boolean
        self.type_check(
            &self.type_of_value(&condition, handler)?,
            Expected::Known(Type::Primitive(Primitive::Bool)),
            syntax_tree.binary.span(),
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

        self.intermediate_representation.control_flow_graph.insert_terminator(
            self.current_block_id,
            Terminator::Jump(Jump::Conditional(ConditionalJump {
                condition,
                true_target: then_block_id,
                false_target: else_block_id,
            })),
        );

        // bind the then block
        self.current_block_id = then_block_id;
        let (then_value, successor_then_block_id) = self
            .bind_group_for_if_else(
                &syntax_tree.then_expression,
                then_scope_id,
                if_else_successor_block_id,
                is_statement_level,
                handler,
            )?;

        // bind the else block
        self.current_block_id = else_block_id;
        let (else_value, successor_else_block_id) = match syntax_tree
            .else_expression
            .as_ref()
            .map(|x| &*x.expression)
        {
            Some(GroupOrIfElse::Group(group)) => self.bind_group_for_if_else(
                group,
                else_scope_id,
                if_else_successor_block_id,
                is_statement_level,
                handler,
            )?,
            Some(GroupOrIfElse::IfElse(if_else)) => {
                self.push_scope_with(else_scope_id, false);

                let expression = if is_statement_level {
                    match self.bind(
                        if_else,
                        Config { target: Target::Statement },
                        handler,
                    ) {
                        Err(Error::Binding(_)) | Ok(_) => None,

                        Err(Error::Unrecoverable(abort)) => {
                            return Err(abort.into())
                        }
                    }
                } else {
                    Some(self.bind_value_or_error(if_else, handler)?)
                };

                self.pop_scope(else_scope_id);

                (expression, self.current_block_id)
            }
            None => {
                self.push_scope_with(else_scope_id, false);
                self.pop_scope(else_scope_id);

                (
                    Some(Value::Literal(Literal::Unit(literal::Unit {
                        span: Some(syntax_tree.span()),
                    }))),
                    self.current_block_id,
                )
            }
        };

        self.intermediate_representation.control_flow_graph.insert_terminator(
            successor_else_block_id,
            Terminator::Jump(Jump::Unconditional(UnconditionalJump {
                target: if_else_successor_block_id,
            })),
        );

        // change the current block to the if else successor block
        self.current_block_id = if_else_successor_block_id;

        if is_statement_level {
            Ok(Expression::RValue(Value::Literal(Literal::Unit(
                literal::Unit { span: Some(syntax_tree.span()) },
            ))))
        } else {
            let value = match self.current_block().predecessors().len() {
                0 => Value::Literal(Literal::Unreachable(Unreachable {
                    r#type: {
                        let inference = InferenceVariable::new();
                        assert!(self
                            .inference_context
                            .register(inference, Constraint::All(true)));

                        Type::Inference(inference)
                    },
                    span: Some(syntax_tree.span()),
                })),
                1 => {
                    if self
                        .current_block()
                        .predecessors()
                        .contains(&successor_then_block_id)
                    {
                        then_value.unwrap()
                    } else {
                        assert!(self
                            .current_block()
                            .predecessors()
                            .contains(&successor_else_block_id));
                        else_value.unwrap()
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

                    let then_value = then_value.unwrap();
                    let else_value = else_value.unwrap();

                    let then_type = self.type_of_value(&then_value, handler)?;
                    let else_type = self.type_of_value(&else_value, handler)?;

                    if syntax_tree.else_expression.is_some() {
                        let _ = self.type_check(
                            &else_type,
                            Expected::Known(then_type.clone()),
                            syntax_tree.else_expression.as_ref().map_or(
                                syntax_tree.span(),
                                SourceElement::span,
                            ),
                            handler,
                        )?;
                    } else {
                        let _ = self.type_check(
                            &then_type,
                            Expected::Known(else_type),
                            syntax_tree.span(),
                            handler,
                        )?;
                    }

                    let phi_register_id = self.create_register_assignmnet(
                        Assignment::Phi(Phi {
                            r#type: then_type,
                            incoming_values: {
                                let mut incoming_values = HashMap::new();
                                incoming_values.insert(
                                    successor_then_block_id,
                                    then_value,
                                );
                                incoming_values.insert(
                                    successor_else_block_id,
                                    else_value,
                                );

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
}
