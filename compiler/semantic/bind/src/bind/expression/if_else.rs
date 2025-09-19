use std::num::NonZeroUsize;

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    control_flow_graph::Block,
    instruction::{ConditionalJump, Jump, Terminator, UnconditionalJump},
    value::{
        literal::{self, Literal, Unreachable},
        register::{Assignment, Phi},
        Value,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::expression::block::{Group, GroupOrIfElse};
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{
        type_check::Expected, Binder, BindingError, Error, UnrecoverableError,
    },
    diagnostic::{Diagnostic, IfMissingElseBranch},
    inference_context::constraint,
};

impl Binder<'_> {
    #[allow(clippy::type_complexity)]
    async fn bind_group_for_if_else(
        &mut self,
        expression: &Group,
        allocated_scope_id: ID<pernixc_ir::scope::Scope>,
        if_else_success_block_id: ID<Block>,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(Option<Value>, ID<Block>), UnrecoverableError> {
        let (value, group_sucessor_block_id) = match &expression {
            Group::Indented(indented_group) => {
                let success_sub_block_id = self.new_block();

                let statements =
                    indented_group.statements().map_or_else(Vec::new, |x| {
                        x.statements()
                            .filter_map(|x| x.into_line().ok())
                            .collect()
                    });

                let block_state = self
                    .bind_block()
                    .maybe_label(indented_group.label().as_ref())
                    .statements(statements.iter())
                    .span(indented_group.span())
                    .scope_id(allocated_scope_id)
                    .successor_block_id(success_sub_block_id)
                    .is_unsafe(indented_group.unsafe_keyword().is_some())
                    .handler(handler)
                    .call()
                    .await?;

                let value = self
                    .bind_value_or_error(
                        block_state,
                        guidance.type_hint(),
                        handler,
                    )
                    .await?;

                (Some(value), success_sub_block_id)
            }

            Group::Inline(inline_expression) => {
                self.push_scope_with(allocated_scope_id, false);

                let value = match guidance {
                    Guidance::Expression(type_hint) => 'result: {
                        let Some(expr) = inline_expression.expression() else {
                            break 'result Some(
                                type_hint.as_ref().map_or_else(
                                    || {
                                        Value::Literal(self.create_error(
                                            inline_expression.span(),
                                        ))
                                    },
                                    |ty| {
                                        Value::error(
                                            (*ty).clone(),
                                            Some(inline_expression.span()),
                                        )
                                    },
                                ),
                            );
                        };

                        Some(
                            self.bind_value_or_error(
                                &expr, *type_hint, handler,
                            )
                            .await?,
                        )
                    }
                    Guidance::Statement => 'result: {
                        let Some(expr) = inline_expression.expression() else {
                            break 'result None;
                        };

                        match self
                            .bind(&expr, &Guidance::Statement, handler)
                            .await
                        {
                            Ok(Expression::RValue(value)) => {
                                let value_ty =
                                    self.type_of_value(&value, handler).await?;

                                // must be type unit
                                self.type_check(
                                    &value_ty,
                                    Expected::Known(Type::unit()),
                                    expr.span(),
                                    handler,
                                )
                                .await?;
                            }

                            // no need to type check lvalue
                            Ok(Expression::LValue(_))
                            | Err(Error::Binding(_)) => {}

                            Err(Error::Unrecoverable(abort)) => {
                                return Err(abort)
                            }
                        }

                        None
                    }
                };

                self.pop_scope(allocated_scope_id);

                (value, self.current_block_id())
            }
        };

        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: if_else_success_block_id },
        )));

        Ok((value, group_sucessor_block_id))
    }
}

impl Bind<&pernixc_syntax::expression::block::IfElse> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::IfElse,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let (Some(binary), Some(then_expr)) =
            (syntax_tree.binary(), syntax_tree.then())
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let condition = Box::pin(self.bind_value_or_error(
            &binary,
            Some(&Type::Primitive(Primitive::Bool)),
            handler,
        ))
        .await?;

        let then_block_id = self.new_block();
        let else_block_id = self.new_block();
        let if_else_successor_block_id = self.new_block();

        let (then_scope_id, else_scope_id) = {
            let scopes = self.new_child_branch(NonZeroUsize::new(2).unwrap());

            (scopes[0], scopes[1])
        };

        self.insert_terminator(Terminator::Jump(Jump::Conditional(
            ConditionalJump {
                condition,
                true_target: then_block_id,
                false_target: else_block_id,
            },
        )));

        // bind the then block
        self.set_current_block_id(then_block_id);

        let (then_value, successor_then_block_id) =
            Box::pin(self.bind_group_for_if_else(
                &then_expr,
                then_scope_id,
                if_else_successor_block_id,
                guidance,
                handler,
            ))
            .await?;

        // bind the else block
        self.set_current_block_id(else_block_id);

        let then_type = if let Some(then_value) = &then_value {
            Some(self.type_of_value(then_value, handler).await?)
        } else {
            None
        };

        let (else_value, successor_else_block_id) =
            match syntax_tree.r#else().and_then(|x| x.group_or_if_else()) {
                Some(GroupOrIfElse::Group(group)) => {
                    Box::pin(self.bind_group_for_if_else(
                        &group,
                        else_scope_id,
                        if_else_successor_block_id,
                        &match guidance {
                            Guidance::Expression(_) => {
                                Guidance::Expression(then_type.as_ref())
                            }
                            Guidance::Statement => Guidance::Statement,
                        },
                        handler,
                    ))
                    .await?
                }
                Some(GroupOrIfElse::IfElse(if_else)) => {
                    self.push_scope_with(else_scope_id, false);

                    let expression = match guidance {
                        Guidance::Expression(ty_hint) => Some(
                            Box::pin(self.bind_value_or_error(
                                &if_else, *ty_hint, handler,
                            ))
                            .await?,
                        ),
                        Guidance::Statement => {
                            match Box::pin(self.bind(
                                &if_else,
                                &Guidance::Statement,
                                handler,
                            ))
                            .await
                            {
                                Ok(_) | Err(Error::Binding(_)) => None,

                                Err(Error::Unrecoverable(abort)) => {
                                    return Err(abort.into())
                                }
                            }
                        }
                    };

                    self.pop_scope(else_scope_id);

                    (expression, self.current_block_id())
                }
                None => {
                    self.push_scope_with(else_scope_id, false);
                    self.pop_scope(else_scope_id);

                    let value = if let Some(then_type) = &then_type {
                        if self
                            .type_check_as_diagnostic(
                                then_type,
                                Expected::Known(Type::unit()),
                                syntax_tree.span(),
                                handler,
                            )
                            .await?
                            .is_some()
                        {
                            handler.receive(Diagnostic::IfMissingElseBranch(
                                IfMissingElseBranch {
                                    if_span: syntax_tree.span(),
                                },
                            ));

                            Value::error(
                                then_type.clone(),
                                Some(syntax_tree.span()),
                            )
                        } else {
                            Value::Literal(Literal::Unit(literal::Unit {
                                span: Some(syntax_tree.span()),
                            }))
                        }
                    } else {
                        Value::Literal(Literal::Unit(literal::Unit {
                            span: Some(syntax_tree.span()),
                        }))
                    };

                    (Some(value), self.current_block_id())
                }
            };

        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: if_else_successor_block_id },
        )));

        // change the current block to the if else successor block
        self.set_current_block_id(if_else_successor_block_id);

        match guidance {
            Guidance::Expression(_) => {
                let value = match self.current_block().predecessors().len() {
                    0 => Value::Literal(Literal::Unreachable(Unreachable {
                        r#type: Type::Inference(self.create_type_inference(
                            constraint::Type::All(true),
                        )),
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

                        let phi_register_id = self.create_register_assignment(
                            Assignment::Phi(Phi {
                                r#type: then_type.unwrap(),
                                incoming_values: {
                                    let mut incoming_values =
                                        HashMap::default();

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
            Guidance::Statement => Ok(Expression::RValue(Value::Literal(
                Literal::Unit(literal::Unit { span: Some(syntax_tree.span()) }),
            ))),
        }
    }
}
