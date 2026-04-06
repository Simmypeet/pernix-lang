use std::num::NonZeroUsize;

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::FxHashMap;
use pernixc_ir::{
    address::Address,
    control_flow_graph::Block,
    instruction::{ConditionalJump, Jump, Terminator, UnconditionalJump},
    pattern::{Refutable, Wildcard},
    value::{
        Value,
        literal::{self, Literal, Unreachable},
        register::{
            Assignment, Binary, Phi, RelationalOperator, VariantNumber,
            load::Load,
        },
    },
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::parent::get_parent;
use pernixc_syntax::expression::block::{
    Group, GroupOrIfElse, IfCondition, IfMatchCondition,
};
use pernixc_target::Global;
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    bind::{
        Bind, Expression, Guidance,
        expression::r#match::{
            get_conditional_value, replace_refutable_in_tuple_pack,
        },
    },
    binder::{
        Binder, BindingError, Error, MatchScrutineeBindingResult,
        UnrecoverableError, type_check::Expected,
    },
    diagnostic::{Diagnostic, IfMissingElseBranch},
    infer::constraint,
    pattern::path::{Path, PathAccess},
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct IfMatchConditionBinding {
    scrutinee: MatchScrutineeBindingResult,
    pattern: Refutable,
}

impl Binder<'_> {
    async fn bind_if_boolean_condition(
        &mut self,
        binary: &pernixc_syntax::expression::binary::Binary,
        then_block_id: ID<Block>,
        else_block_id: ID<Block>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let condition = self
            .bind_value_or_error(
                binary,
                Some(&Type::Primitive(Primitive::Bool)),
                handler,
            )
            .await?;

        self.insert_terminator(Terminator::Jump(Jump::Conditional(
            ConditionalJump {
                condition,
                true_target: then_block_id,
                false_target: else_block_id,
            },
        )));

        Ok(())
    }

    #[allow(clippy::too_many_lines, clippy::too_many_arguments)]
    async fn bind_if_match_paths(
        &mut self,
        pattern: &Refutable,
        address: Address,
        address_ty: Type,
        refutable_paths: std::collections::VecDeque<Path>,
        then_block_id: ID<Block>,
        else_block_id: ID<Block>,
        span: pernixc_lexical::tree::RelativeSpan,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        if refutable_paths.is_empty() {
            self.insert_terminator(Terminator::Jump(Jump::Unconditional(
                UnconditionalJump { target: then_block_id },
            )));

            return Ok(());
        }

        let total_paths = refutable_paths.len();

        for (index, path) in refutable_paths.into_iter().enumerate() {
            let PathAccess { address, r#type: _, pattern } = self
                .access_path_in_pattern(
                    pattern,
                    address.clone(),
                    address_ty.clone(),
                    &path,
                )
                .await?;

            let success_block_id = if index + 1 == total_paths {
                then_block_id
            } else {
                self.new_block()
            };

            match pattern {
                Refutable::Boolean(boolean) => {
                    let load_value = self.create_register_assignment(
                        Assignment::Load(Load::new(address)),
                        span,
                    );

                    let (true_target, false_target) = if boolean.value {
                        (success_block_id, else_block_id)
                    } else {
                        (else_block_id, success_block_id)
                    };

                    self.insert_terminator(Terminator::Jump(
                        Jump::Conditional(ConditionalJump {
                            condition: Value::Register(load_value),
                            true_target,
                            false_target,
                        }),
                    ));
                }

                Refutable::Integer(_) | Refutable::Enum(_) => {
                    let expected_value =
                        get_conditional_value(pattern, self.engine())
                            .await
                            .unwrap();

                    let numeric_value = match pattern {
                        Refutable::Integer(_) => self
                            .create_register_assignment(
                                Assignment::Load(Load::new(address)),
                                span,
                            ),
                        Refutable::Enum(enum_pattern) => self
                            .create_register_assignment(
                                Assignment::VariantNumber(VariantNumber {
                                    address: address.clone(),
                                    enum_id: Global::new(
                                        enum_pattern.variant_id.target_id,
                                        self.engine()
                                            .get_parent(enum_pattern.variant_id)
                                            .await
                                            .unwrap(),
                                    ),
                                }),
                                span,
                            ),
                        _ => unreachable!(),
                    };

                    let comparison_register = self.create_register_assignment(
                        Assignment::Binary(Binary {
                            lhs: Value::Register(numeric_value),
                            rhs: Value::Literal(Literal::Numeric(
                                literal::Numeric {
                                    integer_string: self
                                        .engine()
                                        .intern_unsized(
                                            expected_value.to_string(),
                                        ),
                                    decimal_string: None,
                                    r#type: self
                                        .type_of_register(
                                            numeric_value,
                                            handler,
                                        )
                                        .await?,
                                    span,
                                },
                            )),
                            operator:
                                pernixc_ir::value::register::BinaryOperator::Relational(
                                    RelationalOperator::Equal,
                                ),
                        }),
                        span,
                    );

                    self.insert_terminator(Terminator::Jump(
                        Jump::Conditional(ConditionalJump {
                            condition: Value::Register(comparison_register),
                            true_target: success_block_id,
                            false_target: else_block_id,
                        }),
                    ));
                }

                Refutable::Named(_)
                | Refutable::Tuple(_)
                | Refutable::Structural(_)
                | Refutable::Wildcard(_) => unreachable!(),
            }

            if success_block_id != then_block_id {
                self.set_current_block_id(success_block_id);
            }
        }

        Ok(())
    }

    async fn bind_if_match_condition(
        &mut self,
        if_match_condition: &IfMatchCondition,
        then_block_id: ID<Block>,
        else_block_id: ID<Block>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<IfMatchConditionBinding, Error> {
        let Some(binary) = if_match_condition.binary() else {
            return Err(Error::Binding(BindingError(
                if_match_condition.span(),
            )));
        };

        let Some(refutable_pattern) = if_match_condition.refutable_pattern()
        else {
            return Err(Error::Binding(BindingError(
                if_match_condition.span(),
            )));
        };

        let scrutinee =
            self.bind_match_scrutinee_expression(&binary, handler).await?;
        let mut pattern = self
            .bind_pattern(&refutable_pattern, &scrutinee.address_type, handler)
            .await?
            .unwrap_or_else(|| {
                Wildcard { span: refutable_pattern.span() }.into()
            });

        replace_refutable_in_tuple_pack(&mut pattern, handler);

        let refutable_paths = Path::get_refutable_paths(&pattern);

        if refutable_paths.is_empty() {
            self.insert_terminator(Terminator::Jump(Jump::Unconditional(
                UnconditionalJump { target: then_block_id },
            )));
        } else {
            self.bind_if_match_paths(
                &pattern,
                scrutinee.address.clone(),
                scrutinee.address_type.clone(),
                refutable_paths,
                then_block_id,
                else_block_id,
                if_match_condition.span(),
                handler,
            )
            .await
            .map_err(Error::Unrecoverable)?;
        }

        Ok(IfMatchConditionBinding { scrutinee, pattern })
    }

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

                let unit = Type::unit();
                let value = self
                    .bind_value_or_error(
                        block_state,
                        match guidance {
                            Guidance::Expression(type_hint) => *type_hint,
                            Guidance::Statement => Some(&unit),
                        },
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
                                            inline_expression.span(),
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
                                return Err(abort);
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

    #[allow(clippy::type_complexity, clippy::too_many_lines)]
    async fn bind_group_for_if_match_then(
        &mut self,
        expression: &Group,
        allocated_scope_id: ID<pernixc_ir::scope::Scope>,
        if_match_condition_binding: &IfMatchConditionBinding,
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
                    .bind_block_with_post_enter_hook()
                    .maybe_label(indented_group.label().as_ref())
                    .statements(statements.iter())
                    .span(indented_group.span())
                    .scope_id(allocated_scope_id)
                    .successor_block_id(success_sub_block_id)
                    .is_unsafe(indented_group.unsafe_keyword().is_some())
                    .hook(async move |binder| {
                        let name_binding_point = binder
                            .create_name_binding_point_from_match_scrutinee(
                                &if_match_condition_binding.pattern,
                                &if_match_condition_binding.scrutinee,
                                allocated_scope_id,
                                handler,
                            )
                            .await?;

                        binder.add_named_binding_point(name_binding_point);

                        Ok(())
                    })
                    .handler(handler)
                    .call()
                    .await?;

                let unit = Type::unit();
                let value = self
                    .bind_value_or_error(
                        block_state,
                        match guidance {
                            Guidance::Expression(type_hint) => *type_hint,
                            Guidance::Statement => Some(&unit),
                        },
                        handler,
                    )
                    .await?;

                (Some(value), success_sub_block_id)
            }

            Group::Inline(inline_expression) => {
                self.push_scope_with(allocated_scope_id, false);
                let name_binding_point = self
                    .create_name_binding_point_from_match_scrutinee(
                        &if_match_condition_binding.pattern,
                        &if_match_condition_binding.scrutinee,
                        allocated_scope_id,
                        handler,
                    )
                    .await?;
                self.add_named_binding_point(name_binding_point);

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
                                            inline_expression.span(),
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

                                self.type_check(
                                    &value_ty,
                                    Expected::Known(Type::unit()),
                                    expr.span(),
                                    handler,
                                )
                                .await?;
                            }
                            Ok(Expression::LValue(_))
                            | Err(Error::Binding(_)) => {}
                            Err(Error::Unrecoverable(abort)) => {
                                return Err(abort);
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
        let (Some(condition), Some(then_expr)) =
            (syntax_tree.condition(), syntax_tree.then())
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let then_block_id = self.new_block();
        let else_block_id = self.new_block();
        let if_else_successor_block_id = self.new_block();

        let (then_scope_id, else_scope_id) = {
            let scopes = self.new_child_branch(NonZeroUsize::new(2).unwrap());

            (scopes[0], scopes[1])
        };

        let if_match_condition_binding = match condition {
            IfCondition::Boolean(ref binary) => {
                self.bind_if_boolean_condition(
                    binary,
                    then_block_id,
                    else_block_id,
                    handler,
                )
                .await?;

                None
            }
            IfCondition::Match(ref if_match_condition) => Some(
                self.bind_if_match_condition(
                    if_match_condition,
                    then_block_id,
                    else_block_id,
                    handler,
                )
                .await?,
            ),
        };

        // bind the then block
        self.set_current_block_id(then_block_id);

        let (then_value, successor_then_block_id) = match condition {
            IfCondition::Boolean(_) => {
                self.bind_group_for_if_else(
                    &then_expr,
                    then_scope_id,
                    if_else_successor_block_id,
                    guidance,
                    handler,
                )
                .await?
            }
            IfCondition::Match(_) => {
                self.bind_group_for_if_match_then(
                    &then_expr,
                    then_scope_id,
                    if_match_condition_binding.as_ref().unwrap(),
                    if_else_successor_block_id,
                    guidance,
                    handler,
                )
                .await?
            }
        };

        // bind the else block
        self.set_current_block_id(else_block_id);

        let then_type = if let Some(then_value) = &then_value {
            Some(self.type_of_value(then_value, handler).await?)
        } else {
            None
        };

        let (else_value, successor_else_block_id) = match syntax_tree
            .r#else()
            .and_then(|x| x.group_or_if_else())
        {
            Some(GroupOrIfElse::Group(group)) => {
                (self.bind_group_for_if_else(
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
                        (self.bind_value_or_error(&if_else, *ty_hint, handler))
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
                                return Err(abort.into());
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
                            IfMissingElseBranch { if_span: syntax_tree.span() },
                        ));

                        Value::error(then_type.clone(), syntax_tree.span())
                    } else {
                        Value::Literal(Literal::Unit(literal::Unit {
                            span: syntax_tree.span(),
                        }))
                    }
                } else {
                    Value::Literal(Literal::Unit(literal::Unit {
                        span: syntax_tree.span(),
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
                        span: syntax_tree.span(),
                    })),
                    1 => {
                        if self
                            .current_block()
                            .predecessors()
                            .contains(&successor_then_block_id)
                        {
                            then_value.unwrap()
                        } else {
                            assert!(
                                self.current_block()
                                    .predecessors()
                                    .contains(&successor_else_block_id)
                            );
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
                                        FxHashMap::default();

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
                Literal::Unit(literal::Unit { span: syntax_tree.span() }),
            ))),
        }
    }
}
