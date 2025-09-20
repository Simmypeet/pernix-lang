use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_ir::{control_flow_graph::Block, scope::Scope, value::Value};
use pernixc_source_file::SourceElement;
use pernixc_syntax::expression::block::Group;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{type_check::Expected, Binder, Error, UnrecoverableError},
    diagnostic::Diagnostic,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindGroupTarget<'a> {
    Statement,
    Expression(&'a Type),
}

impl Binder<'_> {
    #[allow(clippy::too_many_lines)]
    pub async fn bind_group(
        &mut self,
        expression: &Group,
        allocated_scope_id: ID<Scope>,
        group_target: BindGroupTarget<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(Value, ID<Block>), UnrecoverableError> {
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
                        match group_target {
                            BindGroupTarget::Expression(type_hint) => {
                                Some(type_hint)
                            }
                            BindGroupTarget::Statement => Some(&unit),
                        },
                        handler,
                    )
                    .await?;

                (value, success_sub_block_id)
            }

            Group::Inline(inline_expression) => {
                self.push_scope_with(allocated_scope_id, false);

                let value = match group_target {
                    BindGroupTarget::Expression(type_hint) => 'result: {
                        let Some(expr) = inline_expression.expression() else {
                            break 'result Value::error(
                                type_hint.clone(),
                                Some(inline_expression.span()),
                            );
                        };

                        self.bind_value_or_error(
                            &expr,
                            Some(type_hint),
                            handler,
                        )
                        .await?
                    }

                    BindGroupTarget::Statement => 'result: {
                        let Some(expr) = inline_expression.expression() else {
                            break 'result Value::error(
                                Type::unit(),
                                Some(inline_expression.span()),
                            );
                        };

                        match self
                            .bind(&expr, &Guidance::Statement, handler)
                            .await
                        {
                            Ok(Expression::RValue(value)) => {
                                let value_ty =
                                    self.type_of_value(&value, handler).await?;

                                // must be type unit
                                self.type_check_as_diagnostic(
                                    &value_ty,
                                    Expected::Known(Type::unit()),
                                    expr.span(),
                                    handler,
                                )
                                .await?
                                .map_or(
                                    value,
                                    |diag| {
                                        handler.receive(diag);

                                        Value::error(
                                            Type::unit(),
                                            Some(expr.span()),
                                        )
                                    },
                                )
                            }

                            // no need to type check lvalue
                            Ok(Expression::LValue(_))
                            | Err(Error::Binding(_)) => {
                                Value::unit(Some(expr.span()))
                            }

                            Err(Error::Unrecoverable(abort)) => {
                                return Err(abort)
                            }
                        }
                    }
                };

                self.pop_scope(allocated_scope_id);

                (value, self.current_block_id())
            }
        };

        Ok((value, group_sucessor_block_id))
    }
}
