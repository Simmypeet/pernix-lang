#![allow(missing_docs)]
use pernixc_handler::Handler;
use pernixc_ir::{
    address::{Address, Memory},
    instruction::{Instruction, RegisterDiscard},
    pattern::{Irrefutable, NameBindingPoint, Wildcard},
    value::{
        literal::{self, Literal},
        Value,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Qualifier, Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{type_check::Expected, Binder, Error, UnrecoverableError},
    diagnostic::Diagnostic,
    inference_context::constraint,
};

impl Binder<'_> {
    /// Binds the given [`pernixc_syntax::statement::Statement`] to the IR.
    #[allow(clippy::missing_errors_doc)]
    pub async fn bind_statement(
        &mut self,
        syntax_tree: &pernixc_syntax::statement::Statement,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        match syntax_tree {
            pernixc_syntax::statement::Statement::VariableDeclaration(
                syntax_tree,
            ) => {
                self.bind_variable_declaration(syntax_tree, handler).await?;

                Ok(())
            }
            pernixc_syntax::statement::Statement::Expression(expression) => {
                let scope_id = self.push_scope(false);

                let result =
                    self.bind(expression, &Guidance::Statement, handler).await;

                let result = match result {
                    Ok(Expression::RValue(Value::Register(register_id))) => {
                        self.push_instruction(Instruction::RegisterDiscard(
                            RegisterDiscard { id: register_id },
                        ));

                        Ok(())
                    }

                    Err(Error::Binding(_)) | Ok(_) => Ok(()),

                    Err(Error::Unrecoverable(err)) => {
                        return Err(err);
                    }
                };

                self.pop_scope(scope_id);

                result
            }
        }
    }

    /// Binds the given [`pernixc_syntax::statement::VariableDeclaration`] to
    /// the IR.
    ///
    /// # Returns
    ///
    /// An `Ok` of tuple address where the initializer value is stored and a
    /// boolean indicating if the address is from the l-value or is newly
    /// created to store the r-value.
    #[allow(clippy::too_many_lines, clippy::missing_errors_doc)]
    pub async fn bind_variable_declaration(
        &mut self,
        syntax_tree: &pernixc_syntax::statement::VariableDeclaration,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<(Address, bool), UnrecoverableError> {
        // capture the current scope id first, since we will be working in
        // a new temporary scope from now on
        let variable_scope_id = self.stack().current_scope().scope_id();

        let scope_id = self.push_scope(false);

        assert_ne!(variable_scope_id, scope_id);

        let type_annotation = if let Some(type_annotation) =
            syntax_tree.type_annotation().and_then(|x| x.r#type())
        {
            Some(
                self.resolve_type_with_inference(&type_annotation, handler)
                    .await?,
            )
        } else {
            None
        };

        let binding_result = match syntax_tree.expression() {
            Some(expression) => {
                self.bind(
                    &expression,
                    &Guidance::Expression(type_annotation.as_ref()),
                    handler,
                )
                .await
            }
            None => Ok(Expression::RValue(Value::Literal(
                pernixc_ir::value::literal::Literal::Error(
                    pernixc_ir::value::literal::Error {
                        r#type: type_annotation.clone().unwrap_or_else(|| {
                            Type::Inference(self.create_type_inference(
                                constraint::Type::All(true),
                            ))
                        }),
                        span: Some(syntax_tree.span()),
                    },
                ),
            ))),
        };

        let (address, qualifier, from_lvalue) = match binding_result {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => {
                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(
                            value,
                            variable_scope_id,
                            Some(
                                syntax_tree.irrefutable_pattern().map_or_else(
                                    || syntax_tree.span(),
                                    |x| x.span(),
                                ),
                            ),
                            syntax_tree.span(),
                            handler,
                        )
                        .await?,
                    )),
                    Qualifier::Mutable, /* has the highest mutability */
                    false,
                )
            }

            Err(err) => match err {
                Error::Binding(semantic_error) => {
                    (
                        Address::Memory(Memory::Alloca({
                            let ty_inference = self.create_type_inference(
                                constraint::Type::All(true),
                            );
                            self.create_alloca_with_value(
                                Value::Literal(Literal::Error(
                                    literal::Error {
                                        r#type: Type::Inference(ty_inference),
                                        span: Some(semantic_error.0),
                                    },
                                )),
                                variable_scope_id,
                                Some(
                                    syntax_tree
                                        .irrefutable_pattern()
                                        .map_or_else(
                                            || syntax_tree.span(),
                                            |x| x.span(),
                                        ),
                                ),
                                syntax_tree.span(),
                                handler,
                            )
                            .await?
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Unrecoverable(internal_error) => {
                    return Err(internal_error)
                }
            },
        };

        let type_of_address = self.type_of_address(&address, handler).await?;

        if let Some(type_annotation) = &type_annotation {
            let _ = self
                .type_check(
                    &type_of_address,
                    Expected::Known(type_annotation.clone()),
                    syntax_tree
                        .expression()
                        .map_or_else(|| syntax_tree.span(), |x| x.span()),
                    handler,
                )
                .await?;
        }

        let type_of_address = self.type_of_address(&address, handler).await?;

        let pattern = match syntax_tree.irrefutable_pattern() {
            Some(syn) => self
                .bind_pattern(&syn, &type_of_address, handler)
                .await?
                .unwrap_or_else(|| Wildcard { span: syn.span() }.into()),
            None => {
                Irrefutable::Wildcard(Wildcard { span: syntax_tree.span() })
            }
        };

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_irrefutable_named_binding_point(
            &mut name_binding_point,
            &pattern,
            &type_of_address,
            address.clone(),
            qualifier,
            &crate::pattern::insert_name_binding::Config {
                must_copy: from_lvalue,
                scope_id: variable_scope_id,
                address_span: Some(
                    syntax_tree
                        .expression()
                        .map_or_else(|| syntax_tree.span(), |x| x.span()),
                ),
            },
            handler,
        )
        .await?;

        // pop temporary scope
        self.pop_scope(scope_id);

        // add the variable to the stack to the captured `variable_scope_id`
        assert_eq!(variable_scope_id, self.stack().current_scope().scope_id());

        self.add_named_binding_point(name_binding_point);

        Ok((address, from_lvalue))
    }
}
