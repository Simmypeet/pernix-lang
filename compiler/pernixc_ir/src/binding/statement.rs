//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::{Qualifier, Type};
use pernixc_type_system::diagnostic::OverflowOperation;

use super::{
    expression::{Bind, Config, Expression, Target},
    infer, AbruptError, Binder, Error,
};
use crate::{
    address::{Address, Memory},
    binding::AddContextExt,
    instruction::{Instruction, RegisterDiscard},
    model,
    pattern::{NameBindingPoint, Wildcard},
    value::literal::{self, Literal},
    Value,
};

impl Binder<'_> {
    /// Binds the given [`syntax_tree::statement::Statement`] to the IR.
    ///
    /// # Errors
    ///
    /// See [`InternalError`] for more information.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &syntax_tree::statement::Statement,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(), AbruptError> {
        match syntax_tree {
            syntax_tree::statement::Statement::VariableDeclaration(
                syntax_tree,
            ) => {
                self.bind_variable_declaration(syntax_tree, handler)?;

                Ok(())
            }
            syntax_tree::statement::Statement::Expressive(expressive) => {
                let scope_id = self.push_scope();

                let result = match expressive {
                    syntax_tree::statement::Expressive::Semi(semi) => self
                        .bind(
                            semi.expression(),
                            Config { target: Target::Statement },
                            handler,
                        ),
                    syntax_tree::statement::Expressive::Brace(brace) => self
                        .bind(
                            brace,
                            Config { target: Target::Statement },
                            handler,
                        ),
                };

                let result = match result {
                    Ok(Expression::RValue(Value::Register(register_id))) => {
                        let _ = self.current_block_mut().add_instruction(
                            Instruction::RegisterDiscard(RegisterDiscard {
                                id: register_id,
                            }),
                        );

                        Ok(())
                    }

                    Err(Error::Semantic(_)) | Ok(_) => Ok(()),

                    Err(Error::Abrupt(err)) => {
                        return Err(err);
                    }
                };

                self.pop_scope(scope_id);

                result
            }
        }
    }

    /// Binds the given [`syntax_tree::statement::VariableDeclaration`] to the
    /// IR.
    ///
    /// # Errors
    ///
    /// See [`InternalError`] for more information.
    ///
    /// # Returns
    ///
    /// An `Ok` of tuple address where the initializer value is stored and a
    /// boolean indicating if the address is from the l-value or is newly
    /// created to store the r-value.
    #[allow(clippy::too_many_lines)]
    pub fn bind_variable_declaration(
        &mut self,
        syntax_tree: &syntax_tree::statement::VariableDeclaration,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<(Address<infer::Model>, bool), AbruptError> {
        // capture the current scope id first, since we will be working in
        // a new temporary scope from now on
        let variable_scope_id = self.stack.current_scope().scope_id();

        let scope_id = self.push_scope();

        assert_ne!(variable_scope_id, scope_id);

        let binding_result = self.bind(
            syntax_tree.expression(),
            Config { target: Target::LValue },
            handler,
        );

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
                            Some(syntax_tree.irrefutable_pattern().span()),
                            syntax_tree.span(),
                        ),
                    )),
                    Qualifier::Mutable, /* has the highest mutability */
                    false,
                )
            }

            Err(err) => match err {
                Error::Semantic(semantic_error) => {
                    (
                        Address::Memory(Memory::Alloca({
                            let ty_inference = self.create_type_inference(
                                model::Constraint::All(false),
                            );
                            self.create_alloca_with_value(
                                Value::Literal(Literal::Error(
                                    literal::Error {
                                        r#type: Type::Inference(ty_inference),
                                        span: Some(semantic_error.0),
                                    },
                                )),
                                variable_scope_id,
                                Some(syntax_tree.irrefutable_pattern().span()),
                                syntax_tree.span(),
                            )
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Abrupt(internal_error) => return Err(internal_error),
            },
        };

        let type_of_address = self.type_of_address(&address).map_err(|x| {
            x.into_type_system_overflow(
                OverflowOperation::TypeOf,
                syntax_tree.expression().span(),
            )
        })?;

        if let Some(type_annotation) = syntax_tree.type_annotation() {
            let type_annotation = self
                .resolve_type_with_inference(type_annotation.r#type(), handler);

            let _ = self.type_check(
                &type_of_address,
                infer::Expected::Known(type_annotation),
                syntax_tree.expression().span(),
                true,
                handler,
            )?;
        };

        let type_of_address = self
            .create_environment()
            .simplify(type_of_address)
            .map_err(|x| {
                x.into_type_system_overflow(
                    OverflowOperation::TypeOf,
                    syntax_tree.expression().span(),
                )
            })?;

        let pattern = self
            .bind_pattern(
                &type_of_address.result,
                syntax_tree.irrefutable_pattern(),
                &self.create_handler_wrapper(handler),
            )?
            .unwrap_or_else(|| {
                Wildcard { span: syntax_tree.irrefutable_pattern().span() }
                    .into()
            });

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_irrefutable_named_binding_point(
            &mut name_binding_point,
            &pattern,
            &type_of_address.result,
            address.clone(),
            Some(syntax_tree.expression().span()),
            qualifier,
            from_lvalue,
            variable_scope_id,
            &self.create_handler_wrapper(handler),
        )?;

        // pop temporary scope
        self.pop_scope(scope_id);

        // add the variable to the stack to the captured `variable_scope_id`
        assert_eq!(variable_scope_id, self.stack.current_scope().scope_id());

        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        Ok((address, from_lvalue))
    }
}

// #[cfg(test)]
// mod test;
