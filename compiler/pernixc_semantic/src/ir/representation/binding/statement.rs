//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self};

use super::{
    expression::{Bind, Config, Expression, Target},
    infer, Binder, Error, InternalError,
};
use crate::{
    error::{self, OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::{Address, Memory},
        instruction::{Instruction, RegisterDiscard},
        pattern::{NameBindingPoint, Wildcard},
        representation::borrow,
        value::{
            literal::{self, Literal},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self, simplify,
        term::r#type::{self, Qualifier, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Binder<'t, S, RO, TO>
{
    /// Binds the given [`syntax_tree::statement::Statement`] to the IR.
    ///
    /// # Errors
    ///
    /// See [`InternalError`] for more information.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &syntax_tree::statement::Statement,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), InternalError> {
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
                    syntax_tree::statement::Expressive::Semi(semi) => match semi
                        .expression()
                    {
                        syntax_tree::statement::SemiExpression::Binary(
                            syntax_tree,
                        ) => self.bind(
                            syntax_tree,
                            Config { target: Target::Statement },
                            handler,
                        ),
                        syntax_tree::statement::SemiExpression::Terminator(
                            syntax_tree,
                        ) => self.bind(
                            syntax_tree,
                            Config { target: Target::Statement },
                            handler,
                        ),
                    },
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

                    Err(Error::Internal(err)) => {
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
    pub fn bind_variable_declaration(
        &mut self,
        syntax_tree: &syntax_tree::statement::VariableDeclaration,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(Address<infer::Model>, bool), InternalError> {
        let scope_id = self.push_scope();
        let binding_result = self.bind(
            syntax_tree.expression(),
            Config { target: Target::LValue },
            handler,
        );
        self.pop_scope(scope_id);

        let (address, qualifier, from_lvalue) = match binding_result {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => {
                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(
                            value,
                            Some(syntax_tree.irrefutable_pattern().span()),
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
                                r#type::Constraint::All(false),
                            );
                            self.create_alloca_with_value(
                                Value::Literal(Literal::Error(
                                    literal::Error {
                                        r#type: Type::Inference(ty_inference),
                                        span: semantic_error.0,
                                    },
                                )),
                                Some(syntax_tree.irrefutable_pattern().span()),
                            )
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Internal(internal_error) => return Err(internal_error),
            },
        };

        let type_of_address = self.type_of_address(&address)?;

        if let Some(type_annotation) = syntax_tree.type_annotation() {
            let type_annotation = self
                .resolve_type_with_inference(type_annotation.r#type(), handler)
                .unwrap_or_else(|| {
                    Type::Inference(
                        self.create_type_inference(r#type::Constraint::All(
                            false,
                        )),
                    )
                });

            let _ = self.type_check(
                &type_of_address,
                r#type::Expected::Known(type_annotation),
                syntax_tree.expression().span(),
                true,
                handler,
            )?;
        };

        let type_of_address =
            simplify::simplify(&type_of_address, &self.create_environment())
                .map_err(|overflow_error| TypeSystemOverflow {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: syntax_tree.expression().span(),
                    overflow_error,
                })?
                .result;

        let pattern = self
            .bind_pattern(
                &type_of_address,
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
            &type_of_address,
            address.clone(),
            Some(syntax_tree.expression().span()),
            qualifier,
            from_lvalue,
            &self.create_handler_wrapper(handler),
        )?;

        // add the variable to the stack
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        Ok((address, from_lvalue))
    }
}

#[cfg(test)]
mod test;
