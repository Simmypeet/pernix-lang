//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self};

use super::{
    expression::{Bind, Config, Expression, Target},
    infer, Binder, Error, InternalError,
};
use crate::{
    error,
    ir::{
        address::{Address, Memory},
        pattern::{self, Irrefutable, NameBindingPoint, Pattern, Wildcard},
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
        TO: type_system::observer::Observer<infer::Model, S>,
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

                self.pop_scope(scope_id);

                match result {
                    Ok(_) | Err(Error::Semantic(_)) => Ok(()),
                    Err(Error::Internal(err)) => Err(err),
                }
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
        let (address, qualifier, from_lvalue) = match self.bind(
            syntax_tree.expression(),
            Config { target: Target::LValue },
            handler,
        ) {
            Ok(Expression::LValue(lvalue)) => {
                (lvalue.address, lvalue.qualifier, true)
            }

            Ok(Expression::RValue(value)) => {
                (
                    Address::Memory(Memory::Alloca(
                        self.create_alloca_with_value(value),
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
                            self.create_alloca_with_value(Value::Literal(
                                Literal::Error(literal::Error {
                                    r#type: Type::Inference(ty_inference),
                                    span: Some(semantic_error.0),
                                }),
                            ))
                        })),
                        Qualifier::Mutable, /* has the highest mutability */
                        false,
                    )
                }
                Error::Internal(internal_error) => return Err(internal_error),
            },

            Ok(Expression::SideEffect) => unreachable!(),
        };
        self.pop_scope(scope_id);

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
                r#type::Expected::Known(type_annotation.clone()),
                syntax_tree.expression().span(),
                true,
                handler,
            );
        };

        let type_of_address =
            simplify::simplify(&type_of_address, &self.create_environment())
                .result;

        let pattern = match Irrefutable::bind(
            syntax_tree.irrefutable_pattern(),
            &type_of_address,
            self.current_site,
            &self.create_environment(),
            handler,
        ) {
            Ok(v) => v.result,
            Err(pattern::Error::Semantic) => Irrefutable::Wildcard(Wildcard {
                span: Some(syntax_tree.irrefutable_pattern().span()),
            }),
            Err(err) => {
                panic!("unexpected error: {err:#?}");
            }
        };

        let mut name_binding_point = NameBindingPoint::default();
        self.insert_named_binding_point(
            &mut name_binding_point,
            &pattern,
            &type_of_address,
            address.clone(),
            qualifier,
            from_lvalue,
            &self.create_handler_wrapper(handler),
        );

        // add the variable to the stack
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        Ok((address, from_lvalue))
    }
}

#[cfg(test)]
mod tests;
