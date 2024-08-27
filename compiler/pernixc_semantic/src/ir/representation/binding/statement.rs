//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{
    expression::{Bind, Config, Target},
    infer, Binder, InternalError,
};
use crate::{
    arena::ID,
    error,
    ir::{
        address::{Address, Memory},
        alloca::Alloca,
        instruction::{Instruction, Store},
        pattern::{Irrefutable, NameBindingPoint, Wildcard},
    },
    symbol::table::{self, resolution},
    type_system::{
        self, simplify,
        term::r#type::{self, Type},
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

                match result {
                    Ok(_) | Err(super::Error::Semantic(_)) => Ok(()),
                    Err(super::Error::Internal(err)) => Err(err),
                }
            }
        }
    }

    /// Binds the given [`syntax_tree::statement::VariableDeclaration`] to the
    /// IR.
    pub fn bind_variable_declaration(
        &mut self,
        syntax_tree: &syntax_tree::statement::VariableDeclaration,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<ID<Alloca<infer::Model>>, InternalError> {
        let initializer =
            self.bind_value_or_error(syntax_tree.expression(), handler)?;

        let initialize_type = self.type_of_value(&initializer)?;

        let mut variable_type = match syntax_tree.type_annotation().as_ref() {
            Some(type_syn) => {
                let type_annotation = self
                    .resolve_type_with_inference(type_syn.ty(), handler)
                    .unwrap_or_else(|| {
                        Type::Inference(self.create_type_inference(
                            r#type::Constraint::All(false),
                        ))
                    });

                let _ = self.type_check(
                    initialize_type,
                    r#type::Expected::Known(type_annotation.clone()),
                    syntax_tree.expression().span(),
                    handler,
                );

                type_annotation
            }
            None => initialize_type,
        };

        let alloca_id =
            self.create_alloca(variable_type.clone(), Some(syntax_tree.span()));

        let _ = self.current_block_mut().insert_instruction(
            Instruction::Store(Store {
                address: Address::Memory(Memory::Alloca(alloca_id)),
                value: initializer,
            }),
        );

        // create the pattern
        variable_type =
            simplify::simplify(&variable_type, &self.create_environment())
                .result;

        let pattern = self
            .create_irrefutable(
                syntax_tree.irrefutable_pattern(),
                &variable_type,
                &Address::Memory(Memory::Alloca(alloca_id)),
                &self.create_handler_wrapper(handler),
            )
            .unwrap_or(Irrefutable::Wildcard(Wildcard));

        let mut name_binding_point = NameBindingPoint::default();
        name_binding_point.add_irrefutable_binding(
            &pattern,
            &self.create_handler_wrapper(handler),
        );

        // add the variable to the stack
        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        Ok(alloca_id)
    }
}

#[cfg(test)]
mod tests;
