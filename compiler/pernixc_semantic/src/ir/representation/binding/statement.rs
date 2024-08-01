//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{
    expression::{Bind, Config, Target},
    infer, Binder,
};
use crate::{
    arena::ID,
    error,
    ir::{
        address::{Address, Memory},
        alloca::Alloca,
        instruction::{AllocaAllocation, Initialize, Instruction},
        pattern::{Irrefutable, NameBindingPoint, Wildcard},
    },
    symbol::table::{self, resolution::Observer},
    type_system::{
        simplify,
        term::r#type::{self, Type},
    },
};

impl<'t, S: table::State, O: Observer<S, infer::Model>> Binder<'t, S, O> {
    /// Binds the given [`syntax_tree::statement::Statement`] to the IR.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &syntax_tree::statement::Statement,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match syntax_tree {
            syntax_tree::statement::Statement::VariableDeclaration(
                syntax_tree,
            ) => {
                self.bind_variable_declaration(syntax_tree, handler);
            }
            syntax_tree::statement::Statement::Expressive(expressive) => {
                match expressive {
                    syntax_tree::statement::Expressive::Semi(semi) => match semi
                        .expression()
                    {
                        syntax_tree::statement::SemiExpression::Binary(
                            syntax_tree,
                        ) => {
                            let _ = self.bind(
                                syntax_tree,
                                Config { target: Target::Statement },
                                handler,
                            );
                        }
                        syntax_tree::statement::SemiExpression::Terminator(
                            _,
                        ) => todo!(),
                    },
                    syntax_tree::statement::Expressive::Brace(_) => todo!(),
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
    ) -> ID<Alloca<infer::Model>> {
        let initializer =
            self.bind_value_or_error(syntax_tree.expression(), handler);

        let initialize_type = self
            .intermediate_representation
            .registers
            .get(initializer)
            .unwrap()
            .r#type
            .clone();

        let mut variable_type = match syntax_tree.type_annotation().as_ref() {
            Some(type_syn) => {
                let type_annotation = self
                    .resolve_type_with_inference(type_syn.ty(), handler)
                    .unwrap_or_else(|| {
                        Type::Inference(
                            self.create_type_inference(r#type::Constraint::All),
                        )
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
            self.intermediate_representation.allocas.insert(Alloca {
                r#type: variable_type.clone(),
                span: Some(syntax_tree.span()),
            });

        self.current_block_mut().insert_basic(Instruction::AllocaAllocation(
            AllocaAllocation { id: alloca_id },
        ));

        self.current_block_mut().insert_basic(Instruction::Initialize(
            Initialize {
                address: Address::Base(Memory::Alloca(alloca_id)),
                value: initializer,
            },
        ));

        // create the pattern
        variable_type =
            simplify::simplify(&variable_type, &self.create_environment())
                .result;

        let pattern = self
            .create_irrefutable(
                syntax_tree.irrefutable_pattern(),
                &variable_type,
                &Address::Base(Memory::Alloca(alloca_id)),
                &self.create_handler_wrapper(handler),
            )
            .unwrap_or(Irrefutable::Wildcard(Wildcard));

        let mut name_binding_point = NameBindingPoint::default();
        name_binding_point.add_irrefutable_binding(
            &pattern,
            &self.create_handler_wrapper(handler),
        );

        self.stack
            .current_scope_mut()
            .add_named_binding_point(name_binding_point);

        alloca_id
    }
}

#[cfg(test)]
mod tests;
