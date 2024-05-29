use pernixc_syntax::syntax_tree;

use super::{
    expression::{Config, Target},
    Binder,
};
use crate::symbol::table::{self, resolution::Observer};

impl<'t, 'h, C, S: table::State, O: Observer<S, super::Model>>
    Binder<'t, 'h, C, S, O>
{
    /// Binds the given [`syntax_tree::statement::Statement`] to the IR.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &syntax_tree::statement::Statement,
    ) {
        match syntax_tree {
            syntax_tree::statement::Statement::VariableDeclaration(_) => {
                todo!()
            }
            syntax_tree::statement::Statement::Expressive(expressive) => {
                match expressive {
                    syntax_tree::statement::Expressive::Semi(semi) => match semi
                        .expression()
                    {
                        syntax_tree::statement::SemiExpression::Binary(
                            syntax_tree,
                        ) => {
                            self.bind_binary(syntax_tree, Config {
                                target: Target::Statement,
                            });
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
}
