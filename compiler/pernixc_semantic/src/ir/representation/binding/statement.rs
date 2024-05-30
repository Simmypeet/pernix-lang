//! Contains the logic to bind a statement syntax tree to the IR.

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{
    expression::{Config, Target},
    Binder,
};
use crate::{
    error,
    symbol::table::{self, resolution::Observer},
};

impl<'t, C, S: table::State, O: Observer<S, super::Model>> Binder<'t, C, S, O> {
    /// Binds the given [`syntax_tree::statement::Statement`] to the IR.
    pub fn bind_statement(
        &mut self,
        syntax_tree: &syntax_tree::statement::Statement,
        handler: &dyn Handler<Box<dyn error::Error>>,
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
                            let _ = self.bind_binary(
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
}
