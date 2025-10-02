use pernixc_handler::Handler;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::block::Do> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Do,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        syntax_tree.with()
    }
}
