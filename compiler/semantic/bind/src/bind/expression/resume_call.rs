use pernixc_handler::Handler;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::unit::ResumeCall> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::ResumeCall,
        guidance: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        todo!()
    }
}
