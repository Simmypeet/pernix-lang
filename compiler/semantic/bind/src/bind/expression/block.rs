use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;

use crate::{
    bind::{Bind, Config, Expression},
    binder::{BindingError, Error},
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::postfix::Postfix>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::postfix::Postfix,
        config: &Config<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(unit) = syntax_tree.unit() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        self.bind(&unit, config, handler).await
    }
}
