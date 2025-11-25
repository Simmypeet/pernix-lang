use pernixc_handler::Handler;
use pernixc_ir::value::{
    Value,
    literal::{self, Literal},
};
use pernixc_source_file::SourceElement;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
};

impl Bind<&pernixc_syntax::expression::unit::String> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::String,
        _: &Guidance<'_>,
        _: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(string) = syntax_tree.string() else {
            return Err(Error::Binding(crate::binder::BindingError(
                syntax_tree.span(),
            )));
        };

        Ok(Expression::RValue(Value::Literal(Literal::String(
            literal::String { value: string.kind.0, span: syntax_tree.span() },
        ))))
    }
}
