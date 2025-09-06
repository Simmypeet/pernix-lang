use pernixc_handler::Handler;
use pernixc_ir::value::{
    literal::{Boolean, Literal},
    Value,
};
use pernixc_source_file::SourceElement;

use crate::{
    bind::{Bind, Config, Expression},
    binder::Error,
    diagnostic::Diagnostic,
};

impl Bind<&pernixc_syntax::expression::unit::Boolean>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Boolean,
        _: Config,
        _: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            pernixc_syntax::expression::unit::Boolean::True(_) => true,
            pernixc_syntax::expression::unit::Boolean::False(_) => false,
        };

        Ok(Expression::RValue(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: Some(syntax_tree.span()),
        }))))
    }
}
