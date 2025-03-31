use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::value::{
        literal::{Boolean, Literal},
        Value,
    },
    diagnostic::Diagnostic,
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{Binder, Error};

impl Bind<&syntax_tree::expression::unit::Boolean> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::unit::Boolean,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::unit::Boolean::True(_) => true,
            syntax_tree::expression::unit::Boolean::False(_) => false,
        };

        Ok(Expression::RValue(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: Some(syntax_tree.span()),
        }))))
    }
}

#[cfg(test)]
mod test;
