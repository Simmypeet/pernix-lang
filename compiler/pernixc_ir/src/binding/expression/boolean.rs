use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;

use super::{Bind, Config, Expression};
use crate::{
    binding::{Binder, Error},
    value::literal::{Boolean, Literal},
    Value,
};

impl Bind<&syntax_tree::expression::Boolean> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::Boolean::True(_) => true,
            syntax_tree::expression::Boolean::False(_) => false,
        };

        Ok(Expression::RValue(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: Some(syntax_tree.span()),
        }))))
    }
}

#[cfg(test)]
mod test;
