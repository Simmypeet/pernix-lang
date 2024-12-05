use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error,
    ir::{
        self,
        representation::binding::{infer, Binder, Error},
        value::{literal::{Boolean, Literal}, Value},
    },
    symbol::table::{self, resolution},
    type_system,
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Bind<&syntax_tree::expression::Boolean> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = match syntax_tree {
            syntax_tree::expression::Boolean::True(_) => true,
            syntax_tree::expression::Boolean::False(_) => false,
        };

        Ok(Expression::RValue(Value::Literal(Literal::Boolean(Boolean {
            value,
            span: syntax_tree.span(),
        }))))
    }
}
