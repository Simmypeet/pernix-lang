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

        let postfix_operators = syntax_tree.operators().collect::<Vec<_>>();

        // directly bind the unit if there are no postfix operators
        if postfix_operators.is_empty() {
            return self.bind(&unit, config, handler).await;
        }

        // otherwise, bind as an rvalue first
        for operator in postfix_operators {
            match operator {
                pernixc_syntax::expression::postfix::Operator::MethodCall(
                    _method_call,
                ) => todo!(),
                pernixc_syntax::expression::postfix::Operator::Cast(_cast) => {
                    todo!()
                }
                pernixc_syntax::expression::postfix::Operator::Access(
                    _access,
                ) => todo!(),
            }
        }

        todo!()
    }
}
