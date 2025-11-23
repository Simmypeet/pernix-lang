use pernixc_handler::Handler;
use pernixc_ir::value::{
    Value,
    literal::{Literal, Unreachable},
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::Error,
    diagnostic::Diagnostic,
    infer::constraint,
};

impl Bind<&pernixc_syntax::expression::unit::Panic>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Panic,
        _: &Guidance<'_>,
        _: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        self.insert_panic_terminator();

        Ok(Expression::RValue(Value::Literal(Literal::Unreachable(
            Unreachable {
                r#type: Type::Inference(
                    self.create_type_inference(constraint::Type::All(true)),
                ),
                span: Some(syntax_tree.span()),
            },
        ))))
    }
}
