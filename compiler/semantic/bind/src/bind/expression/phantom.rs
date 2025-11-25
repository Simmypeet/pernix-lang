use pernixc_handler::Handler;
use pernixc_ir::value::{
    Value,
    literal::{Literal, Phantom},
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{self, Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::Error,
    infer::constraint,
};

impl Bind<&pernixc_syntax::expression::unit::Phantom>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Phantom,
        _: &Guidance<'_>,
        _: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        Ok(Expression::RValue(Value::Literal(Literal::Phantom(Phantom {
            r#type: Type::Phantom(r#type::Phantom(Box::new(Type::Inference(
                self.create_type_inference(constraint::Type::All(false)),
            )))),
            span: syntax_tree.span(),
        }))))
    }
}
