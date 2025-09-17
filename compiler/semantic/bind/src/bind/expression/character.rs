use pernixc_handler::Handler;
use pernixc_ir::value::{
    literal::{self, Literal},
    Value,
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Guidance, Expression},
    binder::Error,
    diagnostic::Diagnostic,
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::unit::Character>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Character,
        _: &Guidance<'_>,
        _: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(character) = syntax_tree.character() else {
            return Err(Error::Binding(crate::binder::BindingError(
                syntax_tree.span(),
            )));
        };

        Ok(Expression::RValue(Value::Literal(Literal::Character(
            literal::Character {
                character: character.kind.0,
                r#type: Type::Inference(
                    self.create_type_inference(constraint::Type::Integer),
                ),
                span: Some(syntax_tree.span()),
            },
        ))))
    }
}
