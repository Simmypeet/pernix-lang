use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::{
        model::Constraint,
        value::{
            literal::{self, Literal},
            Value,
        },
    },
    diagnostic::Diagnostic,
    term::r#type::Type,
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{infer::InferenceVariable, Binder, Error};

impl Bind<&syntax_tree::expression::unit::Phantom> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::unit::Phantom,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let inference = InferenceVariable::new();

        assert!(self
            .inference_context
            .register::<Type<_>>(inference, Constraint::All(false)));

        Ok(Expression::RValue(Value::Literal(Literal::Phantom(
            literal::Phantom {
                r#type: Type::Inference(inference),
                span: Some(syntax_tree.span()),
            },
        ))))
    }
}
