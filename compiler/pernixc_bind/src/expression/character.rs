use pernixc_handler::Handler;
use pernixc_lexical::token;
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

use super::{Bind, Config, Expression};
use crate::{infer::InferenceVariable, Binder, Error};

impl Bind<&token::Character> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &token::Character,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let inference_variable = InferenceVariable::new();

        assert!(self
            .inference_context
            .register::<Type<_>>(inference_variable, Constraint::Integer));

        let value = syntax_tree.value.map_or_else(
            || {
                Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference_variable),
                    span: Some(syntax_tree.span()),
                }))
            },
            |character| {
                Value::Literal(Literal::Character(literal::Character {
                    character,
                    r#type: Type::Inference(inference_variable),
                    span: Some(syntax_tree.span()),
                }))
            },
        );

        Ok(Expression::RValue(value))
    }
}
