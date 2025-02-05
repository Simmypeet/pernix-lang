use pernixc_handler::Handler;
use pernixc_lexical::token;
use pernixc_source_file::SourceElement;
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    r#type::{Array, Primitive, Qualifier, Reference, Type},
};

use super::{Bind, Config, Expression};
use crate::{
    binding::{infer::InferenceVariable, Binder, Error},
    model::NoConstraint,
    value::literal::{self, Literal},
    Value,
};

impl Bind<&token::String> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &token::String,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let value = if let Some(value) = &syntax_tree.value {
            let value = value.as_bytes().to_vec();

            Value::Literal(Literal::String(literal::String {
                value,
                span: Some(syntax_tree.span()),
            }))
        } else {
            let constant_inference = InferenceVariable::new();

            assert!(self
                .inference_context
                .register::<Constant<_>>(constant_inference, NoConstraint));

            Value::Literal(Literal::Error(literal::Error {
                // &'static [uint8: len]
                r#type: Type::Reference(Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: Lifetime::Static,
                    pointee: Box::new(Type::Array(Array {
                        length: Constant::Inference(constant_inference),
                        r#type: Box::new(Type::Primitive(Primitive::Uint8)),
                    })),
                }),
                span: Some(syntax_tree.span()),
            }))
        };

        Ok(Expression::RValue(value))
    }
}
