use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_lexical::token;

use super::{Bind, Config, Expression};
use crate::{
    error,
    ir::{
        self,
        representation::{
            binding::{
                infer::{self, InferenceVariable},
                Binder, Error,
            },
            borrow,
        },
        value::{
            literal::{self, Literal},
            Value,
        },
        NoConstraint,
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{self, Qualifier, Type},
        },
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&token::String> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &token::String,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let value = if let Some(value) = &syntax_tree.value {
            let value = value.as_bytes().to_vec();

            Value::Literal(Literal::String(literal::String {
                value,
                span: syntax_tree.span(),
            }))
        } else {
            let constant_inference = InferenceVariable::new();

            assert!(self
                .inference_context
                .register::<Constant<_>>(constant_inference, NoConstraint));

            Value::Literal(Literal::Error(literal::Error {
                // &'static [uint8: len]
                r#type: Type::Reference(r#type::Reference {
                    qualifier: Qualifier::Immutable,
                    lifetime: Lifetime::Static,
                    pointee: Box::new(Type::Array(r#type::Array {
                        length: Constant::Inference(constant_inference),
                        r#type: Box::new(Type::Primitive(
                            r#type::Primitive::Uint8,
                        )),
                    })),
                }),
                span: (syntax_tree.span()),
            }))
        };

        Ok(Expression::RValue(value))
    }
}
