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
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::r#type::{Constraint, Type},
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&token::Character> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &token::Character,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let inference_variable = InferenceVariable::new();

        assert!(self.inference_context.register::<Type<_>>(
            inference_variable,
            Constraint::UnsignedInteger
        ));

        let value = syntax_tree.value.map_or_else(
            || {
                Value::Literal(Literal::Error(literal::Error {
                    r#type: Type::Inference(inference_variable),
                    span: syntax_tree.span(),
                }))
            },
            |character| {
                Value::Literal(Literal::Character(literal::Character {
                    character,
                    r#type: Type::Inference(inference_variable),
                    span: syntax_tree.span(),
                }))
            },
        );

        Ok(Expression::RValue(value))
    }
}
