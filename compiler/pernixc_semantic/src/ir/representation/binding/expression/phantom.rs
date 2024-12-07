use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

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
    > Bind<&syntax_tree::expression::Phantom> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Phantom,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let inference = InferenceVariable::new();

        assert!(self
            .inference_context
            .register::<Type<_>>(inference, Constraint::All(false)));

        Ok(Expression::RValue(Value::Literal(Literal::Phantom(
            literal::Phantom {
                r#type: Type::Inference(inference),
                span: syntax_tree.span(),
            },
        ))))
    }
}
