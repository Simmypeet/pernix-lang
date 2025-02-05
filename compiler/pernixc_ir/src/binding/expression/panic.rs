use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression};
use crate::{
    error,
    ir::{
        self,
        instruction::Terminator,
        representation::{
            binding::{
                infer::{self, InferenceVariable},
                Binder, Error,
            },
            borrow,
        },
        value::{
            literal::{Literal, Unreachable},
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
    > Bind<&syntax_tree::expression::Panic> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Panic,
        _: Config,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        assert!(self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(self.current_block_id, Terminator::Panic)
            .map_or_else(|e| !e.is_invalid_block_id(), |()| true));

        Ok(Expression::RValue(Value::Literal(Literal::Unreachable(
            Unreachable {
                r#type: {
                    let inference_variable = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference_variable, Constraint::All(true)));

                    Type::Inference(inference_variable)
                },

                span: syntax_tree.span(),
            },
        ))))
    }
}
