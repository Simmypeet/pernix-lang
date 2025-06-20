use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::{
        instruction::Terminator,
        model::Constraint,
        value::{
            literal::{Literal, Unreachable},
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

impl Bind<&syntax_tree::expression::unit::Panic> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::unit::Panic,
        _: Config,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let _ = self
            .intermediate_representation
            .control_flow_graph
            .insert_terminator(self.current_block_id, Terminator::Panic);

        Ok(Expression::RValue(Value::Literal(Literal::Unreachable(
            Unreachable {
                r#type: {
                    let inference_variable = InferenceVariable::new();

                    assert!(self
                        .inference_context
                        .register(inference_variable, Constraint::All(true)));

                    Type::Inference(inference_variable)
                },

                span: Some(syntax_tree.span()),
            },
        ))))
    }
}
