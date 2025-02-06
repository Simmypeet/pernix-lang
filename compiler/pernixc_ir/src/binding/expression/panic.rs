use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{infer::InferenceVariable, Binder, Error},
    instruction::Terminator,
    model::Constraint,
    value::{
        literal::{Literal, Unreachable},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Panic> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Panic,
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
