use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{Jump, Terminator, UnconditionalJump},
    value::{
        literal::{Literal, Unreachable},
        Value,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
    diagnostic::{Diagnostic, LoopControlFlow},
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::terminator::Continue> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::terminator::Continue,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Continue,
            syntax_tree.label().and_then(|x| x.identifier()),
            syntax_tree.span(),
            handler,
        )?;
        let loop_block_id = self.get_loop_state(loop_scope_id).loop_block_id();

        // pop all the needed scopes
        self.pop_all_scope_to(loop_scope_id);

        // jump to the loop block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: loop_block_id },
        )));

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: Type::Inference(
                self.create_type_inference(constraint::Type::All(true)),
            ),
            span: Some(syntax_tree.span()),
        }));

        Ok(Expression::RValue(value))
    }
}
