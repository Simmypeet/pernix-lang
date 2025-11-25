use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{Jump, Terminator, UnconditionalJump},
    value::{
        Value,
        literal::{Literal, Unreachable},
    },
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error, type_check::Expected},
    diagnostic::{Diagnostic, LoopControlFlow},
    infer::constraint,
};

impl Bind<&pernixc_syntax::expression::terminator::Break> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::terminator::Break,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let loop_scope_id = self.find_loop_scope_id(
            LoopControlFlow::Break,
            syntax_tree.label().and_then(|x| x.identifier()),
            syntax_tree.span(),
            handler,
        )?;

        let loop_state = self.get_loop_state(loop_scope_id);
        let exit_block_id = loop_state.exit_block_id();
        let expected_ty = loop_state.get_expected_type();

        let value = match syntax_tree.binary() {
            Some(binary) => {
                Box::pin(self.bind_value_or_error(
                    &binary,
                    expected_ty.as_ref(),
                    handler,
                ))
                .await?
            }
            None => {
                if let Some(expected_ty) = expected_ty {
                    if self
                        .type_check_as_diagnostic(
                            &Type::unit(),
                            Expected::Known(expected_ty.clone()),
                            syntax_tree.span(),
                            handler,
                        )
                        .await?
                        .is_some()
                    {
                        Value::error(expected_ty, syntax_tree.span())
                    } else {
                        Value::unit(syntax_tree.span())
                    }
                } else {
                    Value::unit(syntax_tree.span())
                }
            }
        };

        // add the break value to the loop state
        self.break_value(loop_scope_id, value, handler).await?;

        // pop all the needed scopes
        self.pop_all_scope_to(loop_scope_id);

        // jump to the exit block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: exit_block_id },
        )));

        let value = Value::Literal(Literal::Unreachable(Unreachable {
            r#type: Type::Inference(
                self.create_type_inference(constraint::Type::All(true)),
            ),
            span: syntax_tree.span(),
        }));

        Ok(Expression::RValue(value))
    }
}
