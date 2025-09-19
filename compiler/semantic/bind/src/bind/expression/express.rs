use pernixc_handler::Handler;
use pernixc_ir::{
    instruction::{Jump, Terminator, UnconditionalJump},
    value::{
        literal::{self, Literal},
        Value,
    },
};
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{type_check::Expected, Binder, BindingError, Error},
    diagnostic::{
        Diagnostic, ExpressExpectedAValue, ExpressOutsideScope,
        ScopeWithGivenLableNameNotFound,
    },
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::terminator::Express> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::terminator::Express,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let label_ident = syntax_tree.label().and_then(|x| x.identifier());
        let scope_id = self.search_block_scope_id(
            label_ident.as_ref().map(|x| x.kind.0.as_str()),
        );

        let Some(scope_id) = scope_id else {
            let diag = match syntax_tree.label().and_then(|x| x.identifier()) {
                Some(ident) => Diagnostic::ScopeWithGivenLableNameNotFound(
                    ScopeWithGivenLableNameNotFound { span: ident.span },
                ),
                None => Diagnostic::ExpressOutsideScope(ExpressOutsideScope {
                    span: syntax_tree.span(),
                }),
            };

            handler.receive(diag);

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let block_state = self.get_block_state_from_scope_id(scope_id).unwrap();

        let ty = block_state.express_type().clone();
        let successor_block_id = block_state.successor_block_id();

        let value = if let Some(binary) = syntax_tree.binary() {
            self.bind_value_or_error(&binary, ty.as_ref(), handler).await?
        } else {
            // if there's an existing type bound to it already and this express
            // has no value, it must be unit type
            if let Some(ty) = ty {
                if self
                    .type_check_as_diagnostic(
                        &ty,
                        Expected::Known(Type::unit()),
                        syntax_tree.span(),
                        handler,
                    )
                    .await?
                    .is_some()
                {
                    handler.receive(Diagnostic::ExpressExpectedAValue(
                        ExpressExpectedAValue { span: syntax_tree.span() },
                    ));

                    Value::error(ty.clone(), Some(syntax_tree.span()))
                } else {
                    Value::unit(Some(syntax_tree.span()))
                }
            } else {
                Value::unit(Some(syntax_tree.span()))
            }
        };

        // bind the value to the block state
        self.express_value(scope_id, value, handler).await?;

        // pop all the needed scopes
        self.pop_all_scope_to(scope_id);

        // jump to the block
        self.insert_terminator(Terminator::Jump(Jump::Unconditional(
            UnconditionalJump { target: successor_block_id },
        )));

        let value =
            Value::Literal(Literal::Unreachable(literal::Unreachable {
                r#type: Type::Inference(
                    self.create_type_inference(constraint::Type::All(true)),
                ),
                span: Some(syntax_tree.span()),
            }));

        Ok(Expression::RValue(value))
    }
}
