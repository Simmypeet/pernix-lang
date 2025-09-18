use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, Error},
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::unit::Array> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Array,
        _: &Guidance<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let mut values = Vec::new();

        let array_ty = Type::Inference(
            self.create_type_inference(constraint::Type::All(false)),
        );

        Box::pin(async {
            for element in syntax_tree.expressions() {
                let expression = self
                    .bind_value_or_error(&element, Some(&array_ty), handler)
                    .await?;

                values.push(expression);
            }

            Ok::<(), Error>(())
        })
        .await?;

        let register_id = self.create_register_assignment(
            pernixc_ir::value::register::Assignment::Array(
                pernixc_ir::value::register::Array {
                    elements: values,
                    element_type: array_ty,
                },
            ),
            syntax_tree.span(),
        );

        Ok(Expression::RValue(pernixc_ir::value::Value::Register(register_id)))
    }
}
