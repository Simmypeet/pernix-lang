use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::Type;

use crate::{
    bind::{Bind, Config, Expression},
    binder::{type_check::Expected, Binder, Error},
    inference_context::constraint,
};

impl Bind<&pernixc_syntax::expression::unit::Array> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Array,
        _: &Config<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let mut values = Vec::new();

        Box::pin(async {
            for element in syntax_tree.expressions() {
                let expression =
                    self.bind_value_or_error(&element, handler).await?;

                values.push(expression);
            }

            Ok::<(), Error>(())
        })
        .await?;

        let array_ty = self.create_type_inference(constraint::Type::All(false));

        for value in &values {
            let value_ty = self.type_of_value(value, handler).await?;
            let span = self.span_of_value(value);

            self.type_check(
                &value_ty,
                Expected::Known(Type::Inference(array_ty)),
                span,
                handler,
            )
            .await?;
        }

        let register_id = self.create_register_assignment(
            pernixc_ir::value::register::Assignment::Array(
                pernixc_ir::value::register::Array {
                    elements: values,
                    element_type: Type::Inference(array_ty),
                },
            ),
            syntax_tree.span(),
        );

        Ok(Expression::RValue(pernixc_ir::value::Value::Register(register_id)))
    }
}
