use pernixc_handler::Handler;
use pernixc_ir::value::{
    literal::{self, Literal},
    register::{self, Assignment},
    Value,
};
use pernixc_source_file::SourceElement;
use pernixc_term::{r#type::Type, tuple};

use crate::{
    bind::{
        expression::parenthesized::diagnostic::{
            Diagnostic, MoreThanOneUnpackedInTupleExpression,
        },
        Bind, Config, Expression,
    },
    binder::{BindingError, Error},
};

pub mod diagnostic;

impl Bind<&pernixc_syntax::expression::unit::Parenthesized>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::Parenthesized,
        config: Config,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<crate::bind::Expression, Error> {
        let element = syntax_tree.unpackables().collect::<Vec<_>>();
        let has_comma = syntax_tree.inner_tree().nodes.iter().any(|x| {
            x.as_leaf()
                .and_then(|x| x.kind.as_punctuation())
                .is_some_and(|x| x.0 == ',')
        });
        let has_unpackable = element.iter().any(|x| x.ellipsis().is_some());

        if element.len() == 1 && !has_comma && !has_unpackable {
            let Some(expression) = element[0].expression() else {
                return Err(Error::Binding(BindingError(syntax_tree.span())));
            };

            return Box::pin(self.bind(&expression, config, handler)).await;
        }

        let mut elements = Vec::new();

        for element_syn in &element {
            let value = 'expr: {
                let Some(expression) = element_syn.expression() else {
                    break 'expr Value::Literal(
                        self.create_error(syntax_tree.span()),
                    );
                };

                Box::pin(self.bind_value_or_error(&expression, handler)).await?
            };

            elements.push(register::TupleElement {
                value,
                is_unpacked: element_syn.ellipsis().is_some(),
            });
        }

        let mut tuple_type = tuple::Tuple { elements: Vec::new() };

        for element in &elements {
            let ty = self.type_of_value(&element.value, handler).await?;
            tuple_type.elements.push(tuple::Element {
                term: ty,
                is_unpacked: element.is_unpacked,
            });
        }

        let tuple_type = self
            .simplify_type(Type::Tuple(tuple_type), syntax_tree.span(), handler)
            .await?;

        // more than one unpacked elements
        if tuple_type
            .result
            .as_tuple()
            .unwrap()
            .elements
            .iter()
            .filter(|x| x.is_unpacked)
            .count()
            > 1
        {
            handler.receive(
                Diagnostic::MoreThanOneUnpackedInTupleExpression(
                    MoreThanOneUnpackedInTupleExpression {
                        span: syntax_tree.span(),
                        r#type: tuple_type.result.clone(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                        type_inference_map: self.type_inference_rendering_map(),
                    },
                )
                .into(),
            );

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }

        let value = if elements.is_empty() {
            // return unit Tuple
            Value::Literal(Literal::Unit(literal::Unit {
                span: Some(syntax_tree.span()),
            }))
        } else {
            let create_register_assignmnet = self.create_register_assignment(
                Assignment::Tuple(register::Tuple { elements }),
                syntax_tree.span(),
            );
            Value::Register(create_register_assignmnet)
        };

        Ok(Expression::RValue(value))
    }
}
