use pernixc_handler::Handler;
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{self, ConnectedList};
use pernixc_table::diagnostic::Diagnostic;
use pernixc_term::r#type::Type;

use super::{Bind, Config, Expression};
use crate::{
    binding::{
        diagnostic::MoreThanOneUnpackedInTupleExpression, infer, Binder,
        BindingError, Error,
    },
    value::{
        literal::{self, Literal},
        register::{self, Assignment},
        Value,
    },
};

impl Bind<&syntax_tree::expression::Parenthesized> for Binder<'_> {
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Parenthesized,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        let bind_as_tuple =
            syntax_tree.connected_list().as_ref().is_none_or(|x| {
                !x.rest().is_empty()
                    || x.trailing_separator().is_some()
                    || x.first().ellipsis().is_some()
            });

        if bind_as_tuple {
            let mut elements = Vec::new();

            for element_syn in syntax_tree
                .connected_list()
                .as_ref()
                .into_iter()
                .flat_map(ConnectedList::elements)
            {
                let element = self.bind_value_or_error(
                    &**element_syn.expression(),
                    handler,
                )?;

                elements.push(register::TupleElement {
                    value: element,
                    is_unpacked: element_syn.ellipsis().is_some(),
                });
            }

            let tuple_type = self
                .create_environment()
                .simplify(Type::<infer::Model>::Tuple(pernixc_term::Tuple {
                    elements: elements
                        .iter()
                        .map(|x| {
                            self.type_of_value(&x.value, handler).map(|ty| {
                                pernixc_term::TupleElement {
                                    term: ty,
                                    is_unpacked: x.is_unpacked,
                                }
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                }))
                .map_err(|x| {
                    x.report_overflow(|x| {
                        x.report_as_type_calculating_overflow(
                            syntax_tree.span(),
                            handler,
                        )
                    })
                })?;

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
                handler.receive(Box::new(
                    MoreThanOneUnpackedInTupleExpression {
                        span: syntax_tree.span(),
                        r#type: self
                            .inference_context
                            .transform_type_into_constraint_model(
                                tuple_type.result.clone(),
                                syntax_tree.span(),
                                self.table,
                                handler,
                            )?,
                    },
                ));

                Err(Error::Binding(BindingError(syntax_tree.span())))
            } else {
                let value = if elements.is_empty() {
                    // return unit Tuple
                    Value::Literal(Literal::Unit(literal::Unit {
                        span: Some(syntax_tree.span()),
                    }))
                } else {
                    let create_register_assignmnet = self
                        .create_register_assignmnet(
                            Assignment::Tuple(register::Tuple { elements }),
                            syntax_tree.span(),
                        );
                    Value::Register(create_register_assignmnet)
                };

                Ok(Expression::RValue(value))
            }
        } else {
            // propagate the target
            self.bind(
                &**syntax_tree
                    .connected_list()
                    .as_ref()
                    .unwrap()
                    .first()
                    .expression(),
                config,
                handler,
            )
        }
    }
}

#[cfg(test)]
mod test;
