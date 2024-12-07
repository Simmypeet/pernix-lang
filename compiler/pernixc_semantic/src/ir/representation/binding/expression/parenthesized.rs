use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{Bind, Config, Expression};
use crate::{
    error::{
        self, MoreThanOneUnpackedInTupleExpression, OverflowOperation,
        TypeSystemOverflow,
    },
    ir::{
        self,
        representation::{
            binding::{infer, Binder, Error, SemanticError},
            borrow,
        },
        value::{
            literal::{self, Literal},
            register::{self, Assignment},
            Value,
        },
    },
    symbol::table::{self, resolution},
    type_system::{
        self, simplify,
        term::{
            self,
            r#type::{self, Type},
        },
    },
};

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>
            + type_system::observer::Observer<borrow::Model, S>,
    > Bind<&syntax_tree::expression::Parenthesized> for Binder<'t, S, RO, TO>
{
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Parenthesized,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        let bind_as_tuple =
            syntax_tree.connected_list().as_ref().map_or(true, |x| {
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

            let tuple_type = simplify::simplify(
                &Type::<infer::Model>::Tuple(r#type::Tuple {
                    elements: elements
                        .iter()
                        .map(|x| {
                            self.type_of_value(&x.value).map(|ty| {
                                term::TupleElement {
                                    term: ty,
                                    is_unpacked: x.is_unpacked,
                                }
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                }),
                &self.create_environment(),
            )
            .map_err(|overflow_error| TypeSystemOverflow {
                operation: OverflowOperation::TypeOf,
                overflow_span: syntax_tree.span(),
                overflow_error,
            })?
            .result
            .into_tuple()
            .unwrap();

            // more than one unpacked elements
            if tuple_type.elements.iter().filter(|x| x.is_unpacked).count() > 1
            {
                self.create_handler_wrapper(handler).receive(Box::new(
                    MoreThanOneUnpackedInTupleExpression {
                        span: syntax_tree.span(),
                        r#type: self
                            .inference_context
                            .transform_type_into_constraint_model(Type::Tuple(
                                tuple_type,
                            ))
                            .map_err(|overflow_error| TypeSystemOverflow {
                                operation: OverflowOperation::TypeOf,
                                overflow_span: syntax_tree.span(),
                                overflow_error,
                            })?,
                    },
                ));

                Err(Error::Semantic(SemanticError(syntax_tree.span())))
            } else {
                let value = if elements.is_empty() {
                    // return unit Tuple
                    Value::Literal(Literal::Unit(literal::Unit {
                        span: (syntax_tree.span()),
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
