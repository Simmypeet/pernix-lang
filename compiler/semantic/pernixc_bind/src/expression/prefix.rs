use pernixc_handler::Handler;
use pernixc_semantic::{
    component::derived::ir::{
        model::Constraint,
        value::{
            register::{Assignment, Borrow, Prefix, PrefixOperator},
            Value,
        },
    },
    diagnostic::Diagnostic,
    term::{
        lifetime::Lifetime,
        r#type::{Primitive, Qualifier, Type},
    },
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression, Target};
use crate::{
    diagnostic::MismatchedQualifierForReferenceOf,
    infer::{Expected, NewTypeErased},
    Binder, BindingError, Error,
};

impl Bind<&syntax_tree::expression::prefix::Prefix> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::prefix::Prefix,
        config: Config,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Expression, Error> {
        match &syntax_tree.operator {
            syntax_tree::expression::prefix::PrefixOperator::LogicalNot(_)
            | syntax_tree::expression::prefix::PrefixOperator::Negate(_)
            | syntax_tree::expression::prefix::PrefixOperator::BitwiseNot(_) => {
                let (expected_type, operator) = match &syntax_tree.operator {
                    syntax_tree::expression::prefix::PrefixOperator::LogicalNot(_) => (
                        Some(Expected::Known(Type::Primitive(Primitive::Bool))),
                        PrefixOperator::LogicalNot,
                    ),
                    syntax_tree::expression::prefix::PrefixOperator::Negate(_) => (
                        Some(Expected::Constraint(Constraint::Signed)),
                        PrefixOperator::Negate,
                    ),
                    syntax_tree::expression::prefix::PrefixOperator::BitwiseNot(_) => (
                        Some(Expected::Constraint(Constraint::Integer)),
                        PrefixOperator::BitwiseNot,
                    ),
                    _ => unreachable!(),
                };

                let operand = self
                    .bind(
                        &*syntax_tree.prefixable,
                        Config { target: Target::RValue },
                        handler,
                    )?
                    .into_r_value()
                    .unwrap();

                // if required, type check the operand
                if let Some(expected_type) = expected_type {
                    if !self.type_check(
                        &self.type_of_value(&operand, handler)?,
                        expected_type,
                        syntax_tree.span(),
                        handler,
                    )? {
                        return Err(Error::Binding(BindingError(
                            syntax_tree.span(),
                        )));
                    }
                }
                let register_id = self.create_register_assignmnet(
                    Assignment::Prefix(Prefix { operand, operator }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            syntax_tree::expression::prefix::PrefixOperator::Dereference(_) => {
                self.bind_dereference(
                    &*syntax_tree.prefixable,
                    config,
                    syntax_tree.span(),
                    handler,
                )
            }

            syntax_tree::expression::prefix::PrefixOperator::ReferenceOf(
                reference_of,
            ) => {
                let qualifier = if reference_of.mutable_keyword.is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
                };

                let lvalue = self.bind_as_lvalue(
                    &*syntax_tree.prefixable,
                    true,
                    handler,
                )?;

                if lvalue.qualifier < qualifier {
                    handler.receive(Box::new(
                        MismatchedQualifierForReferenceOf {
                            reference_of_span: syntax_tree.span(),
                            found_qualifier: lvalue.qualifier,
                            expected_qualifier: qualifier,
                            is_behind_reference: lvalue
                                .address
                                .is_behind_reference(),
                        },
                    ));
                }

                let register_id = self.create_register_assignmnet(
                    Assignment::Borrow(Borrow {
                        address: lvalue.address,
                        qualifier,
                        lifetime: Lifetime::Inference(NewTypeErased),
                    }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }
        }
    }
}

#[cfg(test)]
mod test;
