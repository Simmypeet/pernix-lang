use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree;

use super::{Bind, Config, Expression, Target};
use crate::{
    error::{self, MismatchedQualifierForReferenceOf},
    ir::{
        self,
        representation::{
            binding::{infer, Binder, Error, SemanticError},
            borrow,
        },
        value::{
            register::{Assignment, Prefix, PrefixOperator, Borrow},
            Value,
        },
        Erased,
    },
    symbol::table::{self, resolution},
    type_system::{
        self,
        term::{
            lifetime::Lifetime,
            r#type::{self, Expected, Qualifier, Type},
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
    > Bind<&syntax_tree::expression::Prefix> for Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind(
        &mut self,
        syntax_tree: &syntax_tree::expression::Prefix,
        config: Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Expression, Error> {
        match syntax_tree.operator() {
            syntax_tree::expression::PrefixOperator::LogicalNot(_)
            | syntax_tree::expression::PrefixOperator::Negate(_)
            | syntax_tree::expression::PrefixOperator::BitwiseNot(_) => {
                let (expected_type, operator) = match syntax_tree.operator() {
                    syntax_tree::expression::PrefixOperator::LogicalNot(_) => (
                        Some(Expected::Known(Type::Primitive(
                            r#type::Primitive::Bool,
                        ))),
                        PrefixOperator::LogicalNot,
                    ),
                    syntax_tree::expression::PrefixOperator::Negate(_) => (
                        Some(Expected::Constraint(r#type::Constraint::Signed)),
                        PrefixOperator::Negate,
                    ),
                    syntax_tree::expression::PrefixOperator::BitwiseNot(_) => (
                        Some(Expected::Constraint(r#type::Constraint::Integer)),
                        PrefixOperator::BitwiseNot,
                    ),
                    _ => unreachable!(),
                };

                let operand = self
                    .bind(
                        &**syntax_tree.prefixable(),
                        Config { target: Target::RValue },
                        handler,
                    )?
                    .into_r_value()
                    .unwrap();

                // if required, type check the operand
                if let Some(expected_type) = expected_type {
                    if !self.type_check(
                        &self.type_of_value(&operand)?,
                        expected_type,
                        syntax_tree.span(),
                        true,
                        handler,
                    )? {
                        return Err(Error::Semantic(SemanticError(
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

            syntax_tree::expression::PrefixOperator::Dereference(_) => self
                .bind_dereference(
                    &**syntax_tree.prefixable(),
                    config,
                    syntax_tree.span(),
                    handler,
                ),

            syntax_tree::expression::PrefixOperator::ReferenceOf(
                reference_of,
            ) => {
                let qualifier = if reference_of.mutable_keyword().is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
                };

                let lvalue = self.bind_as_lvalue(
                    &**syntax_tree.prefixable(),
                    true,
                    handler,
                )?;

                if lvalue.qualifier < qualifier {
                    self.create_handler_wrapper(handler).receive(Box::new(
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
                        lifetime: Lifetime::Inference(Erased),
                    }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }
        }
    }
}
