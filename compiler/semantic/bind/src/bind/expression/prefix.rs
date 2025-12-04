use pernixc_handler::Handler;
use pernixc_ir::value::{
    Value,
    register::{Assignment, Prefix, PrefixOperator},
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::expression::prefix::Operator as PrefixOperatorSyntax;
use pernixc_term::r#type::{Primitive, Qualifier, Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{BindingError, Error, type_check::Expected},
    infer::constraint,
};

impl Bind<&pernixc_syntax::expression::prefix::Prefix>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::prefix::Prefix,
        _: &Guidance<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let (Some(operator), Some(prefixable)) =
            (syntax_tree.operator(), syntax_tree.prefixable())
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        match operator {
            PrefixOperatorSyntax::LogicalNot(_)
            | PrefixOperatorSyntax::Negate(_)
            | PrefixOperatorSyntax::BitwiseNot(_) => {
                let (expected_type, operator) = match operator {
                    PrefixOperatorSyntax::LogicalNot(_) => (
                        Some(Expected::Known(Type::Primitive(Primitive::Bool))),
                        PrefixOperator::LogicalNot,
                    ),
                    PrefixOperatorSyntax::Negate(_) => (
                        Some(Expected::Constraint(constraint::Type::Signed)),
                        PrefixOperator::Negate,
                    ),
                    PrefixOperatorSyntax::BitwiseNot(_) => (
                        Some(Expected::Constraint(constraint::Type::Integer)),
                        PrefixOperator::BitwiseNot,
                    ),
                    _ => unreachable!(),
                };

                let expected_type = expected_type.map(|x| match x {
                    Expected::Known(known) => known,
                    Expected::Constraint(c) => {
                        Type::Inference(self.create_type_inference(c))
                    }
                });

                let operand = (self.bind_value_or_error(
                    &prefixable,
                    expected_type.as_ref(),
                    handler,
                ))
                .await?;

                let register_id = self.create_register_assignment(
                    Assignment::Prefix(Prefix { operand, operator }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            PrefixOperatorSyntax::Dereference(_) => {
                (self.bind_dereference(
                    &prefixable,
                    syntax_tree.span(),
                    handler,
                ))
                .await
            }

            PrefixOperatorSyntax::ReferenceOf(reference_of) => {
                let qualifier = if reference_of.mut_keyword().is_some() {
                    Qualifier::Mutable
                } else {
                    Qualifier::Immutable
                };

                let lvalue =
                    (self.bind_as_lvalue(&prefixable, true, None, handler))
                        .await?;

                Ok(Expression::RValue(Value::Register(self.borrow_lvalue(
                    lvalue,
                    qualifier,
                    syntax_tree.span(),
                    handler,
                ))))
            }
        }
    }
}
