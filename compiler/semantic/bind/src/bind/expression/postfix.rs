use enum_as_inner::EnumAsInner;
use pernixc_handler::Handler;
use pernixc_ir::address::{self, Address};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Qualifier, Type};

use crate::{
    bind::{Bind, Expression, Guidance, LValue},
    binder::{BindingError, Error, UnrecoverableError},
    diagnostic::Diagnostic,
};

pub mod access;
pub mod diagnostic;
pub mod method_call;

async fn reduce_address_reference(
    binder: &mut crate::binder::Binder<'_>,
    mut lvalue: LValue,
    address_span: RelativeSpan,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<LValue, UnrecoverableError> {
    loop {
        let ty = binder.type_of_address(&lvalue.address, handler).await?;

        let Type::Reference(inner) = ty else {
            return Ok(lvalue);
        };

        let new_qualifier =
            inner.qualifier.min(if lvalue.address.is_behind_reference() {
                lvalue.qualifier
            } else {
                Qualifier::Mutable
            });

        lvalue = LValue {
            address: Address::Reference(address::Reference {
                qualifier: inner.qualifier,
                reference_address: Box::new(lvalue.address),
            }),
            span: address_span,
            qualifier: new_qualifier,
        };
    }
}

#[derive(Debug, EnumAsInner)]
#[allow(clippy::large_enum_variant)]
enum BindState {
    Initial(pernixc_syntax::expression::unit::Unit),
    Bound(Expression),
}

impl Bind<&pernixc_syntax::expression::postfix::Postfix>
    for crate::binder::Binder<'_>
{
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::postfix::Postfix,
        config: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(unit) = syntax_tree.unit() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let postfix_operators = syntax_tree.operators().collect::<Vec<_>>();

        // directly bind the unit if there are no postfix operators
        if postfix_operators.is_empty() {
            return self.bind(&unit, config, handler).await;
        }

        let mut current_span = unit.span();
        let mut bind_state = BindState::Initial(unit);

        // otherwise, bind as an rvalue first
        for operator in postfix_operators {
            match operator {
                pernixc_syntax::expression::postfix::Operator::MethodCall(
                    method_call,
                ) => {
                    let (next_expr, next_span) = method_call::bind_method_call(
                        self,
                        bind_state,
                        current_span,
                        &method_call,
                        handler,
                    )
                    .await?;

                    bind_state = BindState::Bound(next_expr);
                    current_span = next_span;
                }
                pernixc_syntax::expression::postfix::Operator::Cast(_cast) => {}
                pernixc_syntax::expression::postfix::Operator::Access(
                    access,
                ) => {
                    let (next_expr, next_span) = access::bind_access(
                        self,
                        bind_state,
                        current_span,
                        &access,
                        handler,
                    )
                    .await?;

                    bind_state = BindState::Bound(next_expr);
                    current_span = next_span;
                }
            }
        }

        Ok(bind_state.into_bound().unwrap())
    }
}
