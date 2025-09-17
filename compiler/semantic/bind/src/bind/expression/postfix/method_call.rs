use pernixc_ir::{
    address::{Address, Memory},
    value::Value,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::get_kind;
use pernixc_term::{
    generic_arguments::Symbol,
    r#type::{Qualifier, Type},
};

use crate::{
    bind::{
        expression::postfix::{
            diagnostic::Diagnostic,
            method_call::diagnostic::MethodCallNotFound, BindState,
        },
        Bind, Expression, Guidance, LValue,
    },
    binder::{BindingError, Error},
};

pub mod diagnostic;

pub(super) async fn bind_method_call(
    binder: &mut crate::binder::Binder<'_>,
    current_state: BindState,
    current_span: RelativeSpan,
    access: &pernixc_syntax::expression::postfix::MethodCall,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Expression, RelativeSpan), Error> {
    let (Some(identifier), Some(call)) = (
        access.generic_identifier().and_then(|x| x.identifier()),
        access.call(),
    ) else {
        return Err(Error::Binding(BindingError(
            current_span.join(&access.span()),
        )));
    };

    let expression = match current_state {
        BindState::Initial(unit) => {
            binder.bind(&unit, &Guidance::builder().build(), handler).await?
        }
        BindState::Bound(expression) => expression,
    };

    let lvalue = match expression {
        Expression::RValue(value) => {
            let alloca_id = binder
                .create_alloca_with_value(
                    value,
                    binder.stack().current_scope().scope_id(),
                    Some(current_span),
                    current_span,
                    handler,
                )
                .await?;

            LValue {
                address: Address::Memory(Memory::Alloca(alloca_id)),
                span: current_span,
                qualifier: Qualifier::Mutable,
            }
        }
        Expression::LValue(lvalue) => lvalue,
    };

    let ty = binder.type_of_address(&lvalue.address, handler).await?;
    let lvalue =
        match attemp_adt_method_call(binder, lvalue, &ty, access, handler)
            .await?
        {
            Ok(value) => {
                return Ok((
                    Expression::RValue(value),
                    current_span.join(&access.span()),
                ))
            }
            Err(lvalue) => lvalue,
        };

    handler.receive(
        Diagnostic::MethodCallNotFound(MethodCallNotFound {
            method_name: identifier.kind.0,
            receiver_type: ty.clone(),
            method_span: identifier.span,
            receiver_span: current_span,
            type_inference_map: binder.type_inference_rendering_map(),
            constant_inference_map: binder.constant_inference_rendering_map(),
        })
        .into(),
    );

    Err(Error::Binding(BindingError(current_span.join(&access.span()))))
}

async fn attemp_adt_method_call(
    binder: &mut crate::binder::Binder<'_>,
    lvalue: LValue,
    ty: &Type,
    access: &pernixc_syntax::expression::postfix::MethodCall,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Result<Value, LValue>, Error> {
    let ty = ty.reduce_reference();

    let Type::Symbol(Symbol { id, generic_arguments }) = ty else {
        return Ok(Err(lvalue));
    };

    let kind = binder.engine().get_kind(*id).await;

    todo!()
}
