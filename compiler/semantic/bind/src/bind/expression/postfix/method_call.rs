use pernixc_hash::HashSet;
use pernixc_ir::{
    address::{Address, Memory},
    value::Value,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    implemented::get_implemented,
    implements_arguments::get_implements_argument, parameter::get_parameters,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::try_get_members,
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::{
        get_generic_parameters, ConstantParameterID, GenericParameters,
        LifetimeParameterID, TypeParameterID,
    },
    instantiation::Instantiation,
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
};

use crate::{
    bind::{
        expression::{
            function_call::{MethodReceiver, MethodReceiverKind},
            postfix::{
                diagnostic::Diagnostic,
                method_call::diagnostic::MethodCallNotFound,
                reduce_address_reference, BindState,
            },
        },
        Bind, Expression, Guidance, LValue,
    },
    binder::{type_check::Expected, BindingError, Error, UnrecoverableError},
    inference_context::constraint,
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
    let _lvalue = match attemp_adt_method_call(
        binder,
        lvalue,
        &ty,
        access,
        &identifier,
        &call,
        handler,
    )
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

fn extend_inference_instantiation(
    binder: &mut crate::binder::Binder<'_>,
    inst: &mut Instantiation,
    generic_parameters: &GenericParameters,
    global_id: Global<pernixc_symbol::ID>,
) {
    inst.lifetimes.extend(
        generic_parameters.lifetime_parameters_as_order().map(|(id, _)| {
            (
                Lifetime::Parameter(LifetimeParameterID {
                    parent_id: global_id,
                    id,
                }),
                Lifetime::Erased,
            )
        }),
    );

    inst.types.extend(generic_parameters.type_parameters_as_order().map(
        |(id, _)| {
            (
                Type::Parameter(TypeParameterID { parent_id: global_id, id }),
                Type::Inference(
                    binder.create_type_inference(constraint::Type::All(false)),
                ),
            )
        },
    ));

    inst.constants.extend(
        generic_parameters.constant_parameters_as_order().map(|(id, _)| {
            (
                pernixc_term::constant::Constant::Parameter(
                    ConstantParameterID { parent_id: global_id, id },
                ),
                pernixc_term::constant::Constant::Inference(
                    binder.create_constant_inference(),
                ),
            )
        }),
    );
}

#[allow(clippy::too_many_lines)]
async fn attemp_adt_method_call(
    binder: &mut crate::binder::Binder<'_>,
    lvalue: LValue,
    ty: &Type,
    method_call: &pernixc_syntax::expression::postfix::MethodCall,
    identifier: &pernixc_syntax::Identifier,
    call: &pernixc_syntax::expression::Call,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Result<Value, LValue>, Error> {
    let ty = ty.reduce_reference();

    let Type::Symbol(Symbol { id, .. }) = ty else {
        return Ok(Err(lvalue));
    };

    let kind = binder.engine().get_kind(*id).await;

    if !kind.is_adt() {
        return Ok(Err(lvalue));
    }

    let implemented = binder.engine().get_implemented(*id).await?;

    let Some((method_id, receiver_kind)) = find_method_in_implemented(
        binder,
        &implemented,
        *id,
        &identifier.kind.0,
    )
    .await?
    else {
        return Ok(Err(lvalue));
    };

    let mut inst = if let Some(generic_arguments) =
        method_call.generic_identifier().and_then(|x| x.generic_arguments())
    {
        let mut generic_arguments = binder
            .resolve_generic_arguments_with_inference(
                &generic_arguments,
                handler,
            )
            .await?;
        generic_arguments = binder
            .verify_generic_arguments_for_with_inference(
                generic_arguments,
                method_id,
                method_call.generic_identifier().unwrap().span(),
                handler,
            )
            .await?;

        Instantiation::from_generic_arguments(
            generic_arguments,
            method_id,
            &*binder.engine().get_generic_parameters(method_id).await?,
        )
        .expect("generic arguments have been verified")
    } else {
        let mut inst = Instantiation::default();
        extend_inference_instantiation(
            binder,
            &mut inst,
            &*binder.engine().get_generic_parameters(method_id).await?,
            method_id,
        );

        inst
    };

    let parent_impl_id = method_id
        .target_id
        .make_global(binder.engine().get_parent(method_id).await.unwrap());
    let parent_impl_generic_parameters =
        binder.engine().get_generic_parameters(parent_impl_id).await?;

    extend_inference_instantiation(
        binder,
        &mut inst,
        &parent_impl_generic_parameters,
        parent_impl_id,
    );

    let parent_impl_generic_arguments =
        binder.engine().get_implements_argument(parent_impl_id).await?;

    // type check the receiver
    if let Some(parent_impl_generic_arguments) = parent_impl_generic_arguments {
        let expected_ty = Type::Symbol(Symbol {
            id: *id,
            generic_arguments: {
                let mut args = (*parent_impl_generic_arguments).clone();
                args.instantiate(&inst);
                args
            },
        });

        binder
            .type_check(ty, Expected::Known(expected_ty), lvalue.span, handler)
            .await?;
    }

    // reduce reference in the address of the lvalue
    let lvalue_span = lvalue.span;
    let lvalue =
        reduce_address_reference(binder, lvalue, lvalue_span, handler).await?;

    // collect the arguments
    let arguments = call.expressions().collect::<Vec<_>>();

    Ok(Ok(Value::Register(
        binder
            .bind_method_call(
                method_id,
                inst,
                MethodReceiver { kind: receiver_kind, lvalue },
                &arguments,
                call.span(),
                method_call.span(),
                handler,
            )
            .await?,
    )))
}

async fn find_method_in_implemented(
    binder: &mut crate::binder::Binder<'_>,
    implemented: &HashSet<Global<pernixc_symbol::ID>>,
    adt_id: Global<pernixc_symbol::ID>,
    method_name: &str,
) -> Result<
    Option<(Global<pernixc_symbol::ID>, MethodReceiverKind)>,
    UnrecoverableError,
> {
    for impl_id in implemented.iter().copied() {
        let Some(members) = binder.engine().try_get_members(impl_id).await
        else {
            continue;
        };

        let Some(member) = members.member_ids_by_name.get(method_name).copied()
        else {
            continue;
        };
        let member = impl_id.target_id.make_global(member);
        let member_kind = binder.engine().get_kind(member).await;

        // found the method with applicable name and kind
        if member_kind != Kind::ImplementationFunction {
            return Ok(None);
        }

        // check if the method has a receiver
        let parameters = binder.engine().get_parameters(member).await?;

        if parameters.parameters.is_empty() {
            return Ok(None);
        }

        // check if the first parameter is a receiver
        let first_parameter_ty = parameters.parameters
            [parameters.parameter_order.first().copied().unwrap()]
        .r#type
        .clone();

        let Some(generic_arguments) =
            binder.engine().get_implements_argument(impl_id).await?
        else {
            return Ok(None);
        };

        let expected_ty = Type::Symbol(Symbol {
            id: adt_id,
            generic_arguments: (*generic_arguments).clone(),
        });

        match first_parameter_ty {
            Type::Reference(reference) => {
                if *reference.pointee != expected_ty {
                    return Ok(None);
                }

                return Ok(Some((
                    member,
                    MethodReceiverKind::Reference(reference.qualifier),
                )));
            }

            other => {
                if other != expected_ty {
                    return Ok(None);
                }

                return Ok(Some((member, MethodReceiverKind::Value)));
            }
        }
    }

    Ok(None)
}
