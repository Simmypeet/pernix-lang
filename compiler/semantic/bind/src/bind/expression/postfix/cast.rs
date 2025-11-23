use pernixc_ir::value::{
    Value,
    register::{Assignment, Cast},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_source_file::SourceElement;
use pernixc_term::r#type::{Primitive, Type};

use crate::{
    bind::{
        Expression,
        expression::{
            diagnostic::{UnsafeOperation, UnsafeRequired},
            postfix::{
                BindState,
                cast::diagnostic::{
                    InvalidCastType, InvalidPointerTypeCasting, PointerKind,
                },
                diagnostic::Diagnostic,
            },
        },
    },
    binder::{BindingError, Error, type_check::Expected},
    infer::{self, constraint},
};

pub mod diagnostic;

#[allow(clippy::too_many_lines)]
pub(super) async fn bind_cast(
    binder: &mut crate::binder::Binder<'_>,
    current_state: BindState,
    current_span: RelativeSpan,
    cast: &pernixc_syntax::expression::postfix::Cast,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Expression, RelativeSpan), Error> {
    let Some(ty) = cast.r#type() else {
        return Err(Error::Binding(BindingError(
            current_span.join(&cast.span()),
        )));
    };

    let cast_type = binder.resolve_type_with_inference(&ty, handler).await?;
    let cast_type =
        binder.simplify_type(cast_type, current_span, handler).await?;

    let value = match current_state {
        BindState::Initial(unit) => {
            binder.bind_value_or_error(&unit, None, handler).await?
        }
        BindState::Bound(expr) => match expr {
            Expression::RValue(value) => value,
            Expression::LValue(lvalue) => {
                Value::Register(binder.load_lvalue(lvalue))
            }
        },
    };

    let type_of_value = binder.type_of_value(&value, handler).await?;

    // cast between numeric types
    if matches!(
        &cast_type.result,
        Type::Primitive(
            Primitive::Float32
                | Primitive::Float64
                | Primitive::Int8
                | Primitive::Int16
                | Primitive::Int32
                | Primitive::Int64
                | Primitive::Uint8
                | Primitive::Uint16
                | Primitive::Uint32
                | Primitive::Uint64
                | Primitive::Usize
                | Primitive::Isize
        )
    ) {
        binder
            .type_check(
                &type_of_value,
                Expected::Constraint(constraint::Type::Number),
                current_span,
                handler,
            )
            .await?;
    }
    // type cast to pointer
    else if matches!(&cast_type.result, Type::Pointer(_)) {
        // only allowed type to be casted to pointer:
        // - any reference type of any qualifier/type
        // - usize type
        // - pointer type

        if !type_can_pointer_cast(binder, &type_of_value, current_span, handler)
            .await
        {
            handler.receive(
                Diagnostic::InvalidPointerTypeCasting(
                    InvalidPointerTypeCasting {
                        span: current_span,
                        r#type: type_of_value,
                        casted_pointer_kind: PointerKind::RawPointer,
                        type_inference_map: binder
                            .type_inference_rendering_map(),
                        constant_inference_map: binder
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );

            return Err(Error::Binding(BindingError(
                current_span.join(&cast.span()),
            )));
        }
    } else if matches!(&cast_type.result, Type::Reference(_)) {
        if !type_can_pointer_cast(binder, &type_of_value, current_span, handler)
            .await
        {
            handler.receive(
                Diagnostic::InvalidPointerTypeCasting(
                    InvalidPointerTypeCasting {
                        span: current_span,
                        r#type: type_of_value,
                        casted_pointer_kind: PointerKind::RawPointer,
                        type_inference_map: binder
                            .type_inference_rendering_map(),
                        constant_inference_map: binder
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );

            return Err(Error::Binding(BindingError(
                current_span.join(&cast.span()),
            )));
        }

        // require unsafe scope
        if !binder.stack().is_unsafe() {
            handler.receive(
                UnsafeRequired {
                    expression_span: current_span,
                    operation: UnsafeOperation::ReferenceTypeCast,
                }
                .into(),
            );
        }
    } else {
        handler.receive(
            Diagnostic::InvalidCastType(InvalidCastType {
                span: ty.span(),
                r#type: cast_type.result.clone(),
                type_inference_map: binder.type_inference_rendering_map(),
                constant_inference_map: binder
                    .constant_inference_rendering_map(),
            })
            .into(),
        );

        return Err(Error::Binding(BindingError(
            current_span.join(&cast.span()),
        )));
    }

    Ok((
        Expression::RValue(Value::Register(binder.create_register_assignment(
            Assignment::Cast(Cast { value, r#type: cast_type.result.clone() }),
            current_span.join(&cast.span()),
        ))),
        current_span.join(&cast.span()),
    ))
}

async fn type_can_pointer_cast(
    binder: &mut crate::binder::Binder<'_>,
    ty: &Type,
    span: RelativeSpan,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> bool {
    match ty {
        Type::Reference(_)
        | Type::Pointer(_)
        | Type::Primitive(Primitive::Usize) => true,

        Type::Inference(inference) => {
            let inference = binder
                .inference_context()
                .type_table()
                .get_inference(*inference)
                .unwrap();

            match inference {
                infer::table::Inference::Known(known) => {
                    Box::pin(type_can_pointer_cast(
                        binder,
                        &known.clone(),
                        span,
                        handler,
                    ))
                    .await
                }
                infer::table::Inference::Inferring(id) => {
                    let constraint = binder
                        .inference_context()
                        .type_table()
                        .get_constraint(*id)
                        .unwrap();

                    match constraint {
                        constraint::Type::All(_)
                        | constraint::Type::Number
                        | constraint::Type::Integer
                        | constraint::Type::UnsignedInteger => {
                            binder
                                .type_check_as_diagnostic(
                                    ty,
                                    Expected::Known(Type::Primitive(
                                        Primitive::Usize,
                                    )),
                                    span,
                                    handler,
                                )
                                .await
                                .expect("should be successful");

                            true
                        }

                        constraint::Type::SignedInteger
                        | constraint::Type::Signed
                        | constraint::Type::Floating => false,
                    }
                }
            }
        }

        _ => false,
    }
}
