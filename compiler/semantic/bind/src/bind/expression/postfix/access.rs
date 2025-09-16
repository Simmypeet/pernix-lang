use pernixc_ir::{
    address::{self, Address, Memory},
    instruction::{Instruction, Store},
    value::{
        register::{Assignment, Load},
        Value,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::fields::get_fields;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::is_accessible_from_globally,
    kind::{get_kind, Kind},
};
use pernixc_syntax::expression::postfix::AccessKind;
use pernixc_term::{
    generic_arguments::Symbol,
    r#type::{Qualifier, Type},
};

use crate::{
    bind::{
        expression::postfix::{
            access::diagnostic::{
                CannotIndexPastUnpackedTuple, FieldNotFound,
                TooLargeTupleIndex, TupleIndexOutOfBounds,
                UnexpectedTypeForAccess,
            },
            diagnostic::Diagnostic,
            BindState,
        },
        Config, Expression, LValue, Target,
    },
    binder::{BindingError, Error, UnrecoverableError},
    pattern::bind::diagnostic::FieldIsNotAccessible,
};

pub mod diagnostic;

pub(super) async fn reduce_address_reference(
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

#[allow(clippy::too_many_lines)]
pub(super) async fn bind_access(
    binder: &mut crate::binder::Binder<'_>,
    current_state: BindState,
    current_span: RelativeSpan,
    access: &pernixc_syntax::expression::postfix::Access,
    config: &Config<'_>,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Expression, RelativeSpan), Error> {
    let new_span = current_span.join(&access.span());

    let Some(kind) = access.kind() else {
        return Err(Error::Binding(BindingError(new_span)));
    };

    let lvalue = match current_state {
        BindState::Initial(unit) => {
            let lvalue =
                binder.bind_as_lvalue(&unit, true, None, handler).await?;

            lvalue
        }

        BindState::Bound(Expression::LValue(lvalue)) => lvalue,

        BindState::Bound(Expression::RValue(value)) => {
            let value_ty = binder.type_of_value(&value, handler).await?;
            let value_span = binder.span_of_value(&value);
            let alloca = binder.create_alloca(value_ty, value_span);

            binder.push_instruction(Instruction::Store(Store {
                address: Address::Memory(Memory::Alloca(alloca)),
                value,
                span: Some(current_span),
            }));

            LValue {
                address: Address::Memory(Memory::Alloca(alloca)),
                span: value_span,
                qualifier: Qualifier::Mutable,
            }
        }
    };

    let reduced_lvalue =
        reduce_address_reference(binder, lvalue, current_span, handler).await?;
    let qualifier = reduced_lvalue.qualifier;

    let address_ty =
        binder.type_of_address(&reduced_lvalue.address, handler).await?;

    assert!(
        !address_ty.is_reference(),
        "should've reduced to non-reference type"
    );

    let address = match kind {
        AccessKind::StructField(ident) => {
            access_struct(
                binder,
                reduced_lvalue,
                ident,
                current_span,
                new_span,
                address_ty,
                handler,
            )
            .await?
        }

        AccessKind::TupleIndex(tuple_index) => access_tuple(
            binder,
            reduced_lvalue,
            &tuple_index,
            current_span,
            new_span,
            address_ty,
            handler,
        )?,

        AccessKind::ArrayIndex(_array_index) => {
            todo!()
        }
    };

    let joined_span = new_span;

    match config.target {
        Target::RValue(_) => {
            // will be optimized to move later
            let register_id = binder.create_register_assignment(
                Assignment::Load(Load { address }),
                new_span,
            );

            Ok((Expression::RValue(Value::Register(register_id)), joined_span))
        }

        // qualifier should've been checked earlier
        Target::Statement | Target::LValue { .. } => Ok((
            Expression::LValue(LValue {
                address,
                span: joined_span,
                qualifier,
            }),
            joined_span,
        )),
    }
}

async fn access_struct(
    binder: &mut crate::binder::Binder<'_>,
    reduced_lvalue: LValue,
    ident: pernixc_syntax::Identifier,
    current_span: RelativeSpan,
    new_span: RelativeSpan,
    address_ty: Type,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Address, Error> {
    let struct_id = match &address_ty {
        Type::Symbol(Symbol { id: struct_id, .. })
            if {
                let symbol_kind = binder.engine().get_kind(*struct_id).await;

                symbol_kind == Kind::Struct
            } =>
        {
            *struct_id
        }

        _ => {
            handler.receive(
                Diagnostic::UnexpectedTypeForAccess(UnexpectedTypeForAccess {
                    expected_type: diagnostic::ExpectedType::Struct,
                    span: current_span,
                    found_type: address_ty,
                    type_inference_map: binder.type_inference_rendering_map(),
                    constant_inference_map: binder
                        .constant_inference_rendering_map(),
                })
                .into(),
            );

            return Err(Error::Binding(BindingError(new_span)));
        }
    };

    let fields = binder.engine().get_fields(struct_id).await?;

    let Some(field_id) =
        fields.field_ids_by_name.get(ident.kind.as_str()).copied()
    else {
        handler.receive(
            Diagnostic::FieldNotFound(FieldNotFound {
                identifier_span: ident.span(),
                struct_id,
                field_name: ident.kind.0,
            })
            .into(),
        );

        return Err(Error::Binding(BindingError(new_span)));
    };

    let field_accessibility = fields.fields[field_id].accessibility;

    // field is not accessible, soft error, keep going
    if !binder
        .engine()
        .is_accessible_from_globally(
            binder.current_site(),
            field_accessibility.into_global(struct_id.target_id),
        )
        .await
    {
        handler.receive(
            Diagnostic::FieldIsNotAccessible(FieldIsNotAccessible {
                field_id,
                struct_id,
                referring_site: binder.current_site(),
                referring_identifier_span: ident.span(),
            })
            .into(),
        );
    }

    Ok(Address::Field(address::Field {
        struct_address: Box::new(reduced_lvalue.address),
        id: field_id,
    }))
}

fn access_tuple(
    binder: &mut crate::binder::Binder<'_>,
    reduced_lvalue: LValue,
    tuple_index: &pernixc_syntax::expression::postfix::TupleIndex,
    current_span: RelativeSpan,
    new_span: RelativeSpan,
    address_ty: Type,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Address, Error> {
    let tuple_ty = match address_ty {
        Type::Tuple(tuple_ty) => tuple_ty,
        found_type => {
            handler.receive(
                Diagnostic::UnexpectedTypeForAccess(UnexpectedTypeForAccess {
                    expected_type: diagnostic::ExpectedType::Tuple,
                    span: current_span,
                    found_type,
                    type_inference_map: binder.type_inference_rendering_map(),
                    constant_inference_map: binder
                        .constant_inference_rendering_map(),
                })
                .into(),
            );

            return Err(Error::Binding(BindingError(new_span)));
        }
    };

    // sanity check
    if tuple_ty.elements.iter().filter(|x| x.is_unpacked).count() > 1 {
        return Err(Error::Binding(BindingError(new_span)));
    }

    // used to chec if the index is past the unpacked index
    let unpacked_index = tuple_ty.elements.iter().position(|x| x.is_unpacked);

    let Some(index_str) = tuple_index.index() else {
        return Err(Error::Binding(BindingError(new_span)));
    };

    let index = match index_str.kind.0.parse::<usize>() {
        Ok(number) => number,
        Err(err) => match err.kind() {
            std::num::IntErrorKind::NegOverflow
            | std::num::IntErrorKind::PosOverflow => {
                handler.receive(
                    Diagnostic::TooLargeTupleIndex(TooLargeTupleIndex {
                        access_span: index_str.span,
                    })
                    .into(),
                );

                return Err(Error::Binding(BindingError(current_span)));
            }

            _ => {
                unreachable!()
            }
        },
    };

    if index >= tuple_ty.elements.len() {
        handler.receive(
            Diagnostic::TupleIndexOutOfBounds(TupleIndexOutOfBounds {
                access_span: index_str.span,
                element_count: tuple_ty.elements.len(),
                accessed_index: index,
            })
            .into(),
        );

        return Err(Error::Binding(BindingError(current_span)));
    }

    if let Some(unpacked_index) = unpacked_index {
        // can't access past the unpacked index
        let pass_unpacked = if tuple_index.minus().is_some() {
            index.cmp(&(tuple_ty.elements.len() - unpacked_index - 1))
        } else {
            index.cmp(&unpacked_index)
        };

        // report error
        if pass_unpacked.is_ge() {
            handler.receive(
                Diagnostic::CannotIndexPastUnpackedTuple(
                    CannotIndexPastUnpackedTuple {
                        index_span: tuple_index.span(),
                        tuple_type: tuple_ty,
                        unpacked_position: unpacked_index,
                        offset: match (
                            pass_unpacked.is_eq(),
                            tuple_index.minus(),
                        ) {
                            (true, _) => address::Offset::Unpacked,
                            (_, Some(_)) => address::Offset::FromEnd(index),
                            (_, None) => address::Offset::FromStart(index),
                        },
                        type_inference_map: binder
                            .type_inference_rendering_map(),
                        constant_inference_map: binder
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );

            return Err(Error::Binding(BindingError(current_span)));
        }
    }

    Ok(Address::Tuple(address::Tuple {
        tuple_address: Box::new(reduced_lvalue.address),
        offset: if tuple_index.minus().is_some() {
            address::Offset::FromEnd(index)
        } else {
            address::Offset::FromStart(index)
        },
    }))
}
