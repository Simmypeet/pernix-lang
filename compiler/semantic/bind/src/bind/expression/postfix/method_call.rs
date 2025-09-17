use pernixc_hash::HashSet;
use pernixc_ir::{
    address::{self, Address, Memory},
    value::Value,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::diagnostic::{
    Diagnostic as ResolutionDiagnostic, SymbolIsNotAccessible,
};
use pernixc_semantic_element::{
    implemented::get_implemented, implements::get_implements,
    implements_arguments::get_implements_argument, import::get_import_map,
    parameter::get_parameters,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::symbol_accessible,
    kind::{get_kind, Kind},
    member::{get_members, try_get_members},
    parent::{get_closest_module_id, get_parent, scope_walker},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{GenericArguments, Symbol},
    generic_parameters::{
        get_generic_parameters, ConstantParameterID, GenericParameters,
        LifetimeParameterID, TypeParameterID,
    },
    instantiation::Instantiation,
    lifetime::Lifetime,
    predicate::Predicate,
    r#type::{Qualifier, Type},
};

use crate::{
    bind::{
        expression::{
            function_call::{MethodReceiver, MethodReceiverKind},
            postfix::{
                diagnostic::Diagnostic,
                method_call::diagnostic::{
                    AmbiguousMethodCall, MethodCallNotFound,
                },
                reduce_address_reference, BindState,
            },
        },
        Bind, Expression, Guidance, LValue,
    },
    binder::{
        type_check::Expected, Binder, BindingError, Error, UnrecoverableError,
    },
    inference_context::constraint,
};

pub mod diagnostic;

pub(super) async fn bind_method_call(
    binder: &mut crate::binder::Binder<'_>,
    current_state: BindState,
    current_span: RelativeSpan,
    method_call: &pernixc_syntax::expression::postfix::MethodCall,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Expression, RelativeSpan), Error> {
    let (Some(identifier), Some(call)) = (
        method_call.generic_identifier().and_then(|x| x.identifier()),
        method_call.call(),
    ) else {
        return Err(Error::Binding(BindingError(
            current_span.join(&method_call.span()),
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
    let lvalue = match attempt_adt_method_call(
        binder,
        lvalue,
        &ty,
        method_call,
        &identifier,
        &call,
        handler,
    )
    .await?
    {
        Ok(value) => {
            return Ok((
                Expression::RValue(value),
                current_span.join(&method_call.span()),
            ))
        }
        Err(lvalue) => lvalue,
    };

    if let Some(value) = attempt_trait_method_call(
        binder,
        lvalue,
        method_call,
        &identifier,
        &call,
        handler,
    )
    .await?
    {
        return Ok((
            Expression::RValue(value),
            current_span.join(&method_call.span()),
        ));
    }

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

    Err(Error::Binding(BindingError(current_span.join(&method_call.span()))))
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

struct TraitMethodCandidate {
    method_id: Global<pernixc_symbol::ID>,
    receiver_kind: MethodReceiverKind,
}

async fn visible_traits(
    binder: &Binder<'_>,
) -> Result<HashSet<Global<pernixc_symbol::ID>>, UnrecoverableError> {
    let mut visible_traits = HashSet::default();
    let nearest_module_id = Global::new(
        binder.current_site().target_id,
        binder.engine().get_closest_module_id(binder.current_site()).await,
    );

    // visible traits refer to:
    // 1. All traits defined in the current module
    // 2. All traits imported into the current module
    // 3. Trait appearing in the current `implements` block
    // 4. Traits appear in the where clause of the current environment.

    // 1 & 2
    {
        let members = binder.engine().get_members(nearest_module_id).await;
        let imports = binder.engine().get_import_map(nearest_module_id).await;

        for member_id in members
            .member_ids_by_name
            .values()
            .copied()
            .chain(members.unnameds.iter().copied())
            .map(|x| nearest_module_id.target_id.make_global(x))
            .chain(imports.values().map(|x| x.id))
        {
            let kind = binder.engine().get_kind(member_id).await;

            if kind == Kind::Trait {
                visible_traits.insert(member_id);
            }
        }
    }

    // 3
    {
        let mut scope_walker =
            binder.engine().scope_walker(binder.current_site());

        while let Some(current_id) = scope_walker.next().await {
            let current_id =
                Global::new(binder.current_site().target_id, current_id);

            let kind = binder.engine().get_kind(current_id).await;

            if kind != Kind::PositiveImplementation {
                continue;
            }

            let Some(implemented) =
                binder.engine().get_implements(current_id).await?
            else {
                continue;
            };

            if binder.engine().get_kind(implemented).await == Kind::Trait {
                visible_traits.insert(implemented);
                break;
            }
        }
    }

    // 4
    visible_traits.extend(
        binder
            .premise()
            .predicates
            .iter()
            .filter_map(|x| x.as_positive_trait().map(|x| x.trait_id)),
    );

    // all of the visible traits should have been accessible-checked before
    Ok(visible_traits)
}

async fn trait_method_candidates(
    binder: &mut crate::binder::Binder<'_>,
    method_ident: &str,
    visibile_traits: &HashSet<Global<pernixc_symbol::ID>>,
) -> Result<Vec<TraitMethodCandidate>, UnrecoverableError> {
    let mut candidates = Vec::new();

    for trait_id in visibile_traits.iter().copied() {
        // check if the trait has atleast one type generic parameter
        let generic_parameters =
            binder.engine().get_generic_parameters(trait_id).await?;

        // skip traits without type parameters
        let Some(first_type_parameter) =
            generic_parameters.type_order().first().copied()
        else {
            continue;
        };

        // try to find the method in the trait with matching name
        let members = binder.engine().get_members(trait_id).await;
        let Some(method_id) = members
            .member_ids_by_name
            .get(method_ident)
            .copied()
            .map(|x| trait_id.target_id.make_global(x))
        else {
            continue;
        };

        // must be a method
        if binder.engine().get_kind(method_id).await != Kind::TraitFunction {
            continue;
        }

        let parameters = binder.engine().get_parameters(method_id).await?;

        // the first parameter must be either `T`, `&T` or `&mut T` where `T` is
        // the first type parameter of the trait
        let Some(first_parameter_ty) = parameters
            .parameter_order
            .first()
            .copied()
            .map(|i| &parameters.parameters[i].r#type)
        else {
            continue;
        };

        let expected_ty = Type::Parameter(TypeParameterID {
            parent_id: trait_id,
            id: first_type_parameter,
        });

        match first_parameter_ty {
            Type::Reference(reference) => {
                if *reference.pointee != expected_ty {
                    continue;
                }

                candidates.push(TraitMethodCandidate {
                    method_id,
                    receiver_kind: MethodReceiverKind::Reference(
                        reference.qualifier,
                    ),
                });
            }
            rest => {
                if *rest != expected_ty {
                    continue;
                }

                candidates.push(TraitMethodCandidate {
                    method_id,
                    receiver_kind: MethodReceiverKind::Value,
                });
            }
        }
    }

    Ok(candidates)
}

async fn attempt_match_trait_method_candidate(
    binder: &mut crate::binder::Binder<'_>,
    lvalue: &LValue,
    lvalue_ty: &Type,
    lvalue_receiver_kind: MethodReceiverKind,
    current_candidate: &TraitMethodCandidate,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Option<Type>, UnrecoverableError> {
    let parent_trait_id = current_candidate.method_id.target_id.make_global(
        binder.engine().get_parent(current_candidate.method_id).await.unwrap(),
    );

    let trait_generic_parameters =
        binder.engine().get_generic_parameters(parent_trait_id).await?;

    // assume `T` is the type of `LValue` and `U` is the type in the method
    // receiver.
    let ty = match (lvalue_receiver_kind, current_candidate.receiver_kind) {
        // `T` match `U`
        (MethodReceiverKind::Value, MethodReceiverKind::Value) => {
            lvalue_ty.clone()
        }

        // `T` cannot match `&(mut)? U`, `T` needs to be autoref in the next
        // round
        (MethodReceiverKind::Value, MethodReceiverKind::Reference(_)) => {
            return Ok(None)
        }

        // `&(mut)? T` match `U` as `U = &T`
        (
            MethodReceiverKind::Reference(qualifier),
            MethodReceiverKind::Value,
        ) => Type::Reference(pernixc_term::r#type::Reference {
            qualifier,
            pointee: Box::new(lvalue_ty.clone()),
            lifetime: Lifetime::Erased,
        }),

        // `&(mut)? T` match `&(mut)? U` as `T = U` if the qualifier matches
        (
            MethodReceiverKind::Reference(l_qualifier),
            MethodReceiverKind::Reference(method_qualifier),
        ) => {
            if l_qualifier == method_qualifier {
                lvalue_ty.clone()
            } else {
                return Ok(None);
            }
        }
    };

    // check if the trait is satisfied for the current candidates
    let trait_generic_arguments = GenericArguments {
        lifetimes: trait_generic_parameters
            .lifetimes()
            .iter()
            .map(|_| Lifetime::Erased)
            .collect(),
        types: std::iter::once(ty.clone())
            .chain(trait_generic_parameters.types().iter().skip(1).map(|_| {
                Type::Inference(
                    binder.create_type_inference(constraint::Type::All(false)),
                )
            }))
            .collect(),
        constants: trait_generic_parameters
            .constants()
            .iter()
            .map(|_| {
                pernixc_term::constant::Constant::Inference(
                    binder.create_constant_inference(),
                )
            })
            .collect(),
    };

    let environment = binder.create_environment();

    let trait_predicate = pernixc_term::predicate::PositiveTrait::new(
        parent_trait_id,
        false,
        trait_generic_arguments,
    );

    match environment.query(&trait_predicate).await {
        Ok(Some(_)) => Ok(Some(ty)),
        Ok(None) => Ok(None),
        Err(err) => match err {
            pernixc_type_system::Error::Overflow(overflow_error) => {
                overflow_error.report_as_undecidable_predicate(
                    Predicate::PositiveTrait(trait_predicate),
                    None,
                    lvalue.span,
                    &handler,
                );

                Err(UnrecoverableError::Reported)
            }
            pernixc_type_system::Error::CyclicDependency(cyclic_error) => {
                Err(UnrecoverableError::CyclicDependency(cyclic_error))
            }
        },
    }
}

#[allow(clippy::too_many_lines)]
async fn attempt_trait_method_call(
    binder: &mut crate::binder::Binder<'_>,
    mut lvalue: LValue,
    method_call: &pernixc_syntax::expression::postfix::MethodCall,
    identifier: &pernixc_syntax::Identifier,
    call: &pernixc_syntax::expression::Call,
    handler: &dyn pernixc_handler::Handler<crate::diagnostic::Diagnostic>,
) -> Result<Option<Value>, Error> {
    let visible_traits = visible_traits(binder).await?;
    let candidates =
        trait_method_candidates(binder, &identifier.kind.0, &visible_traits)
            .await?;

    let candidate = loop {
        let address_ty =
            binder.type_of_address(&lvalue.address, handler).await?;

        let mut matched_candidates = Vec::new();

        for lvalue_receiver_kind in [
            MethodReceiverKind::Value,
            MethodReceiverKind::Reference(Qualifier::Immutable),
            MethodReceiverKind::Reference(Qualifier::Mutable),
        ] {
            for current_candidate in &candidates {
                if let Some(receiver_ty) = attempt_match_trait_method_candidate(
                    binder,
                    &lvalue,
                    &address_ty,
                    lvalue_receiver_kind,
                    current_candidate,
                    handler,
                )
                .await?
                {
                    matched_candidates.push((
                        current_candidate,
                        receiver_ty,
                        lvalue_receiver_kind,
                    ));
                }
            }
        }

        // exactly one candidate matched
        match candidates.len() {
            0 => {
                let Type::Reference(inner) = address_ty else {
                    // can no longer deref
                    break None;
                };

                let new_qualifier = inner.qualifier.min(
                    if lvalue.address.is_behind_reference() {
                        lvalue.qualifier
                    } else {
                        Qualifier::Mutable
                    },
                );

                lvalue = LValue {
                    address: Address::Reference(address::Reference {
                        qualifier: inner.qualifier,
                        reference_address: Box::new(lvalue.address),
                    }),
                    span: lvalue.span,
                    qualifier: new_qualifier,
                };
            }
            1 => break Some(matched_candidates.pop().unwrap()),
            2.. => {
                handler.receive(
                    Diagnostic::AmbiguousMethodCall(AmbiguousMethodCall {
                        method_name: identifier.kind.0.clone(),
                        receiver_type: address_ty.clone(),
                        method_span: identifier.span,
                        receiver_span: lvalue.span,
                        candidates: matched_candidates
                            .iter()
                            .map(|x| x.0.method_id)
                            .collect(),
                        type_inference_map: binder
                            .type_inference_rendering_map(),
                        constant_inference_map: binder
                            .constant_inference_rendering_map(),
                    })
                    .into(),
                );
            }
        }
    };

    let Some((candidate, receiver_ty, lvalue_receiver_kind)) = candidate else {
        // no matched candidates
        return Ok(None);
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
                candidate.method_id,
                method_call.generic_identifier().unwrap().span(),
                handler,
            )
            .await?;

        Instantiation::from_generic_arguments(
            generic_arguments,
            candidate.method_id,
            &*binder
                .engine()
                .get_generic_parameters(candidate.method_id)
                .await?,
        )
        .expect("generic arguments have been verified")
    } else {
        let mut inst = Instantiation::default();
        extend_inference_instantiation(
            binder,
            &mut inst,
            &*binder
                .engine()
                .get_generic_parameters(candidate.method_id)
                .await?,
            candidate.method_id,
        );

        inst
    };

    let parent_trait_id = candidate.method_id.target_id.make_global(
        binder.engine().get_parent(candidate.method_id).await.unwrap(),
    );
    let parent_trait_generic_parameters =
        binder.engine().get_generic_parameters(parent_trait_id).await?;

    inst.lifetimes.extend(
        parent_trait_generic_parameters.lifetime_parameters_as_order().map(
            |(id, _)| {
                (
                    Lifetime::Parameter(LifetimeParameterID {
                        parent_id: parent_trait_id,
                        id,
                    }),
                    Lifetime::Erased,
                )
            },
        ),
    );

    inst.types.extend(
        parent_trait_generic_parameters
            .type_parameters_as_order()
            .enumerate()
            .map(|(index, (id, _))| {
                (
                    Type::Parameter(TypeParameterID {
                        parent_id: parent_trait_id,
                        id,
                    }),
                    if index == 0 {
                        receiver_ty.clone()
                    } else {
                        Type::Inference(binder.create_type_inference(
                            constraint::Type::All(false),
                        ))
                    },
                )
            }),
    );

    inst.constants.extend(
        parent_trait_generic_parameters.constant_parameters_as_order().map(
            |(id, _)| {
                (
                    pernixc_term::constant::Constant::Parameter(
                        ConstantParameterID { parent_id: parent_trait_id, id },
                    ),
                    pernixc_term::constant::Constant::Inference(
                        binder.create_constant_inference(),
                    ),
                )
            },
        ),
    );

    // collect the arguments
    let arguments = call.expressions().collect::<Vec<_>>();

    // check if the adt method call is accessible
    if !binder
        .engine()
        .symbol_accessible(binder.current_site(), candidate.method_id)
        .await
    {
        handler.receive(
            ResolutionDiagnostic::SymbolIsNotAccessible(
                SymbolIsNotAccessible {
                    referring_site: binder.current_site(),
                    referred: candidate.method_id,
                    referred_span: identifier.span,
                },
            )
            .into(),
        );

        // soft error, continue
    }

    Ok(Some(Value::Register(
        binder
            .bind_method_call(
                candidate.method_id,
                inst,
                MethodReceiver { kind: lvalue_receiver_kind, lvalue },
                &arguments,
                call.span(),
                method_call.span(),
                handler,
            )
            .await?,
    )))
}

#[allow(clippy::too_many_lines)]
async fn attempt_adt_method_call(
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

    // check if the adt method call is accessible
    if !binder
        .engine()
        .symbol_accessible(binder.current_site(), method_id)
        .await
    {
        handler.receive(
            ResolutionDiagnostic::SymbolIsNotAccessible(
                SymbolIsNotAccessible {
                    referring_site: binder.current_site(),
                    referred: method_id,
                    referred_span: identifier.span,
                },
            )
            .into(),
        );

        // soft error, continue
    }

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
