use pernixc_handler::Handler;
use pernixc_ir::value::{
    register::{Assignment, FunctionCall, Load, Register, Variant},
    Value,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::{
    do_effect::get_do_effects, elided_lifetime::get_elided_lifetimes,
    implements_arguments::get_implements_argument, parameter::get_parameters,
    variant::get_variant_associated_type,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    linkage::{get_linkage, Linkage, C},
    parent::get_parent,
};
use pernixc_target::Global;
use pernixc_term::{
    effect,
    generic_parameters::get_generic_parameters,
    instantiation::{
        get_instantiation, get_instantiation_for_associated_symbol,
        Instantiation,
    },
    lifetime::{ElidedLifetimeID, Lifetime},
    r#type::Qualifier,
};
use pernixc_type_system::{deduction, environment::Environment};

use crate::{
    bind::{
        expression::function_call::diagnostic::{
            Diagnostic, ExtraneousArgumentsToAssociatedValue,
            MismatchedArgumentsCount, MismatchedImplementationArguments,
            SymbolIsNotCallable, VariantAssociatedValueExpected,
            VariantDoesntHaveAssociatedValue,
        },
        LValue,
    },
    binder::UnrecoverableError,
    diagnostic::UnhandledEffects,
    inference_context,
};

pub mod diagnostic;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{Binder, BindingError, Error},
};

// has to map the elided lifetimes to some erased lifetime
// everytime, since the all the generic parameters (including
// the elided lifetimes) must be specified
async fn map_elided_lifetimes_to_erased(
    binder: &mut Binder<'_>,
    id: Global<pernixc_symbol::ID>,
    inst: &mut Instantiation,
) -> Result<(), Error> {
    let elided_lts = binder.engine().get_elided_lifetimes(id).await?;

    inst.lifetimes.extend(elided_lts.ids().map(|x| {
        (
            Lifetime::Elided(ElidedLifetimeID { parent_id: id, id: x }),
            Lifetime::Erased,
        )
    }));

    Ok(())
}

#[allow(clippy::too_many_lines)]
async fn get_function_instantiation(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<(Global<pernixc_symbol::ID>, Instantiation), Error> {
    let Some(qualified_identifier) = syntax_tree.qualified_identifier() else {
        return Err(Error::Binding(BindingError(syntax_tree.span())));
    };

    let resolution = binder
        .resolve_qualified_identifier_with_inference(
            &qualified_identifier,
            handler,
        )
        .await?;

    // get the function id and instantation
    let (id, instantation) = match resolution {
        Resolution::Variant(variant) => (
            variant.variant_id,
            binder
                .engine()
                .get_instantiation(
                    variant.variant_id.target_id.make_global(
                        binder
                            .engine()
                            .get_parent(variant.variant_id)
                            .await
                            .unwrap(),
                    ),
                    variant.generic_arguments,
                )
                .await?
                .unwrap(),
        ),
        Resolution::Generic(generic)
            if {
                let kind = binder.engine().get_kind(generic.id).await;
                kind.has_function_signature()
            } =>
        {
            (
                generic.id,
                binder
                    .engine()
                    .get_instantiation(generic.id, generic.generic_arguments)
                    .await?
                    .unwrap(),
            )
        }

        Resolution::MemberGeneric(member_generic)
            if {
                let kind = binder.engine().get_kind(member_generic.id).await;
                kind.has_function_signature()
            } =>
        'result: {
            let kind = binder.engine().get_kind(member_generic.id).await;

            // no need to perform argument deduction for the implements
            if kind != Kind::ImplementationFunction {
                break 'result (
                    member_generic.id,
                    binder
                        .engine()
                        .get_instantiation_for_associated_symbol(
                            member_generic.id,
                            member_generic.parent_generic_arguments,
                            member_generic.member_generic_arguments,
                        )
                        .await?
                        .unwrap(),
                );
            }

            let parent_impl_id = member_generic.id.target_id.make_global(
                binder.engine().get_parent(member_generic.id).await.unwrap(),
            );

            let Some(impl_args) =
                binder.engine().get_implements_argument(parent_impl_id).await?
            else {
                return Err(Error::Binding(BindingError(syntax_tree.span())));
            };

            let env = binder.create_environment();

            let mut instantiation = match env
                .deduce(&impl_args, &member_generic.parent_generic_arguments)
                .await
            {
                Ok(deduction) => deduction.result.instantiation,

                Err(deduction::Error::MismatchedGenericArgumentCount(_)) => {
                    unreachable!()
                }

                Err(deduction::Error::UnificationFailure(_)) => {
                    handler.receive(
                        Diagnostic::MismatchedImplementationArguments(
                            MismatchedImplementationArguments {
                                implementation_id: parent_impl_id,
                                found_generic_arguments: member_generic
                                    .member_generic_arguments
                                    .clone(),
                                instantiation_span: syntax_tree.span(),
                                type_inference_map: binder
                                    .type_inference_rendering_map(),
                                constant_inference_map: binder
                                    .constant_inference_rendering_map(),
                            },
                        )
                        .into(),
                    );

                    return Err(Error::Binding(BindingError(
                        syntax_tree.span(),
                    )));
                }

                Err(deduction::Error::CyclicDependency(cyclic)) => {
                    return Err(Error::Unrecoverable(
                        UnrecoverableError::CyclicDependency(cyclic),
                    ))
                }

                Err(deduction::Error::Overflow(overflow)) => {
                    overflow.report_as_type_calculating_overflow(
                        syntax_tree.span(),
                        &handler,
                    );

                    return Err(Error::Unrecoverable(
                        UnrecoverableError::Reported,
                    ));
                }
            };

            // append the instantiation for the member generic arguments
            instantiation
                .append_from_generic_arguments(
                    member_generic.member_generic_arguments,
                    member_generic.id,
                    binder
                        .engine()
                        .get_generic_parameters(member_generic.id)
                        .await?
                        .as_ref(),
                )
                .expect("should have correct generic arguments count");

            (member_generic.id, instantiation)
        }

        resolution => {
            // the symbol can't be called as a function
            handler.receive(
                Diagnostic::SymbolIsNotCallable(SymbolIsNotCallable {
                    symbol_id: resolution.global_id(),
                    span: syntax_tree.span(),
                })
                .into(),
            );

            return Err(Error::Binding(BindingError(syntax_tree.span())));
        }
    };

    Ok((id, instantation))
}

async fn get_callable_expected_types(
    binder: &mut Binder<'_>,
    callable_id: Global<pernixc_symbol::ID>,
    instantiation: &Instantiation,
) -> Result<Vec<pernixc_term::r#type::Type>, Error> {
    // can either be a function or an enum variant
    let kind = binder.engine().get_kind(callable_id).await;

    if kind == Kind::Variant {
        let associated_type = binder
            .engine()
            .get_variant_associated_type(callable_id)
            .await?
            .as_deref()
            .cloned();

        let Some(mut associated_type) = associated_type else {
            return Ok(Vec::new());
        };

        instantiation.instantiate(&mut associated_type);

        Ok(vec![associated_type])
    } else {
        let parameters = binder.engine().get_parameters(callable_id).await?;

        let mut expected_types =
            Vec::with_capacity(parameters.parameters.len());

        for (_, parameter) in parameters.parameters_as_order() {
            let mut ty = parameter.r#type.clone();
            instantiation.instantiate(&mut ty);
            expected_types.push(ty);
        }

        Ok(expected_types)
    }
}

impl Bind<&pernixc_syntax::expression::unit::FunctionCall> for Binder<'_> {
    #[allow(clippy::too_many_lines)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
        _config: &Guidance<'_>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Expression, Error> {
        let (callable_id, instantiation) =
            get_function_instantiation(self, syntax_tree, handler).await?;

        let mut expected_types =
            get_callable_expected_types(self, callable_id, &instantiation)
                .await?;

        let kind = self.engine().get_kind(callable_id).await;
        let arguments = syntax_tree
            .call()
            .map(|x| x.expressions().collect::<Vec<_>>())
            .unwrap_or_default();

        match (kind, expected_types.len()) {
            (Kind::Variant, 0) => {
                handler.receive(
                    Diagnostic::VariantDoesntHaveAssociatedValue(
                        VariantDoesntHaveAssociatedValue {
                            variant_id: callable_id,
                            span: syntax_tree.span(),
                            supplied_count: arguments.len(),
                        },
                    )
                    .into(),
                );

                Err(Error::Binding(BindingError(syntax_tree.span())))
            }

            (Kind::Variant, 1) => {
                let inner_value = match arguments.len() {
                    0 => {
                        handler.receive(
                            Diagnostic::VariantAssociatedValueExpected(
                                VariantAssociatedValueExpected {
                                    variant_id: callable_id,
                                    span: syntax_tree.span(),
                                },
                            )
                            .into(),
                        );

                        Value::error(
                            expected_types.pop().unwrap(),
                            Some(syntax_tree.span()),
                        )
                    }

                    1 => {
                        Box::pin(self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        ))
                        .await?
                    }

                    _ => {
                        handler.receive(
                            Diagnostic::ExtraneousArgumentsToAssociatedValue(
                                ExtraneousArgumentsToAssociatedValue {
                                    variant_id: callable_id,
                                    span: syntax_tree.call().map_or_else(
                                        || syntax_tree.span(),
                                        |c| c.span(),
                                    ),
                                    supplied_count: arguments.len(),
                                },
                            )
                            .into(),
                        );

                        Box::pin(self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        ))
                        .await?
                    }
                };

                let enum_id = callable_id.target_id.make_global(
                    self.engine().get_parent(callable_id).await.unwrap(),
                );
                let generic_parameters =
                    self.engine().get_generic_parameters(enum_id).await?;

                let register_id = self.create_register_assignment(
                    Assignment::Variant(Variant {
                        variant_id: callable_id,
                        associated_value: Some(inner_value),
                        generic_arguments: instantiation
                            .convert_to_generic_arguments(
                                &generic_parameters,
                                enum_id,
                            )
                            .expect(
                                "should have correct generic arguments count",
                            ),
                    }),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            _ => self
                .bind_function_call_internal(
                    &expected_types,
                    &arguments,
                    syntax_tree
                        .call()
                        .map_or_else(|| syntax_tree.span(), |c| c.span()),
                    syntax_tree.span(),
                    callable_id,
                    instantiation,
                    None,
                    handler,
                )
                .await
                .map(|x| Expression::RValue(Value::Register(x))),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum MethodReceiverKind {
    Value,
    Reference(Qualifier),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct MethodReceiver {
    pub kind: MethodReceiverKind,
    pub lvalue: LValue,
}

impl Binder<'_> {
    #[allow(clippy::too_many_arguments)]
    pub(super) async fn bind_method_call(
        &mut self,
        method_id: Global<pernixc_symbol::ID>,
        instantiation: Instantiation,
        receiver: MethodReceiver,
        arguments: &[pernixc_syntax::expression::Expression],
        call_span: RelativeSpan,
        whole_span: RelativeSpan,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<pernixc_arena::ID<Register>, Error> {
        let expected_types =
            get_callable_expected_types(self, method_id, &instantiation)
                .await?;

        self.bind_function_call_internal(
            &expected_types,
            arguments,
            call_span,
            whole_span,
            method_id,
            instantiation,
            Some(receiver),
            handler,
        )
        .await
    }

    #[allow(clippy::too_many_arguments)]
    async fn bind_function_call_internal(
        &mut self,
        expected_types: &[pernixc_term::r#type::Type],
        arguments: &[pernixc_syntax::expression::Expression],
        call_span: RelativeSpan,
        whole_span: RelativeSpan,
        callable_id: pernixc_target::Global<pernixc_symbol::ID>,
        mut instantiation: Instantiation,
        method_receiver: Option<MethodReceiver>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<pernixc_arena::ID<Register>, Error> {
        // map the elided lifetimes to erased lifetime

        // it's important that all the generic parameters (including the
        // elided lifetimes) is mapped, since the function call
        // might be monomorphized and the elided lifetimes are
        // replaced with concrete lifetimes
        map_elided_lifetimes_to_erased(self, callable_id, &mut instantiation)
            .await?;

        // deduct the by one if it's method, receiver is not counted as an
        // argument
        let expected =
            expected_types.len() - usize::from(method_receiver.is_some());
        let supplied = arguments.len();

        let callable_kind = self.engine().get_kind(callable_id).await;
        let is_vargs = callable_kind == Kind::ExternFunction
            && matches!(
                self.engine().get_linkage(callable_id).await,
                Linkage::C(C { variadic: true })
            );

        let count_error =
            if is_vargs { supplied < expected } else { supplied != expected };

        if count_error {
            handler.receive(
                Diagnostic::MismatchedArgumentsCount(
                    MismatchedArgumentsCount {
                        function_id: callable_id,
                        expected,
                        supplied,
                        span: call_span,
                    },
                )
                .into(),
            );
        }

        // bind the argument and type-check
        let mut argument_values = Vec::with_capacity(
            arguments.len() + usize::from(method_receiver.is_some()),
        );

        for (i, arg) in arguments.iter().enumerate() {
            let expected_ty =
                expected_types.get(i + usize::from(method_receiver.is_some()));

            argument_values.push(
                Box::pin(self.bind_value_or_error(arg, expected_ty, handler))
                    .await?,
            );
        }

        // insert the method receiver at the fron (if any), it's intended to
        // bind the method receiver last without type-checking
        if let Some(method_receiver) = method_receiver {
            let value = match method_receiver.kind {
                // load the lvalue
                MethodReceiverKind::Value => self.create_register_assignment(
                    Assignment::Load(Load {
                        address: method_receiver.lvalue.address,
                    }),
                    method_receiver.lvalue.span,
                ),
                // borrow the lvalue
                MethodReceiverKind::Reference(qualifier) => {
                    let borrow_span = method_receiver.lvalue.span;
                    self.borrow_lvalue(
                        method_receiver.lvalue,
                        qualifier,
                        borrow_span,
                        handler,
                    )
                }
            };

            argument_values.insert(0, Value::Register(value));
        }

        // truncuate or fill the arguments to match the expected types
        // `usize::from(...)` is for the method receiver
        match argument_values.len().cmp(&expected_types.len()) {
            std::cmp::Ordering::Less => {
                for ty in expected_types.iter().skip(argument_values.len()) {
                    argument_values
                        .push(Value::error(ty.clone(), Some(call_span)));
                }
            }
            std::cmp::Ordering::Greater => {
                // don't truncate if it's extra variadic arguments
                if !is_vargs {
                    argument_values.truncate(expected_types.len());
                }
            }
            std::cmp::Ordering::Equal => {}
        }

        let capabilities = if self
            .engine()
            .get_kind(self.current_site())
            .await
            .has_do_effects()
        {
            Some(self.engine().get_do_effects(self.current_site()).await?)
        } else {
            None
        };

        let assignment = FunctionCall {
            callable_id,
            arguments: argument_values,
            instantiation,
        };

        self.effect_check(
            &assignment,
            whole_span,
            capabilities.as_deref().unwrap_or(&effect::Effect::default()),
            handler,
        )
        .await?;

        Ok(self.create_register_assignment(
            Assignment::FunctionCall(assignment),
            whole_span,
        ))
    }

    async fn effect_compatible(
        env: &Environment<'_, inference_context::InferenceContext>,
        capability: &effect::Unit,
        effect: &effect::Unit,
        span: RelativeSpan,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<bool, UnrecoverableError> {
        if capability.id != effect.id {
            return Ok(false);
        }

        let result = env
            .subtypes_generic_arguments(
                &capability.generic_arguments,
                &effect.generic_arguments,
            )
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(span, &handler)
            })?;

        Ok(result.is_some())
    }

    async fn effect_check(
        &mut self,
        function_call: &FunctionCall,
        span: RelativeSpan,
        capabilities: &effect::Effect,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<(), UnrecoverableError> {
        let mut unhandled_effects = Vec::new();

        let effects =
            self.engine().get_do_effects(function_call.callable_id).await?;

        let environment = self.create_environment();

        'next: for effect_unit in &effects.effects {
            for capability in &capabilities.effects {
                if Self::effect_compatible(
                    &environment,
                    capability,
                    effect_unit,
                    span,
                    handler,
                )
                .await?
                {
                    continue 'next;
                }
            }

            // cannot find a compatible capability
            unhandled_effects.push(effect_unit.clone());
        }

        if !unhandled_effects.is_empty() {
            handler.receive(crate::diagnostic::Diagnostic::UnhandledEffects(
                UnhandledEffects {
                    effects: unhandled_effects,
                    span,
                    type_inference_map: self.type_inference_rendering_map(),
                    constant_inference_map: self
                        .constant_inference_rendering_map(),
                },
            ));

            return Ok(());
        }

        Ok(())
    }
}
