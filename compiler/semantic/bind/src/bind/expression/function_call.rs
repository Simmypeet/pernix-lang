use enum_as_inner::EnumAsInner;
use pernixc_arena::{ID, OrderedArena};
use pernixc_handler::Handler;
use pernixc_hash::FxHashMap;
use pernixc_ir::value::{
    Value,
    register::{
        Assignment, EffectHandlerArgument, FunctionCall, Register, Variant,
        function_call::Callee, load::Load,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_semantic_element::{
    effect_annotation::get_effect_annotation,
    elided_lifetime::get_elided_lifetimes,
    implements_arguments::get_implements_argument, parameter::get_parameters,
    variant::get_variant_associated_type,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    linkage::{C, Linkage, get_linkage},
    r#unsafe::is_function_unsafe,
};
use pernixc_target::Global;
use pernixc_term::{
    effect,
    generic_arguments::{AssociatedSymbol, GenericArguments},
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    lifetime::{ElidedLifetime, Lifetime},
    r#type::Qualifier,
};
use pernixc_type_system::environment::Environment;

use crate::{
    bind::{
        LValue,
        expression::function_call::diagnostic::{
            Diagnostic, ExtraneousArgumentsToAssociatedValue,
            MismatchedArgumentsCount, MismatchedImplementationArguments,
            SymbolIsNotCallable, UnsafeFunctionCallOutsideUnsafeScope,
            VariantAssociatedValueExpected, VariantDoesntHaveAssociatedValue,
        },
    },
    binder::{UnrecoverableError, inference_context},
    diagnostic::UnhandledEffects,
};

pub mod diagnostic;

use crate::{
    bind::{
        Bind, Expression, Guidance,
        expression::{
            postfix::{
                access::bind_struct_field_access,
                method_call::bind_method_call_from_parts,
            },
            qualified_identifier::bind_local_root,
        },
    },
    binder::{Binder, BindingError, Error},
};

// has to map the elided lifetimes to some erased lifetime
// everytime, since the all the generic parameters (including
// the elided lifetimes) must be specified
async fn map_elided_lifetimes_to_erased(
    binder: &mut Binder<'_>,
    id: Global<pernixc_symbol::SymbolID>,
) -> FxHashMap<ID<ElidedLifetime>, Lifetime> {
    let elided_lts = binder.engine().get_elided_lifetimes(id).await;

    elided_lts.ids().map(|x| (x, Lifetime::Erased)).collect()
}

#[derive(Debug, EnumAsInner)]
enum CalleeOrVariant {
    Callee(Callee),
    Variant(pernixc_resolution::qualified_identifier::Variant),
}

#[allow(clippy::too_many_lines)]
async fn get_function_instantiation(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<CalleeOrVariant, Error> {
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
    match resolution {
        Resolution::Variant(variant) => Ok(CalleeOrVariant::Variant(variant)),

        Resolution::GenericSymbol(generic)
            if {
                let kind = binder.engine().get_kind(generic.id()).await;
                kind.has_function_signature()
            } =>
        {
            Ok(CalleeOrVariant::Callee(Callee::Function(generic)))
        }

        Resolution::GenericAssociatedSymbol(member_generic)
            if {
                let kind = binder.engine().get_kind(member_generic.id()).await;
                kind.has_function_signature()
                    || kind == Kind::EffectOperation
                        && kind != Kind::TraitAssociatedFunction
            } =>
        {
            Ok(CalleeOrVariant::Callee(Callee::AssociatedFunction(
                member_generic,
            )))
        }

        Resolution::InstanceAssociatedSymbol(inst)
            if {
                let kind = binder
                    .engine()
                    .get_kind(inst.trait_associated_symbol_id())
                    .await;
                kind == Kind::TraitAssociatedFunction
            } =>
        {
            Ok(CalleeOrVariant::Callee(Callee::InstanceAssociatedFunction(
                inst,
            )))
        }

        Resolution::IntermediateAdtImplSymbol(sym) => {
            let parent_impl_id = sym.get_parent_impl_id(binder.engine()).await;
            let impl_generic_parameters =
                binder.engine().get_generic_parameters(parent_impl_id).await;

            let Some(impl_args) =
                binder.engine().get_implements_argument(parent_impl_id).await
            else {
                return Err(Error::Binding(BindingError(syntax_tree.span())));
            };

            let env = binder.create_environment();

            let instantiation =
                match env.deduce(&impl_args, sym.adt_generic_arguments()).await
                {
                    Ok(Some(deduction)) => deduction.result.instantiation,

                    Ok(None) => {
                        handler.receive(
                            Diagnostic::MismatchedImplementationArguments(
                                MismatchedImplementationArguments {
                                    implementation_id: parent_impl_id,
                                    found_generic_arguments: sym
                                        .adt_generic_arguments()
                                        .clone(),
                                    instantiation_span: syntax_tree.span(),
                                    rendering_map: binder.get_rendering_map(),
                                },
                            )
                            .into(),
                        );

                        return Err(Error::Binding(BindingError(
                            syntax_tree.span(),
                        )));
                    }

                    Err(overflow) => {
                        overflow.report_as_type_calculating_overflow(
                            syntax_tree.span(),
                            &handler,
                        );

                        return Err(Error::Unrecoverable(
                            UnrecoverableError::Reported,
                        ));
                    }
                };

            let impl_generic_arguments =
                GenericArguments::from_generic_parameters_and_instantiation(
                    &instantiation,
                    &impl_generic_parameters,
                    parent_impl_id,
                );

            Ok(CalleeOrVariant::Callee(Callee::AssociatedFunction(
                AssociatedSymbol::new(
                    sym.impl_associated_symbol_id(),
                    impl_generic_arguments,
                    sym.into_impl_associated_generic_arguments(),
                ),
            )))
        }

        resolution => {
            // the symbol can't be called as a function
            handler.receive(
                Diagnostic::SymbolIsNotCallable(SymbolIsNotCallable {
                    found: resolution,
                    span: syntax_tree.span(),
                })
                .into(),
            );

            Err(Error::Binding(BindingError(syntax_tree.span())))
        }
    }
}

async fn get_callable_expected_types(
    binder: &mut Binder<'_>,
    span: RelativeSpan,
    callee_or_variant: &CalleeOrVariant,
) -> Result<Vec<pernixc_term::r#type::Type>, Error> {
    match callee_or_variant {
        CalleeOrVariant::Callee(callee) => {
            let Some(inst) = callee.create_instantiation(binder.engine()).await
            else {
                return Err(Error::new_binding_error(span));
            };

            let callable_id = callee.get_symbol_id();
            let parameters = binder.engine().get_parameters(callable_id).await;

            let mut result = Vec::with_capacity(parameters.len());

            for (_, parameter) in parameters.parameters_as_order() {
                let mut ty = parameter.r#type.clone();

                inst.instantiate(&mut ty);

                result.push(ty);
            }

            Ok(result)
        }

        CalleeOrVariant::Variant(variant) => {
            let associated_type = binder
                .engine()
                .get_variant_associated_type(variant.variant_id())
                .await
                .as_deref()
                .cloned();

            let Some(mut ty) = associated_type else { return Ok(Vec::new()) };

            let instantiation =
                variant.create_instantiation(binder.engine()).await;

            instantiation.instantiate(&mut ty);

            Ok(vec![ty])
        }
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
        if let Some(expression) =
            bind_local_function_call(self, syntax_tree, handler).await?
        {
            return Ok(expression);
        }

        let callee_or_variant =
            get_function_instantiation(self, syntax_tree, handler).await?;

        let mut expected_types = get_callable_expected_types(
            self,
            syntax_tree.span(),
            &callee_or_variant,
        )
        .await?;

        let arguments = syntax_tree
            .call()
            .map(|x| x.expressions().collect::<Vec<_>>())
            .unwrap_or_default();

        match (callee_or_variant, expected_types.len()) {
            (CalleeOrVariant::Variant(var), 0) => {
                handler.receive(
                    Diagnostic::VariantDoesntHaveAssociatedValue(
                        VariantDoesntHaveAssociatedValue {
                            variant_id: var.variant_id(),
                            span: syntax_tree.span(),
                            supplied_count: arguments.len(),
                        },
                    )
                    .into(),
                );

                Err(Error::Binding(BindingError(syntax_tree.span())))
            }

            (CalleeOrVariant::Variant(var), 1) => {
                let inner_value = match arguments.len() {
                    0 => {
                        handler.receive(
                            Diagnostic::VariantAssociatedValueExpected(
                                VariantAssociatedValueExpected {
                                    variant_id: var.variant_id(),
                                    span: syntax_tree.span(),
                                },
                            )
                            .into(),
                        );

                        Value::error(
                            expected_types.pop().unwrap(),
                            syntax_tree.span(),
                        )
                    }

                    1 => {
                        self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        )
                        .await?
                    }

                    _ => {
                        handler.receive(
                            Diagnostic::ExtraneousArgumentsToAssociatedValue(
                                ExtraneousArgumentsToAssociatedValue {
                                    variant_id: var.variant_id(),
                                    span: syntax_tree.call().map_or_else(
                                        || syntax_tree.span(),
                                        |c| c.span(),
                                    ),
                                    supplied_count: arguments.len(),
                                },
                            )
                            .into(),
                        );

                        self.bind_value_or_error(
                            &arguments[0],
                            Some(&expected_types.pop().unwrap()),
                            handler,
                        )
                        .await?
                    }
                };

                let register_id = self.create_register_assignment(
                    Assignment::Variant(Variant::new(var, Some(inner_value))),
                    syntax_tree.span(),
                );

                Ok(Expression::RValue(Value::Register(register_id)))
            }

            (CalleeOrVariant::Variant(_), _) => {
                unreachable!(
                    "the expected types of a variant can only be 0 or 1, got \
                     {}",
                    expected_types.len()
                );
            }

            (CalleeOrVariant::Callee(callee), _) => self
                .bind_function_call_internal(
                    &expected_types,
                    &arguments,
                    syntax_tree
                        .call()
                        .map_or_else(|| syntax_tree.span(), |c| c.span()),
                    syntax_tree.span(),
                    callee,
                    None,
                    handler,
                )
                .await
                .map(|x| Expression::RValue(Value::Register(x))),
        }
    }
}

async fn bind_local_function_call(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::unit::FunctionCall,
    handler: &dyn Handler<crate::diagnostic::Diagnostic>,
) -> Result<Option<Expression>, Error> {
    let Some(qualified_identifier) = syntax_tree.qualified_identifier() else {
        return Ok(None);
    };

    let Some(pernixc_syntax::QualifiedIdentifierRoot::GenericIdentifier(root)) =
        qualified_identifier.root()
    else {
        return Ok(None);
    };

    if qualified_identifier.subsequences().count() == 0 {
        return Ok(None);
    }

    let Some(mut receiver) =
        bind_local_root(binder, &root, root.span(), handler).await?
    else {
        return Ok(None);
    };

    let subsequences = qualified_identifier.subsequences().collect::<Vec<_>>();
    let (last, prefixes) = subsequences
        .split_last()
        .expect("checked that the qualified identifier is non-empty");

    for subsequent in prefixes {
        let Some(generic_identifier) = subsequent.generic_identifier() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        if generic_identifier.generic_arguments().is_some() {
            return Ok(None);
        }

        let Some(identifier) = generic_identifier.identifier() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        receiver =
            bind_struct_field_access(binder, receiver, identifier, handler)
                .await?;
    }

    let Some(generic_identifier) = last.generic_identifier() else {
        return Err(Error::Binding(BindingError(syntax_tree.span())));
    };
    let Some(call) = syntax_tree.call() else {
        return Err(Error::Binding(BindingError(syntax_tree.span())));
    };

    bind_method_call_from_parts(
        binder,
        receiver,
        &generic_identifier,
        &call,
        syntax_tree.span(),
        handler,
    )
    .await
    .map(Some)
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
    pub(super) async fn bind_function_callee(
        &mut self,
        callee: Callee,
        receiver: MethodReceiver,
        arguments: &[pernixc_syntax::expression::Expression],
        call_span: RelativeSpan,
        whole_span: RelativeSpan,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<pernixc_arena::ID<Register>, Error> {
        let callee_or_variant = CalleeOrVariant::Callee(callee);

        let expected_types =
            get_callable_expected_types(self, whole_span, &callee_or_variant)
                .await?;

        let callee = callee_or_variant
            .into_callee()
            .expect("we've just created as callee");

        self.bind_function_call_internal(
            &expected_types,
            arguments,
            call_span,
            whole_span,
            callee,
            Some(receiver),
            handler,
        )
        .await
    }

    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    async fn bind_function_call_internal(
        &mut self,
        expected_types: &[pernixc_term::r#type::Type],
        arguments: &[pernixc_syntax::expression::Expression],
        call_span: RelativeSpan,
        whole_span: RelativeSpan,
        callee: Callee,
        method_receiver: Option<MethodReceiver>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<pernixc_arena::ID<Register>, Error> {
        // deduct the by one if it's method, receiver is not counted as an
        // argument
        let expected =
            expected_types.len() - usize::from(method_receiver.is_some());
        let supplied = arguments.len();

        let callable_kind =
            self.engine().get_kind(callee.get_symbol_id()).await;
        let is_vargs = callable_kind == Kind::ExternFunction
            && matches!(
                self.engine().get_linkage(callee.get_symbol_id()).await,
                Linkage::C(C { variadic: true })
            );

        let count_error =
            if is_vargs { supplied < expected } else { supplied != expected };

        if count_error {
            handler.receive(
                Diagnostic::MismatchedArgumentsCount(
                    MismatchedArgumentsCount {
                        function_id: callee.get_symbol_id(),
                        expected,
                        supplied,
                        span: call_span,
                    },
                )
                .into(),
            );
        }

        if matches!(
            callable_kind,
            Kind::Function
                | Kind::ImplementationAssociatedFunction
                | Kind::TraitAssociatedFunction
        ) {
            let is_unsafe =
                self.engine().is_function_unsafe(callee.get_symbol_id()).await;

            if is_unsafe && !self.stack().is_unsafe() {
                handler.receive(
                    Diagnostic::UnsafeFunctionCallOutsideUnsafeScope(
                        UnsafeFunctionCallOutsideUnsafeScope {
                            call_span: whole_span,
                            function_id: callee.get_symbol_id(),
                        },
                    )
                    .into(),
                );
            }
        }

        // bind the argument and type-check
        let mut argument_values = Vec::with_capacity(
            arguments.len() + usize::from(method_receiver.is_some()),
        );

        for (i, arg) in arguments.iter().enumerate() {
            let expected_ty =
                expected_types.get(i + usize::from(method_receiver.is_some()));

            argument_values.push(
                self.bind_value_or_error(arg, expected_ty, handler).await?,
            );
        }

        // insert the method receiver at the fron (if any), it's intended to
        // bind the method receiver last without type-checking
        if let Some(method_receiver) = method_receiver {
            let value = match method_receiver.kind {
                // load the lvalue
                MethodReceiverKind::Value => self.create_register_assignment(
                    Assignment::Load(Load::new(method_receiver.lvalue.address)),
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
                    argument_values.push(Value::error(ty.clone(), call_span));
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

        let capabilities =
            self.engine().get_effect_annotation(self.current_site()).await;

        let capability_arguments = self
            .effect_check(
                callee.get_symbol_id(),
                callee.create_instantiation(self.engine()).await,
                whole_span,
                &capabilities,
                handler,
            )
            .await?;

        // map the elided lifetimes to erased lifetime

        // it's important that all the generic parameters (including the
        // elided lifetimes) is mapped, since the function call
        // might be monomorphized and the elided lifetimes are
        // replaced with concrete lifetimes
        let elided_lifetimes_instantiation =
            map_elided_lifetimes_to_erased(self, callee.get_symbol_id()).await;

        // the "forall" lifetimes should be instantiated to some lifetimes (in
        // this case, just erase them). This is required so that the forall
        // lifetime won't appear in the return type of the function call.
        let forall_lifetimes_instantiation = callee
            .get_instance_associated_forall_lifetimes(
                &Lifetime::Erased,
                self.engine(),
            )
            .await
            .unwrap_or_default();

        let assignment = FunctionCall::new(
            callee,
            argument_values,
            elided_lifetimes_instantiation,
            forall_lifetimes_instantiation,
            capability_arguments,
        );

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
        if capability.effect_id() != effect.effect_id() {
            return Ok(false);
        }

        let result = env
            .subtypes_generic_arguments(
                effect.generic_arguments(),
                capability.generic_arguments(),
            )
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(span, &handler)
            })?;

        let Some(result) = result else {
            return Ok(false);
        };

        Ok(result.result.forall_lifetime_errors.is_empty())
    }

    async fn effect_check(
        &mut self,
        callee_id: Global<pernixc_symbol::SymbolID>,
        instantiation: Option<Instantiation>,
        span: RelativeSpan,
        available_capabilities: &OrderedArena<effect::Unit>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<
        FxHashMap<ID<effect::Unit>, EffectHandlerArgument>,
        UnrecoverableError,
    > {
        let required_capabilities =
            self.engine().get_effect_annotation(callee_id).await;

        let Some(instantiation) = instantiation else {
            // we couldn't obtain the instantiation because the function
            // signature is malformed, the best we can do is to return
            // effect arguments as unhandled
            return Ok(required_capabilities
                .ids()
                .map(|x| (x, EffectHandlerArgument::Unhandled))
                .collect());
        };

        self.check_effect_units(
            available_capabilities,
            &required_capabilities,
            &instantiation,
            span,
            callee_id,
            handler,
        )
        .await
    }

    async fn check_effect_units(
        &mut self,
        available_capabilities: &OrderedArena<effect::Unit>,
        required_capabilities: &OrderedArena<effect::Unit>,
        instantiation: &Instantiation,
        span: RelativeSpan,
        callable_id: Global<pernixc_symbol::SymbolID>,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<
        FxHashMap<ID<effect::Unit>, EffectHandlerArgument>,
        UnrecoverableError,
    > {
        let mut effect_arguments = FxHashMap::default();

        // First, we'll traverse the capability handlers stack first, then we'll

        'next: for (required_id, required) in required_capabilities.iter() {
            let mut required = required.clone();
            required.instantiate(instantiation);

            // traverse in the handler stack
            if let Some(effect_handler_id) = self
                .search_handler_clause(
                    required.effect_id(),
                    required.generic_arguments(),
                )
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(span, &handler)
                })?
            {
                effect_arguments.insert(
                    required_id,
                    EffectHandlerArgument::FromEffectHandler(effect_handler_id),
                );

                continue 'next;
            }

            let environment = self.create_environment();

            for (available_id, available) in available_capabilities.iter() {
                if Self::effect_compatible(
                    &environment,
                    available,
                    &required,
                    span,
                    handler,
                )
                .await?
                {
                    effect_arguments.insert(
                        required_id,
                        EffectHandlerArgument::FromEffectAnnotation(
                            available_id,
                        ),
                    );
                    continue 'next;
                }
            }

            // cannot find a compatible capability
            effect_arguments
                .insert(required_id, EffectHandlerArgument::Unhandled);
        }

        if effect_arguments
            .iter()
            .any(|x| x.1 == &EffectHandlerArgument::Unhandled)
        {
            handler.receive(crate::diagnostic::Diagnostic::UnhandledEffects(
                UnhandledEffects {
                    effects: effect_arguments
                        .iter()
                        .filter_map(|x| {
                            if x.1 == &EffectHandlerArgument::Unhandled {
                                let mut required =
                                    required_capabilities[*x.0].clone();

                                required.instantiate(instantiation);

                                Some(required)
                            } else {
                                None
                            }
                        })
                        .collect(),
                    callable_id,
                    span,
                    rendering_map: self.get_rendering_map(),
                },
            ));
        }

        Ok(effect_arguments)
    }
}
