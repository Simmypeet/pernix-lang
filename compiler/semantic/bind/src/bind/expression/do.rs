use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{Address, Memory},
    effect_handler::{EffectHandler, HandlerGroup},
    pattern::{Irrefutable, NameBindingPoint, Wildcard},
    value::Value,
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_resolution::{
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    Config,
};
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    member::get_members,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    instantiation::{get_instantiation, Instantiation},
    r#type::{Qualifier, Type},
};
use pernixc_type_system::UnrecoverableError;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{
        self, inference_context::ErasedLifetimeProvider, Binder, BindingError,
        Error,
    },
    diagnostic::{
        Diagnostic, DuplicatedEffectHandler, DuplicatedEffectOperationHandler,
        EffectExpected, MismatchedArgumentCountInEffectOperationHandler,
        UnhandledEffectOperations, UnknownEffectOperation,
    },
    infer::constraint,
    pattern::insert_name_binding,
};

impl Bind<&pernixc_syntax::expression::block::Do> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Do,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let Some(do_statements) = syntax_tree.statements() else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let captures = self.create_captures(handler).await?;

        let with_blocks =
            extract_effect_handlers(self, syntax_tree, handler).await?;

        let _handler_group_id = create_handler_group(self, &with_blocks);

        let expected_return_type = Type::Inference(
            self.create_type_inference(constraint::Type::All(true)),
        );

        let _do_closure = Box::pin(self.new_closure_binder(
            async |x| {
                build_do_block(x, &do_statements, handler).await?;
                Ok(())
            },
            expected_return_type.clone(),
            do_statements.span(),
            &captures,
            handler,
        ))
        .await?;

        build_with_blocks(
            self,
            with_blocks,
            &expected_return_type,
            &captures,
            handler,
        )
        .await?;

        Ok(Expression::RValue(Value::error(
            Type::Inference(
                self.create_type_inference(constraint::Type::All(true)),
            ),
            Some(syntax_tree.span()),
        )))
    }
}

struct WithBlock {
    qualified_identifier: QualifiedIdentifier,
    effect_id: Global<pernixc_symbol::ID>,
    generic_arguments: GenericArguments,
    handlers: HashMap<pernixc_symbol::ID, HandlerBlock>,
}

struct HandlerBlock {
    identifier: pernixc_syntax::Identifier,
    statements: pernixc_syntax::statement::Statements,
    parameters: Vec<pernixc_syntax::pattern::Irrefutable>,
    handler: pernixc_syntax::expression::block::Handler,
}

fn create_handler_group(
    binder: &mut Binder<'_>,
    with_blocks: &[WithBlock],
) -> pernixc_arena::ID<HandlerGroup> {
    let handler_group_id = binder.insert_effect_handler_group();

    for with_block in with_blocks {
        binder.insert_effect_handler_to_group(
            handler_group_id,
            EffectHandler::new(
                with_block.effect_id,
                with_block.generic_arguments.clone(),
            ),
        );
    }

    handler_group_id
}

async fn build_with_blocks(
    binder: &mut Binder<'_>,
    with_blocks: Vec<WithBlock>,
    expected_type: &Type,
    captures: &binder::closure::Captures,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), Error> {
    for with_block in with_blocks {
        let instantiation = binder
            .engine()
            .get_instantiation(
                with_block.effect_id,
                with_block.generic_arguments,
            )
            .await?
            .expect("instantiation must be available");

        for (effect_operation_id, handler_block) in with_block.handlers {
            let statements_span = handler_block.statements.span();

            binder
                .new_closure_binder(
                    async |x| {
                        Box::pin(build_handler_block(
                            x,
                            handler_block,
                            with_block
                                .effect_id
                                .target_id
                                .make_global(effect_operation_id),
                            &instantiation,
                            handler,
                        ))
                        .await?;

                        Ok(())
                    },
                    expected_type.clone(),
                    statements_span,
                    captures,
                    handler,
                )
                .await?;
        }
    }

    Ok(())
}

async fn build_do_block(
    binder: &mut Binder<'_>,
    statements: &pernixc_syntax::statement::Statements,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    for statement in statements.statements().filter_map(|x| x.into_line().ok())
    {
        binder.bind_statement(&statement, handler).await?;
    }

    Ok(())
}

async fn build_handler_block(
    binder: &mut Binder<'_>,
    handler_block: HandlerBlock,
    effect_operation_id: Global<pernixc_symbol::ID>,
    effect_inst: &Instantiation,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    // start binding all the arguments as parameters
    let mut name_binding_point = NameBindingPoint::default();

    let effect_operation_parameters =
        binder.engine().get_parameters(effect_operation_id).await?;

    for (parameter_id, parameter_syn) in effect_operation_parameters
        .parameter_order
        .iter()
        .copied()
        .zip(&handler_block.parameters)
    {
        let mut parameter_ty =
            effect_operation_parameters.parameters[parameter_id].r#type.clone();

        effect_inst.instantiate(&mut parameter_ty);

        let simplified_type = binder
            .create_environment()
            .simplify(parameter_ty)
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(parameter_syn.span(), &handler)
            })?;

        let parameter_ty = &simplified_type.result;

        let pattern = binder
            .bind_pattern(parameter_syn, parameter_ty, handler)
            .await?
            .unwrap_or_else(|| {
                Irrefutable::Wildcard(Wildcard { span: parameter_syn.span() })
            });

        binder
            .insert_name_binding_point(
                &mut name_binding_point,
                &pattern,
                parameter_ty,
                Address::Memory(Memory::Parameter(parameter_id)),
                Qualifier::Mutable,
                &insert_name_binding::Config {
                    must_copy: false,
                    scope_id: binder.stack().current_scope().scope_id(),
                    address_span: Some(parameter_syn.span()),
                },
                handler,
            )
            .await?;
    }

    binder.add_named_binding_point(name_binding_point);

    // start binding all the statements
    if let Some(statements) = handler_block.handler.statements() {
        for statement in
            statements.statements().filter_map(|x| x.into_line().ok())
        {
            binder.bind_statement(&statement, handler).await?;
        }
    }

    Ok(())
}

async fn extract_effect_operations<
    I: IntoIterator<Item = pernixc_syntax::expression::block::Handler>,
>(
    binder: &mut Binder<'_>,
    effect_handlers: I,
    effect_id: Global<pernixc_symbol::ID>,
    with_span: RelativeSpan,
    handler: &dyn Handler<Diagnostic>,
) -> Result<HashMap<pernixc_symbol::ID, HandlerBlock>, Error> {
    let effect_operations = binder.engine().get_members(effect_id).await;
    let mut handlers = HashMap::<pernixc_symbol::ID, HandlerBlock>::default();

    for handler_syntax in effect_handlers {
        let (Some(identifier), Some(statements)) =
            (handler_syntax.identifier(), handler_syntax.statements())
        else {
            continue;
        };

        // obtains the effect operation id
        let Some(effect_operation_id) = effect_operations
            .member_ids_by_name
            .get(identifier.kind.0.as_str())
        else {
            handler.receive(Diagnostic::UnknownEffectOperation(
                UnknownEffectOperation {
                    effect_id,
                    operation_name: identifier.kind.0.clone(),
                    operation_span: identifier.span(),
                },
            ));
            continue;
        };

        // checks if the effect operation has already been handled
        if let Some(previous_handler) = handlers.get(effect_operation_id) {
            handler.receive(Diagnostic::DuplicatedEffectOperationHandler(
                DuplicatedEffectOperationHandler {
                    effect_id,
                    operation_name: identifier.kind.0.clone(),
                    first_span: previous_handler.identifier.span(),
                    second_span: identifier.span(),
                },
            ));
            continue;
        }

        let mut parameters = Vec::new();
        if let Some(arguments) = handler_syntax.arguments() {
            parameters.extend(arguments.irrefutable_patterns());
        }

        let global_effect_operation_id =
            effect_id.target_id.make_global(*effect_operation_id);

        // checks the number of parameters
        let operation_parameters =
            binder.engine().get_parameters(global_effect_operation_id).await?;

        if operation_parameters.parameters.len() != parameters.len() {
            handler.receive(
                Diagnostic::MismatchedArgumentCountInEffectOperationHandler(
                    MismatchedArgumentCountInEffectOperationHandler {
                        expected: operation_parameters.parameters.len(),
                        found: parameters.len(),
                        operation_id: global_effect_operation_id,
                        span: identifier.span(),
                    },
                ),
            );
        }

        handlers.insert(*effect_operation_id, HandlerBlock {
            parameters,
            statements,
            identifier: identifier.clone(),
            handler: handler_syntax,
        });
    }

    let unhandleds = effect_operations
        .member_ids_by_name
        .values()
        .copied()
        .map(|x| effect_id.target_id.make_global(x))
        .filter(|id| !handlers.contains_key(&id.id))
        .collect::<Vec<_>>();

    // Report unhandled effect operations
    if !unhandleds.is_empty() {
        handler.receive(Diagnostic::UnhandledEffectOperations(
            UnhandledEffectOperations {
                unhandled_effect_operations: unhandleds,
                effect_id,
                with_span,
            },
        ));
    }

    Ok(handlers)
}

#[allow(clippy::too_many_lines)]
async fn extract_effect_handlers(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::block::Do,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<WithBlock>, Error> {
    let mut with_handlers = Vec::<WithBlock>::new();

    for with in syntax_tree.with() {
        let Some(qualified_identifier) = with.effect() else {
            continue;
        };

        // resolves for the effect
        let resolution = match binder
            .engine()
            .resolve_qualified_identifier(
                &qualified_identifier,
                Config::builder()
                    .elided_lifetime_provider(&mut ErasedLifetimeProvider)
                    .extra_namespace(binder.extra_namespace())
                    .referring_site(binder.current_site())
                    .build(),
                &handler,
            )
            .await
        {
            Ok(resolution) => resolution,
            Err(pernixc_resolution::Error::Abort) => {
                // failed to resolve
                continue;
            }
            Err(pernixc_resolution::Error::Cyclic(cyclic)) => {
                return Err(cyclic.into());
            }
        };

        // try extract the effect
        let effect = match resolution {
            Resolution::Generic(generic)
                if {
                    // must be a kind of effect
                    let kind = binder.engine().get_kind(generic.id).await;
                    kind == Kind::Effect
                } =>
            {
                generic
            }

            // not an effect, report error and skip
            resolution => {
                handler.receive(Diagnostic::EffectExpected(EffectExpected {
                    span: qualified_identifier.span(),
                    global_id: resolution.global_id(),
                }));

                continue;
            }
        };

        // check if the effect has already been handled
        let environment = binder.create_environment();
        for effect_handler in &with_handlers {
            if effect_handler.effect_id != effect.id {
                continue;
            }

            // check generic arguments compatibility
            if environment
                .subtypes_generic_arguments(
                    &effect_handler.generic_arguments,
                    &effect.generic_arguments,
                )
                .await
                .map_err(|x| {
                    x.report_as_type_check_overflow(
                        qualified_identifier.span(),
                        &handler,
                    )
                })?
                .is_some()
            {
                handler.receive(Diagnostic::DuplicatedEffectHandler(
                    DuplicatedEffectHandler {
                        effect_id: effect.id,
                        generic_arguments: effect.generic_arguments.clone(),
                        type_inference_map: binder
                            .type_inference_rendering_map(),
                        constant_inference_map: binder
                            .constant_inference_rendering_map(),
                        first_span: effect_handler.qualified_identifier.span(),
                        second_span: qualified_identifier.span(),
                    },
                ));
            }
        }

        let with_span = with.span().join(&qualified_identifier.span());

        let handlers = if let Some(x) = with.body() {
            extract_effect_operations(
                binder,
                x.handlers().filter_map(|x| x.into_line().ok()),
                effect.id,
                with_span,
                handler,
            )
            .await?
        } else {
            extract_effect_operations(
                binder,
                std::iter::empty(),
                effect.id,
                with_span,
                handler,
            )
            .await?
        };

        with_handlers.push(WithBlock {
            qualified_identifier: qualified_identifier.clone(),
            effect_id: effect.id,
            generic_arguments: effect.generic_arguments,
            handlers,
        });
    }

    Ok(with_handlers)
}
