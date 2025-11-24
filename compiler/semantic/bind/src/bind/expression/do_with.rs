use std::collections::hash_map::Entry;

use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    address::{Address, Memory},
    capture::{builder::CapturesWithNameBindingPoint, pruning::PruneMode},
    closure_parameters::ClosureParameters,
    handling_scope::{HandlerClause, HandlingScope},
    pattern::{Irrefutable, NameBindingPoint, Wildcard},
    value::{
        Value,
        register::{
            self, Assignment,
            do_with::{Do, OperationHandler},
        },
    },
};
use pernixc_lexical::tree::{RelativeLocation, RelativeSpan};
use pernixc_resolution::{
    Config,
    qualified_identifier::{Resolution, resolve_qualified_identifier},
};
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_source_file::{SourceElement, Span};
use pernixc_symbol::{
    kind::{Kind, get_kind},
    member::get_members,
};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::GenericArguments,
    instantiation::get_instantiation,
    r#type::{Qualifier, Type},
};
use pernixc_type_system::UnrecoverableError;

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{
        Binder, BindingError, Error, inference_context::ErasedLifetimeProvider,
    },
    diagnostic::{
        Diagnostic, DuplicatedEffectHandler, DuplicatedEffectOperationHandler,
        EffectExpected, MismatchedArgumentCountInEffectOperationHandler,
        UnhandledEffectOperations, UnknownEffectOperation,
    },
    infer::constraint,
    pattern::insert_name_binding,
};

fn span_of_multi_syntax<T: SourceElement<Location = RelativeLocation>>(
    elements: impl Iterator<Item = T>,
) -> Option<RelativeSpan> {
    let mut span = None::<Span<RelativeLocation>>;

    for element in elements {
        span = span.map_or_else(
            || Some(element.span()),
            |current_span| Some(current_span.join(&element.span())),
        );
    }

    span
}

impl Bind<&pernixc_syntax::expression::block::Do> for Binder<'_> {
    #[allow(unreachable_code, unused_variables)]
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Do,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let (Some(do_kw), Some(do_statements)) =
            (syntax_tree.do_keyword(), syntax_tree.statements())
        else {
            return Err(Error::Binding(BindingError(syntax_tree.span())));
        };

        let captures = self.create_captures(handler).await?;

        let effect_handlers =
            extract_handler_chain(self, syntax_tree, handler).await?;

        let expected_return_type = Type::Inference(
            self.create_type_inference(constraint::Type::All(true)),
        );

        let mut do_closure = Box::pin(self.new_closure_binder(
            async |x| {
                build_do_block(x, &do_statements, handler).await?;
                Ok(())
            },
            expected_return_type.clone(),
            do_statements.span(),
            &captures,
            None,
            handler,
        ))
        .await?;
        let mut do_captures = captures.captures().clone();

        // prune the captures
        do_captures.prune_capture_ir(
            std::iter::once(&mut do_closure),
            PruneMode::Once,
        );

        // pop the closure from the stack
        self.pop_handling_scope(effect_handlers.effect_handler_group_id);

        let do_capture_arguments =
            self.bind_capture_arguments(do_captures, do_kw.span());

        let with = build_with_blocks(
            self,
            effect_handlers.with_blocks,
            &expected_return_type,
            span_of_multi_syntax(syntax_tree.with()).unwrap_or(do_kw.span),
            captures,
            handler,
        )
        .await?;

        let do_assignment = register::do_with::DoWith::new(
            effect_handlers.effect_handler_group_id,
            Do::new(do_capture_arguments, do_closure),
            with,
            expected_return_type,
        );

        Ok(Expression::RValue(Value::Register(
            self.create_register_assignment(
                Assignment::Do(do_assignment),
                syntax_tree.span(),
            ),
        )))
    }
}

struct HandlerChain {
    with_blocks: Vec<HandlerClauseBlock>,
    effect_handler_group_id: pernixc_arena::ID<HandlingScope>,
}

struct HandlerClauseBlock {
    qualified_identifier: QualifiedIdentifier,
    effect_id: Global<pernixc_symbol::ID>,
    generic_arguments: GenericArguments,
    handler_clause_id: pernixc_arena::ID<HandlerClause>,
    handlers: HashMap<pernixc_symbol::ID, OperationHanderBlock>,
}

struct OperationHanderBlock {
    identifier: pernixc_syntax::Identifier,
    statements: pernixc_syntax::statement::Statements,
    parameters: Vec<pernixc_syntax::pattern::Irrefutable>,
    handler: pernixc_syntax::expression::block::Handler,
}

#[allow(unreachable_code, unused_variables)]
async fn build_with_blocks(
    binder: &mut Binder<'_>,
    with_blocks: Vec<HandlerClauseBlock>,
    expected_type: &Type,
    with_span: RelativeSpan,
    captures: CapturesWithNameBindingPoint,
    handler: &dyn Handler<Diagnostic>,
) -> Result<register::do_with::HandlerChain, Error> {
    let mut with_irs = HashMap::default();

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
            let effect_operation_parameters = binder
                .engine()
                .get_parameters(Global::new(
                    with_block.effect_id.target_id,
                    effect_operation_id,
                ))
                .await?;

            let closure_parameters =
                ClosureParameters::from_original_parameters_and_instantiation(
                    &effect_operation_parameters,
                    &instantiation,
                    handler_block.parameters.iter().map(SourceElement::span),
                );

            let ir = binder
                .new_closure_binder(
                    async |x| {
                        Box::pin(build_handler_block(
                            x,
                            &closure_parameters,
                            handler_block,
                            handler,
                        ))
                        .await?;

                        Ok(())
                    },
                    expected_type.clone(),
                    statements_span,
                    &captures,
                    Some(&closure_parameters),
                    handler,
                )
                .await?;

            // only insert if it's not duplication
            let Entry::Vacant(entry) = with_irs
                .entry((with_block.handler_clause_id, effect_operation_id))
            else {
                continue;
            };

            entry.insert((ir, closure_parameters));
        }
    }

    let mut underlying_captures = captures.into_captures();

    // prune the captures
    underlying_captures.prune_capture_ir(
        with_irs.values_mut().map(|(ir, _)| ir),
        PruneMode::Multiple,
    );

    let mut with = register::do_with::HandlerChain::new(
        binder.bind_capture_arguments(underlying_captures, with_span),
    );

    for ((effect_handler_id, effect_operation_id), (ir, closure_parameters)) in
        with_irs
    {
        let effect_handler = with.insert_handler_clause(effect_handler_id);

        effect_handler.insert_effect_operation_handler_closure(
            effect_operation_id,
            OperationHandler::new(ir, closure_parameters),
        );
    }

    Ok(with)
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
    closure_parameters: &ClosureParameters,
    handler_block: OperationHanderBlock,
    handler: &dyn Handler<Diagnostic>,
) -> Result<(), UnrecoverableError> {
    // start binding all the arguments as parameters
    let mut name_binding_point = NameBindingPoint::default();

    for ((parameter_id, parameter), parameter_syn) in
        closure_parameters.parameters_as_order().zip(&handler_block.parameters)
    {
        let simplified_type = binder
            .create_environment()
            .simplify(parameter.r#type.clone())
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
                Address::Memory(Memory::ClosureParameter(parameter_id)),
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
) -> Result<HashMap<pernixc_symbol::ID, OperationHanderBlock>, Error> {
    let effect_operations = binder.engine().get_members(effect_id).await;
    let mut handlers =
        HashMap::<pernixc_symbol::ID, OperationHanderBlock>::default();

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

        handlers.insert(*effect_operation_id, OperationHanderBlock {
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
async fn extract_handler_chain(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::block::Do,
    handler: &dyn Handler<Diagnostic>,
) -> Result<HandlerChain, Error> {
    // create a new handler group for this one
    let handler_group_id = binder.insert_effect_handler_group();

    let mut with_handlers = Vec::<HandlerClauseBlock>::new();

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

        with_handlers.push(HandlerClauseBlock {
            qualified_identifier: qualified_identifier.clone(),
            effect_id: effect.id,
            handler_clause_id: binder.insert_handler_clause_to_handling_scope(
                handler_group_id,
                HandlerClause::new(effect.id, effect.generic_arguments.clone()),
            ),
            generic_arguments: effect.generic_arguments,
            handlers,
        });
    }

    Ok(HandlerChain {
        with_blocks: with_handlers,
        effect_handler_group_id: handler_group_id,
    })
}
