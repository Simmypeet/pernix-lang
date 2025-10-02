use pernixc_handler::Handler;
use pernixc_ir::value::Value;
use pernixc_resolution::{
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    Config,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::kind::{get_kind, Kind};
use pernixc_syntax::QualifiedIdentifier;
use pernixc_target::Global;
use pernixc_term::{generic_arguments::GenericArguments, r#type::Type};

use crate::{
    bind::{Bind, Expression, Guidance},
    binder::{inference_context::ErasedLifetimeProvider, Binder, Error},
    diagnostic::{Diagnostic, DuplicatedEffectHandler, EffectExpected},
    infer::constraint,
};

impl Bind<&pernixc_syntax::expression::block::Do> for Binder<'_> {
    async fn bind(
        &mut self,
        syntax_tree: &pernixc_syntax::expression::block::Do,
        _: &Guidance<'_>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Expression, Error> {
        let _ = extract_effect_handlers(self, syntax_tree, handler).await?;

        Ok(Expression::RValue(Value::error(
            Type::Inference(
                self.create_type_inference(constraint::Type::All(true)),
            ),
            Some(syntax_tree.span()),
        )))
    }
}

struct WithHandler {
    qualified_identifier: QualifiedIdentifier,
    effect_id: Global<pernixc_symbol::ID>,
    generic_arguments: GenericArguments,
}

async fn extract_effect_handlers(
    binder: &mut Binder<'_>,
    syntax_tree: &pernixc_syntax::expression::block::Do,
    handler: &dyn Handler<Diagnostic>,
) -> Result<Vec<WithHandler>, Error> {
    let mut with_handlers = Vec::<WithHandler>::new();

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

        with_handlers.push(WithHandler {
            qualified_identifier: qualified_identifier.clone(),
            effect_id: effect.id,
            generic_arguments: effect.generic_arguments,
        });
    }

    Ok(with_handlers)
}
