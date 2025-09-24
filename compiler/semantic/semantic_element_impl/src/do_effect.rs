use std::sync::Arc;

use pernixc_handler::Storage;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_resolution::{
    forall_lifetimes::create_forall_lifetimes,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    Config, ExtraNamespace,
};
use pernixc_semantic_element::do_effect;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    syntax::get_function_do_effect_syntax,
};
use pernixc_syntax::item::function::{EffectUnit, EffectUnitListKind};
use pernixc_target::Global;
use pernixc_term::{
    effect::{self, Effect},
    generic_arguments::Symbol,
};

use crate::{
    build::{Build, Output},
    occurrences::Occurrences,
};

pub mod diagnostic;

async fn build_do_effect(
    engine: &TrackedEngine,
    effect_unit_syntax: EffectUnit,
    current_site: Global<pernixc_symbol::ID>,
    observer: &mut Occurrences,
    extra_namespace: &ExtraNamespace,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<Option<effect::Unit>, CyclicError> {
    let Some(q_ident) = effect_unit_syntax.qualified_identifier() else {
        return Ok(None);
    };

    let with_forall_lifetime =
        effect_unit_syntax.higher_ranked_lifetimes().map(|x| {
            let mut extra_namespace = extra_namespace.clone();

            create_forall_lifetimes(
                &mut extra_namespace.lifetimes,
                &x,
                handler,
            );

            extra_namespace
        });

    let extra_namespace =
        with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

    let config = Config::builder()
        .observer(observer)
        .referring_site(current_site)
        .extra_namespace(extra_namespace)
        .build();

    let resolution = match engine
        .resolve_qualified_identifier(&q_ident, config, handler)
        .await
    {
        Ok(resolution) => resolution,
        Err(er) => match er {
            pernixc_resolution::Error::Abort => return Ok(None),
            pernixc_resolution::Error::Cyclic(cyclic_error) => {
                return Err(cyclic_error);
            }
        },
    };

    let Resolution::Generic(symbol) = resolution else {
        return Ok(None);
    };

    let kind = engine.get_kind(symbol.id).await;

    Ok((kind == Kind::Effect).then_some(effect::Unit(Symbol {
        id: symbol.id,
        generic_arguments: symbol.generic_arguments,
    })))
}

impl Build for do_effect::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(
        engine: &TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, CyclicError> {
        let do_effect_syntax =
            engine.get_function_do_effect_syntax(key.0).await;

        let generic_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;
        let mut observer = Occurrences::default();
        let mut do_effects = Vec::new();
        let storage = Storage::<diagnostic::Diagnostic>::default();

        // extract the effect units from the do effect syntax
        if let Some(do_effect_syntax) =
            do_effect_syntax.and_then(|x| x.effect_unit_list())
        {
            match do_effect_syntax {
                EffectUnitListKind::EffectUnitList(effect_unit_list) => {
                    for effect_unit in effect_unit_list.effect_units() {
                        if let Some(effect_unit) = build_do_effect(
                            engine,
                            effect_unit,
                            key.0,
                            &mut observer,
                            &generic_namespace,
                            &storage,
                        )
                        .await?
                        {
                            do_effects.push(effect_unit);
                        }
                    }
                }
                EffectUnitListKind::ParenthesizedEffectUnitList(
                    parenthesized_effect_unit_list,
                ) => {
                    for effect_unit in
                        parenthesized_effect_unit_list.effect_units()
                    {
                        if let Some(effect_unit) = build_do_effect(
                            engine,
                            effect_unit,
                            key.0,
                            &mut observer,
                            &generic_namespace,
                            &storage,
                        )
                        .await?
                        {
                            do_effects.push(effect_unit);
                        }
                    }
                }
            }
        }

        Ok(Output {
            item: Arc::new(Effect {
                effects: do_effects.into_iter().collect(),
            }),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(observer),
        })
    }
}

crate::build::register_build!(do_effect::Key);
