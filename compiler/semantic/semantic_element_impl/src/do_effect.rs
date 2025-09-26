use std::{borrow::Cow, collections::BTreeSet, sync::Arc};

use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor::CyclicError, TrackedEngine};
use pernixc_resolution::{
    forall_lifetimes::create_forall_lifetimes,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{resolve_qualified_identifier, Resolution},
    Config, ExtraNamespace,
};
use pernixc_semantic_element::do_effect;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    syntax::get_function_do_effect_syntax,
};
use pernixc_syntax::item::function::{EffectUnit, EffectUnitListKind};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    effect::{self, Effect},
    generic_arguments::Symbol,
    lifetime::Lifetime,
    r#type::Type,
};
use pernixc_type_system::{
    environment::{get_active_premise, Environment},
    normalizer,
    unification::{self, Log, Unification},
    Satisfied, Succeeded, UnrecoverableError,
};

use crate::{
    build::{Build, Output},
    occurrences::Occurrences,
};

pub mod diagnostic;

async fn to_effect(
    engine: &TrackedEngine,
    resolution: Resolution,
) -> Option<effect::Unit> {
    let Resolution::Generic(symbol) = resolution else {
        return None;
    };

    let kind = engine.get_kind(symbol.id).await;

    (kind == Kind::Effect).then_some(effect::Unit(Symbol {
        id: symbol.id,
        generic_arguments: symbol.generic_arguments,
    }))
}

async fn build_do_effect(
    engine: &TrackedEngine,
    effect_unit_syntax: EffectUnit,
    current_site: Global<pernixc_symbol::ID>,
    observer: &mut Occurrences,
    extra_namespace: &ExtraNamespace,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<Option<(effect::Unit, RelativeSpan)>, CyclicError> {
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
    let id = resolution.global_id();

    to_effect(engine, resolution).await.map_or_else(
        || {
            handler.receive(diagnostic::Diagnostic::EffectExpected(
                diagnostic::EffectExpected {
                    found: id,
                    found_span: q_ident.span(),
                },
            ));

            Ok(None)
        },
        |effect| Ok(Some((effect, effect_unit_syntax.span()))),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct LifetimeUnifyingPredicate;

impl unification::Predicate<Lifetime> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        _: &[Log],
        _: &[Log],
    ) -> pernixc_type_system::Result<Satisfied> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Type,
        _: &Type,
        _: &[Log],
        _: &[Log],
    ) -> pernixc_type_system::Result<Satisfied> {
        Ok(None)
    }
}

impl unification::Predicate<Constant> for LifetimeUnifyingPredicate {
    fn unifiable(
        &self,
        _: &Constant,
        _: &Constant,
        _: &[Log],
        _: &[Log],
    ) -> pernixc_type_system::Result<Satisfied> {
        Ok(None)
    }
}

async fn effect_equivalent(
    env: &Environment<'_, normalizer::NoOp>,
    a: &effect::Unit,
    b: &effect::Unit,
    span: RelativeSpan,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<bool, UnrecoverableError> {
    // compare the effects by their resolved symbols
    if a.id != b.id {
        return Ok(false);
    }

    // check if all the generic arguments are compatible
    if !a.generic_arguments.has_same_arguments_count(&b.generic_arguments) {
        return Ok(false);
    }

    // lifetimes are always compatible with each other

    for (ty_a, ty_b) in
        a.generic_arguments.types.iter().zip(b.generic_arguments.types.iter())
    {
        let found_diff = env
            .query(&Unification::new(
                ty_a.clone(),
                ty_b.clone(),
                LifetimeUnifyingPredicate,
            ))
            .await
            .map_err(|e| e.report_as_type_calculating_overflow(span, handler))?
            .is_none();

        if found_diff {
            return Ok(false);
        }
    }

    for (const_a, const_b) in a
        .generic_arguments
        .constants
        .iter()
        .zip(b.generic_arguments.constants.iter())
    {
        let found_diff = env
            .query(&Unification::new(
                const_a.clone(),
                const_b.clone(),
                LifetimeUnifyingPredicate,
            ))
            .await
            .map_err(|e| e.report_as_type_calculating_overflow(span, handler))?
            .is_none();

        if found_diff {
            return Ok(false);
        }
    }

    Ok(true)
}

async fn detect_duplicating_group<
    'a,
    I: Iterator<Item = (&'a effect::Unit, &'a RelativeSpan)>,
>(
    engine: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    effects: I,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<BTreeSet<effect::Unit>, UnrecoverableError> {
    let active_premise = engine.get_active_premise(symbol_id).await?;
    let env = Environment::new(
        Cow::Borrowed(&active_premise),
        Cow::Borrowed(engine),
        normalizer::NO_OP,
    );

    let mut groups: Vec<Vec<(&effect::Unit, &RelativeSpan)>> = Vec::new();

    for effect in effects {
        let mut unique = true;

        for group in &mut groups {
            let first = &group.first().unwrap().0;

            if effect_equivalent(&env, first, effect.0, *effect.1, handler)
                .await?
            {
                group.push(effect);
                unique = false;
                break;
            }
        }

        if unique {
            groups.push(vec![effect]);
        }
    }

    todo!()
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
        let mut do_effects = HashMap::default();
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
                            do_effects.insert(effect_unit.0, effect_unit.1);
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
                            do_effects.insert(effect_unit.0, effect_unit.1);
                        }
                    }
                }
            }
        }

        let effects = match detect_duplicating_group(
            engine,
            key.0,
            do_effects.iter(),
            &storage,
        )
        .await
        {
            Ok(effect) => effect,

            Err(UnrecoverableError::Reported) => BTreeSet::default(),
            Err(UnrecoverableError::CyclicDependency(e)) => return Err(e),
        };

        Ok(Output {
            item: Arc::new(Effect { effects }),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(observer),
        })
    }
}

crate::build::register_build!(do_effect::Key);
