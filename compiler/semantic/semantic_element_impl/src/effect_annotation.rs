use std::borrow::Cow;

use pernixc_arena::OrderedArena;
use pernixc_handler::{Handler, Storage};
use pernixc_hash::HashMap;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    Config, ExtraNamespace,
    forall_lifetimes::create_forall_lifetimes,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{Resolution, resolve_qualified_identifier},
};
use pernixc_semantic_element::effect_annotation;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    parent::get_parent_global,
    syntax::get_function_effect_annotation_syntax,
};
use pernixc_syntax::item::function::{EffectUnit, EffectUnitListKind};
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    effect,
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    lifetime::{Forall, FromSemanticElement, GeneratedForall, Lifetime},
    r#type::Type,
};
use pernixc_type_system::{
    Satisfied, Succeeded, UnrecoverableError,
    environment::{Environment, get_active_premise},
    normalizer,
    unification::{self, Log, Unification},
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

struct ElidedForallLifetimeProvider {
    count: usize,
    current_site: Global<pernixc_symbol::ID>,
}

impl pernixc_resolution::ElidedTermProvider<Lifetime>
    for ElidedForallLifetimeProvider
{
    fn create(&mut self) -> Lifetime {
        let counter = self.count;
        self.count += 1;

        Lifetime::Forall(Forall::Generated(GeneratedForall {
            from_id: self.current_site,
            from_semantic_element: FromSemanticElement::DoEffect,
            unique_counter: counter,
        }))
    }
}

async fn build_effect_annotation(
    engine: &TrackedEngine,
    effect_unit_syntax: EffectUnit,
    current_site: Global<pernixc_symbol::ID>,
    observer: &mut Occurrences,
    extra_namespace: &ExtraNamespace,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Option<(effect::Unit, RelativeSpan)> {
    let q_ident = effect_unit_syntax.qualified_identifier()?;

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

    let mut elided_lifetime_provider =
        ElidedForallLifetimeProvider { count: 0, current_site };

    let config = Config::builder()
        .observer(observer)
        .referring_site(current_site)
        .extra_namespace(extra_namespace)
        .elided_lifetime_provider(&mut elided_lifetime_provider)
        .build();

    let resolution = match engine
        .resolve_qualified_identifier(&q_ident, config, handler)
        .await
    {
        Ok(resolution) => resolution,
        Err(er) => match er {
            pernixc_resolution::Error::Abort => return None,
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

            None
        },
        |effect| Some((effect, effect_unit_syntax.span())),
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
    I: Iterator<Item = (&'a effect::Unit, &'a (RelativeSpan, usize))>,
>(
    engine: &TrackedEngine,
    symbol_id: Global<pernixc_symbol::ID>,
    effects: I,
    handler: &Storage<diagnostic::Diagnostic>,
) -> Result<OrderedArena<effect::Unit>, UnrecoverableError> {
    let active_premise = engine.get_active_premise(symbol_id).await;
    let env = Environment::new(
        Cow::Borrowed(&active_premise),
        Cow::Borrowed(engine),
        normalizer::NO_OP,
    );

    let mut groups: Vec<Vec<(&effect::Unit, &(RelativeSpan, usize))>> =
        Vec::new();

    for effect in effects {
        let mut unique = true;

        for group in &mut groups {
            let first = &group.first().unwrap().0;

            if effect_equivalent(&env, first, effect.0, effect.1.0, handler)
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

    let mut result = OrderedArena::new();
    for mut group in groups {
        if group.len() > 1 {
            group.sort_by_key(|(_, (_, index))| *index);

            handler.receive(diagnostic::Diagnostic::AmbiguousEffectDefinition(
                diagnostic::AmbiguousEffectDefinition {
                    first_effect: (*group.first().unwrap().0).clone(),
                    ambiguos_spans: group
                        .iter()
                        .map(|(_, span)| span.0)
                        .collect(),
                },
            ));
        } else {
            let (effect, _) = group.first().unwrap();
            result.insert((*effect).clone());
        }
    }

    Ok(result)
}

impl Build for effect_annotation::Key {
    type Diagnostic = diagnostic::Diagnostic;

    async fn execute(engine: &TrackedEngine, key: &Self) -> Output<Self> {
        let kind = engine.get_kind(key.symbol_id).await;

        if kind == Kind::EffectOperation {
            // simply return the parent effect's do effect
            let parent_effect_id =
                engine.get_parent_global(key.symbol_id).await.unwrap();
            let parent_generic_parameters =
                engine.get_generic_parameters(parent_effect_id).await;

            let arguments = parent_generic_parameters
                .create_identity_generic_arguments(parent_effect_id);

            let mut arena = OrderedArena::new();
            arena.insert(effect::Unit(Symbol {
                id: parent_effect_id,
                generic_arguments: arguments,
            }));

            return Output {
                item: engine.intern(arena),
                diagnostics: engine.intern_unsized([]),
                occurrences: engine.intern(Occurrences::default()),
            };
        }

        let effect_annotation_syntax =
            engine.get_function_effect_annotation_syntax(key.symbol_id).await;

        let generic_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;
        let mut observer = Occurrences::default();
        let mut effect_annotations = HashMap::default();
        let storage = Storage::<diagnostic::Diagnostic>::default();

        // extract the effect units from the do effect syntax
        if let Some(effect_annotation_syntax) =
            effect_annotation_syntax.and_then(|x| x.effect_unit_list())
        {
            match effect_annotation_syntax {
                EffectUnitListKind::EffectUnitList(effect_unit_list) => {
                    for effect_unit in effect_unit_list.effect_units() {
                        if let Some(effect_unit) = build_effect_annotation(
                            engine,
                            effect_unit,
                            key.symbol_id,
                            &mut observer,
                            &generic_namespace,
                            &storage,
                        )
                        .await
                        {
                            let len = effect_annotations.len();
                            effect_annotations
                                .entry(effect_unit.0)
                                .or_insert((effect_unit.1, len));
                        }
                    }
                }
                EffectUnitListKind::ParenthesizedEffectUnitList(
                    parenthesized_effect_unit_list,
                ) => {
                    for effect_unit in
                        parenthesized_effect_unit_list.effect_units()
                    {
                        if let Some(effect_unit) = build_effect_annotation(
                            engine,
                            effect_unit,
                            key.symbol_id,
                            &mut observer,
                            &generic_namespace,
                            &storage,
                        )
                        .await
                        {
                            let len = effect_annotations.len();
                            effect_annotations
                                .entry(effect_unit.0)
                                .or_insert((effect_unit.1, len));
                        }
                    }
                }
            }
        }

        let effects = detect_duplicating_group(
            engine,
            key.symbol_id,
            effect_annotations.iter(),
            &storage,
        )
        .await
        .unwrap_or_default();

        Output {
            item: engine.intern(effects),
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(observer),
        }
    }
}

crate::build::register_build!(effect_annotation::Key);
