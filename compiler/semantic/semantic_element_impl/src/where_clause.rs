//! Contains the builder for the where clause.

use std::sync::Arc;

use pernixc_handler::{Handler, Storage};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::{runtime::executor, TrackedEngine};
use pernixc_resolution::{
    forall_lifetimes::create_forall_lifetimes,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::{
        resolve_qualified_identifier, resolve_type_bound, Generic, Resolution,
    },
    term::{resolve_lifetime, resolve_type},
    Config, ExtraNamespace,
};
use pernixc_semantic_element::where_clause;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{get_kind, Kind},
    syntax::{get_generic_parameters_syntax, get_where_clause_syntax},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_arguments::{MemberSymbol, TraitMember},
    generic_parameters::{get_generic_parameters, TypeParameterID},
    lifetime::Lifetime,
    predicate::{self, Compatible, NegativeMarker, PositiveMarker},
    r#type::Type,
    visitor::RecursiveIterator,
};

use crate::{
    build::{self, Output},
    occurrences::Occurrences,
    where_clause::diagnostic::{
        Diagnostic, ForallLifetimeIsNotAllowedInOutlivesPredicate,
        PredicateKind, UnexpectedSymbolInPredicate,
        UnexpectedTypeEqualityPredicate,
    },
};

pub mod diagnostic;

async fn create_trait_member_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::ID>,
    syntax_tree: &pernixc_syntax::predicate::TraitTypeEquality,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let (Some(lhs), Some(rhs)) = (syntax_tree.lhs(), syntax_tree.rhs()) else {
        return Ok(());
    };

    let with_forall_lifetime = syntax_tree.higher_ranked_lifetimes().map(|x| {
        let mut extra_namespace = extra_namespace.clone();
        create_forall_lifetimes(&mut extra_namespace.lifetimes, &x, handler);

        extra_namespace
    });

    let extra_namespace =
        with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

    let mut config = Config::builder()
        .observer(occurrences)
        .referring_site(global_id)
        .extra_namespace(extra_namespace)
        .build();

    let ty = engine.resolve_type(&lhs, config.reborrow(), handler).await?;

    match ty {
        // trait type
        Type::TraitMember(TraitMember(MemberSymbol {
            id,
            member_generic_arguments,
            parent_generic_arguments,
        })) if engine.get_kind(id).await == Kind::TraitType => {
            let resolve_ty = engine.resolve_type(&rhs, config, handler).await?;

            where_clause.push(where_clause::Predicate {
                predicate: predicate::Predicate::TraitTypeCompatible(
                    Compatible {
                        lhs: TraitMember(MemberSymbol {
                            id,
                            member_generic_arguments,
                            parent_generic_arguments,
                        }),

                        rhs: resolve_ty,
                    },
                ),
                span: Some(syntax_tree.span()),
            });
        }

        resolution => {
            handler.receive(Diagnostic::UnexpectedTypeEqualityPredicate(
                UnexpectedTypeEqualityPredicate {
                    invalid_lhs_type_span: lhs.span(),
                    found_type: resolution,
                },
            ));
        }
    }

    Ok(())
}

async fn create_trait_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::ID>,
    syntax_tree: &pernixc_syntax::predicate::Trait,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    for trait_predicate in syntax_tree.bounds() {
        let Some(qualified_identifier) = trait_predicate.qualified_identifier()
        else {
            continue;
        };

        let with_forall_lifetime =
            trait_predicate.higher_ranked_lifetimes().map(|x| {
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

        let resolution = match engine
            .resolve_qualified_identifier(
                &qualified_identifier,
                Config::builder()
                    .observer(occurrences)
                    .extra_namespace(extra_namespace)
                    .referring_site(global_id)
                    .build(),
                handler,
            )
            .await
        {
            Ok(resolution) => resolution,

            // couldn't resolve a symbol, couldn't go further
            Err(pernixc_resolution::Error::Abort) => {
                return Ok(());
            }

            Err(pernixc_resolution::Error::Cyclic(error)) => {
                return Err(error);
            }
        };

        match resolution {
            Resolution::Generic(Generic { id, generic_arguments })
                if engine.get_kind(id).await == Kind::Trait =>
            {
                let predicate = if trait_predicate.not_keyword().is_none() {
                    predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            is_const: trait_predicate.const_keyword().is_some(),
                            generic_arguments,
                            trait_id: id,
                        },
                    )
                } else {
                    predicate::Predicate::NegativeTrait(
                        predicate::NegativeTrait {
                            generic_arguments,
                            trait_id: id,
                        },
                    )
                };

                where_clause.push(where_clause::Predicate {
                    predicate,
                    span: Some(trait_predicate.span()),
                });
            }

            resolution => {
                handler.receive(Diagnostic::UnexpectedSymbolInPredicate(
                    UnexpectedSymbolInPredicate {
                        predicate_kind: PredicateKind::Trait,
                        found_id: resolution.global_id(),
                        qualified_identifier_span: qualified_identifier.span(),
                    },
                ));
            }
        }
    }

    Ok(())
}

#[allow(clippy::too_many_lines)]
async fn create_outlives_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::ID>,
    syntax_tree: &pernixc_syntax::predicate::LifetimeOutlives,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let Some(operand) = syntax_tree.operand().and_then(|x| x.identifier())
    else {
        return Ok(());
    };

    let mut bounds = Vec::new();

    let mut config = Config::builder()
        .observer(occurrences)
        .extra_namespace(extra_namespace)
        .referring_site(global_id)
        .build();

    for bound_syn in syntax_tree.bounds() {
        bounds.push(
            engine
                .resolve_lifetime(&bound_syn, config.reborrow(), handler)
                .await,
        );
    }

    let Some(operand) =
        extra_namespace.lifetimes.get(operand.kind.0.as_str()).cloned()
    else {
        handler.receive(
            pernixc_resolution::diagnostic::LifetimeParameterNotFound {
                referred_span: operand.span,
                name: operand.kind.0,
                referring_site: global_id,
            },
        );

        return Ok(());
    };

    for bound in bounds {
        where_clause.push(where_clause::Predicate {
            predicate: predicate::Predicate::LifetimeOutlives(
                predicate::Outlives { operand: operand.clone(), bound },
            ),
            span: Some(syntax_tree.span()),
        });
    }

    Ok(())
}

async fn create_marker_predicate(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::ID>,
    syntax_tree: &pernixc_syntax::predicate::Marker,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    for marker_bound in syntax_tree.bounds() {
        let Some(qualified_identifier) = marker_bound.qualified_identifier()
        else {
            continue;
        };

        let with_forall_lifetime =
            marker_bound.higher_ranked_lifetimes().map(|x| {
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

        let resolution = match engine
            .resolve_qualified_identifier(
                &qualified_identifier,
                Config::builder()
                    .observer(occurrences)
                    .extra_namespace(extra_namespace)
                    .referring_site(global_id)
                    .build(),
                handler,
            )
            .await
        {
            Ok(resolution) => resolution,
            Err(pernixc_resolution::Error::Abort) => continue,
            Err(pernixc_resolution::Error::Cyclic(error)) => return Err(error),
        };

        match resolution {
            Resolution::Generic(Generic { id, generic_arguments })
                if engine.get_kind(id).await == Kind::Marker =>
            {
                let predicate = if marker_bound.not_keyword().is_none() {
                    predicate::Predicate::PositiveMarker(PositiveMarker {
                        marker_id: id,
                        generic_arguments,
                    })
                } else {
                    predicate::Predicate::NegativeMarker(NegativeMarker {
                        marker_id: id,
                        generic_arguments,
                    })
                };

                where_clause.push(where_clause::Predicate {
                    predicate,
                    span: Some(marker_bound.span()),
                });
            }

            resolution => {
                handler.receive(Diagnostic::UnexpectedSymbolInPredicate(
                    UnexpectedSymbolInPredicate {
                        predicate_kind: PredicateKind::Marker,
                        found_id: resolution.global_id(),
                        qualified_identifier_span: qualified_identifier.span(),
                    },
                ));
            }
        }
    }

    Ok(())
}

async fn create_type_bound_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::ID>,
    syntax_tree: &pernixc_syntax::predicate::Type,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    let Some(ty_syn) = syntax_tree.r#type() else { return Ok(()) };

    let with_forall_lifetime = syntax_tree.higher_ranked_lifetimes().map(|x| {
        let mut extra_namespace = extra_namespace.clone();
        create_forall_lifetimes(&mut extra_namespace.lifetimes, &x, handler);

        extra_namespace
    });

    let extra_namespace =
        with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

    let mut config = Config::builder()
        .observer(occurrences)
        .extra_namespace(extra_namespace)
        .referring_site(global_id)
        .build();

    let ty = engine.resolve_type(&ty_syn, config.reborrow(), handler).await?;

    create_type_bound_predicates_internal(
        engine,
        &ty,
        &ty_syn.span(),
        syntax_tree.bounds(),
        config,
        where_clause,
        handler,
    )
    .await
}

#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
async fn create_type_bound_predicates_internal(
    engine: &TrackedEngine,
    ty: &Type,
    type_span: &RelativeSpan,
    bounds: impl IntoIterator<Item = pernixc_syntax::predicate::TypeBound>,
    mut config: Config<'_, '_, '_, '_, '_>,
    where_clause: &mut Vec<where_clause::Predicate>,
    handler: &Storage<Diagnostic>,
) -> Result<(), executor::CyclicError> {
    for bound in bounds {
        match bound {
            pernixc_syntax::predicate::TypeBound::QualifiedIdentifier(
                qualified_identifier_bound,
            ) => {
                let Some(qualified_identifier) =
                    qualified_identifier_bound.qualified_identifier()
                else {
                    continue;
                };

                let original_extra_namespace = config.extra_namespace;

                let optional_namespace = if let Some(forall_lifetimes) =
                    qualified_identifier_bound.higher_ranked_lifetimes()
                {
                    let mut extra_namespace =
                        config.extra_namespace.cloned().unwrap_or_default();

                    create_forall_lifetimes(
                        &mut extra_namespace.lifetimes,
                        &forall_lifetimes,
                        handler,
                    );

                    Some(extra_namespace)
                } else {
                    None
                };

                let config = Config {
                    extra_namespace: optional_namespace
                        .as_ref()
                        .or(original_extra_namespace),
                    ..config.reborrow()
                };

                let resolution = match engine
                    .resolve_type_bound(
                        &qualified_identifier,
                        ty,
                        config,
                        handler,
                    )
                    .await
                {
                    Ok(resolution) => resolution,
                    Err(pernixc_resolution::Error::Abort) => continue,
                    Err(pernixc_resolution::Error::Cyclic(error)) => {
                        return Err(error);
                    }
                };

                let resolved_kind =
                    engine.get_kind(resolution.global_id()).await;

                let preidcate = match (
                    resolved_kind,
                    resolution,
                    qualified_identifier_bound.not_keyword().is_some(),
                ) {
                    (
                        Kind::Trait,
                        Resolution::Generic(Generic { id, generic_arguments }),
                        false,
                    ) => predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            is_const: false,
                            generic_arguments,
                            trait_id: id,
                        },
                    ),

                    (
                        Kind::Trait,
                        Resolution::Generic(Generic { id, generic_arguments }),
                        true,
                    ) => predicate::Predicate::NegativeTrait(
                        predicate::NegativeTrait {
                            generic_arguments,
                            trait_id: id,
                        },
                    ),

                    (
                        Kind::Marker,
                        Resolution::Generic(Generic { id, generic_arguments }),
                        false,
                    ) => predicate::Predicate::PositiveMarker(PositiveMarker {
                        marker_id: id,
                        generic_arguments,
                    }),

                    (
                        Kind::Marker,
                        Resolution::Generic(Generic { id, generic_arguments }),
                        true,
                    ) => predicate::Predicate::NegativeMarker(NegativeMarker {
                        marker_id: id,
                        generic_arguments,
                    }),

                    (_, found_resolution, _) => {
                        handler.receive(
                            Diagnostic::UnexpectedSymbolInPredicate(
                                UnexpectedSymbolInPredicate {
                                    predicate_kind: PredicateKind::TypeBound,
                                    found_id: found_resolution.global_id(),
                                    qualified_identifier_span:
                                        qualified_identifier.span(),
                                },
                            ),
                        );

                        continue;
                    }
                };

                where_clause.push(where_clause::Predicate {
                    predicate: preidcate,
                    span: Some(qualified_identifier_bound.span()),
                });
            }

            pernixc_syntax::predicate::TypeBound::Const(syn) => {
                where_clause.push(where_clause::Predicate {
                    predicate: predicate::Predicate::ConstantType(
                        predicate::ConstantType(ty.clone()),
                    ),
                    span: Some(syn.span()),
                });
            }
            pernixc_syntax::predicate::TypeBound::Tuple(syn) => {
                where_clause.push(where_clause::Predicate {
                    predicate: predicate::Predicate::TupleType(
                        predicate::Tuple(ty.clone()),
                    ),
                    span: Some(syn.span()),
                });
            }
            pernixc_syntax::predicate::TypeBound::Outlives(lifetime) => {
                let bound = engine
                    .resolve_lifetime(&lifetime, config.reborrow(), handler)
                    .await;

                let forall_lts_in_ty = RecursiveIterator::new(ty)
                    .filter_map(|x| {
                        x.0.as_lifetime().and_then(|x| x.as_forall().cloned())
                    })
                    .collect::<Vec<_>>();

                if !forall_lts_in_ty.is_empty() {
                    handler.receive(Diagnostic::from(
                        ForallLifetimeIsNotAllowedInOutlivesPredicate {
                            forall_lifetime_span: *type_span,
                            forall_lifetimes: forall_lts_in_ty,
                        },
                    ));
                    continue;
                }

                if let Lifetime::Forall(forall_lt) = bound {
                    handler.receive(Diagnostic::from(
                        ForallLifetimeIsNotAllowedInOutlivesPredicate {
                            forall_lifetime_span: lifetime.span(),
                            forall_lifetimes: vec![forall_lt],
                        },
                    ));
                    continue;
                }

                where_clause.push(where_clause::Predicate {
                    predicate: predicate::Predicate::TypeOutlives(
                        predicate::Outlives { operand: ty.clone(), bound },
                    ),
                    span: Some(lifetime.span()),
                });
            }
        }
    }

    Ok(())
}

impl build::Build for pernixc_semantic_element::where_clause::Key {
    type Diagnostic = diagnostic::Diagnostic;

    #[allow(clippy::too_many_lines)]
    async fn execute(
        engine: &pernixc_query::TrackedEngine,
        key: &Self,
    ) -> Result<Output<Self>, executor::CyclicError> {
        let kind = engine.get_kind(key.0).await;
        let where_clause_syntax_tree =
            engine.get_where_clause_syntax(key.0).await;

        let mut where_clause = Vec::default();

        let mut occurrences = Occurrences::default();
        let storage = Storage::<Diagnostic>::new();

        let extra_namespace =
            engine.get_generic_parameter_namespace(key.0).await?;

        for predicate in where_clause_syntax_tree
            .into_iter()
            .flat_map(|x| x.predicates().collect::<Vec<_>>())
            .filter_map(|x| x.into_line().ok())
        {
            match predicate {
                pernixc_syntax::predicate::Predicate::Type(ty_bound) => {
                    create_type_bound_predicates(
                        engine,
                        key.0,
                        &ty_bound,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await?;
                }

                pernixc_syntax::predicate::Predicate::TraitTypeEquality(
                    trait_type_equality,
                ) => {
                    create_trait_member_predicates(
                        engine,
                        key.0,
                        &trait_type_equality,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await?;
                }

                pernixc_syntax::predicate::Predicate::Trait(tr) => {
                    create_trait_predicates(
                        engine,
                        key.0,
                        &tr,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await?;
                }

                pernixc_syntax::predicate::Predicate::LifetimeOutlives(
                    lifetime_outlives,
                ) => {
                    create_outlives_predicates(
                        engine,
                        key.0,
                        &lifetime_outlives,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await?;
                }

                pernixc_syntax::predicate::Predicate::Marker(marker) => {
                    create_marker_predicate(
                        engine,
                        key.0,
                        &marker,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await?;
                }
            }
        }

        {
            if kind.has_generic_parameters() {
                let generic_parameters_syn =
                    engine.get_generic_parameters_syntax(key.0).await;

                let generic_parameters =
                    engine.get_generic_parameters(key.0).await?;

                for (ty_param, bounds) in generic_parameters_syn
                    .into_iter()
                    .flat_map(|x| x.parameters().collect::<Vec<_>>())
                    .filter_map(|x| x.into_type().ok())
                    .filter_map(|x| x.bound().map(|y| (x, y)))
                {
                    let Some(identifier) = ty_param.identifier() else {
                        continue;
                    };

                    let Some(id) = generic_parameters
                        .type_parameter_ids_by_name()
                        .get(identifier.kind.0.as_str())
                    else {
                        continue;
                    };

                    create_type_bound_predicates_internal(
                        engine,
                        &Type::Parameter(TypeParameterID::new(key.0, *id)),
                        &identifier.span,
                        bounds.bounds(),
                        Config::builder()
                            .observer(&mut occurrences)
                            .extra_namespace(&extra_namespace)
                            .referring_site(key.0)
                            .build(),
                        &mut where_clause,
                        &storage,
                    )
                    .await?;
                }
            }
        }

        Ok(Output {
            item: where_clause.into(),
            diagnostics: storage.into_vec().into(),
            occurrences: Arc::new(occurrences),
        })
    }
}

build::register_build!(pernixc_semantic_element::where_clause::Key);
