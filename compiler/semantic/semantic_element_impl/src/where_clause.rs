//! Contains the builder for the where clause.

use pernixc_handler::{Handler, Storage};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::TrackedEngine;
use pernixc_resolution::{
    ExtraNamespace, Resolver,
    generic_parameter_namespace::get_generic_parameter_namespace,
    qualified_identifier::Resolution,
};
use pernixc_semantic_element::where_clause;
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    kind::{Kind, get_kind},
    syntax::{get_generic_parameters_syntax, get_where_clause_syntax},
};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::{TypeParameterID, get_generic_parameters},
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
    global_id: Global<pernixc_symbol::SymbolID>,
    syntax_tree: &pernixc_syntax::predicate::TraitTypeEquality,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) {
    let (Some(lhs), Some(rhs)) = (syntax_tree.lhs(), syntax_tree.rhs()) else {
        return;
    };

    let mut resolver = Resolver::builder()
        .tracked_engine(engine)
        .handler(handler)
        .observer(occurrences)
        .referring_site(global_id)
        .extra_namespace(extra_namespace)
        .build();

    resolver.push_higher_ranked_lifetimes(
        syntax_tree.higher_ranked_lifetimes().as_ref(),
    );

    let ty = resolver.resolve_type(&lhs).await;

    match ty {
        // trait type
        Type::InstanceAssociated(instance_associated) => {
            let resolve_ty = resolver.resolve_type(&rhs).await;

            resolver.pop_higher_ranked_lifetimes();

            where_clause.push(where_clause::Predicate {
                predicate: predicate::Predicate::InstanceAssociatedTypeEquality(
                    Compatible { lhs: instance_associated, rhs: resolve_ty },
                ),
                span: Some(syntax_tree.span()),
            });
        }

        resolution => {
            resolver.pop_higher_ranked_lifetimes();

            handler.receive(Diagnostic::UnexpectedTypeEqualityPredicate(
                UnexpectedTypeEqualityPredicate {
                    invalid_lhs_type_span: lhs.span(),
                    found_type: resolution,
                },
            ));
        }
    }
}

#[allow(clippy::too_many_lines)]
fn create_outlives_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::SymbolID>,
    syntax_tree: &pernixc_syntax::predicate::LifetimeOutlives,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) {
    let Some(operand) = syntax_tree.operand().and_then(|x| x.identifier())
    else {
        return;
    };

    let mut bounds = Vec::new();

    for bound_syn in syntax_tree.bounds() {
        bounds.push(
            Resolver::builder()
                .tracked_engine(engine)
                .handler(handler)
                .observer(occurrences)
                .extra_namespace(extra_namespace)
                .referring_site(global_id)
                .build()
                .resolve_lifetime(&bound_syn),
        );
    }

    let Some(operand) =
        extra_namespace.get_lifetime(operand.kind.0.as_ref()).cloned()
    else {
        handler.receive(
            pernixc_resolution::diagnostic::LifetimeParameterNotFound {
                referred_span: operand.span,
                name: operand.kind.0,
                referring_site: global_id,
            },
        );

        return;
    };

    for bound in bounds {
        where_clause.push(where_clause::Predicate {
            predicate: predicate::Predicate::LifetimeOutlives(
                predicate::Outlives { operand: operand.clone(), bound },
            ),
            span: Some(syntax_tree.span()),
        });
    }
}

async fn create_type_bound_predicates(
    engine: &TrackedEngine,
    global_id: Global<pernixc_symbol::SymbolID>,
    syntax_tree: &pernixc_syntax::predicate::Type,
    extra_namespace: &ExtraNamespace,
    where_clause: &mut Vec<where_clause::Predicate>,
    occurrences: &mut Occurrences,
    handler: &Storage<Diagnostic>,
) {
    let Some(ty_syn) = syntax_tree.r#type() else { return };

    let mut resolver = Resolver::builder()
        .tracked_engine(engine)
        .handler(handler)
        .referring_site(global_id)
        .observer(occurrences)
        .extra_namespace(extra_namespace)
        .build();

    resolver.push_higher_ranked_lifetimes(
        syntax_tree.higher_ranked_lifetimes().as_ref(),
    );

    let ty = resolver.resolve_type(&ty_syn).await;

    create_type_bound_predicates_internal(
        engine,
        &ty,
        &ty_syn.span(),
        syntax_tree.bounds(),
        &mut resolver,
        where_clause,
        handler,
    )
    .await;

    resolver.pop_higher_ranked_lifetimes();
}

#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
async fn create_type_bound_predicates_internal(
    engine: &TrackedEngine,
    ty: &Type,
    type_span: &RelativeSpan,
    bounds: impl IntoIterator<Item = pernixc_syntax::predicate::TypeBound>,
    resolver: &mut Resolver<'_, '_>,
    where_clause: &mut Vec<where_clause::Predicate>,
    handler: &Storage<Diagnostic>,
) {
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

                resolver.push_higher_ranked_lifetimes(
                    qualified_identifier_bound
                        .higher_ranked_lifetimes()
                        .as_ref(),
                );

                let resolution = match resolver
                    .resolve_type_bound(&qualified_identifier, ty)
                    .await
                {
                    Ok(resolution) => resolution,
                    Err(pernixc_resolution::Error::Abort) => {
                        resolver.pop_higher_ranked_lifetimes();
                        continue;
                    }
                };

                resolver.pop_higher_ranked_lifetimes();

                let preidcate = match resolution {
                    Resolution::GenericSymbol(symbol)
                        if {
                            matches!(
                                engine.get_kind(symbol.id()).await,
                                Kind::Marker
                            )
                        } =>
                    {
                        if qualified_identifier_bound.not_keyword().is_none() {
                            predicate::Predicate::PositiveMarker(
                                PositiveMarker::from_symbol(symbol),
                            )
                        } else {
                            predicate::Predicate::NegativeMarker(
                                NegativeMarker::from_symbol(symbol),
                            )
                        }
                    }

                    found_resolution => {
                        handler.receive(
                            Diagnostic::UnexpectedSymbolInPredicate(
                                UnexpectedSymbolInPredicate {
                                    predicate_kind: PredicateKind::TypeBound,
                                    found: found_resolution,
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
                let bound = resolver.resolve_lifetime(&lifetime);

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
}

impl build::Build for pernixc_semantic_element::where_clause::Key {
    type Diagnostic = diagnostic::Diagnostic;

    #[allow(clippy::too_many_lines)]
    async fn execute(engine: &TrackedEngine, key: &Self) -> Output<Self> {
        let kind = engine.get_kind(key.symbol_id).await;
        let where_clause_syntax_tree =
            engine.get_where_clause_syntax(key.symbol_id).await;

        let mut where_clause = Vec::default();

        let mut occurrences = Occurrences::default();
        let storage = Storage::<Diagnostic>::new();

        let extra_namespace =
            engine.get_generic_parameter_namespace(key.symbol_id).await;

        for predicate in where_clause_syntax_tree
            .into_iter()
            .flat_map(|x| x.predicates().collect::<Vec<_>>())
            .filter_map(|x| x.into_line().ok())
        {
            match predicate {
                pernixc_syntax::predicate::Predicate::Type(ty_bound) => {
                    create_type_bound_predicates(
                        engine,
                        key.symbol_id,
                        &ty_bound,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await;
                }

                pernixc_syntax::predicate::Predicate::TraitTypeEquality(
                    trait_type_equality,
                ) => {
                    create_trait_member_predicates(
                        engine,
                        key.symbol_id,
                        &trait_type_equality,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    )
                    .await;
                }

                pernixc_syntax::predicate::Predicate::LifetimeOutlives(
                    lifetime_outlives,
                ) => {
                    create_outlives_predicates(
                        engine,
                        key.symbol_id,
                        &lifetime_outlives,
                        &extra_namespace,
                        &mut where_clause,
                        &mut occurrences,
                        &storage,
                    );
                }
            }
        }

        {
            if kind.has_generic_parameters() {
                let generic_parameters_syn =
                    engine.get_generic_parameters_syntax(key.symbol_id).await;

                let generic_parameters =
                    engine.get_generic_parameters(key.symbol_id).await;

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
                        .get_parameter_id_by_name(identifier.kind.0.as_ref())
                    else {
                        continue;
                    };

                    let mut resolver = Resolver::builder()
                        .tracked_engine(engine)
                        .handler(&storage)
                        .observer(&mut occurrences)
                        .extra_namespace(&extra_namespace)
                        .referring_site(key.symbol_id)
                        .build();

                    create_type_bound_predicates_internal(
                        engine,
                        &Type::Parameter(TypeParameterID::new(
                            key.symbol_id,
                            id,
                        )),
                        &identifier.span,
                        bounds.bounds(),
                        &mut resolver,
                        &mut where_clause,
                        &storage,
                    )
                    .await;
                }
            }
        }

        Output {
            item: engine.intern_unsized(where_clause),
            diagnostics: engine.intern_unsized(storage.into_vec()),
            occurrences: engine.intern(occurrences),
        }
    }
}

build::register_build!(pernixc_semantic_element::where_clause::Key);
