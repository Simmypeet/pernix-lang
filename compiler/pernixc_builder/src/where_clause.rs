//! Contains the builder for the where clause.

use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use diagnostic::{
    ForallLifetimeIsNotAllowedInOutlivesPredicate,
    HigherRankedLifetimeRedefinition, PredicateKind,
    UnexpectedSymbolInPredicate, UnexpectedTypeEqualityPredicate,
};
use pernixc_handler::Handler;
use pernixc_resolution::{
    diagnostic::LifetimeParameterNotFound,
    qualified_identifier::{Generic, Resolution},
    Config, Ext, ExtraNamespace, GetGenericParameterNamespaceExt as _,
};
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, ConnectedList, QualifiedIdentifierRoot,
};
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_term::{
    forall_lifetime::{
        self, ForallLifetime, ForallLifetimeID, ForallLifetimes,
    },
    generic_arguments::GenericArguments,
    generic_parameter::{GenericParameters, TypeParameterID},
    lifetime::Lifetime,
    predicate::{
        self, Compatible, NegativeMarker, PositiveMarker, PositiveTrait,
        Predicate,
    },
    r#type::{TraitMember, Type},
    visitor::RecursiveIterator,
    where_clause::{self, WhereClause},
    Default, MemberSymbol,
};

pub mod diagnostic;

use crate::{
    builder::Builder,
    occurrences::{self, Occurrences},
};

fn create_forall_lifetimes(
    table: &Table,
    global_id: GlobalID,
    namespace: &mut HashMap<String, Lifetime<Default>>,
    syntax_tree: &syntax_tree::predicate::HigherRankedLifetimes,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for syn in syntax_tree
        .lifetime_parameters
        .connected_list
        .iter()
        .flat_map(ConnectedList::elements)
    {
        match namespace.entry(syn.identifier.span.str().to_owned()) {
            Entry::Vacant(entry) => {
                let forall = table
                    .query::<ForallLifetimes>(global_id)
                    .unwrap()
                    .insert(ForallLifetime::Named(forall_lifetime::Named {
                        name: syn.identifier.span.str().to_owned(),
                        span: Some(syn.identifier.span.clone()),
                    }));

                entry.insert(Lifetime::Forall(ForallLifetimeID {
                    parent: global_id,
                    id: forall,
                }));
            }
            Entry::Occupied(_) => {
                handler.receive(Box::new(HigherRankedLifetimeRedefinition {
                    redefinition_span: syn.span(),
                }));
            }
        }
    }
}

fn create_trait_member_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::TraitTypeEquality,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let with_forall_lifetime =
        syntax_tree.higher_ranked_lifetimes.as_ref().map(|x| {
            let mut extra_namespace = extra_namespace.clone();
            create_forall_lifetimes(
                table,
                global_id,
                &mut extra_namespace.lifetimes,
                x,
                handler,
            );

            extra_namespace
        });

    let extra_namespace =
        with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

    let mut observer = occurrences::Observer;
    let mut config = Config {
        elided_lifetime_provider: None,
        elided_type_provider: None,
        elided_constant_provider: None,
        observer: Some(&mut observer),
        extra_namespace: Some(extra_namespace),
    };

    let ty = table.resolve_type(
        &syntax_tree.lhs_type,
        global_id,
        config.reborrow(),
        handler,
    );

    match ty {
        // trait type
        Type::TraitMember(TraitMember(MemberSymbol {
            id,
            member_generic_arguments,
            parent_generic_arguments,
        })) if *table.get::<SymbolKind>(id) == SymbolKind::TraitType => {
            let resolve_ty = table.resolve_type(
                &syntax_tree.rhs_type,
                global_id,
                config,
                handler,
            );

            where_clause.predicates.push(where_clause::Predicate {
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
            handler.receive(Box::new(UnexpectedTypeEqualityPredicate {
                invalid_lhs_type_span: syntax_tree.lhs_type.span(),
                found_type: resolution,
            }));
        }
    }
}

fn create_trait_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::Trait,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for trait_predicate in syntax_tree.bounds.elements() {
        let with_forall_lifetime =
            trait_predicate.higher_ranked_lifetimes.as_ref().map(|x| {
                let mut extra_namespace = extra_namespace.clone();
                create_forall_lifetimes(
                    table,
                    global_id,
                    &mut extra_namespace.lifetimes,
                    x,
                    handler,
                );

                extra_namespace
            });

        let extra_namespace =
            with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

        let Ok(resolution) = table.resolve_qualified_identifier(
            &trait_predicate.qualified_identifier,
            global_id,
            Config {
                elided_lifetime_provider: None,
                elided_type_provider: None,
                elided_constant_provider: None,
                observer: Some(&mut occurrences::Observer),
                extra_namespace: Some(extra_namespace),
            },
            handler,
        ) else {
            continue;
        };

        match resolution {
            Resolution::Generic(Generic { id, generic_arguments })
                if *table.get::<SymbolKind>(id) == SymbolKind::Trait =>
            {
                let predicate = if trait_predicate.not_keyword.is_none() {
                    predicate::Predicate::PositiveTrait(
                        predicate::PositiveTrait {
                            is_const: trait_predicate.const_keyword.is_some(),
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

                where_clause.predicates.push(where_clause::Predicate {
                    predicate,
                    span: Some(trait_predicate.span()),
                });
            }

            resolution => {
                handler.receive(Box::new(UnexpectedSymbolInPredicate {
                    predicate_kind: PredicateKind::Trait,
                    found_id: resolution.global_id(),
                    qualified_identifier_span: trait_predicate
                        .qualified_identifier
                        .span(),
                }));
            }
        }
    }
}

#[allow(clippy::too_many_lines)]
fn create_outlives_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::LifetimeOutlives,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let mut bounds = Vec::new();

    let mut observer = occurrences::Observer;

    let mut config = Config {
        elided_lifetime_provider: None,
        elided_type_provider: None,
        elided_constant_provider: None,
        observer: Some(&mut observer),
        extra_namespace: Some(extra_namespace),
    };

    for bound_syn in syntax_tree.bounds.elements() {
        bounds.push(table.resolve_lifetime(
            bound_syn,
            global_id,
            config.reborrow(),
            handler,
        ));
    }

    let Some(lifetime_parameter) = extra_namespace
        .lifetimes
        .get(syntax_tree.operand.identifier.span.str())
        .copied()
    else {
        handler.receive(Box::new(LifetimeParameterNotFound {
            referred_span: syntax_tree.operand.identifier.span.clone(),
            referring_site: global_id,
        }));
        return;
    };

    for bound in bounds.iter().copied() {
        where_clause.predicates.push(where_clause::Predicate {
            predicate: predicate::Predicate::LifetimeOutlives(
                predicate::Outlives { operand: lifetime_parameter, bound },
            ),
            span: Some(syntax_tree.span()),
        });
    }
}

fn create_marker_predicate(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::Marker,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for marker_bound in syntax_tree.bounds.elements() {
        let with_forall_lifetime =
            marker_bound.higher_ranked_lifetimes.as_ref().map(|x| {
                let mut extra_namespace = extra_namespace.clone();
                create_forall_lifetimes(
                    table,
                    global_id,
                    &mut extra_namespace.lifetimes,
                    x,
                    handler,
                );

                extra_namespace
            });

        let extra_namespace =
            with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

        let Ok(resolution) = table.resolve_qualified_identifier(
            &marker_bound.qualified_identifier,
            global_id,
            Config {
                elided_lifetime_provider: None,
                elided_type_provider: None,
                elided_constant_provider: None,
                observer: Some(&mut occurrences::Observer),
                extra_namespace: Some(extra_namespace),
            },
            handler,
        ) else {
            continue;
        };

        match resolution {
            Resolution::Generic(Generic { id, generic_arguments })
                if *table.get::<SymbolKind>(id) == SymbolKind::Marker =>
            {
                let predicate = if marker_bound.not_keyword.is_none() {
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

                where_clause.predicates.push(where_clause::Predicate {
                    predicate,
                    span: Some(marker_bound.span()),
                });
            }

            resolution => {
                handler.receive(Box::new(UnexpectedSymbolInPredicate {
                    predicate_kind: PredicateKind::Marker,
                    found_id: resolution.global_id(),
                    qualified_identifier_span: marker_bound
                        .qualified_identifier
                        .span(),
                }));
            }
        }
    }
}

fn create_type_bound_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::Type,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let with_forall_lifetime =
        syntax_tree.higher_ranked_lifetimes.as_ref().map(|x| {
            let mut extra_namespace = extra_namespace.clone();
            create_forall_lifetimes(
                table,
                global_id,
                &mut extra_namespace.lifetimes,
                x,
                handler,
            );

            extra_namespace
        });

    let extra_namespace =
        with_forall_lifetime.as_ref().unwrap_or(extra_namespace);

    let mut observer = occurrences::Observer;
    let mut config = Config {
        elided_lifetime_provider: None,
        elided_type_provider: None,
        elided_constant_provider: None,
        observer: Some(&mut observer),
        extra_namespace: Some(extra_namespace),
    };

    let ty = table.resolve_type(
        &syntax_tree.r#type,
        global_id,
        config.reborrow(),
        handler,
    );

    create_type_bound_predicates_internal(
        table,
        &ty,
        &syntax_tree.r#type.span(),
        global_id,
        syntax_tree.bounds.elements(),
        config,
        where_clause,
        handler,
    );
}

#[allow(clippy::too_many_lines, clippy::too_many_arguments)]
fn create_type_bound_predicates_internal<'a>(
    table: &Table,
    ty: &Type<Default>,
    type_span: &Span,
    global_id: GlobalID,
    bounds: impl IntoIterator<Item = &'a syntax_tree::predicate::TypeBound>,
    mut config: Config<Default>,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for bound in bounds {
        match bound {
            syntax_tree::predicate::TypeBound::QualifiedIdentifier(
                qualified_identifier_bound,
            ) => {
                let new_extra_namespace = if let Some(hlt) =
                    &qualified_identifier_bound.higher_ranked_lifetimes
                {
                    let mut extra_namespace =
                        config.extra_namespace.cloned().unwrap();

                    create_forall_lifetimes(
                        table,
                        global_id,
                        &mut extra_namespace.lifetimes,
                        hlt,
                        handler,
                    );

                    Some(extra_namespace)
                } else {
                    None
                };

                let extra_namespace = new_extra_namespace
                    .as_ref()
                    .unwrap_or(config.extra_namespace.as_ref().unwrap());

                let Some(resolution) = table.resolve_implementation_path(
                    &qualified_identifier_bound.qualified_identifier,
                    global_id,
                    handler,
                ) else {
                    continue;
                };

                let symbol_kind = *table.get::<SymbolKind>(resolution);
                match symbol_kind {
                    SymbolKind::Trait | SymbolKind::Marker => {}
                    _ => continue,
                };

                let (generic_arguments, span) = qualified_identifier_bound
                    .qualified_identifier
                    .rest
                    .last()
                    .map_or_else(
                        || match &qualified_identifier_bound
                            .qualified_identifier
                            .root
                        {
                            QualifiedIdentifierRoot::Target(x)
                            | QualifiedIdentifierRoot::This(x) => {
                                (None, x.span())
                            }
                            QualifiedIdentifierRoot::GenericIdentifier(
                                generic_identifier,
                            ) => {
                                let a = generic_identifier
                                    .generic_arguments
                                    .as_ref();
                                (
                                    a,
                                    a.map_or_else(
                                        || generic_identifier.identifier.span(),
                                        SourceElement::span,
                                    ),
                                )
                            }
                        },
                        |x| {
                            let a = x.1.generic_arguments.as_ref();
                            (
                                a,
                                a.map_or_else(
                                    || x.1.identifier.span(),
                                    SourceElement::span,
                                ),
                            )
                        },
                    );

                let mut observer = occurrences::Observer;

                let mut config = Config {
                    elided_lifetime_provider: None,
                    elided_type_provider: None,
                    elided_constant_provider: None,
                    observer: Some(&mut observer),
                    extra_namespace: Some(extra_namespace),
                };

                let mut generic_arguments = generic_arguments.map_or_else(
                    GenericArguments::default,
                    |x| {
                        table.resolve_generic_arguments(
                            x,
                            global_id,
                            config.reborrow(),
                            handler,
                        )
                    },
                );

                generic_arguments.types.insert(0, ty.clone());

                let Ok((generic_arguments, diagnostics)) = table
                    .verify_generic_arguments_for(
                        generic_arguments,
                        resolution,
                        span,
                        config,
                    )
                else {
                    continue;
                };

                // add to the occurrences
                table.get::<Occurrences>(global_id).write().resolutions.push((
                    Resolution::Generic(Generic {
                        id: resolution,
                        generic_arguments: generic_arguments.clone(),
                    }),
                    qualified_identifier_bound.span(),
                ));

                for diagnostic in diagnostics {
                    handler.receive(diagnostic);
                }

                let pred = match symbol_kind {
                    SymbolKind::Trait => {
                        if qualified_identifier_bound.not_keyword.is_none() {
                            where_clause::Predicate {
                                predicate: Predicate::PositiveTrait(
                                    PositiveTrait {
                                        trait_id: resolution,
                                        is_const: qualified_identifier_bound
                                            .const_keyword
                                            .is_some(),
                                        generic_arguments,
                                    },
                                ),
                                span: Some(qualified_identifier_bound.span()),
                            }
                        } else {
                            where_clause::Predicate {
                                predicate: Predicate::NegativeTrait(
                                    predicate::NegativeTrait {
                                        trait_id: resolution,
                                        generic_arguments,
                                    },
                                ),
                                span: Some(qualified_identifier_bound.span()),
                            }
                        }
                    }
                    SymbolKind::Marker => {
                        if qualified_identifier_bound.not_keyword.is_none() {
                            where_clause::Predicate {
                                predicate: Predicate::PositiveMarker(
                                    PositiveMarker {
                                        marker_id: resolution,
                                        generic_arguments,
                                    },
                                ),
                                span: Some(qualified_identifier_bound.span()),
                            }
                        } else {
                            where_clause::Predicate {
                                predicate: Predicate::NegativeMarker(
                                    NegativeMarker {
                                        marker_id: resolution,
                                        generic_arguments,
                                    },
                                ),
                                span: Some(qualified_identifier_bound.span()),
                            }
                        }
                    }
                    _ => unreachable!(),
                };

                where_clause.predicates.push(pred);
            }
            syntax_tree::predicate::TypeBound::Const(syn) => {
                where_clause.predicates.push(where_clause::Predicate {
                    predicate: predicate::Predicate::ConstantType(
                        predicate::ConstantType(ty.clone()),
                    ),
                    span: Some(syn.span()),
                });
            }
            syntax_tree::predicate::TypeBound::Tuple(syn) => {
                where_clause.predicates.push(where_clause::Predicate {
                    predicate: predicate::Predicate::TupleType(
                        predicate::Tuple(ty.clone()),
                    ),
                    span: Some(syn.span()),
                });
            }
            syntax_tree::predicate::TypeBound::Outlives(lifetime) => {
                let bound = table.resolve_lifetime(
                    lifetime,
                    global_id,
                    config.reborrow(),
                    handler,
                );

                let forall_lts_in_ty = RecursiveIterator::new(ty)
                    .filter_map(|x| {
                        x.0.as_lifetime().and_then(|x| x.as_forall().copied())
                    })
                    .collect::<Vec<_>>();

                if !forall_lts_in_ty.is_empty() {
                    handler.receive(Box::new(
                        ForallLifetimeIsNotAllowedInOutlivesPredicate {
                            forall_lifetime_span: type_span.clone(),
                            forall_lifetimes: forall_lts_in_ty,
                        },
                    ));
                    continue;
                }

                if let Lifetime::Forall(forall_lt) = bound {
                    handler.receive(Box::new(
                        ForallLifetimeIsNotAllowedInOutlivesPredicate {
                            forall_lifetime_span: lifetime.span(),
                            forall_lifetimes: vec![forall_lt],
                        },
                    ));
                    continue;
                }

                where_clause.predicates.push(where_clause::Predicate {
                    predicate: predicate::Predicate::TypeOutlives(
                        predicate::Outlives { operand: ty.clone(), bound },
                    ),
                    span: Some(lifetime.span()),
                });
            }
        }
    }
}

impl query::Builder<WhereClause> for Builder {
    #[allow(clippy::too_many_lines)]
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<WhereClause>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);

        if !symbol_kind.has_where_clause() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            WhereClause::component_name(),
        );

        let where_clause_syntax_tree =
            table.get::<syntax_tree_component::WhereClause>(global_id);

        let mut where_clause = WhereClause::default();
        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

        if let Some(where_clause_syntax_tree) =
            where_clause_syntax_tree.0.as_ref()
        {
            for predicate in where_clause_syntax_tree
                .predicates
                .iter()
                .filter_map(|x| x.as_option())
            {
                match predicate {
                    syntax_tree::predicate::Predicate::Type(ty_bound) => {
                        create_type_bound_predicates(
                            table,
                            global_id,
                            &extra_namespace,
                            ty_bound,
                            &mut where_clause,
                            handler,
                        );
                    }

                    syntax_tree::predicate::Predicate::TraitTypeEquality(
                        trait_type_equality,
                    ) => {
                        create_trait_member_predicates(
                            table,
                            global_id,
                            &extra_namespace,
                            trait_type_equality,
                            &mut where_clause,
                            handler,
                        );
                    }

                    syntax_tree::predicate::Predicate::Trait(tr) => {
                        create_trait_predicates(
                            table,
                            global_id,
                            &extra_namespace,
                            tr,
                            &mut where_clause,
                            handler,
                        );
                    }

                    syntax_tree::predicate::Predicate::LifetimeOutlives(
                        lifetime_outlives,
                    ) => {
                        create_outlives_predicates(
                            table,
                            global_id,
                            &extra_namespace,
                            lifetime_outlives,
                            &mut where_clause,
                            handler,
                        );
                    }

                    syntax_tree::predicate::Predicate::Marker(marker) => {
                        create_marker_predicate(
                            table,
                            global_id,
                            &extra_namespace,
                            marker,
                            &mut where_clause,
                            handler,
                        );
                    }
                }
            }
        }

        'out: {
            if symbol_kind.has_generic_parameters() {
                let generic_parameters_syn =
                    table.get::<syntax_tree_component::GenericParameters>(
                        global_id,
                    );

                let Ok(generic_parameters) =
                    table.query::<GenericParameters>(global_id)
                else {
                    break 'out;
                };

                for (ty_param, bounds) in generic_parameters_syn
                    .iter()
                    .flat_map(|x| {
                        x.connected_list
                            .iter()
                            .flat_map(ConnectedList::elements)
                    })
                    .filter_map(|x| x.as_type())
                    .filter_map(|x| x.bounds.as_ref().map(|y| (x, y)))
                {
                    let Some(id) = generic_parameters
                        .type_parameter_ids_by_name()
                        .get(ty_param.identifier.span.str())
                    else {
                        continue;
                    };

                    create_type_bound_predicates_internal(
                        table,
                        &Type::Parameter(TypeParameterID::new(global_id, *id)),
                        &ty_param.identifier.span,
                        global_id,
                        bounds.bounds.elements(),
                        Config {
                            elided_lifetime_provider: None,
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(&mut occurrences::Observer),
                            extra_namespace: Some(&extra_namespace),
                        },
                        &mut where_clause,
                        handler,
                    );
                }
            }
        }

        Some(Arc::new(where_clause))
    }
}

#[cfg(test)]
mod test;
