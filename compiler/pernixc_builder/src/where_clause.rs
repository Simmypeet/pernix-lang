//! Contains the builder for the where clause.

use std::{
    collections::{hash_map::Entry, HashMap},
    sync::Arc,
};

use diagnostic::{
    HigherRankedLifetimeRedefinition, PredicateKind,
    UnexpectedSymbolInPredicate,
};
use pernixc_handler::Handler;
use pernixc_resolution::{
    diagnostic::LifetimeParameterNotFound,
    qualified_identifier::{Generic, MemberGeneric, Resolution},
    Config, Ext, ExtraNamespace, GetGenericParameterNamespaceExt as _,
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{self, ConnectedList};
use pernixc_table::{
    component::{syntax_tree as syntax_tree_component, Derived, SymbolKind},
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_term::{
    forall_lifetime::{self, ForallLifetimeID},
    lifetime::Lifetime,
    predicate::{
        self, Compatible, ConstantType, NegativeMarker, PositiveMarker,
    },
    r#type::TraitMember,
    where_clause::{self, WhereClause},
    Default, MemberSymbol,
};

pub mod diagnostic;

use crate::{builder::Builder, occurrences};

fn create_forall_lifetimes(
    table: &Table,
    global_id: GlobalID,
    namespace: &mut HashMap<String, Lifetime<Default>>,
    syntax_tree: &syntax_tree::predicate::HigherRankedLifetimes,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for syn in syntax_tree
        .lifetime_parameters()
        .connected_list()
        .iter()
        .flat_map(ConnectedList::elements)
    {
        match namespace.entry(syn.identifier().span.str().to_owned()) {
            Entry::Vacant(entry) => {
                let forall = table
                    .query::<forall_lifetime::Map>(global_id)
                    .unwrap()
                    .insert(forall_lifetime::ForallLifetime::Named(
                        forall_lifetime::Named {
                            name: syn.identifier().span.str().to_owned(),
                            span: Some(syn.identifier().span.clone()),
                        },
                    ));

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

#[allow(clippy::too_many_lines)]
fn create_tuple_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::Tuple,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for tuple in syntax_tree.operands().elements() {
        let with_forall_lifetime =
            tuple.higher_ranked_lifetimes().as_ref().map(|x| {
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

        let resolve_type = table.resolve_type(
            tuple.r#type(),
            global_id,
            Config {
                elided_lifetime_provider: None,
                elided_type_provider: None,
                elided_constant_provider: None,
                observer: Some(&mut occurrences::Observer),
                extra_namespace: Some(extra_namespace),
            },
            handler,
        );

        where_clause.predicates.push(where_clause::Predicate {
            predicate: predicate::Predicate::TupleType(predicate::Tuple(
                resolve_type,
            )),
            span: Some(syntax_tree.span()),
        });
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
        syntax_tree.higher_ranked_lifetimes().as_ref().map(|x| {
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

    let Ok(resolution) = table.resolve_qualified_identifier(
        syntax_tree.qualified_identifier(),
        global_id,
        config.reborrow(),
        handler,
    ) else {
        return;
    };

    match resolution {
        // trait type
        Resolution::MemberGeneric(MemberGeneric {
            id,
            parent_generic_arguments,
            member_generic_arguments,
        }) if *table.get::<SymbolKind>(id) == SymbolKind::TraitType => {
            let resolve_ty = table.resolve_type(
                syntax_tree.r#type(),
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
            handler.receive(Box::new(UnexpectedSymbolInPredicate {
                predicate_kind: PredicateKind::TraitTypeEquality,
                found_id: resolution.global_id(),
                qualified_identifier_span: syntax_tree
                    .qualified_identifier()
                    .span(),
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
    for trait_predicate in syntax_tree.bounds().elements() {
        let with_forall_lifetime =
            trait_predicate.higher_ranked_lifetimes().as_ref().map(|x| {
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
            trait_predicate.qualified_identifier(),
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
                let predicate = if trait_predicate.negation().is_none() {
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
                        .qualified_identifier()
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
    syntax_tree: &syntax_tree::predicate::Outlives,
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

    for bound_syn in syntax_tree.bounds().elements() {
        bounds.push(table.resolve_lifetime(
            bound_syn,
            global_id,
            config.reborrow(),
            handler,
        ));
    }

    match syntax_tree.operand() {
        syntax_tree::predicate::OutlivesOperand::LifetimeParameter(
            lt_parameter,
        ) => {
            let Some(lifetime_parameter) = extra_namespace
                .lifetimes
                .get(lt_parameter.identifier().span.str())
                .copied()
            else {
                handler.receive(Box::new(LifetimeParameterNotFound {
                    referred_span: lt_parameter.identifier().span.clone(),
                    referring_site: global_id,
                }));
                return;
            };

            for bound in bounds.iter().copied() {
                where_clause.predicates.push(where_clause::Predicate {
                    predicate: predicate::Predicate::LifetimeOutlives(
                        predicate::Outlives {
                            operand: lifetime_parameter,
                            bound,
                        },
                    ),
                    span: Some(syntax_tree.span()),
                });
            }
        }
        syntax_tree::predicate::OutlivesOperand::Type(ty) => {
            let ty =
                table.resolve_type(ty, global_id, config.reborrow(), handler);

            for bound in bounds.iter().copied() {
                where_clause.predicates.push(where_clause::Predicate {
                    predicate: predicate::Predicate::TypeOutlives(
                        predicate::Outlives { operand: ty.clone(), bound },
                    ),
                    span: Some(syntax_tree.span()),
                });
            }
        }
    }
}

fn create_constant_type_predicates(
    table: &Table,
    global_id: GlobalID,
    extra_namespace: &ExtraNamespace<Default>,
    syntax_tree: &syntax_tree::predicate::ConstantType,
    where_clause: &mut WhereClause,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    for bound in syntax_tree.bounds().elements() {
        let with_forall_lifetime =
            bound.higher_ranked_lifetimes().as_ref().map(|x| {
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

        let ty = table.resolve_type(
            bound.r#type(),
            global_id,
            Config {
                elided_lifetime_provider: None,
                elided_type_provider: None,
                elided_constant_provider: None,
                observer: Some(&mut occurrences::Observer),
                extra_namespace: Some(extra_namespace),
            },
            handler,
        );

        where_clause.predicates.push(where_clause::Predicate {
            predicate: predicate::Predicate::ConstantType(ConstantType(ty)),
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
    for marker_bound in syntax_tree.bounds().elements() {
        let with_forall_lifetime =
            marker_bound.higher_ranked_lifetimes().as_ref().map(|x| {
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
            marker_bound.qualified_identifier(),
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
                let predicate = if marker_bound.negation().is_none() {
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
                        .qualified_identifier()
                        .span(),
                }));
            }
        }
    }
}

impl query::Builder<WhereClause> for Builder {
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

        let Some(where_clause_syntax_tree) =
            where_clause_syntax_tree.0.as_ref()
        else {
            return Some(Arc::new(WhereClause::default()));
        };

        let mut where_clause = WhereClause::default();
        let extra_namespace = table.get_generic_parameter_namepsace(global_id);

        for predicate in where_clause_syntax_tree.predicate_list().elements() {
            match predicate {
                syntax_tree::predicate::Predicate::Tuple(tuple) => {
                    create_tuple_predicates(
                        table,
                        global_id,
                        &extra_namespace,
                        tuple,
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

                syntax_tree::predicate::Predicate::Outlives(
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

                syntax_tree::predicate::Predicate::ConstantType(
                    constant_type,
                ) => {
                    create_constant_type_predicates(
                        table,
                        global_id,
                        &extra_namespace,
                        constant_type,
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

        Some(Arc::new(where_clause))
    }
}

#[cfg(test)]
mod test;
