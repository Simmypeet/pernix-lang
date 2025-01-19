//! Contains the builder for the `function_signature` component.

use std::{collections::HashSet, sync::Arc};

use pernixc_arena::Arena;
use pernixc_component::{
    function_signature::{FunctionSignature, Parameter},
    implied_predicates::{ImpliedPredicate, ImpliedPredicates},
};
use pernixc_handler::Handler;
use pernixc_resolution::{Config, ElidedTermProvider, Ext};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::ConnectedList;
use pernixc_table::{
    component::{
        syntax_tree as syntax_tree_component, Derived, Parent, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_term::{
    elided_lifetimes::{ElidedLifetime, ElidedLifetimeID, ElidedLifetimes},
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
    where_clause::WhereClause,
    Default, Tuple,
};
use pernixc_type_system::{environment::Environment, AbruptError};

use crate::{
    builder::Builder,
    generic_parameters::Ext as _,
    handle_query_result, handle_term_resolution_result, occurrences,
    type_system::{diagnostic::UndecidablePredicate, TableExt as _},
};

/// The intermediate result of calculating the `function_signature` component.
/// The `implied_predicates` and `ellided_lifetimes` are the byproducts of
/// calculating the `function_signature`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Intermediate {
    function_signature: Arc<FunctionSignature>,
    implied_predicates: Arc<ImpliedPredicates>,
    ellided_lifetimes: Arc<ElidedLifetimes>,
}

impl Derived for Intermediate {
    fn component_name() -> &'static str { "function signature" }
}

struct ElidedLifetimesProvider<'a> {
    global_id: GlobalID,
    elided_lifetimes: &'a mut ElidedLifetimes,
}

impl ElidedTermProvider<Lifetime<Default>> for ElidedLifetimesProvider<'_> {
    fn create(&mut self) -> Lifetime<Default> {
        let id =
            self.elided_lifetimes.elided_lifetimes.insert(ElidedLifetime {});

        Lifetime::Elided(ElidedLifetimeID { parent: self.global_id, id })
    }
}

impl query::Builder<Intermediate> for Builder {
    #[allow(clippy::too_many_lines)]
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<Intermediate>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            FunctionSignature::component_name(),
        );

        let extra_namespace =
            table.get_generic_parameter_namepsace(global_id, handler);
        let mut elided_lifetimes = ElidedLifetimes::default();
        let mut elided_lifetimes_provider = ElidedLifetimesProvider {
            global_id,
            elided_lifetimes: &mut elided_lifetimes,
        };

        let syntax_tree = table
            .get::<syntax_tree_component::FunctionSignature>(global_id)
            .unwrap();

        let parameters = syntax_tree
            .parameters
            .connected_list()
            .iter()
            .flat_map(ConnectedList::elements)
            .map(|syn| {
                (
                    handle_term_resolution_result!(
                        table.resolve_type(
                            syn.r#type(),
                            global_id,
                            Config {
                                elided_lifetime_provider: Some(
                                    &mut elided_lifetimes_provider
                                ),
                                elided_type_provider: None,
                                elided_constant_provider: None,
                                observer: Some(&mut occurrences::Observer),
                                extra_namespace: Some(&extra_namespace),
                            },
                            handler
                        ),
                        handler,
                        Type::Error(pernixc_term::Error)
                    ),
                    syn.span(),
                )
            })
            .collect::<Vec<_>>();

        let return_type = syntax_tree.return_type.as_ref().map(|x| {
            (
                handle_term_resolution_result!(
                    table.resolve_type(
                        x.r#type(),
                        global_id,
                        Config {
                            elided_lifetime_provider: Some(
                                &mut elided_lifetimes_provider
                            ),
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(&mut occurrences::Observer),
                            extra_namespace: Some(&extra_namespace),
                        },
                        handler,
                    ),
                    handler,
                    Type::Error(pernixc_term::Error)
                ),
                x.span(),
            )
        });

        let mut implied_predicates = ImpliedPredicates::default();
        let mut active_premise = table.get_active_premise::<Default>(
            GlobalID::new(
                global_id.target_id,
                table.get::<Parent>(global_id).unwrap().0,
            ),
            handler,
        );

        'out: {
            let where_clause = handle_query_result!(
                table.query::<WhereClause>(global_id),
                handler,
                break 'out,
            );

            for predicate in &where_clause.predicates {
                active_premise.predicates.insert(predicate.predicate.clone());
            }
        }

        for (implied_predicate, declared_span, inst_span) in parameters
            .iter()
            .map(|x| (&x.0, &x.1))
            .chain(return_type.as_ref().map(|x| (&x.0, &x.1)))
            .flat_map(|(x, y)| RecursiveIterator::new(x).map(move |z| (z, y)))
            .filter_map(|((term, _), span)| {
                term.into_type().ok().map(|x| (x, span))
            })
            .flat_map(|(x, span)| match x {
                Type::Symbol(symbol) => {
                    let where_clause = handle_query_result!(
                        table.query::<WhereClause>(symbol.id),
                        handler,
                        return Vec::new(),
                    );

                    where_clause
                        .predicates
                        .iter()
                        .filter_map(|x| match &x.predicate {
                            Predicate::LifetimeOutlives(outlives) => Some((
                                ImpliedPredicate::LifetimeOutlives(
                                    outlives.clone(),
                                ),
                                x.span.clone(),
                                span,
                            )),
                            Predicate::TypeOutlives(outlives) => Some((
                                ImpliedPredicate::TypeOutlives(
                                    outlives.clone(),
                                ),
                                x.span.clone(),
                                span,
                            )),
                            _ => None,
                        })
                        .collect()
                }
                Type::Reference(reference) => {
                    vec![(
                        ImpliedPredicate::TypeOutlives(Outlives::new(
                            (*reference.pointee).clone(),
                            reference.lifetime,
                        )),
                        None,
                        span,
                    )]
                }
                _ => Vec::new(),
            })
        {
            let (environment, _) =
                Environment::new(active_premise.clone(), table);

            let result = match &implied_predicate {
                ImpliedPredicate::LifetimeOutlives(outlives) => {
                    environment.query(outlives)
                }
                ImpliedPredicate::TypeOutlives(outlives) => {
                    environment.query(outlives)
                }
            };

            match result {
                Ok(Some(_)) => {}
                Ok(None) => {
                    active_premise
                        .predicates
                        .insert(implied_predicate.clone().into());
                    implied_predicates
                        .implied_predicates
                        .insert(implied_predicate);
                }
                Err(AbruptError::CyclicDependency(error)) => {
                    handler.receive(Box::new(error));
                }
                Err(AbruptError::Overflow(overflow_error)) => {
                    handler.receive(Box::new(UndecidablePredicate {
                        predicate: implied_predicate.into(),
                        predicate_declaration_span: declared_span,
                        instantiation_span: inst_span.clone(),
                        overflow_error,
                    }));
                }
            }
        }

        Some(Arc::new(Intermediate {
            function_signature: Arc::new({
                let mut parameter_arena = Arena::default();
                let mut parameter_orders = Vec::new();

                for (r#type, span) in parameters {
                    parameter_orders.push(
                        parameter_arena
                            .insert(Parameter { r#type, span: Some(span) }),
                    );
                }

                FunctionSignature {
                    parameters: parameter_arena,
                    parameter_order: parameter_orders,
                    return_type: return_type.map_or_else(
                        || Type::Tuple(Tuple { elements: Vec::new() }),
                        |x| x.0,
                    ),
                }
            }),
            implied_predicates: Arc::new(implied_predicates),
            ellided_lifetimes: Arc::new(elided_lifetimes),
        }))
    }
}

impl query::Builder<FunctionSignature> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<FunctionSignature>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            FunctionSignature::component_name(),
        );

        match table.query::<Intermediate>(global_id) {
            Ok(result) => Some(result.function_signature.clone()),
            Err(query::Error::CyclicDependency(error)) => {
                table.handler().receive(Box::new(error));
                Some(Arc::new(FunctionSignature {
                    parameters: Arena::default(),
                    parameter_order: Vec::new(),
                    return_type: Type::Tuple(Tuple { elements: Vec::new() }),
                }))
            }
            Err(
                query::Error::NoBuilderFound
                | query::Error::SymbolNotFoundOrInvalidComponent,
            ) => None,
        }
    }
}

impl query::Builder<ElidedLifetimes> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<ElidedLifetimes>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            ElidedLifetimes::component_name(),
        );

        match table.query::<Intermediate>(global_id) {
            Ok(result) => Some(result.ellided_lifetimes.clone()),
            Err(query::Error::CyclicDependency(error)) => {
                table.handler().receive(Box::new(error));
                Some(Arc::new(ElidedLifetimes {
                    elided_lifetimes: Arena::default(),
                }))
            }
            Err(
                query::Error::NoBuilderFound
                | query::Error::SymbolNotFoundOrInvalidComponent,
            ) => None,
        }
    }
}

impl query::Builder<ImpliedPredicates> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<ImpliedPredicates>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id)?;
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            ImpliedPredicates::component_name(),
        );

        match table.query::<Intermediate>(global_id) {
            Ok(result) => Some(result.implied_predicates.clone()),
            Err(query::Error::CyclicDependency(error)) => {
                table.handler().receive(Box::new(error));
                Some(Arc::new(ImpliedPredicates {
                    implied_predicates: HashSet::new(),
                }))
            }
            Err(
                query::Error::NoBuilderFound
                | query::Error::SymbolNotFoundOrInvalidComponent,
            ) => None,
        }
    }
}
