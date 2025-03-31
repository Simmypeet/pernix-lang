//! Contains the builder for the `function_signature` component.

use std::{borrow::Cow, collections::HashSet, sync::Arc};

use pernixc_abort::Abort;
use pernixc_arena::{Arena, ID};
use pernixc_component::{
    function_signature::{FunctionSignature, Parameter},
    implied_predicates::{ImpliedPredicate, ImpliedPredicates},
    late_bound::LateBound,
};
use pernixc_handler::Handler;
use pernixc_resolution::{
    Config, ElidedTermProvider, Ext, GetGenericParameterNamespaceExt as _,
};
use pernixc_semantic::{
    component::{
        syntax_tree as syntax_tree_component, Derived, Parent, SymbolKind,
    },
    diagnostic::Diagnostic,
    query, GlobalID, Table,
};
use pernixc_source_file::SourceElement;
use pernixc_syntax::syntax_tree::{
    item::function::ParameterKind, ConnectedList,
};
use pernixc_term::{
    elided_lifetimes::{ElidedLifetime, ElidedLifetimeID, ElidedLifetimes},
    generic_arguments::GenericArguments,
    generic_parameter::{GenericParameters, LifetimeParameter},
    lifetime::Lifetime,
    predicate::{Outlives, Predicate},
    r#type::Type,
    visitor::RecursiveIterator,
    where_clause::WhereClause,
    Default, Tuple,
};
use pernixc_type_system::{
    environment::{Environment, GetActivePremiseExt},
    normalizer, Error,
};

use crate::{builder::Builder, occurrences, type_system::EnvironmentExt as _};

/// The intermediate result of calculating the `function_signature` component.
/// The `implied_predicates` and `ellided_lifetimes` are the byproducts of
/// calculating the `function_signature`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Intermediate {
    function_signature: Arc<FunctionSignature>,
    implied_predicates: Arc<ImpliedPredicates>,
    ellided_lifetimes: Arc<ElidedLifetimes>,
    late_bound: Arc<LateBound>,
}

impl Derived for Intermediate {
    fn component_name() -> &'static str { "function signature" }
}

struct ParametersElidedLifetimeProvider<'a> {
    global_id: GlobalID,
    elided_lifetimes: &'a mut ElidedLifetimes,
}

impl ElidedTermProvider<Lifetime<Default>>
    for ParametersElidedLifetimeProvider<'_>
{
    fn create(&mut self) -> Lifetime<Default> {
        let id =
            self.elided_lifetimes.elided_lifetimes.insert(ElidedLifetime {});

        Lifetime::Elided(ElidedLifetimeID { parent: self.global_id, id })
    }
}

struct ReturnElidedLifetimeProvider {
    lifetime: Lifetime<Default>,
}

impl ElidedTermProvider<Lifetime<Default>> for ReturnElidedLifetimeProvider {
    fn create(&mut self) -> Lifetime<Default> { self.lifetime }
}

struct AllLifetimeParameters {
    // list of lifetimes that will be checked if they are late bound
    lifetimes: HashSet<ID<LifetimeParameter>>,
    current_function_id: GlobalID,
}

impl AllLifetimeParameters {
    // if the lifetime appears in the where clause, it is not late bound
    fn exclude_late_bound(&mut self, predicate: &Predicate<Default>) {
        match predicate {
            Predicate::TraitTypeCompatible(compatible) => {
                self.exclude_late_bound_in_generic_arguments(
                    &compatible.lhs.member_generic_arguments,
                );
                self.exclude_late_bound_in_generic_arguments(
                    &compatible.lhs.parent_generic_arguments,
                );

                self.exclude_late_bound_in_type(&compatible.rhs);
            }
            Predicate::ConstantType(constant_type) => {
                self.exclude_late_bound_in_type(&constant_type.0);
            }
            Predicate::LifetimeOutlives(outlives) => {
                self.exclude_late_bound_in_lifetime(&outlives.bound);
                self.exclude_late_bound_in_lifetime(&outlives.operand);
            }
            Predicate::TypeOutlives(outlives) => {
                self.exclude_late_bound_in_lifetime(&outlives.bound);
                self.exclude_late_bound_in_type(&outlives.operand);
            }
            Predicate::TupleType(tuple) => {
                self.exclude_late_bound_in_type(&tuple.0);
            }
            Predicate::PositiveTrait(positive) => {
                self.exclude_late_bound_in_generic_arguments(
                    &positive.generic_arguments,
                );
            }
            Predicate::NegativeTrait(negative) => {
                self.exclude_late_bound_in_generic_arguments(
                    &negative.generic_arguments,
                );
            }
            Predicate::PositiveMarker(positive) => {
                self.exclude_late_bound_in_generic_arguments(
                    &positive.generic_arguments,
                );
            }
            Predicate::NegativeMarker(negative) => {
                self.exclude_late_bound_in_generic_arguments(
                    &negative.generic_arguments,
                );
            }
        }
    }

    fn exclude_late_bound_in_lifetime(&mut self, lifetime: &Lifetime<Default>) {
        if let Lifetime::Parameter(lifetime) = lifetime {
            if lifetime.parent == self.current_function_id {
                self.lifetimes.remove(&lifetime.id);
            }
        }
    }

    fn exclude_late_bound_in_type(&mut self, ty: &Type<Default>) {
        for lt in RecursiveIterator::new(ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }
    }

    fn exclude_late_bound_in_generic_arguments(
        &mut self,
        generic_arguments: &GenericArguments<Default>,
    ) {
        for lt in generic_arguments
            .lifetimes
            .iter()
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }

        for lt in generic_arguments
            .types
            .iter()
            .flat_map(|x| {
                RecursiveIterator::new(x)
                    .filter_map(|x| x.0.into_lifetime().ok())
            })
            .filter_map(|x| x.as_parameter())
            .filter_map(|x| {
                (x.parent == self.current_function_id).then_some(x.id)
            })
        {
            self.lifetimes.remove(&lt);
        }

        // Constant should not have lifetime parameters (at most 'static).
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
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            FunctionSignature::component_name(),
        );

        let extra_namespace = table.get_generic_parameter_namepsace(global_id);
        let mut elided_lifetimes = ElidedLifetimes::default();
        let mut elided_lifetimes_provider = ParametersElidedLifetimeProvider {
            global_id,
            elided_lifetimes: &mut elided_lifetimes,
        };

        let syntax_tree =
            table.get::<syntax_tree_component::FunctionSignature>(global_id);

        let parameters = syntax_tree
            .parameters
            .connected_list
            .iter()
            .flat_map(ConnectedList::elements)
            .filter_map(ParameterKind::as_regular)
            .map(|syn| {
                (
                    table.resolve_type(
                        &syn.r#type,
                        global_id,
                        Config {
                            elided_lifetime_provider: Some(
                                &mut elided_lifetimes_provider,
                            ),
                            elided_type_provider: None,
                            elided_constant_provider: None,
                            observer: Some(&mut occurrences::Observer),
                            extra_namespace: Some(&extra_namespace),
                        },
                        handler,
                    ),
                    syn.span(),
                )
            })
            .collect::<Vec<_>>();

        let mut return_elided_lifetime_provider = (elided_lifetimes
            .elided_lifetimes
            .len()
            == 1)
            .then(|| ReturnElidedLifetimeProvider {
                lifetime: Lifetime::Elided(ElidedLifetimeID {
                    parent: global_id,
                    id: elided_lifetimes.elided_lifetimes.ids().next().unwrap(),
                }),
            });

        let return_type = syntax_tree.return_type.as_ref().map(|x| {
            (
                table.resolve_type(
                    &x.r#type,
                    global_id,
                    Config {
                        elided_lifetime_provider:
                            return_elided_lifetime_provider
                                .as_mut()
                                .map(|x| x as _),
                        elided_type_provider: None,
                        elided_constant_provider: None,
                        observer: Some(&mut occurrences::Observer),
                        extra_namespace: Some(&extra_namespace),
                    },
                    handler,
                ),
                x.span(),
            )
        });

        let mut implied_predicates = ImpliedPredicates::default();
        let mut active_premise =
            table.get_active_premise::<Default>(GlobalID::new(
                global_id.target_id,
                table.get::<Parent>(global_id).parent.unwrap(),
            ));

        'out: {
            let Ok(where_clause) = table.query::<WhereClause>(global_id) else {
                break 'out;
            };

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
                    let Ok(where_clause) =
                        table.query::<WhereClause>(symbol.id)
                    else {
                        return Vec::new();
                    };

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
            let environment = Environment::new(
                Cow::Borrowed(&active_premise),
                table,
                normalizer::NO_OP,
            );

            let result = match &implied_predicate {
                ImpliedPredicate::LifetimeOutlives(outlives) => {
                    environment.query(outlives)
                }
                ImpliedPredicate::TypeOutlives(outlives) => {
                    environment.query(outlives)
                }
            };

            match result {
                Ok(None) => {
                    active_premise
                        .predicates
                        .insert(implied_predicate.clone().into());
                    implied_predicates
                        .implied_predicates
                        .insert(implied_predicate);
                }
                Err(Error::Overflow(overflow_error)) => {
                    overflow_error.report_as_undecidable_predicate(
                        implied_predicate.into(),
                        declared_span,
                        inst_span.clone(),
                        handler,
                    );
                }

                Err(Error::Abort(Abort)) | Ok(Some(_)) => {}
            }
        }

        let function_signature = {
            let environment = Environment::new(
                Cow::Borrowed(&active_premise),
                table,
                normalizer::NO_OP,
            );

            let mut parameter_arena = Arena::default();
            let mut parameter_orders = Vec::new();

            for (r#type, span) in parameters {
                parameter_orders.push(parameter_arena.insert(Parameter {
                    r#type:
                        environment.simplify_and_check_lifetime_constraints(
                            &r#type, &span, handler,
                        ),
                    span: Some(span),
                }));
            }

            FunctionSignature {
                parameters: parameter_arena,
                parameter_order: parameter_orders,
                return_type: return_type.map_or_else(
                    || Type::Tuple(Tuple { elements: Vec::new() }),
                    |x| {
                        environment.simplify_and_check_lifetime_constraints(
                            &x.0, &x.1, handler,
                        )
                    },
                ),
            }
        };

        let late_bound = 'out: {
            let (Ok(generic_params), Ok(where_clause)) = (
                table.query::<GenericParameters>(global_id),
                table.query::<WhereClause>(global_id),
            ) else {
                break 'out LateBound::default();
            };

            let mut all_lifetime_parameters = AllLifetimeParameters {
                lifetimes: generic_params.lifetimes().ids().collect(),
                current_function_id: global_id,
            };

            for predicate in &where_clause.predicates {
                all_lifetime_parameters
                    .exclude_late_bound(&predicate.predicate);
            }

            LateBound(all_lifetime_parameters.lifetimes)
        };

        Some(Arc::new(Intermediate {
            function_signature: Arc::new(function_signature),
            implied_predicates: Arc::new(implied_predicates),
            ellided_lifetimes: Arc::new(elided_lifetimes),
            late_bound: Arc::new(late_bound),
        }))
    }
}

impl query::Builder<FunctionSignature> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<FunctionSignature>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            FunctionSignature::component_name(),
        );

        Some(table.query::<Intermediate>(global_id).map_or_else(
            |Abort| {
                Arc::new(FunctionSignature {
                    parameters: Arena::default(),
                    parameter_order: Vec::new(),
                    return_type: Type::Error(pernixc_term::Error),
                })
            },
            |x| x.function_signature.clone(),
        ))
    }
}

impl query::Builder<ElidedLifetimes> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<ElidedLifetimes>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            ElidedLifetimes::component_name(),
        );

        Some(table.query::<Intermediate>(global_id).map_or_else(
            |Abort| {
                Arc::new(ElidedLifetimes { elided_lifetimes: Arena::default() })
            },
            |x| x.ellided_lifetimes.clone(),
        ))
    }
}

impl query::Builder<ImpliedPredicates> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<ImpliedPredicates>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope = self.start_building(
            table,
            global_id,
            ImpliedPredicates::component_name(),
        );

        Some(table.query::<Intermediate>(global_id).map_or_else(
            |Abort| {
                Arc::new(ImpliedPredicates {
                    implied_predicates: HashSet::new(),
                })
            },
            |x| x.implied_predicates.clone(),
        ))
    }
}

impl query::Builder<LateBound> for Builder {
    fn build(
        &self,
        global_id: GlobalID,
        table: &Table,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Arc<LateBound>> {
        let symbol_kind = *table.get::<SymbolKind>(global_id);
        if !symbol_kind.has_function_signature() {
            return None;
        }

        let _scope =
            self.start_building(table, global_id, LateBound::component_name());

        Some(table.query::<Intermediate>(global_id).map_or_else(
            |Abort| Arc::new(LateBound::default()),
            |x| x.late_bound.clone(),
        ))
    }
}

#[cfg(test)]
mod test;
