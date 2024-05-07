//! Contains the code for creating where clause predicates for generic symbols.

use std::collections::{hash_map::Entry, HashMap};

use pernixc_base::{diagnostic::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{
    self,
    item::{LifetimePredicateOperand, TuplePredicateOperand},
    ConnectedList,
};

use super::{
    finalize::r#trait, finalizer, occurrences::Occurrences, Finalizer,
};
use crate::{
    arena::ID,
    error::{
        self, ExpectTrait, RedefinedHigherRankedLifetime, TraitMemberExpected,
    },
    semantic::{
        instantiation::Instantiation,
        predicate::{self, Outlives, TraitMemberEquality},
        term::{self, lifetime::Forall},
    },
    symbol::{self, Generic, GenericID, GlobalID, PredicateKind, Trait},
    table::{
        self,
        resolution::{self, MemberGeneric, MemberGenericID, Resolution},
        Element, Table,
    },
};

impl Table<Finalizer> {
    /// Creates where clause predicates for the given generic symbol.
    #[allow(clippy::too_many_lines)]
    fn create_trait_member_predicate<T: Generic + Element>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::TraitMemberEqualityPredicate,
        mut config: resolution::Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let Ok(resolution) = self.resolve(
            syntax_tree.qualified_identifier(),
            generic_id.into(),
            config.reborrow(),
            handler,
        ) else {
            return;
        };

        match resolution {
            // trait type
            Resolution::MemberGeneric(MemberGeneric {
                id: MemberGenericID::TraitType(trait_ty_id),
                parent_generic_arguments,
                generic_arguments,
            }) => {
                let Ok(resolve_ty) = self.resolve_type(
                    syntax_tree.r#type(),
                    generic_id.into(),
                    config,
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol =
                    T::get_arena(self).get(generic_id).unwrap().write();
                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::TraitTypeEquality(
                            TraitMemberEquality {
                                trait_member: term::r#type::TraitMember {
                                    member_generic_arguments: generic_arguments,
                                    id: trait_ty_id,
                                    parent_generic_arguments,
                                },

                                equivalent: resolve_ty,
                            },
                        ),
                        kind: PredicateKind::Explicit(Some(syntax_tree.span())),
                    },
                );
            }

            _ => {
                handler.receive(Box::new(TraitMemberExpected {
                    non_trait_member_span: syntax_tree
                        .qualified_identifier()
                        .span(),
                }));
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn create_trait_bound_predicates<
        T: Generic + table::Element + finalizer::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::TraitPredicate,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let (forall_lifetimes_by_name, syntax_trees_by_foall_lifetimes) =
            syntax_tree
                .higher_ranked_lifetime_parameters()
                .as_ref()
                .map_or_else(
                    || (HashMap::default(), HashMap::default()),
                    |x| {
                        let mut forall_lifetimes_by_name = HashMap::new();
                        let mut syntax_trees_by_foall_lifetimes =
                            HashMap::new();

                        for syn in x
                            .lifetime_parameter_list()
                            .iter()
                            .flat_map(ConnectedList::elements)
                        {
                            match forall_lifetimes_by_name
                                .entry(syn.identifier().span.str().to_owned())
                            {
                                Entry::Vacant(entry) => {
                                    let forall = Forall::generate();
                                    entry.insert(forall);
                                    assert!(syntax_trees_by_foall_lifetimes
                                        .insert(forall, syn)
                                        .is_none());
                                }
                                Entry::Occupied(_) => {
                                    handler.receive(Box::new(
                                        RedefinedHigherRankedLifetime {
                                            redefinition_span: syn.span(),
                                        },
                                    ));
                                }
                            }
                        }

                        (
                            forall_lifetimes_by_name,
                            syntax_trees_by_foall_lifetimes,
                        )
                    },
                );

        for qualified_identifier in
            syntax_tree.qualified_identifiers().elements()
        {
            let mut forall_lifetimes_by_name_cloned = HashMap::new();
            let mut syntax_trees_by_foall_lifetimes_cloned = HashMap::new();

            for (name, forall) in &forall_lifetimes_by_name {
                let syn = syntax_trees_by_foall_lifetimes.get(forall).unwrap();
                let new_forall = Forall::generate();

                assert!(
                    forall_lifetimes_by_name_cloned
                        .insert(name.clone(), new_forall)
                        .is_none(),
                    "should have no duplication"
                );
                assert!(
                    syntax_trees_by_foall_lifetimes_cloned
                        .insert(new_forall, syn)
                        .is_none(),
                    "should have no duplication"
                );
            }

            let Ok(resolution) = self.resolve(
                qualified_identifier,
                generic_id.into(),
                resolution::Config {
                    ellided_lifetime_provider: None,
                    ellided_type_provider: None,
                    ellided_constant_provider: None,
                    observer: Some(occurrences),
                    higher_ranked_liftimes: Some(
                        &forall_lifetimes_by_name_cloned,
                    ),
                },
                handler,
            ) else {
                return;
            };

            match resolution {
                Resolution::Generic(resolution::Generic {
                    id: resolution::GenericID::Trait(trait_id),
                    generic_arguments,
                }) => {
                    // make sure the trait is built to have the where clause
                    match self.build_to::<Trait>(
                        trait_id,
                        Some(generic_id.into()),
                        r#trait::WHERE_CLAUSE_STATE,
                        true,
                        handler,
                    ) {
                        Ok(()) => {}
                        Err(_) => {
                            continue;
                        }
                    }

                    let trait_predicate = predicate::Trait {
                        id: trait_id,
                        is_const: syntax_tree.const_keyword().is_some(),
                        generic_arguments: generic_arguments.clone(),
                    };

                    occurrences.add_trait_predicate(
                        trait_predicate.clone(),
                        qualified_identifier.clone(),
                    );

                    T::get_arena(self)
                        .get(generic_id)
                        .unwrap()
                        .write()
                        .generic_declaration_mut()
                        .predicates
                        .push(symbol::Predicate {
                            predicate: predicate::Predicate::Trait(
                                trait_predicate,
                            ),
                            kind: PredicateKind::Explicit(Some(
                                qualified_identifier.span(),
                            )),
                        });

                    let trait_sym = self
                        .representation
                        .traits
                        .get(trait_id)
                        .unwrap()
                        .read();

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    let instantiation = Instantiation::from_generic_arguments(
                        generic_arguments,
                        trait_id.into(),
                        &trait_sym.generic_declaration.parameters,
                    )
                    .unwrap();

                    // create implied by trait bound predicates
                    generic_symbol.generic_declaration_mut().predicates.extend(
                        trait_sym
                            .generic_declaration
                            .predicates
                            .iter()
                            .filter_map(|x| {
                                let mut predicate = x.predicate.clone();
                                predicate.instantiate(&instantiation);

                                if predicate.contains_forall_lifetime() {
                                    return None;
                                }

                                Some(symbol::Predicate {
                                    predicate,
                                    kind: PredicateKind::ImpliedByTraitBound(
                                        Some(qualified_identifier.span()),
                                    ),
                                })
                            }),
                    );
                }

                resolution => handler.receive(Box::new(ExpectTrait {
                    found_id: resolution.global_id(),
                    trait_path: qualified_identifier.span(),
                })),
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn create_lifetime_outlives_predicates<
        T: Generic + table::Element + finalizer::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::LifetimePredicate,
        mut config: resolution::Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let mut bounds = Vec::new();

        for bound_syn in syntax_tree.bounds().elements() {
            let Ok(bound) = self.resolve_lifetime(
                bound_syn,
                generic_id.into(),
                config.reborrow(),
                handler,
            ) else {
                continue;
            };

            bounds.push(bound);
        }

        for operand in syntax_tree.operands().elements() {
            match operand {
                LifetimePredicateOperand::LifetimeParameter(lt_parameter) => {
                    let Ok(lifetime_parameter_id) = self
                        .resolve_lifetime_parameter(
                            lt_parameter.identifier(),
                            generic_id.into(),
                            handler,
                        )
                    else {
                        return;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    for bound in bounds.iter().copied() {
                        generic_symbol
                            .generic_declaration_mut()
                            .predicates
                            .push(symbol::Predicate {
                            predicate: predicate::Predicate::LifetimeOutlives(
                                Outlives {
                                    operand:
                                        term::lifetime::Lifetime::Parameter(
                                            lifetime_parameter_id,
                                        ),
                                    bound,
                                },
                            ),
                            kind: PredicateKind::Explicit(Some(
                                syntax_tree.span(),
                            )),
                        });
                    }
                }
                LifetimePredicateOperand::Type(qualified_identifier) => {
                    let Ok(ty) = self.resolve_type(
                        qualified_identifier,
                        generic_id.into(),
                        config.reborrow(),
                        handler,
                    ) else {
                        return;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    for bound in bounds.iter().copied() {
                        generic_symbol
                            .generic_declaration_mut()
                            .predicates
                            .push(symbol::Predicate {
                                predicate: predicate::Predicate::TypeOutlives(
                                    Outlives { operand: ty.clone(), bound },
                                ),
                                kind: PredicateKind::Explicit(Some(
                                    syntax_tree.span(),
                                )),
                            });
                    }
                }
            }
        }
    }

    // create a constant type predicate
    #[allow(clippy::too_many_lines)]
    fn create_constant_type_predicates<
        T: Generic + table::Element + finalizer::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::ConstantTypePredicate,
        mut config: resolution::Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        for constant_type in syntax_tree.types().elements() {
            let Ok(ty) = self.resolve_type(
                constant_type,
                generic_id.into(),
                config.reborrow(),
                handler,
            ) else {
                continue;
            };

            let mut generic_symbol =
                T::get_arena(self).get(generic_id).unwrap().write();

            generic_symbol.generic_declaration_mut().predicates.push(
                symbol::Predicate {
                    predicate: predicate::Predicate::ConstantType(
                        predicate::ConstantType(ty),
                    ),
                    kind: PredicateKind::Explicit(Some(syntax_tree.span())),
                },
            );
        }
    }

    // create a constant type predicate
    #[allow(clippy::too_many_lines)]
    fn create_tuple_predicates<T: Generic + Element>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::TuplePredicate,
        mut config: resolution::Config,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        for tuple in syntax_tree.operands().elements() {
            match tuple {
                TuplePredicateOperand::Type(ty) => {
                    let Ok(ty) = self.resolve_type(
                        ty,
                        generic_id.into(),
                        config.reborrow(),
                        handler,
                    ) else {
                        continue;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TupleType(
                                predicate::Tuple(ty),
                            ),
                            kind: PredicateKind::Explicit(Some(
                                syntax_tree.span(),
                            )),
                        },
                    );
                }
                TuplePredicateOperand::Constant(expr) => {
                    let Ok(constant) = self.evaluate(
                        expr.expression(),
                        generic_id.into(),
                        handler,
                    ) else {
                        continue;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TupleConstant(
                                predicate::Tuple(constant),
                            ),
                            kind: PredicateKind::Explicit(Some(
                                syntax_tree.span(),
                            )),
                        },
                    );
                }
            }
        }
    }

    /// Creates where clause predicates for the given generic symbol.
    pub(in crate::table::building) fn create_where_clause_predicates<
        T: Generic + table::Element + finalizer::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: Option<&syntax_tree::item::WhereClause>,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let Some(where_clause) = syntax_tree else {
            return;
        };

        for clause in where_clause.predicate_list().elements() {
            match clause {
                syntax_tree::item::Predicate::TraitMember(trait_member) => self
                    .create_trait_member_predicate(
                        generic_id,
                        trait_member,
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(occurrences),
                            higher_ranked_liftimes: None,
                        },
                        handler,
                    ),
                syntax_tree::item::Predicate::Trait(trait_bound) => self
                    .create_trait_bound_predicates(
                        generic_id,
                        trait_bound,
                        occurrences,
                        handler,
                    ),
                syntax_tree::item::Predicate::Lifetime(lifetime_predicates) => {
                    self.create_lifetime_outlives_predicates(
                        generic_id,
                        lifetime_predicates,
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(occurrences),
                            higher_ranked_liftimes: None,
                        },
                        handler,
                    );
                }
                syntax_tree::item::Predicate::ConstantType(constant_types) => {
                    self.create_constant_type_predicates(
                        generic_id,
                        constant_types,
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(occurrences),
                            higher_ranked_liftimes: None,
                        },
                        handler,
                    );
                }
                syntax_tree::item::Predicate::Tuple(tuple_types) => {
                    self.create_tuple_predicates(
                        generic_id,
                        tuple_types,
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(occurrences),
                            higher_ranked_liftimes: None,
                        },
                        handler,
                    );
                }
            }
        }
    }
}
