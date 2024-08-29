//! Contains the code for creating where clause predicates for generic symbols.

use std::collections::{hash_map::Entry, HashMap};

use pernixc_base::{handler::Handler, source_file::SourceElement};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{builder, occurrences::Occurrences};
use crate::{
    arena::ID,
    error::{
        self, ExpectTrait, ExpectTraitMember, HigherRankedLifetimeRedefinition,
        UnexpectedInference,
    },
    symbol::{
        self,
        table::{
            self,
            representation::{
                building::finalizing::{self, Finalizer},
                Element, RwLockContainer,
            },
            resolution::{
                self, MemberGeneric, MemberGenericID, Observer, Resolution,
            },
            Building, Table,
        },
        Generic, GenericID, GlobalID,
    },
    type_system::{
        equality::Equality,
        predicate::{self, Outlives},
        term::{self, lifetime::Forall},
    },
};

impl Table<Building<RwLockContainer, Finalizer>> {
    /// Creates where clause predicates for the given generic symbol.
    #[allow(clippy::too_many_lines)]
    fn create_trait_member_predicates<T: Generic + Element>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::predicate::TraitTypeEquality,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let higher_ranked_lifetimes = syntax_tree
            .higher_ranked_lifetimes()
            .as_ref()
            .map(|x| Self::create_higher_ranked_lifetimes(x, handler));

        let mut basic = builder::Resolution::basic();
        let mut observer = basic.chain(occurrences);

        let mut config = resolution::Config {
            ellided_lifetime_provider: None,
            ellided_type_provider: None,
            ellided_constant_provider: None,
            observer: Some(&mut observer),
            higher_ranked_lifetimes: higher_ranked_lifetimes.as_ref(),
        };

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

                let equality = Equality {
                    lhs: term::r#type::TraitMember {
                        member_generic_arguments: generic_arguments,
                        id: trait_ty_id,
                        parent_generic_arguments,
                    },

                    rhs: resolve_ty,
                };

                let mut generic_symbol =
                    T::get_arena(self).get(generic_id).unwrap().write();

                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::TraitTypeEquality(
                            equality,
                        ),
                        span: Some(syntax_tree.span()),
                    },
                );
            }

            _ => {
                handler.receive(Box::new(ExpectTraitMember {
                    non_trait_member_span: syntax_tree
                        .qualified_identifier()
                        .span(),
                }));
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn create_trait_bound_predicates<
        T: Generic + table::representation::Element + finalizing::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::predicate::Trait,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        for trait_bound in syntax_tree.bounds().elements() {
            let higher_ranked_lifetimes = trait_bound
                .higher_rankded_lifetimes()
                .as_ref()
                .map(|x| Self::create_higher_ranked_lifetimes(x, handler));

            let Ok(resolution) = self.resolve(
                trait_bound.qualified_identifier(),
                generic_id.into(),
                resolution::Config {
                    ellided_lifetime_provider: None,
                    ellided_type_provider: None,
                    ellided_constant_provider: None,
                    observer: Some(
                        &mut (&mut builder::Resolution::basic())
                            .chain(occurrences),
                    ),
                    higher_ranked_lifetimes: higher_ranked_lifetimes.as_ref(),
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
                    let trait_predicate = predicate::Trait {
                        id: trait_id,
                        is_const: trait_bound.const_keyword().is_some(),
                        generic_arguments: generic_arguments.clone(),
                    };

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
                            span: Some(trait_bound.span()),
                        });
                }

                resolution => handler.receive(Box::new(ExpectTrait {
                    found_id: resolution.global_id(),
                    trait_path: trait_bound.span(),
                })),
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn create_lifetime_outlives_predicates<
        T: Generic + table::representation::Element + finalizing::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::predicate::Outlives,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let mut bounds = Vec::new();

        let mut basic = builder::Resolution::basic();
        let mut observer = basic.chain(occurrences);

        let mut config = resolution::Config {
            ellided_lifetime_provider: None,
            ellided_type_provider: None,
            ellided_constant_provider: None,
            observer: Some(&mut observer),
            higher_ranked_lifetimes: None,
        };

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

        match syntax_tree.operand() {
            syntax_tree::predicate::OutlivesOperand::LifetimeParameter(
                lt_parameter,
            ) => {
                let Ok(Some(lifetime_parameter_id)) = self
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

                for bound in bounds.iter().cloned() {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::LifetimeOutlives(
                                Outlives {
                                    operand:
                                        term::lifetime::Lifetime::Parameter(
                                            lifetime_parameter_id,
                                        ),
                                    bound,
                                },
                            ),
                            span: Some(syntax_tree.span()),
                        },
                    );
                }
            }
            syntax_tree::predicate::OutlivesOperand::Type(
                qualified_identifier,
            ) => {
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

                for bound in bounds.iter().cloned() {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TypeOutlives(
                                Outlives { operand: ty.clone(), bound },
                            ),
                            span: Some(syntax_tree.span()),
                        },
                    );
                }
            }
        }
    }
    // create a constant type predicate
    #[allow(clippy::too_many_lines)]
    fn create_constant_type_predicates<
        T: Generic + table::representation::Element + finalizing::Element,
    >(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::predicate::ConstantType,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        let mut basic = builder::Resolution::basic();
        let mut observer = basic.chain(occurrences);

        for bound in syntax_tree.bounds().elements() {
            let higher_ranked_lifetimes = bound
                .higher_ranked_lifetimes()
                .as_ref()
                .map(|x| Self::create_higher_ranked_lifetimes(x, handler));

            let config = resolution::Config {
                ellided_lifetime_provider: None,
                ellided_type_provider: None,
                ellided_constant_provider: None,
                observer: Some(&mut observer),
                higher_ranked_lifetimes: higher_ranked_lifetimes.as_ref(),
            };

            let Ok(ty) = self.resolve_type(
                bound.r#type(),
                generic_id.into(),
                config,
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
                    span: Some(syntax_tree.span()),
                },
            );
        }
    }

    // create a constant type predicate
    #[allow(clippy::too_many_lines)]
    fn create_tuple_predicates<T: Generic + Element>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::predicate::Tuple,
        occurrences: &mut Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        ID<T>: Into<GlobalID> + Into<GenericID>,
    {
        for tuple in syntax_tree.operands().elements() {
            match tuple.kind() {
                syntax_tree::predicate::TupleOperandKind::Type(ty) => {
                    let higher_ranked_lifetimes =
                        tuple.higher_ranked_lifetimes().as_ref().map(|x| {
                            Self::create_higher_ranked_lifetimes(x, handler)
                        });

                    let resolve_type = self.resolve_type(
                        ty,
                        generic_id.into(),
                        resolution::Config {
                            ellided_lifetime_provider: None,
                            ellided_type_provider: None,
                            ellided_constant_provider: None,
                            observer: Some(
                                &mut (&mut builder::Resolution::basic())
                                    .chain(occurrences),
                            ),
                            higher_ranked_lifetimes: higher_ranked_lifetimes
                                .as_ref(),
                        },
                        handler,
                    );
                    let Ok(ty) = resolve_type else {
                        continue;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TupleType(
                                predicate::Tuple(ty),
                            ),
                            span: Some(syntax_tree.span()),
                        },
                    );
                }
                syntax_tree::predicate::TupleOperandKind::Constant(expr) => {
                    let Ok(constant) = (match expr.constant() {
                        syntax_tree::Constant::Expression(expression) => self
                            .evaluate(&expression, generic_id.into(), handler),
                        syntax_tree::Constant::Elided(elided) => {
                            handler.receive(Box::new(UnexpectedInference {
                                unexpected_span: elided.span(),
                                generic_kind: symbol::GenericKind::Constant,
                            }));

                            continue;
                        }
                    }) else {
                        continue;
                    };

                    let mut generic_symbol =
                        T::get_arena(self).get(generic_id).unwrap().write();

                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TupleConstant(
                                predicate::Tuple(constant),
                            ),
                            span: Some(syntax_tree.span()),
                        },
                    );
                }
            }
        }
    }

    fn create_higher_ranked_lifetimes(
        syntax_tree: &syntax_tree::predicate::HigherRankedLifetimes,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> HashMap<String, Forall> {
        let mut forall_lifetimes_by_name = HashMap::new();

        for syn in syntax_tree
            .lifetime_parameter_list()
            .iter()
            .flat_map(ConnectedList::elements)
        {
            match forall_lifetimes_by_name
                .entry(syn.identifier().span.str().to_owned())
            {
                Entry::Vacant(entry) => {
                    let forall =
                        Forall::generate(Some(syn.identifier().span.clone()));
                    entry.insert(forall);
                }
                Entry::Occupied(_) => {
                    handler.receive(Box::new(
                        HigherRankedLifetimeRedefinition {
                            redefinition_span: syn.span(),
                        },
                    ));
                }
            }
        }

        forall_lifetimes_by_name
    }

    /// Creates where clause predicates for the given generic symbol.
    pub fn create_where_clause<
        T: Generic + table::representation::Element + finalizing::Element,
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

        for predicate in where_clause.predicate_list().elements() {
            match predicate {
                syntax_tree::predicate::Predicate::TraitTypeEquality(
                    trait_type_equality,
                ) => {
                    self.create_trait_member_predicates(
                        generic_id,
                        trait_type_equality,
                        occurrences,
                        handler,
                    );
                }

                syntax_tree::predicate::Predicate::Trait(tr) => {
                    self.create_trait_bound_predicates(
                        generic_id,
                        tr,
                        occurrences,
                        handler,
                    );
                }

                syntax_tree::predicate::Predicate::Outlives(
                    lifetime_outlives,
                ) => {
                    self.create_lifetime_outlives_predicates(
                        generic_id,
                        lifetime_outlives,
                        occurrences,
                        handler,
                    );
                }

                syntax_tree::predicate::Predicate::ConstantType(
                    constant_type,
                ) => {
                    self.create_constant_type_predicates(
                        generic_id,
                        constant_type,
                        occurrences,
                        handler,
                    );
                }

                syntax_tree::predicate::Predicate::Tuple(tuple) => {
                    self.create_tuple_predicates(
                        generic_id,
                        tuple,
                        occurrences,
                        handler,
                    );
                }
            }
        }
    }
}
