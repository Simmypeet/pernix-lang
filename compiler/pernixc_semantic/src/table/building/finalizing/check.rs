use std::ops::Deref;

use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;

use super::{occurrences::Occurrences, Finalizer};
use crate::{
    error::{self, UndecidablePredicate, UnsatisifedPredicate},
    semantic::{
        self, equality,
        instantiation::Instantiation,
        predicate::{self, ConstantType, Outlives, Trait, Tuple},
        session::{self, ExceedLimitError, Limit, Session},
        term::{self, GenericArguments, Term},
        Premise, Semantic,
    },
    symbol::{GenericID, GlobalID},
    table::{Index, Table},
};

impl Table<Finalizer> {
    /// Do predicates check for the given instantiation.
    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    pub fn check_instantiation_predicates_from_generic_arguments(
        &self,
        caller_premise: &Premise,
        instantiated: GenericID,
        generic_arguments: GenericArguments,
        instantiation_span: &Span,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        self.check_instantiation_predicates(
            caller_premise,
            instantiated,
            &Instantiation::from_generic_arguments(
                generic_arguments,
                instantiated,
                &self
                    .get_generic(instantiated)
                    .unwrap()
                    .generic_declaration()
                    .parameters,
            )
            .unwrap(),
            instantiation_span,
            semantic,
            session,
            handler,
        );
    }

    /// Do predicates check for the given instantiation.
    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    pub fn check_instantiation_predicates(
        &self,
        caller_premise: &Premise,
        instantiated: GenericID,
        instantiation: &Instantiation,
        instantiation_span: &Span,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // get all the predicates and instantiate them with the given generic
        // arguments
        let instantiated_sym = self.get_generic(instantiated).unwrap();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &instantiated_sym.generic_declaration().predicates
        {
            let mut predicate = predicate_info.predicate.clone();
            predicate.instantiate(instantiation);

            // check if the predicate is satisfied
            let result = match &predicate {
                predicate::Predicate::TypeEquality(eq) => equality::equals(
                    &eq.lhs,
                    &eq.rhs,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                ),
                predicate::Predicate::ConstantEquality(eq) => equality::equals(
                    &eq.lhs,
                    &eq.rhs,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                ),
                predicate::Predicate::ConstantType(pred) => {
                    ConstantType::satisfies(
                        &pred.0,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                }
                predicate::Predicate::LifetimeOutlives(pred) => {
                    Outlives::satisfies(
                        &pred.operand,
                        &pred.bound,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                }
                predicate::Predicate::TypeOutlives(pred) => {
                    Outlives::satisfies(
                        &pred.operand,
                        &pred.bound,
                        caller_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    )
                }
                predicate::Predicate::TupleType(pred) => Tuple::satisfies(
                    &pred.0,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                ),
                predicate::Predicate::TupleConstant(pred) => Tuple::satisfies(
                    &pred.0,
                    caller_premise,
                    self,
                    semantic,
                    &mut Limit::new(session),
                ),
                predicate::Predicate::Trait(pred) => {
                    || -> Result<bool, ExceedLimitError> {
                        let Some(lifetime_constraints) = Trait::satisfies(
                            pred.id,
                            pred.is_const,
                            &pred.generic_arguments,
                            caller_premise,
                            self,
                            semantic,
                            &mut Limit::new(session),
                        )?
                        else {
                            return Ok(false);
                        };

                        // check if the lifetime constraints are satisfied
                        for constraint in
                            lifetime_constraints.lifetime_constraints
                        {
                            match constraint {
                                predicate::LifetimeConstraint::LifetimeOutlives(pred) => {
                                     if !Outlives::satisfies(
                                        &pred.operand,
                                        &pred.bound,
                                        caller_premise,
                                        self,
                                        semantic,
                                        &mut Limit::new(session),
                                    )? {
                                        return Ok(false);
                                    }
                                }
                                predicate::LifetimeConstraint::TypeOutlives(pred) => {
                                    if !Outlives::satisfies(
                                        &pred.operand,
                                        &pred.bound,
                                        caller_premise,
                                        self,
                                        semantic,
                                        &mut Limit::new(session),
                                    )? {
                                        return Ok(false);
                                    }
                                }
                            }
                        }

                        Ok(true)
                    }()
                }
            };

            match result {
                Ok(false) => handler.receive(Box::new(UnsatisifedPredicate {
                    predicate,
                    instantiation_span: instantiation_span.clone(),
                    predicate_declaration_span: predicate_info
                        .kind
                        .span()
                        .cloned(),
                })),
                Ok(true) => {}
                Err(_) => {
                    handler.receive(Box::new(UndecidablePredicate {
                        instantiation_span: instantiation_span.clone(),
                        predicate: predicate_info.predicate.clone(),
                        predicate_declaration_span: predicate_info
                            .kind
                            .span()
                            .cloned(),
                    }));
                }
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn check_type_ocurrences<'a>(
        &self,
        caller_active_premise: &Premise,
        occurrences: impl IntoIterator<
            Item = (&'a term::r#type::Type, &'a syntax_tree::r#type::Type),
        >,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for (ty, syn) in occurrences {
            match ty {
                term::r#type::Type::Tuple(_)
                | term::r#type::Type::Local(_)
                | term::r#type::Type::Pointer(_)
                | term::r#type::Type::Primitive(_)
                | term::r#type::Type::Parameter(_)
                | term::r#type::Type::Inference(_) => {
                    /* no additional check */
                }

                term::r#type::Type::Symbol(term::Symbol {
                    id,
                    generic_arguments,
                }) => {
                    self.check_instantiation_predicates_from_generic_arguments(
                        caller_active_premise,
                        (*id).into(),
                        generic_arguments.clone(),
                        &syn.span(),
                        semantic,
                        session,
                        handler,
                    );
                }

                term::r#type::Type::Reference(reference) => {
                    match Outlives::satisfies(
                        &*reference.pointee,
                        &reference.lifetime,
                        caller_active_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    ) {
                        Ok(true) => {}
                        Ok(false) => {
                            handler.receive(Box::new(UnsatisifedPredicate {
                                predicate: predicate::Predicate::TypeOutlives(
                                    Outlives {
                                        operand: reference
                                            .pointee
                                            .deref()
                                            .clone(),
                                        bound: reference.lifetime,
                                    },
                                ),
                                instantiation_span: syn.span(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(_) => {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: syn.span(),
                                predicate: predicate::Predicate::TypeOutlives(
                                    Outlives {
                                        operand: reference
                                            .pointee
                                            .deref()
                                            .clone(),
                                        bound: reference.lifetime,
                                    },
                                ),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
                term::r#type::Type::Array(_) => {
                    todo!("check if the length is type of usize")
                }

                term::r#type::Type::MemberSymbol(member_symbol) => {
                    match member_symbol.id {
                        term::r#type::MemberSymbolID::TraitImplementation(
                            trait_implementation_type,
                        ) => {
                            let implementation_id = self
                                .get(trait_implementation_type)
                                .unwrap()
                                .parent_id;

                            self.check_instantiation_predicates_from_generic_arguments(
                                caller_active_premise,
                                implementation_id.into(),
                                member_symbol.parent_generic_arguments.clone(),
                                &syn.span(),
                                semantic,
                                session,
                                handler,
                            );
                            self.check_instantiation_predicates_from_generic_arguments(
                                caller_active_premise,
                                trait_implementation_type.into(),
                                member_symbol.member_generic_arguments.clone(),
                                &syn.span(),
                                semantic,
                                session,
                                handler,
                            );
                        }
                        term::r#type::MemberSymbolID::AdtImplementation(
                            adt_implementation_type,
                        ) => {
                            let adt_implementation_id = self
                                .get(adt_implementation_type)
                                .unwrap()
                                .parent_id;

                            let Ok(Some(deduction)) =
                                member_symbol.parent_generic_arguments.deduce(
                                    &self
                                        .get(adt_implementation_id)
                                        .unwrap()
                                        .signature
                                        .arguments,
                                    caller_active_premise,
                                    self,
                                    semantic,
                                    &mut Limit::new(session),
                                )
                            else {
                                todo!("report overflow deducing");
                            };

                            self.check_instantiation_predicates(
                                caller_active_premise,
                                adt_implementation_id.into(),
                                &deduction,
                                &syn.span(),
                                semantic,
                                session,
                                handler,
                            );
                            self.check_instantiation_predicates_from_generic_arguments(
                                caller_active_premise,
                                adt_implementation_type.into(),
                                member_symbol.member_generic_arguments.clone(),
                                &syn.span(),
                                semantic,
                                session,
                                handler,
                            );
                        }
                    }
                }

                term::r#type::Type::TraitMember(trait_member) => {
                    let parent_trait_id =
                        self.get(trait_member.id).unwrap().parent_id;

                    // check if the trait predicate is satisfied
                    Trait::satisfies(
                        parent_trait_id,
                        false,
                        &trait_member.parent_generic_arguments,
                        caller_active_premise,
                        self,
                        semantic,
                        &mut Limit::new(session),
                    ).map_or_else(|_| {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: syn.span(),
                                predicate: predicate::Predicate::Trait(Trait {
                                    id: parent_trait_id,
                                    is_const: false,
                                    generic_arguments: trait_member
                                        .parent_generic_arguments
                                        .clone(),
                                }),
                                predicate_declaration_span: None,
                            }));
                        }, |satisfiability| satisfiability.map_or_else(|| {
                                handler.receive(Box::new(
                                    UnsatisifedPredicate {
                                        predicate: predicate::Predicate::Trait(
                                            Trait {
                                                id: parent_trait_id,
                                                is_const: false,
                                                generic_arguments: trait_member
                                                    .parent_generic_arguments
                                                    .clone(),
                                            },
                                        ),
                                        instantiation_span: syn.span(),
                                        predicate_declaration_span: None,
                                    },
                                ));
                            }, |satisfiability| {
                                let satisfied =
                                    || -> Result<bool, ExceedLimitError> {
                                        for constraint in
                                            satisfiability.lifetime_constraints
                                        {
                                            if !match constraint {
                                                predicate::LifetimeConstraint::LifetimeOutlives(outlives) => {
                                                    Outlives::satisfies(
                                                        &outlives.operand,
                                                        &outlives.bound,
                                                        caller_active_premise,
                                                        self,
                                                        semantic,
                                                        &mut Limit::new(session),
                                                    )
                                                },
                                                predicate::LifetimeConstraint::TypeOutlives(outlives) => {
                                                    Outlives::satisfies(
                                                        &outlives.operand,
                                                        &outlives.bound,
                                                        caller_active_premise,
                                                        self,
                                                        semantic,
                                                        &mut Limit::new(session),
                                                    )
                                                },
                                            }? {
                                                return Ok(false);
                                            }
                                        }

                                        Ok(true)
                                    }();
                                match satisfied {
                                    Ok(false) => {
                                        handler.receive(Box::new(UnsatisifedPredicate {
                                            predicate: predicate::Predicate::Trait(
                                                Trait {
                                                    id: parent_trait_id,
                                                    is_const: false,
                                                    generic_arguments: trait_member
                                                        .parent_generic_arguments
                                                        .clone(),
                                                },
                                            ),
                                            instantiation_span: syn.span(),
                                            predicate_declaration_span: None,
                                        }));
                                    },
                                    Ok(true) => {},
                                    Err(_) => {
                                        handler.receive(Box::new(UndecidablePredicate {
                                            instantiation_span: syn.span(),
                                            predicate: predicate::Predicate::Trait(Trait {
                                                id: parent_trait_id,
                                                is_const: false,
                                                generic_arguments: trait_member
                                                    .parent_generic_arguments
                                                    .clone(),
                                            }),
                                            predicate_declaration_span: None,
                                        }));
                                    }
                                }
                            }));

                    self.check_instantiation_predicates_from_generic_arguments(
                        caller_active_premise,
                        parent_trait_id.into(),
                        trait_member.parent_generic_arguments.clone(),
                        &syn.span(),
                        semantic,
                        session,
                        handler,
                    );
                    self.check_instantiation_predicates_from_generic_arguments(
                        caller_active_premise,
                        trait_member.id.into(),
                        trait_member.member_generic_arguments.clone(),
                        &syn.span(),
                        semantic,
                        session,
                        handler,
                    );
                }
            }
        }
    }

    fn check_unpacked_ocurrences<'a, T: Term + 'a, Syn: SourceElement + 'a>(
        &self,
        premise: &Premise,
        occurrences: impl Iterator<Item = (&'a T, &'a Syn)>,
        semantic: &mut semantic::Default,
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        semantic::Default: Semantic<T>,
        session::Default: Session<T>,
        predicate::Predicate: From<Tuple<T>>,
    {
        for (term, ocurrence) in occurrences {
            match Tuple::satisfies(
                term,
                premise,
                self,
                semantic,
                &mut Limit::new(session),
            ) {
                Ok(true) => {}
                Ok(false) => {
                    handler.receive(Box::new(error::UnsatisifedPredicate {
                        predicate: Tuple(term.clone()).into(),
                        instantiation_span: ocurrence.span(),
                        predicate_declaration_span: None,
                    }));
                }
                Err(ExceedLimitError) => {
                    handler.receive(Box::new(error::UndecidablePredicate {
                        instantiation_span: ocurrence.span(),
                        predicate: Tuple(term.clone()).into(),
                        predicate_declaration_span: None,
                    }));
                }
            }
        }
    }

    /// Checks if the occurrences of symbols are valid (i.e. they satisfy the
    /// where clause predicates).
    pub fn check_occurrences(
        &self,
        id: GlobalID,
        occurrences: &Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let active_premise = self.get_active_premise(id).unwrap();
        let mut semantic = semantic::Default;
        let mut session = session::Default::default();

        #[allow(clippy::map_identity)]
        self.check_type_ocurrences(
            &active_premise,
            occurrences.types().iter().map(|(ty, syn)| (ty, syn)),
            &mut semantic,
            &mut session,
            handler,
        );

        #[allow(clippy::map_identity)]
        self.check_unpacked_ocurrences(
            &active_premise,
            occurrences
                .unpacked_types()
                .iter()
                .map(|(tuple, syn)| (tuple, syn)),
            &mut semantic,
            &mut session,
            handler,
        );

        #[allow(clippy::map_identity)]
        self.check_unpacked_ocurrences(
            &active_premise,
            occurrences
                .constant_types()
                .iter()
                .map(|(tuple, syn)| (tuple, syn)),
            &mut semantic,
            &mut session,
            handler,
        );
    }
}
