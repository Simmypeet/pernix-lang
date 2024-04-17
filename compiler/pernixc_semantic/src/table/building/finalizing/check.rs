//! Contains code related to well-formedness checking of each instantiation of
//! symbols and types.

use std::{collections::HashSet, ops::Deref};

use parking_lot::RwLockReadGuard;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree;

use super::{occurrences::Occurrences, Finalizer};
use crate::{
    arena::ID,
    error::{
        self, ExtraneousTraitMemberPredicate,
        MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationConstantTypeParameter, UndecidablePredicate,
        UnsatisfiedTraitMemberPredicate, UnsatisifedPredicate,
        UnusedGenericParameterInImplementation,
    },
    semantic::{
        equality,
        instantiation::{self, Instantiation},
        predicate::{self, ConstantType, Outlives, Trait, Tuple},
        session::{self, ExceedLimitError, Limit, Session},
        term::{
            self, constant,
            lifetime::{self, Lifetime},
            r#type, GenericArguments, Term,
        },
        Environment, Premise,
    },
    symbol::{
        ConstantParameter, ConstantParameterID, GenericID, GenericParameter,
        GenericParameters, GlobalID, ImplementationTemplate, LifetimeParameter,
        LifetimeParameterID, TraitImplementationMemberID, TraitMemberID,
        TypeParameter, TypeParameterID,
    },
    table::{Element, Index, Representation, State, Table},
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
            session,
            handler,
        );
    }

    #[allow(clippy::too_many_lines)]
    fn predicate_satisfied(
        &self,
        caller_premise: &Premise,
        predicate: &predicate::Predicate,
        session: &mut session::Default,
    ) -> Result<bool, ExceedLimitError> {
        // check if the predicate is satisfied
        let environment = Environment { premise: caller_premise, table: self };

        let result = match &predicate {
            predicate::Predicate::TraitTypeEquality(eq) => equality::equals(
                &r#type::Type::TraitMember(eq.trait_member.clone()),
                &eq.equivalent,
                &environment,
                &mut Limit::new(session),
            ),
            predicate::Predicate::TraitConstantEquality(eq) => {
                equality::equals(
                    &constant::Constant::TraitMember(eq.trait_member.clone()),
                    &eq.equivalent,
                    &environment,
                    &mut Limit::new(session),
                )
            }
            predicate::Predicate::ConstantType(pred) => {
                ConstantType::satisfies(
                    &pred.0,
                    &environment,
                    &mut Limit::new(session),
                )
            }
            predicate::Predicate::LifetimeOutlives(pred) => {
                Outlives::satisfies(
                    &pred.operand,
                    &pred.bound,
                    &environment,
                    &mut Limit::new(session),
                )
            }
            predicate::Predicate::TypeOutlives(pred) => Outlives::satisfies(
                &pred.operand,
                &pred.bound,
                &environment,
                &mut Limit::new(session),
            ),
            predicate::Predicate::TupleType(pred) => Tuple::satisfies(
                &pred.0,
                &environment,
                &mut Limit::new(session),
            ),
            predicate::Predicate::TupleConstant(pred) => Tuple::satisfies(
                &pred.0,
                &environment,
                &mut Limit::new(session),
            ),
            predicate::Predicate::Trait(pred) => {
                || -> Result<bool, ExceedLimitError> {
                    let Some(lifetime_constraints) = Trait::satisfies(
                        pred.id,
                        pred.is_const,
                        &pred.generic_arguments,
                        &environment,
                        &mut Limit::new(session),
                    )?
                    else {
                        return Ok(false);
                    };

                    // check if the lifetime constraints are satisfied
                    for constraint in lifetime_constraints.lifetime_constraints
                    {
                        match constraint {
                            predicate::LifetimeConstraint::LifetimeOutlives(
                                pred,
                            ) => {
                                if !Outlives::satisfies(
                                    &pred.operand,
                                    &pred.bound,
                                    &environment,
                                    &mut Limit::new(session),
                                )? {
                                    return Ok(false);
                                }
                            }
                            predicate::LifetimeConstraint::TypeOutlives(
                                pred,
                            ) => {
                                if !Outlives::satisfies(
                                    &pred.operand,
                                    &pred.bound,
                                    &environment,
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

        result
    }

    /// Do predicates check for the given instantiation.
    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    pub fn check_instantiation_predicates(
        &self,
        caller_premise: &Premise,
        instantiated: GenericID,
        instantiation: &Instantiation,
        instantiation_span: &Span,
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

            match self.predicate_satisfied(caller_premise, &predicate, session)
            {
                Ok(false) => handler.receive(Box::new(UnsatisifedPredicate {
                    predicate,
                    instantiation_span: instantiation_span.clone(),
                    predicate_declaration_span: predicate_info
                        .kind
                        .span()
                        .cloned(),
                })),

                Ok(true) => {}

                Err(_) => handler.receive(Box::new(UndecidablePredicate {
                    instantiation_span: instantiation_span.clone(),
                    predicate,
                    predicate_declaration_span: predicate_info
                        .kind
                        .span()
                        .cloned(),
                })),
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
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let environment =
            Environment { premise: caller_active_premise, table: self };

        for (ty, syn) in occurrences {
            match ty {
                term::r#type::Type::Tuple(_)
                | term::r#type::Type::Local(_)
                | term::r#type::Type::Phantom(_)
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
                        session,
                        handler,
                    );
                }

                term::r#type::Type::Reference(reference) => {
                    match Outlives::satisfies(
                        &*reference.pointee,
                        &reference.lifetime,
                        &environment,
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
                                session,
                                handler,
                            );
                            self.check_instantiation_predicates_from_generic_arguments(
                                caller_active_premise,
                                trait_implementation_type.into(),
                                member_symbol.member_generic_arguments.clone(),
                                &syn.span(),
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
                                    &environment,
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
                                session,
                                handler,
                            );
                            self.check_instantiation_predicates_from_generic_arguments(
                                caller_active_premise,
                                adt_implementation_type.into(),
                                member_symbol.member_generic_arguments.clone(),
                                &syn.span(),
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
                        &environment,
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
                                                        &environment,
                                                        &mut Limit::new(session),
                                                    )
                                                },
                                                predicate::LifetimeConstraint::TypeOutlives(outlives) => {
                                                    Outlives::satisfies(
                                                        &outlives.operand,
                                                        &outlives.bound,
                                                        &environment,
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
                        session,
                        handler,
                    );
                    self.check_instantiation_predicates_from_generic_arguments(
                        caller_active_premise,
                        trait_member.id.into(),
                        trait_member.member_generic_arguments.clone(),
                        &syn.span(),
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
        session: &mut session::Default,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        session::Default: Session<T>,
        predicate::Predicate: From<Tuple<T>>,
    {
        let environment = Environment { premise, table: self };
        for (term, ocurrence) in occurrences {
            match Tuple::satisfies(term, &environment, &mut Limit::new(session))
            {
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
        let mut session = session::Default::default();

        #[allow(clippy::map_identity)]
        self.check_type_ocurrences(
            &active_premise,
            occurrences.types().iter().map(|(ty, syn)| (ty, syn)),
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
            &mut session,
            handler,
        );

        #[allow(clippy::map_identity)]
        self.check_unpacked_ocurrences(
            &active_premise,
            occurrences
                .unpacked_constants()
                .iter()
                .map(|(tuple, syn)| (tuple, syn)),
            &mut session,
            handler,
        );
    }
}

/// Contains all the unused generic parameters in a generic arguments.
pub struct UnusedGenericParameters {
    pub lifetimes: HashSet<LifetimeParameterID>,
    pub types: HashSet<TypeParameterID>,
    pub constants: HashSet<ConstantParameterID>,
}

impl UnusedGenericParameters {
    /// Gets the list of unused generic parameters.
    ///
    /// # Parameters
    ///
    /// - `generic_id`: The [`GenericID`] where the `generic_parameters` are
    ///   from.
    /// - `generic_parameters`: The [`GenericParameters`] to compare the
    /// - `generic_arguments`: The generic arguments to search for unused
    ///   parameters.
    pub fn get_unused_generic_parameters(
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
        generic_arguments: &GenericArguments,
    ) -> Self {
        let mut unused_generic_arguments = Self {
            lifetimes: generic_parameters
                .lifetime_order()
                .iter()
                .map(|x| LifetimeParameterID { parent: generic_id, id: *x })
                .collect(),
            types: generic_parameters
                .type_order()
                .iter()
                .map(|x| TypeParameterID { parent: generic_id, id: *x })
                .collect(),
            constants: generic_parameters
                .constant_order()
                .iter()
                .map(|x| ConstantParameterID { parent: generic_id, id: *x })
                .collect(),
        };

        unused_generic_arguments.check_in_generic_arguments(generic_arguments);

        unused_generic_arguments
    }

    /// Reports the unused generic parameters in an implementation.
    pub fn report_as_unused_generic_parameters_in_implementation<
        ID: Into<GenericID> + Copy + std::fmt::Debug + Send + Sync + 'static,
    >(
        &self,
        implementation_kind_id: ID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for unused_lt in &self.lifetimes {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_lt.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }

        for unused_ty in &self.types {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_ty.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }

        for unused_val in &self.constants {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_val.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }
    }

    fn check_in_generic_arguments(&mut self, args: &GenericArguments) {
        for lt in &args.lifetimes {
            self.check_in_lifetime(lt);
        }

        for ty in &args.types {
            self.check_in_type(ty);
        }

        for val in &args.constants {
            self.check_in_constant(val);
        }
    }

    fn check_in_type(&mut self, ty: &r#type::Type) {
        match ty {
            r#type::Type::Parameter(parameter) => {
                self.types.remove(parameter);
            }
            r#type::Type::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }
            r#type::Type::Pointer(pointer) => {
                self.check_in_type(&pointer.pointee);
            }
            r#type::Type::Reference(reference) => {
                self.check_in_lifetime(&reference.lifetime);
                self.check_in_type(&reference.pointee);
            }
            r#type::Type::Array(array) => {
                self.check_in_type(&array.r#type);
                self.check_in_constant(&array.length);
            }
            r#type::Type::Tuple(tuple) => {
                for ty in &tuple.elements {
                    self.check_in_type(ty.as_term());
                }
            }
            r#type::Type::Local(local) => {
                self.check_in_type(&local.0);
            }
            r#type::Type::Phantom(phantom) => {
                self.check_in_type(&phantom.0);
            }
            r#type::Type::MemberSymbol(member_symbol) => {
                self.check_in_generic_arguments(
                    &member_symbol.member_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &member_symbol.parent_generic_arguments,
                );
            }

            r#type::Type::TraitMember(_)
            | r#type::Type::Inference(_)
            | r#type::Type::Primitive(_) => {}
        }
    }

    fn check_in_lifetime(&mut self, lt: &lifetime::Lifetime) {
        match lt {
            lifetime::Lifetime::Parameter(parameter) => {
                self.lifetimes.remove(parameter);
            }

            lifetime::Lifetime::Inference(_)
            | lifetime::Lifetime::Static
            | lifetime::Lifetime::Local(_)
            | lifetime::Lifetime::Forall(_) => {}
        }
    }

    fn check_in_constant(&mut self, val: &constant::Constant) {
        match val {
            constant::Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            constant::Constant::Phantom(_)
            | constant::Constant::Primitive(_)
            | constant::Constant::TraitMember(_)
            | constant::Constant::Inference(_) => {}

            constant::Constant::Struct(val) => {
                for field in &val.fields {
                    self.check_in_constant(field);
                }
            }

            constant::Constant::Enum(val) => {
                if let Some(value) = &val.associated_value {
                    self.check_in_constant(value);
                }
            }

            constant::Constant::Array(array) => {
                for element in &array.elements {
                    self.check_in_constant(element);
                }
            }

            constant::Constant::Local(local) => {
                self.check_in_constant(&local.0);
            }

            constant::Constant::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.check_in_constant(element.as_term());
                }
            }

            constant::Constant::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }

            constant::Constant::MemberSymbol(member_symbol) => {
                self.check_in_generic_arguments(
                    &member_symbol.member_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &member_symbol.parent_generic_arguments,
                );
            }
        }
    }
}

impl Table<Finalizer> {
    /// Checks if the given `implementation_member_id` has a matching generic
    /// declaration with the `trait_member_id`.
    ///
    /// Assumes that `implementation_member_id` is an implementation member of
    /// the trait member `trait_member_id`, `trait_instantiation` is the
    /// instantiation of the implemented trait.
    ///
    /// The errors (if any) are reported to the handler.
    ///
    /// # Checks
    ///
    /// - Checks if the generic parameters count match.
    /// - Checks if the constant parameters' types match.
    /// - Checks if the predicates are satisfied and exactly the same.
    #[allow(
        clippy::too_many_lines,
        clippy::significant_drop_tightening,
        clippy::significant_drop_in_scrutinee
    )]
    pub fn implementation_member_check(
        &self,
        implementation_member_id: TraitImplementationMemberID,
        trait_member_id: TraitMemberID,
        mut trait_instantiation: Instantiation,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let implementation_member_sym =
            self.get_generic(implementation_member_id.into()).unwrap();
        let trait_member_sym =
            self.get_generic(trait_member_id.into()).unwrap();

        // check generic parameter count check
        Self::check_implementation_member_generic_parameter_count::<
            LifetimeParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        Self::check_implementation_member_generic_parameter_count::<
            TypeParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        Self::check_implementation_member_generic_parameter_count::<
            ConstantParameter,
        >(
            implementation_member_id,
            trait_member_id,
            &implementation_member_sym.generic_declaration().parameters,
            &trait_member_sym.generic_declaration().parameters,
            handler,
        );

        trait_instantiation.lifetimes.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .lifetime_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .lifetime_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: trait_member_sym.generic_id(),
                            id: tr,
                        }),
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: implementation_member_sym.generic_id(),
                            id: im,
                        }),
                    )
                }),
        );
        trait_instantiation.types.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .type_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .type_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        r#type::Type::Parameter(TypeParameterID {
                            parent: trait_member_sym.generic_id(),
                            id: tr,
                        }),
                        r#type::Type::Parameter(TypeParameterID {
                            parent: implementation_member_sym.generic_id(),
                            id: im,
                        }),
                    )
                }),
        );
        trait_instantiation.constants.extend(
            implementation_member_sym
                .generic_declaration()
                .parameters
                .constant_order()
                .iter()
                .copied()
                .zip(
                    trait_member_sym
                        .generic_declaration()
                        .parameters
                        .constant_order()
                        .iter()
                        .copied(),
                )
                .map(|(im, tr)| {
                    (
                        constant::Constant::Parameter(ConstantParameterID {
                            parent: trait_member_sym.generic_id(),
                            id: tr,
                        }),
                        constant::Constant::Parameter(ConstantParameterID {
                            parent: implementation_member_sym.generic_id(),
                            id: im,
                        }),
                    )
                }),
        );

        // check if the constant type matches
        let implementation_member_active_premise =
            self.get_active_premise(implementation_member_id.into()).unwrap();

        let mut session = session::Default::default();

        for ((tr_const_id, tr_const_param), (im_const_id, im_const_param)) in
            trait_member_sym
                .generic_declaration()
                .parameters
                .constant_parameters_as_order()
                .zip(
                    implementation_member_sym
                        .generic_declaration()
                        .parameters
                        .constant_parameters_as_order(),
                )
        {
            let mut tr_const_ty = tr_const_param.r#type.clone();
            instantiation::instantiate(&mut tr_const_ty, &trait_instantiation);

            match equality::equals(
                &tr_const_ty,
                &im_const_param.r#type,
                &Environment {
                    table: self,
                    premise: &implementation_member_active_premise,
                },
                &mut Limit::new(&mut session),
            ) {
                Ok(false) => handler.receive(Box::new(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: implementation_member_sym.generic_id(),
                                id: im_const_id,
                            },
                        trait_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: trait_member_sym.generic_id(),
                                id: tr_const_id,
                            },
                    },
                )),
                Ok(true) => {}
                Err(_) => todo!(),
            }
        }

        // check if the predicates match
        for predicate in &trait_member_sym.generic_declaration().predicates {
            let mut predicate_instantiated = predicate.predicate.clone();
            predicate_instantiated.instantiate(&trait_instantiation);

            match self.predicate_satisfied(
                &implementation_member_active_premise,
                &predicate_instantiated,
                &mut session,
            ) {
                Ok(false) => {
                    handler.receive(Box::new(
                        UnsatisfiedTraitMemberPredicate {
                            trait_implementation_id: implementation_member_id,
                            predicate: predicate_instantiated,
                            predicate_span: predicate.kind.span().cloned(),
                        },
                    ));
                }
                Ok(true) => {}
                Err(_) => handler.receive(Box::new(UndecidablePredicate {
                    instantiation_span: implementation_member_sym
                        .span()
                        .cloned()
                        .unwrap(),
                    predicate: predicate_instantiated,
                    predicate_declaration_span: None,
                })),
            }
        }

        let trait_member_active_premise = {
            let stub = self.get_active_premise(trait_member_id.into()).unwrap();

            let mut trait_member_active_premise = Premise::default();

            trait_member_active_premise.trait_context =
                implementation_member_active_premise.trait_context;
            trait_member_active_premise.append_from_predicates(
                stub.predicates().iter().map(|x| {
                    let mut predicate = x.clone();
                    predicate.instantiate(&trait_instantiation);
                    predicate
                }),
            );

            trait_member_active_premise
        };

        // check for any extraneous predicates defined in the implementation
        // member that are not in the trait member
        for predicate in
            &implementation_member_sym.generic_declaration().predicates
        {
            match self.predicate_satisfied(
                &trait_member_active_premise,
                &predicate.predicate,
                &mut session,
            ) {
                Ok(false) => {
                    handler.receive(Box::new(ExtraneousTraitMemberPredicate {
                        trait_implementation_member_id:
                            implementation_member_id,
                        predicate: predicate.predicate.clone(),
                        predicate_span: predicate.kind.span().cloned(),
                    }));
                }
                Ok(true) => {}
                Err(_) => handler.receive(Box::new(UndecidablePredicate {
                    instantiation_span: implementation_member_sym
                        .span()
                        .cloned()
                        .unwrap(),
                    predicate: predicate.predicate.clone(),
                    predicate_declaration_span: None,
                })),
            }
        }
    }

    fn check_implementation_member_generic_parameter_count<
        T: GenericParameter,
    >(
        implementation_member_id: TraitImplementationMemberID,
        trait_member_id: TraitMemberID,
        implementation_member_generic_parameters: &GenericParameters,
        trait_member_generic_parameters: &GenericParameters,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        if T::get_generic_parameters_arena(
            implementation_member_generic_parameters,
        )
        .len()
            != T::get_generic_parameters_arena(trait_member_generic_parameters)
                .len()
        {
            handler.receive(Box::new(
                MismatchedGenericParameterCountInImplementation {
                    implementation_member_id,
                    trait_member_id,
                    expected_count: T::get_generic_parameters_arena(
                        trait_member_generic_parameters,
                    )
                    .len(),
                    declared_count: T::get_generic_parameters_arena(
                        implementation_member_generic_parameters,
                    )
                    .len(),
                    generic_kind: T::kind(),
                },
            ));
        }
    }

    /// Checks if the given implementation has a matching signature with its
    /// implemented symbol.
    ///
    /// The errors (if any) are reported to the handler.
    ///
    /// # Checks
    ///
    /// - Checks if all the generic parameters are used in the implementation
    ///   arguments.
    /// - Checks if the implementation's generic arguments satisfy the
    ///   implemented symbol's predicates.
    pub fn implementation_signature_check<
        'a,
        T: Element,
        U: 'a + Copy + Into<GenericID>,
        V: 'static,
    >(
        &'a self,
        implementation_id: ID<T>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        Representation<<Finalizer as State>::Container>: Index<
            ID<T>,
            Output<'a> = RwLockReadGuard<'a, ImplementationTemplate<U, V>>,
        >,
        ID<T>: Into<GenericID> + Into<GlobalID>,
    {
        let implementation_sym = self.get(implementation_id).unwrap();

        // check if all the generic parameters are used in the
        // implementation arguments
        let unused_generic_parameters =
            UnusedGenericParameters::get_unused_generic_parameters(
                implementation_id.into(),
                &implementation_sym.signature.generic_declaration.parameters,
                &implementation_sym.signature.arguments,
            );
        unused_generic_parameters
            .report_as_unused_generic_parameters_in_implementation(
                implementation_id,
                handler,
            );

        let trait_sym = self
            .get_generic(implementation_sym.signature.implemented_id.into())
            .unwrap();

        // early return if the generic parameters count does not match
        if implementation_sym.signature.arguments.lifetimes.len()
            != trait_sym.generic_declaration().parameters.lifetimes().len()
            || implementation_sym.signature.arguments.types.len()
                != trait_sym.generic_declaration().parameters.types().len()
            || implementation_sym.signature.arguments.constants.len()
                != trait_sym.generic_declaration().parameters.constants().len()
        {
            return;
        }

        drop(trait_sym);

        // check if the signature matches the trait definition
        let mut session = session::Default::default();
        let premise =
            self.get_active_premise(implementation_id.into()).unwrap();

        self.check_instantiation_predicates_from_generic_arguments(
            &premise,
            implementation_sym.signature.implemented_id.into(),
            implementation_sym.signature.arguments.clone(),
            implementation_sym.span.as_ref().unwrap(),
            &mut session,
            handler,
        );
    }
}
