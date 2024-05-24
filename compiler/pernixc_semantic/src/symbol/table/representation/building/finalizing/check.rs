//! Contains code related to well-formedness checking of each instantiation of
//! symbols and types.

use std::{collections::HashSet, ops::Deref};

use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};

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
        model::{Default, Model},
        normalizer::{NoOp, Normalizer},
        predicate::{self, ConstantType, Outlives, Predicate, Trait, Tuple},
        session::{self, ExceedLimitError, Limit, Session},
        term::{
            self, constant,
            lifetime::{self, Lifetime},
            r#type, GenericArguments, Term,
        },
        Environment, Premise,
    },
    symbol::{
        table::{
            representation::{Element, Index, RwLockContainer},
            resolution, Building, State, Table,
        },
        ConstantParameter, ConstantParameterID, GenericID, GenericParameter,
        GenericParameters, GenericTemplate, GlobalID, ImplementationTemplate,
        LifetimeParameter, LifetimeParameterID, TraitImplementationMemberID,
        TraitMemberID, TypeParameter, TypeParameterID,
    },
};

impl<'a, M: Model, T: State, N: Normalizer<M>> Environment<'a, M, T, N> {
    /// Checks if the given `resolution` is well-formed. The errors are reported
    /// to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `resolution`: The resolution to check.
    /// - `resolution_span`: The span location of the `resolution`.
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///  checked.
    /// - `session`: The session to use for caching and limiting the
    ///   computation.
    /// - `handler`: The handler to report the errors.
    pub fn check_resolution_occurrence(
        &self,
        resolution: &resolution::Resolution<M>,
        resolution_span: &Span,
        do_outlives_check: bool,
        session: &mut session::Default<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match resolution {
            resolution::Resolution::Module(_) => {}
            resolution::Resolution::Variant(_) => {}
            resolution::Resolution::Generic(generic) => {
                // check for trait predicates
                if let resolution::GenericID::Trait(trait_id) = generic.id {
                    let predicate =
                        predicate::Predicate::Trait(predicate::Trait {
                            id: trait_id,
                            generic_arguments: generic
                                .generic_arguments
                                .clone(),
                        });

                    match self.predicate_satisfied(
                        &predicate,
                        do_outlives_check,
                        session,
                    ) {
                        Ok(true) => {}
                        Ok(false) => {
                            handler.receive(Box::new(UnsatisifedPredicate {
                                predicate,
                                instantiation_span: resolution_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(ExceedLimitError) => {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: resolution_span.clone(),
                                predicate,
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }

                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id.into(),
                    generic.generic_arguments.clone(),
                    resolution_span,
                    do_outlives_check,
                    session,
                    handler,
                )
            }
            resolution::Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let adt_implementation_check = match member_generic.id {
                    resolution::MemberGenericID::AdtImplementationFunction(
                        id,
                    ) => Some(self.table.get(id).unwrap().parent_id),
                    resolution::MemberGenericID::AdtImplementationType(id) => {
                        Some(self.table.get(id).unwrap().parent_id)
                    }
                    resolution::MemberGenericID::AdtImplementationConstant(
                        id,
                    ) => Some(self.table.get(id).unwrap().parent_id),

                    _ => None,
                };

                if let Some(adt_implementation_id) = adt_implementation_check {
                    let adt_implementation =
                        self.table.get(adt_implementation_id).unwrap();

                    let deduced = match GenericArguments::from_default_model(
                        adt_implementation.arguments.clone(),
                    )
                    .deduce(
                        &member_generic.parent_generic_arguments,
                        self,
                        &mut Limit::new(session),
                    ) {
                        Ok(deduced) => {
                            deduced.expect("should be able to deduce")
                        }
                        Err(_) => todo!("report undecidable error"),
                    };

                    // check if the deduced generic arguments are correct
                    self.check_instantiation_predicates(
                        adt_implementation_id.into(),
                        &deduced,
                        resolution_span,
                        do_outlives_check,
                        session,
                        handler,
                    );
                }

                self.check_instantiation_predicates_by_generic_arguments(
                    member_generic.id.into(),
                    member_generic.generic_arguments.clone(),
                    resolution_span,
                    do_outlives_check,
                    session,
                    handler,
                )
            }
        }
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `instantiated`: The [`GenericID`] of the instantiated symbol.
    /// - `generic_arguments`: The generic arguments supplied to the
    ///   `instantiated`.
    /// - `instantiation_span`: The span location of the instantiation, used for
    ///   error reporting
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///   checked.
    /// - `session`: The session to use for caching and limiting the
    ///   computation.
    /// - `handler`: The handler to report the errors.
    pub fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: GenericID,
        generic_arguments: GenericArguments<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        session: &mut session::Default<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // convert the generic arguments to an instantiation and delegate the
        // check to the `check_instantiation_predicates` method
        self.check_instantiation_predicates(
            instantiated,
            &Instantiation::from_generic_arguments(
                generic_arguments,
                instantiated,
                &self
                    .table
                    .get_generic(instantiated)
                    .unwrap()
                    .generic_declaration()
                    .parameters,
            )
            .unwrap(),
            instantiation_span,
            do_outlives_check,
            session,
            handler,
        )
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    ///
    /// # Parameters
    ///
    /// - `instantiated`: The [`GenericID`] of the instantiated symbol.
    /// - `instantiation`: The instantiation of the `instantiated`.
    /// - `instantiation_span`: The span location of the instantiation, used for
    ///  error reporting
    /// - `do_outlives_check`: Determines if the outlives predicates should be
    ///   checked.
    /// - `session`: The session to use for caching and limiting the
    ///  computation.
    /// - `handler`: The handler to report the errors.
    pub fn check_instantiation_predicates(
        &self,
        instantiated: GenericID,
        instantiation: &Instantiation<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        session: &mut session::Default<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // get all the predicates and instantiate them with the given generic
        // arguments
        let instantiated_sym = self.table.get_generic(instantiated).unwrap();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &instantiated_sym.generic_declaration().predicates
        {
            let mut predicate =
                Predicate::from_default_model(predicate_info.predicate.clone());

            predicate.instantiate(instantiation);

            match self.predicate_satisfied(
                &predicate,
                do_outlives_check,
                session,
            ) {
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

    fn predicate_satisfied(
        &self,
        predicate: &Predicate<M>,
        do_outlives_check: bool,
        session: &mut session::Default<M>,
    ) -> Result<bool, ExceedLimitError> {
        let result = match &predicate {
            predicate::Predicate::TraitTypeEquality(eq) => equality::equals(
                &r#type::Type::TraitMember(eq.lhs.clone()),
                &eq.rhs,
                self,
                &mut Limit::new(session),
            ),
            predicate::Predicate::ConstantType(pred) => {
                ConstantType::satisfies(&pred.0, self, &mut Limit::new(session))
            }
            predicate::Predicate::LifetimeOutlives(pred) => {
                if !do_outlives_check {
                    return Ok(true);
                }

                Outlives::satisfies(
                    &pred.operand,
                    &pred.bound,
                    self,
                    &mut Limit::new(session),
                )
            }
            predicate::Predicate::TypeOutlives(pred) => {
                if !do_outlives_check {
                    return Ok(true);
                }

                Outlives::satisfies(
                    &pred.operand,
                    &pred.bound,
                    self,
                    &mut Limit::new(session),
                )
            }
            predicate::Predicate::TupleType(pred) => {
                Tuple::satisfies(&pred.0, self, &mut Limit::new(session))
            }
            predicate::Predicate::TupleConstant(pred) => {
                Tuple::satisfies(&pred.0, self, &mut Limit::new(session))
            }
            predicate::Predicate::Trait(pred) => {
                || -> Result<bool, ExceedLimitError> {
                    let Some(lifetime_constraints) = Trait::satisfies(
                        pred.id,
                        &pred.generic_arguments,
                        self,
                        &mut Limit::new(session),
                    )?
                    else {
                        return Ok(false);
                    };

                    if do_outlives_check {
                        // check if the lifetime constraints are satisfied
                        for constraint in
                            lifetime_constraints.lifetime_constraints
                        {
                            match constraint {
                            predicate::LifetimeConstraint::LifetimeOutlives(
                                pred,
                            ) => {
                                if !Outlives::satisfies(
                                    &pred.operand,
                                    &pred.bound,
                                    self,
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
                                    self,
                                    &mut Limit::new(session),
                                )? {
                                    return Ok(false);
                                }
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

    /// Do predicates check for the given type occurrences.
    pub fn check_type_ocurrence(
        &self,
        ty: &r#type::Type<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        session: &mut session::Default<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match ty {
            term::r#type::Type::Tuple(_)
            | term::r#type::Type::Local(_)
            | term::r#type::Type::Phantom(_)
            | term::r#type::Type::Pointer(_)
            | term::r#type::Type::Primitive(_)
            | term::r#type::Type::Parameter(_)
            | term::r#type::Type::Symbol(_)
            | term::r#type::Type::MemberSymbol(_)
            | term::r#type::Type::TraitMember(_)
            | term::r#type::Type::Inference(_) => { /* no additional check */ }

            term::r#type::Type::Reference(reference) => {
                if do_outlives_check {
                    match Outlives::satisfies(
                        &*reference.pointee,
                        &reference.lifetime,
                        self,
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
                                        bound: reference.lifetime.clone(),
                                    },
                                ),
                                instantiation_span: instantiation_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                        Err(_) => {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: instantiation_span.clone(),
                                predicate: predicate::Predicate::TypeOutlives(
                                    Outlives {
                                        operand: reference
                                            .pointee
                                            .deref()
                                            .clone(),
                                        bound: reference.lifetime.clone(),
                                    },
                                ),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
            }
            term::r#type::Type::Array(_) => {
                todo!("check if the length is type of usize")
            }
        }
    }

    fn check_unpacked_ocurrences<U: Term<Model = M> + 'a>(
        &self,
        unpacked_term: &U,
        instantiation_span: &Span,
        session: &mut session::Default<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        session::Default<M>: Session<U>,
        predicate::Predicate<M>: From<Tuple<U>>,
    {
        match Tuple::satisfies(unpacked_term, self, &mut Limit::new(session)) {
            Ok(true) => {}
            Ok(false) => {
                handler.receive(Box::new(error::UnsatisifedPredicate {
                    predicate: Tuple(unpacked_term.clone()).into(),
                    instantiation_span: instantiation_span.clone(),
                    predicate_declaration_span: None,
                }));
            }
            Err(ExceedLimitError) => {
                handler.receive(Box::new(error::UndecidablePredicate {
                    instantiation_span: instantiation_span.clone(),
                    predicate: Tuple(unpacked_term.clone()).into(),
                    predicate_declaration_span: None,
                }));
            }
        }
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
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
        let environment = Environment {
            premise: &active_premise,
            table: self,
            normalizer: &NoOp,
        };

        // check resolution occurrences
        for (resolution, generic_identifier) in occurrences.resolutions() {
            environment.check_resolution_occurrence(
                resolution,
                &generic_identifier.span(),
                true,
                &mut session,
                handler,
            )
        }

        // check type occurrences
        for (ty, syn) in occurrences.types() {
            environment.check_type_ocurrence(
                ty,
                &syn.span(),
                true,
                &mut session,
                handler,
            )
        }

        // check unpacked type occurrences
        for (unpacked, syn) in occurrences.unpacked_types() {
            environment.check_unpacked_ocurrences(
                unpacked,
                &syn.span(),
                &mut session,
                handler,
            )
        }

        // check unpacked constant occurrences
        for (unpacked, syn) in occurrences.unpacked_constants() {
            environment.check_unpacked_ocurrences(
                unpacked,
                &syn.span(),
                &mut session,
                handler,
            )
        }
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
        generic_arguments: &GenericArguments<Default>,
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

    fn check_in_generic_arguments(&mut self, args: &GenericArguments<Default>) {
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

    fn check_in_type(&mut self, ty: &r#type::Type<Default>) {
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

    fn check_in_lifetime(&mut self, lt: &lifetime::Lifetime<Default>) {
        match lt {
            lifetime::Lifetime::Parameter(parameter) => {
                self.lifetimes.remove(parameter);
            }

            lifetime::Lifetime::Inference(_)
            | lifetime::Lifetime::Static
            | lifetime::Lifetime::Forall(_) => {}
        }
    }

    fn check_in_constant(&mut self, val: &constant::Constant<Default>) {
        match val {
            constant::Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            constant::Constant::Phantom(_)
            | constant::Constant::Primitive(_)
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
        }
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
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
        mut trait_instantiation: Instantiation<Default>,
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
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        Lifetime::Parameter(LifetimeParameterID {
                            parent: trait_member_id.into(),
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
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        r#type::Type::Parameter(TypeParameterID {
                            parent: trait_member_id.into(),
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
                            parent: trait_member_id.into(),
                            id: tr,
                        }),
                        constant::Constant::Parameter(ConstantParameterID {
                            parent: trait_member_id.into(),
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
                    normalizer: &NoOp,
                },
                &mut Limit::new(&mut session),
            ) {
                Ok(false) => handler.receive(Box::new(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: trait_member_id.into(),
                                id: im_const_id,
                            },
                        trait_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: trait_member_id.into(),
                                id: tr_const_id,
                            },
                    },
                )),
                Ok(true) => {}
                Err(_) => todo!(),
            }
        }
        let implementation_member_environment = Environment {
            premise: &implementation_member_active_premise,
            table: self,
            normalizer: &NoOp,
        };

        // check if the predicates match
        for predicate in &trait_member_sym.generic_declaration().predicates {
            let mut predicate_instantiated = predicate.predicate.clone();
            predicate_instantiated.instantiate(&trait_instantiation);

            match implementation_member_environment.predicate_satisfied(
                &predicate_instantiated,
                true,
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
        let trait_member_environment = Environment {
            premise: &trait_member_active_premise,
            table: self,
            normalizer: &NoOp,
        };

        // check for any extraneous predicates defined in the implementation
        // member that are not in the trait member
        for predicate in
            &implementation_member_sym.generic_declaration().predicates
        {
            match trait_member_environment.predicate_satisfied(
                &predicate.predicate,
                true,
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
        ParentID: Copy + Into<GlobalID>,
        ImplementedID: Copy + Into<GenericID>,
        Definition: 'static,
    >(
        &self,
        implementation_id: ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ImplementedID, Definition>,
            >,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        GenericTemplate<
            ParentID,
            ImplementationTemplate<ImplementedID, Definition>,
        >: Element,
        ID<
            GenericTemplate<
                ParentID,
                ImplementationTemplate<ImplementedID, Definition>,
            >,
        >: Into<GlobalID> + Into<GenericID>,
    {
        let implementation_sym = self.get(implementation_id).unwrap();

        // check if all the generic parameters are used in the
        // implementation arguments
        let unused_generic_parameters =
            UnusedGenericParameters::get_unused_generic_parameters(
                implementation_id.into(),
                &implementation_sym.generic_declaration.parameters,
                &implementation_sym.arguments,
            );
        unused_generic_parameters
            .report_as_unused_generic_parameters_in_implementation(
                implementation_id,
                handler,
            );

        let implemented_sym =
            self.get_generic(implementation_sym.implemented_id.into()).unwrap();

        // early return if the generic parameters count does not match
        if implementation_sym.arguments.lifetimes.len()
            != implemented_sym
                .generic_declaration()
                .parameters
                .lifetimes()
                .len()
            || implementation_sym.arguments.types.len()
                != implemented_sym
                    .generic_declaration()
                    .parameters
                    .types()
                    .len()
            || implementation_sym.arguments.constants.len()
                != implemented_sym
                    .generic_declaration()
                    .parameters
                    .constants()
                    .len()
        {
            return;
        }

        drop(implemented_sym);

        // check if the signature matches the trait definition
        let mut session = session::Default::default();
        let premise =
            self.get_active_premise(implementation_id.into()).unwrap();

        let environment =
            Environment { premise: &premise, table: self, normalizer: &NoOp };

        environment.check_instantiation_predicates_by_generic_arguments(
            implementation_sym.implemented_id.into(),
            implementation_sym.arguments.clone(),
            implementation_sym.span().as_ref().unwrap(),
            true,
            &mut session,
            handler,
        );
    }
}
