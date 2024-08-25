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
        self, AdtImplementationIsNotGeneralEnough, AmbiguousPredicates, Error,
        ExtraneousTraitMemberPredicate,
        MismatchedGenericParameterCountInImplementation,
        MismatchedImplementationArguments,
        MismatchedImplementationConstantTypeParameter,
        TraitImplementationIsNotGeneralEnough, UndecidablePredicate,
        UnsatisifedPredicate, UnusedGenericParameterInImplementation,
    },
    symbol::{
        table::{
            self,
            representation::{Element, Index, RwLockContainer},
            resolution, Building, State, Table,
        },
        ConstantParameter, ConstantParameterID, Generic, GenericID,
        GenericParameter, GenericParameters, GenericTemplate, GlobalID,
        ImplementationTemplate, LifetimeParameter, LifetimeParameterID,
        TraitImplementationMemberID, TraitMemberID, TypeParameter,
        TypeParameterID,
    },
    type_system::{
        compatible::{Compatibility, Compatible},
        deduction,
        environment::{self, Environment},
        instantiation::{self, Instantiation},
        model::{Default, Model},
        normalizer::{Normalizer, NO_OP},
        observer::{self, Observer},
        predicate::{self, Outlives, Predicate, Tuple},
        simplify,
        term::{
            self, constant,
            lifetime::{self, Lifetime},
            r#type, GenericArguments, Term,
        },
        type_check::TypeCheck,
        variance::Variance,
        Compute, LifetimeConstraint, OverflowError, Premise, Satisfied,
        Succeeded,
    },
};

/// An enumeration of ways the predicate can be erroneous.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PredicateError<M: Model> {
    /// The predicate isn't satisfied.
    Unsatisfied {
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
    },

    /// The type system can't determine if the predicate is satisfiable or not.
    Undecidable {
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
    },

    /// The solved trait implementation is not general enough for the forall
    /// lifetime requirements.
    TraitImplementationIsNotGeneralEnough {
        required_trait_predicate: predicate::Trait<M>,
        resolved_implementation: predicate::Implementation<M>,
        predicate_declaration_span: Option<Span>,
    },
}

impl<M: Model> PredicateError<M>
where
    predicate::Predicate<M>: table::Display<table::Suboptimal>,

    M::LifetimeInference: table::Display<table::Suboptimal>,
    M::TypeInference: table::Display<table::Suboptimal>,
    M::ConstantInference: table::Display<table::Suboptimal>,
{
    fn report(
        self,
        instantiation_span: Span,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match self {
            Self::Unsatisfied { predicate, predicate_declaration_span } => {
                handler.receive(Box::new(UnsatisifedPredicate {
                    predicate,
                    instantiation_span,
                    predicate_declaration_span,
                }));
            }

            Self::Undecidable { predicate, predicate_declaration_span } => {
                if predicate.contains_error() {
                    return;
                }

                handler.receive(Box::new(UndecidablePredicate {
                    instantiation_span,
                    predicate,
                    predicate_declaration_span,
                }));
            }

            Self::TraitImplementationIsNotGeneralEnough {
                required_trait_predicate,
                resolved_implementation,
                predicate_declaration_span,
            } => {
                if required_trait_predicate.contains_error() {
                    return;
                }

                handler.receive(Box::new(
                    TraitImplementationIsNotGeneralEnough {
                        positive_trait_implementation_id:
                            resolved_implementation.id,
                        required_trait_predicate,
                        instantiation_span,
                        predicate_declaration_span,
                    },
                ));
            }
        }
    }
}

impl<'a, M: Model, T: State, N: Normalizer<M, T>, O: Observer<M, T>>
    Environment<'a, M, T, N, O>
where
    predicate::Predicate<M>: table::Display<table::Suboptimal>,

    M::LifetimeInference: table::Display<table::Suboptimal>,
    M::TypeInference: table::Display<table::Suboptimal>,
    M::ConstantInference: table::Display<table::Suboptimal>,
{
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
    #[allow(clippy::too_many_lines)]
    pub(super) fn check_resolution_occurrence(
        &self,
        resolution: &resolution::Resolution<M>,
        resolution_span: &Span,
        do_outlives_check: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match resolution {
            resolution::Resolution::Module(_)
            | resolution::Resolution::Variant(_) => {}

            resolution::Resolution::Generic(generic) => {
                // check for trait predicates
                if let resolution::GenericID::Trait(trait_id) = generic.id {
                    let predicate =
                        predicate::Predicate::Trait(predicate::Trait {
                            id: trait_id,
                            is_const: false,
                            generic_arguments: generic
                                .generic_arguments
                                .clone(),
                        });

                    let errors = self.predicate_satisfied(
                        predicate,
                        None,
                        do_outlives_check,
                    );

                    for error in errors {
                        error.report(resolution_span.clone(), handler);
                    }
                }

                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id.into(),
                    generic.generic_arguments.clone(),
                    resolution_span,
                    do_outlives_check,
                    handler,
                );
            }

            resolution::Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let adt_implementation_check = match member_generic.id {
                    resolution::MemberGenericID::AdtImplementationFunction(
                        id,
                    ) => Some(self.table().get(id).unwrap().parent_id),

                    _ => None,
                };

                // extract the instantiation for the parent
                let mut parent_instantiation =
                    if let Some(adt_implementation_id) =
                        adt_implementation_check
                    {
                        // deduce the generic arguments

                        let adt_implementation =
                            self.table().get(adt_implementation_id).unwrap();

                        let result = match GenericArguments::from_default_model(
                        adt_implementation.arguments.clone(),
                    )
                    .deduce(&member_generic.parent_generic_arguments, self)
                    {
                        Ok(deduced) => deduced,

                        Err(
                            deduction::Error::MismatchedGenericArgumentCount(_),
                        ) => {
                            unreachable!()
                        }

                        Err(deduction::Error::UnificationFailure(_)) => {
                            handler.receive(Box::new(
                                MismatchedImplementationArguments {
                                    adt_implementation_id,
                                    found_generic_arguments: member_generic
                                        .parent_generic_arguments
                                        .clone(),
                                    instantiation_span: resolution_span.clone(),
                                },
                            ));

                            return; // can't continue
                        }

                        Err(deduction::Error::TypeSystem(_)) => {
                            todo!("report overflow")
                        }
                    };

                        self.check_lifetime_constraints(
                            result.constraints,
                            resolution_span,
                            handler,
                        );

                        // check if the deduced generic arguments are correct
                        self.check_instantiation_predicates(
                            adt_implementation_id.into(),
                            &result.result.instantiation,
                            resolution_span,
                            do_outlives_check,
                            handler,
                        );

                        // the implementation is not general enough
                        if result.result.is_not_general_enough {
                            handler.receive(Box::new(
                                AdtImplementationIsNotGeneralEnough {
                                    adt_implementation_id,
                                    generic_arguments: member_generic
                                        .parent_generic_arguments
                                        .clone(),
                                    instantiation_span: resolution_span.clone(),
                                },
                            ));
                        }

                        result.result.instantiation
                    } else {
                        let parent_id = GenericID::try_from(
                            self.table()
                                .get_global(member_generic.id.into())
                                .unwrap()
                                .parent_global_id()
                                .unwrap(), // should have parent
                        )
                        .unwrap(); // parent should be a generic

                        Instantiation::from_generic_arguments(
                            member_generic.parent_generic_arguments.clone(),
                            parent_id,
                            &self
                                .table()
                                .get_generic(parent_id)
                                .unwrap()
                                .generic_declaration()
                                .parameters,
                        )
                        .unwrap() // should have no mismatched
                    };

                parent_instantiation
                    .append_from_generic_arguments(
                        member_generic.generic_arguments.clone(),
                        member_generic.id.into(),
                        &self
                            .table()
                            .get_generic(member_generic.id.into())
                            .unwrap()
                            .generic_declaration()
                            .parameters,
                    )
                    .unwrap();

                self.check_instantiation_predicates(
                    member_generic.id.into(),
                    &parent_instantiation,
                    resolution_span,
                    do_outlives_check,
                    handler,
                );
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
    pub(super) fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: GenericID,
        generic_arguments: GenericArguments<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
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
                    .table()
                    .get_generic(instantiated)
                    .unwrap()
                    .generic_declaration()
                    .parameters,
            )
            .unwrap(),
            instantiation_span,
            do_outlives_check,
            handler,
        );
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
    pub(super) fn check_instantiation_predicates(
        &self,
        instantiated: GenericID,
        instantiation: &Instantiation<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        predicate::Predicate<M>: table::Display<table::Suboptimal>,
    {
        // get all the predicates and instantiate them with the given generic
        // arguments
        let instantiated_sym = self.table().get_generic(instantiated).unwrap();

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &instantiated_sym.generic_declaration().predicates
        {
            let mut predicate =
                Predicate::from_default_model(predicate_info.predicate.clone());

            predicate.instantiate(instantiation);

            let errors = self.predicate_satisfied(
                predicate,
                predicate_info.span.clone(),
                do_outlives_check,
            );

            for error in errors {
                error.report(instantiation_span.clone(), handler);
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn predicate_satisfied(
        &self,
        predicate: Predicate<M>,
        predicate_declaration_span: Option<Span>,
        do_outlives_check: bool,
    ) -> Vec<PredicateError<M>> {
        let (result, mut extra_predicate_error) = match &predicate {
            predicate::Predicate::TraitTypeEquality(eq) => {
                let result = r#type::Type::TraitMember(eq.lhs.clone())
                    .compatible(&eq.rhs, Variance::Covariant, self);

                (
                    match result {
                        Ok(Some(Succeeded {
                            result: Compatibility { forall_lifetime_errors, .. },
                            constraints,
                        })) => {
                            if forall_lifetime_errors.is_empty() {
                                Ok(Some(Succeeded::satisfied_with(constraints)))
                            } else {
                                Ok(None)
                            }
                        }

                        Ok(None) => Ok(None),

                        Err(OverflowError) => Err(OverflowError),
                    },
                    Vec::new(),
                )
            }
            predicate::Predicate::ConstantType(pred) => {
                (pred.query(self), Vec::new())
            }
            predicate::Predicate::LifetimeOutlives(pred) => {
                if !do_outlives_check {
                    return Vec::new();
                }

                return match pred.query(self) {
                    Ok(Some(Satisfied)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(OverflowError) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }
                };
            }
            predicate::Predicate::TypeOutlives(pred) => {
                if !do_outlives_check {
                    return Vec::new();
                }

                return match pred.query(self) {
                    Ok(Some(Satisfied)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(OverflowError) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }
                };
            }
            predicate::Predicate::TupleType(pred) => {
                (pred.query(self), Vec::new())
            }
            predicate::Predicate::TupleConstant(pred) => {
                (pred.query(self), Vec::new())
            }
            predicate::Predicate::Trait(pred) => match pred.query(self) {
                Ok(None) => (Ok(None), Vec::new()),
                Ok(Some(Succeeded { result, constraints })) => match result {
                    predicate::TraitSatisfied::ByPremise
                    | predicate::TraitSatisfied::ByTraitContext => (
                        Ok(Some(Succeeded::satisfied_with(constraints))),
                        Vec::new(),
                    ),

                    predicate::TraitSatisfied::ByImplementation(
                        implementation,
                    ) => {
                        let mut errors = Vec::new();

                        if implementation.is_not_general_enough {
                            errors.push(PredicateError::TraitImplementationIsNotGeneralEnough {
                                required_trait_predicate: pred.clone(),
                                resolved_implementation: implementation.clone(),
                                predicate_declaration_span: predicate_declaration_span.clone(),
                            });
                        }

                        let implementation_sym =
                            self.table().get(implementation.id).unwrap();

                        // check for each predicate in the implementation
                        for predicate in
                            &implementation_sym.generic_declaration().predicates
                        {
                            let mut predicate_instantiated =
                                Predicate::from_default_model(
                                    predicate.predicate.clone(),
                                );

                            predicate_instantiated
                                .instantiate(&implementation.instantiation);

                            errors.extend(self.predicate_satisfied(
                                predicate_instantiated,
                                predicate.span.clone(),
                                do_outlives_check,
                            ));
                        }

                        (
                            Ok(Some(Succeeded::satisfied_with(constraints))),
                            errors,
                        )
                    }
                },
                Err(OverflowError) => (Err(OverflowError), Vec::new()),
            },
        };

        match result {
            Ok(Some(Succeeded { result: Satisfied, constraints })) => {
                // if do_outlives_check is false, then we don't need to check
                if !do_outlives_check {
                    return Vec::new();
                }

                for constraint in constraints {
                    match constraint {
                        LifetimeConstraint::LifetimeOutlives(pred) => {
                            match pred.query(self) {
                                Ok(None) => {
                                    extra_predicate_error.push(
                                        PredicateError::Unsatisfied {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    pred,
                                                ),

                                            predicate_declaration_span: None,
                                        },
                                    );
                                }
                                Err(_) => {
                                    extra_predicate_error.push(
                                        PredicateError::Undecidable {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    pred,
                                                ),

                                            predicate_declaration_span: None,
                                        },
                                    );
                                }

                                Ok(Some(_)) => {}
                            }
                        }
                    }
                }

                extra_predicate_error
            }

            Ok(None) => {
                extra_predicate_error.push(PredicateError::Unsatisfied {
                    predicate,
                    predicate_declaration_span,
                });

                extra_predicate_error
            }

            Err(OverflowError) => {
                extra_predicate_error.push(PredicateError::Undecidable {
                    predicate,
                    predicate_declaration_span,
                });

                extra_predicate_error
            }
        }
    }

    /// Do predicates check for the given type occurrences.
    pub(super) fn check_type_ocurrence(
        &self,
        ty: &r#type::Type<M>,
        instantiation_span: &Span,
        do_outlives_check: bool,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        match ty {
            r#type::Type::Error(_)
            | term::r#type::Type::Tuple(_)
            | term::r#type::Type::Local(_)
            | term::r#type::Type::Phantom(_)
            | term::r#type::Type::Pointer(_)
            | term::r#type::Type::Primitive(_)
            | term::r#type::Type::Parameter(_)
            | term::r#type::Type::Symbol(_)
            | term::r#type::Type::TraitMember(_)
            | term::r#type::Type::Inference(_) => { /* no additional check */ }

            term::r#type::Type::Reference(reference) => {
                if do_outlives_check {
                    let outlives = Outlives {
                        operand: (*reference.pointee).clone(),
                        bound: reference.lifetime.clone(),
                    };

                    match outlives.query(self) {
                        Ok(Some(Satisfied)) => {}

                        Ok(None) => {
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

                        Err(OverflowError) => {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: instantiation_span.clone(),
                                predicate: predicate::Predicate::TypeOutlives(
                                    outlives,
                                ),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
            }

            term::r#type::Type::Array(array) => {
                let expected_type =
                    r#type::Type::Primitive(r#type::Primitive::Usize);
                let type_check =
                    TypeCheck::new(array.length.clone(), expected_type);

                match type_check.query(self) {
                    Ok(Some(Succeeded { result: Satisfied, .. })) => {}

                    Ok(None) => todo!("report unsatisfied"),

                    Err(_) => {
                        todo!("report undecidable")
                    }
                }
            }
        }
    }

    fn check_unpacked_ocurrences<U: Term<Model = M> + 'a>(
        &self,
        unpacked_term: U,
        instantiation_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        predicate::Predicate<M>:
            From<Tuple<U>> + table::Display<table::Suboptimal>,
    {
        let tuple_predicate = Tuple(unpacked_term);

        let errors =
            self.predicate_satisfied(tuple_predicate.into(), None, true);

        for error in errors {
            error.report(instantiation_span.clone(), handler);
        }
    }

    /// Checks if the gien list of lifetime constraints are satisfiable or not.
    ///
    /// If the lifetime constraints are not satisfiable, then the errors are
    /// reported to the `handler`.
    pub(super) fn check_lifetime_constraints(
        &self,
        lifetime_constraints: impl IntoIterator<Item = LifetimeConstraint<M>>,
        instantiation_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for lifetime_constraint in lifetime_constraints {
            match lifetime_constraint {
                LifetimeConstraint::LifetimeOutlives(outlives) => {
                    match outlives.query(self) {
                        Ok(Some(Satisfied)) => {}

                        Ok(None) => {
                            handler.receive(Box::new(UnsatisifedPredicate {
                                predicate: Predicate::LifetimeOutlives(
                                    outlives,
                                ),
                                instantiation_span: instantiation_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }

                        Err(OverflowError) => {
                            handler.receive(Box::new(UndecidablePredicate {
                                instantiation_span: instantiation_span.clone(),
                                predicate:
                                    predicate::Predicate::LifetimeOutlives(
                                        outlives,
                                    ),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
            }
        }
    }

    /// Simplifies the given [`r#type::Type`] term and immediately checks for
    /// the lifetime constraints coming with simplification.
    ///
    /// The errors (if any) are reported to the `handler`.
    pub(super) fn simplify_and_check_lifetime_constraints(
        &self,
        ty: &r#type::Type<M>,
        instantiation_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> r#type::Type<M> {
        let Succeeded { result: simplified, constraints } =
            simplify::simplify(ty, self);

        self.check_lifetime_constraints(
            constraints,
            instantiation_span,
            handler,
        );

        simplified
    }
}

impl Table<Building<RwLockContainer, Finalizer>> {
    pub(super) fn check_where_clause(
        &self,
        id: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        if GenericID::try_from(id).is_err() {
            return;
        };

        let (_, environment_errors) = Environment::new_with(
            self.get_active_premise::<Default>(id).unwrap(),
            self,
            &NO_OP,
            &observer::NO_OP,
        );

        for error in environment_errors {
            match error {
                environment::Error::AmbiguousPredicates(predicates) => handler
                    .receive(Box::new(AmbiguousPredicates {
                        predicates,
                        occurred_at_global_id: id,
                    })),

                environment::Error::DefinintePremise(_) => todo!(),

                environment::Error::RecursiveTraitTypeEqualityPredicate(_) => {
                    todo!()
                }

                environment::Error::Overflow(_, _) => {
                    todo!()
                }
            }
        }
    }

    /// Checks if the occurrences of symbols are valid (i.e. they satisfy the
    /// where clause predicates).
    pub(super) fn check_occurrences(
        &self,
        id: GlobalID,
        occurrences: &Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let active_premise = self.get_active_premise(id).unwrap();
        let (environment, _) = Environment::new_with(
            active_premise,
            self,
            &NO_OP,
            &observer::NO_OP,
        );

        // check resolution occurrences
        for (resolution, generic_identifier) in occurrences.resolutions() {
            environment.check_resolution_occurrence(
                resolution,
                &generic_identifier.span(),
                true,
                handler,
            );
        }

        // check type occurrences
        for (ty, syn) in occurrences.types() {
            environment.check_type_ocurrence(ty, &syn.span(), true, handler);
        }

        // check unpacked type occurrences
        for (unpacked, syn) in occurrences.unpacked_types() {
            environment.check_unpacked_ocurrences(
                unpacked.clone(),
                &syn.span(),
                handler,
            );
        }

        // check unpacked constant occurrences
        for (unpacked, syn) in occurrences.unpacked_constants() {
            environment.check_unpacked_ocurrences(
                unpacked.clone(),
                &syn.span(),
                handler,
            );
        }
    }
}

/// Contains all the unused generic parameters in a generic arguments.
struct UnusedGenericParameters {
    lifetimes: HashSet<LifetimeParameterID>,
    types: HashSet<TypeParameterID>,
    constants: HashSet<ConstantParameterID>,
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
    pub(super) fn get_unused_generic_parameters(
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
    pub(super) fn report_as_unused_generic_parameters_in_implementation<
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
                    self.check_in_type(&ty.term);
                }
            }
            r#type::Type::Local(local) => {
                self.check_in_type(&local.0);
            }
            r#type::Type::Phantom(phantom) => {
                self.check_in_type(&phantom.0);
            }

            r#type::Type::TraitMember(_)
            | r#type::Type::Inference(_)
            | r#type::Type::Error(_)
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
            | lifetime::Lifetime::Error(_)
            | lifetime::Lifetime::Forall(_) => {}
        }
    }

    fn check_in_constant(&mut self, val: &constant::Constant<Default>) {
        match val {
            constant::Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            constant::Constant::Phantom
            | constant::Constant::Error(_)
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
                    self.check_in_constant(&element.term);
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

        let (environment, _) = Environment::new_with(
            implementation_member_active_premise.clone(),
            self,
            &NO_OP,
            &observer::NO_OP,
        );

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

            match im_const_param.r#type.compatible(
                &tr_const_ty,
                Variance::Covariant,
                &environment,
            ) {
                Ok(Some(_)) => {}

                Ok(None) => handler.receive(Box::new(
                    MismatchedImplementationConstantTypeParameter {
                        implementation_member_constant_parameter_id:
                            ConstantParameterID {
                                parent: implementation_member_id.into(),
                                id: im_const_id,
                            },
                        trait_member_constant_parameter_id: {
                            ConstantParameterID {
                                parent: trait_member_id.into(),
                                id: tr_const_id,
                            }
                        },
                    },
                )),

                Err(_) => todo!("report undecidable error"),
            }
        }

        // check if the predicates match
        for predicate in &trait_member_sym.generic_declaration().predicates {
            let mut predicate_instantiated = predicate.predicate.clone();
            predicate_instantiated.instantiate(&trait_instantiation);

            for error in environment.predicate_satisfied(
                predicate_instantiated,
                predicate.span.clone(),
                true,
            ) {
                error.report(
                    implementation_member_sym.span().cloned().unwrap(),
                    handler,
                );
            }
        }

        let trait_member_active_premise = {
            let stub = self.get_active_premise(trait_member_id.into()).unwrap();

            Premise {
                trait_context: implementation_member_active_premise
                    .trait_context,
                predicates: stub
                    .predicates
                    .into_iter()
                    .map(|mut x| {
                        x.instantiate(&trait_instantiation);
                        x
                    })
                    .collect(),
            }
        };

        let (environment, _) = Environment::new_with(
            trait_member_active_premise,
            self,
            &NO_OP,
            &observer::NO_OP,
        );

        // check for any extraneous predicates defined in the implementation
        // member that are not in the trait member
        for predicate_decl in
            &implementation_member_sym.generic_declaration().predicates
        {
            for error in environment.predicate_satisfied(
                predicate_decl.predicate.clone(),
                predicate_decl.span.clone(),
                true,
            ) {
                match error {
                    PredicateError::Undecidable { predicate, .. } => {
                        if predicate.contains_error() {
                            continue;
                        }

                        handler.receive(Box::new(UndecidablePredicate {
                            instantiation_span: implementation_member_sym
                                .span()
                                .cloned()
                                .unwrap(),
                            predicate,
                            predicate_declaration_span: None,
                        }));
                    }

                    PredicateError::Unsatisfied {
                        predicate,
                        predicate_declaration_span,
                    } => {
                        if predicate.contains_error() {
                            continue;
                        }

                        handler.receive(Box::new(
                            ExtraneousTraitMemberPredicate {
                                trait_implementation_member_id:
                                    implementation_member_id,
                                predicate,
                                predicate_span: predicate_declaration_span,
                            },
                        ));
                    }

                    PredicateError::TraitImplementationIsNotGeneralEnough {
                        ..
                    } => todo!("report this error"),
                }
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
        let premise =
            self.get_active_premise(implementation_id.into()).unwrap();

        let (environment, _) =
            Environment::new_with(premise, self, &NO_OP, &observer::NO_OP);

        environment.check_instantiation_predicates_by_generic_arguments(
            implementation_sym.implemented_id.into(),
            implementation_sym.arguments.clone(),
            implementation_sym.span().as_ref().unwrap(),
            true,
            handler,
        );
    }
}

// #[cfg(test)]
// mod tests;
