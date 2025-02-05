//! Contains the definition of [`Occurrences`].

use std::{borrow::Cow, collections::BTreeSet};

use derive_new::new;
use diagnostic::{
    AdtImplementationIsNotGeneralEnough, ConstantArgumentTypeMismatched,
    ImplementationIsNotGeneralEnough, MismatchedImplementationArguments,
};
use enum_as_inner::EnumAsInner;
use parking_lot::RwLock;
use pernixc_component::implementation::Implementation;
use pernixc_handler::Handler;
use pernixc_resolution::qualified_identifier::Resolution;
use pernixc_source_file::{SourceElement, Span};
use pernixc_syntax::syntax_tree;
use pernixc_table::{
    component::{Input, Parent, SymbolKind},
    diagnostic::Diagnostic,
    query::CyclicDependencyError,
    GlobalID, Table,
};
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    instantiation::Instantiation,
    lifetime::Lifetime,
    predicate::{self, Outlives, PositiveTrait, Predicate},
    r#type::{Primitive, Type},
    variance::Variance,
    visitor::RecursiveIterator,
    where_clause::WhereClause,
    Default,
};
use pernixc_type_system::{
    compatible::Compatibility,
    deduction,
    environment::Environment,
    normalizer,
    predicate::{
        NegativeMarkerSatisfied, NegativeTraitSatisfied,
        PositiveMarkerSatisfied, PositiveTraitSatisfied,
    },
    resolution,
    type_check::TypeCheck,
    AbruptError, LifetimeConstraint, OverflowError, Satisfied, Succeeded,
};

use crate::type_system::{
    diagnostic::{
        OverflowOperation, TypeSystemOverflow, UndecidablePredicate,
        UnsatisfiedPredicate,
    },
    EnvironmentExt, TableExt,
};

pub mod diagnostic;

#[derive(Debug, Default)]
#[allow(missing_docs)]
pub(crate) struct Repr {
    pub types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
    pub lifetimes: Vec<(Lifetime<Default>, syntax_tree::Lifetime)>,
    pub constants: Vec<(Constant<Default>, syntax_tree::Constant)>,

    pub resolutions: Vec<(Resolution<Default>, Span)>,

    pub unpacked_types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
    pub unpacked_constants:
        Vec<(Constant<Default>, syntax_tree::expression::Expression)>,

    pub constant_types: Vec<(Type<Default>, syntax_tree::r#type::Type)>,
}

/// A structure containing the list of all resolution resolved so far in the
/// finalizing process.
///
/// This is primarily used for well-formedness checking of all instantiations
/// made in the program.
#[derive(Debug, Default, derive_more::Deref, derive_more::DerefMut)]
pub(crate) struct Occurrences(RwLock<Repr>);

impl Input for Occurrences {}

/// An observer object for the [`pernixc_resolution::Observer`] that will
/// add the resolution to the [`Occurrences`] component to the referring site
/// symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Observer;

impl pernixc_resolution::Observer<Default> for Observer {
    fn on_resolution_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        resolution: &Resolution<Default>,
        span: &Span,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .resolutions
            .push((resolution.clone(), span.clone()));
    }

    fn on_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .types
            .push((ty.clone(), syntax_tree.clone()));
    }

    fn on_lifetime_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        lifetime: &Lifetime<Default>,
        syntax_tree: &syntax_tree::Lifetime,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .lifetimes
            .push((*lifetime, syntax_tree.clone()));
    }

    fn on_constant_arguments_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<Default>,
        syntax_tree: &syntax_tree::Constant,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .constants
            .push((constant.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_type_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        ty: &Type<Default>,
        syntax_tree: &syntax_tree::r#type::Type,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .unpacked_types
            .push((ty.clone(), syntax_tree.clone()));
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        table: &Table,
        referring_site: GlobalID,
        constant: &Constant<Default>,
        syntax_tree: &syntax_tree::expression::Expression,
        _: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        table
            .get::<Occurrences>(referring_site)
            .write()
            .unpacked_constants
            .push((constant.clone(), syntax_tree.clone()));
    }
}

/// An enumeration of ways the predicate can be erroneous.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub(super) enum PredicateError {
    /// The predicate isn't satisfied.
    Unsatisfied {
        predicate: Predicate<Default>,
        predicate_declaration_span: Option<Span>,
    },

    /// The type system can't determine if the predicate is satisfiable or not.
    Undecidable {
        predicate: Predicate<Default>,
        predicate_declaration_span: Option<Span>,
        overflow_error: OverflowError,
    },

    /// The solved trait/marker implementation is not general enough for the
    /// forall lifetime requirements.
    ImplementationIsNotGeneralEnough {
        resolved_implementation: resolution::Implementation<Default>,
        generic_arguments: GenericArguments<Default>,
        predicate_declaration_span: Option<Span>,
    },
}

impl PredicateError {
    pub(super) fn report(
        self,
        instantiation_span: Span,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) {
        match self {
            Self::Unsatisfied { predicate, predicate_declaration_span } => {
                handler.receive(Box::new(UnsatisfiedPredicate {
                    predicate,
                    instantiation_span,
                    predicate_declaration_span,
                }));
            }

            Self::Undecidable {
                predicate,
                predicate_declaration_span,
                overflow_error,
            } => {
                if predicate.contains_error() {
                    return;
                }

                handler.receive(Box::new(UndecidablePredicate {
                    predicate,
                    predicate_declaration_span,
                    instantiation_span,
                    overflow_error,
                }));
            }

            Self::ImplementationIsNotGeneralEnough {
                resolved_implementation,
                predicate_declaration_span,
                generic_arguments,
            } => {
                if generic_arguments.contains_error() {
                    return;
                }

                handler.receive(Box::new(ImplementationIsNotGeneralEnough {
                    resolvable_implementation_id: resolved_implementation.id,
                    instantiation_span,
                    predicate_declaration_span,
                    generic_arguments,
                }));
            }
        }
    }
}

/// Checker for the well-formedness of the instantiations.
#[derive(new)]
pub(super) struct Checker<'a> {
    environment: &'a Environment<'a, Default, normalizer::NoOp>,
    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
}

impl Checker<'_> {
    fn create_instantiation_for_adt(
        &self,
        adt_implementation_id: GlobalID,
        parent_generic_arguments: &GenericArguments<Default>,
        resolution_span: &Span,
    ) -> Option<Instantiation<Default>> {
        // deduce the generic arguments
        let arguments = self
            .environment
            .table()
            .query::<Implementation>(adt_implementation_id)
            .ok()?
            .generic_arguments
            .clone();

        let result = match self.environment.deduce(
            &GenericArguments::from_default_model(arguments),
            parent_generic_arguments,
        ) {
            Ok(deduced) => deduced,

            Err(deduction::Error::MismatchedGenericArgumentCount(_)) => {
                unreachable!()
            }

            Err(deduction::Error::UnificationFailure(_)) => {
                self.handler.receive(Box::new(
                    MismatchedImplementationArguments {
                        adt_implementation_id,
                        found_generic_arguments: parent_generic_arguments
                            .clone(),
                        instantiation_span: resolution_span.clone(),
                    },
                ));

                return None; // can't continue
            }

            Err(deduction::Error::Abrupt(AbruptError::CyclicDependency(
                CyclicDependencyError,
            ))) => {
                return None;
            }

            Err(deduction::Error::Abrupt(AbruptError::Overflow(
                overflow_error,
            ))) => {
                self.handler.receive(Box::new(TypeSystemOverflow {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: resolution_span.clone(),
                    overflow_error,
                }));

                return None;
            }
        };

        self.environment.check_lifetime_constraints(
            result.constraints.iter(),
            resolution_span,
            self.handler,
        );

        // check if the deduced generic arguments are correct
        self.check_instantiation_predicates(
            adt_implementation_id,
            &result.result.instantiation,
            resolution_span,
        );

        // the implementation is not general enough
        if result.result.is_not_general_enough {
            self.handler.receive(Box::new(
                AdtImplementationIsNotGeneralEnough {
                    adt_implementation_id,
                    generic_arguments: parent_generic_arguments.clone(),
                    instantiation_span: resolution_span.clone(),
                },
            ));
        }

        Some(result.result.instantiation)
    }

    fn check_trait_instantiation(
        &self,
        trait_id: GlobalID,
        generic_arguments: GenericArguments<Default>,
        instantiation_span: &Span,
    ) {
        for error in self.predicate_satisfied(
            Predicate::PositiveTrait(PositiveTrait {
                trait_id,
                is_const: false,
                generic_arguments,
            }),
            None,
        ) {
            error.report(instantiation_span.clone(), self.handler);
        }
    }

    /// Checks if the given `resolution` is well-formed. The errors are reported
    /// to the `handler`.
    #[allow(clippy::too_many_lines, unused)]
    pub(super) fn check_resolution_occurrence(
        &self,
        resolution: &Resolution<Default>,
        resolution_span: &Span,
    ) {
        match resolution {
            Resolution::Module(_)
            | Resolution::PositiveTraitImplementation(_)
            | Resolution::Variant(_) => {}

            Resolution::Generic(generic) => {
                self.check_instantiation_predicates_by_generic_arguments(
                    generic.id,
                    generic.generic_arguments.clone(),
                    resolution_span,
                );
            }

            Resolution::MemberGeneric(member_generic) => {
                // additional adt implementation check

                // the trait implementation doesn't need to be checked here
                // because it can never be referred directly in the source code
                let symbol_kind = *self
                    .environment
                    .table()
                    .get::<SymbolKind>(member_generic.id);

                let adt_implementation_check = match symbol_kind {
                    SymbolKind::AdtImplementationFunction => {
                        Some(GlobalID::new(
                            member_generic.id.target_id,
                            self.environment
                                .table()
                                .get::<Parent>(member_generic.id)
                                .parent
                                .unwrap(),
                        ))
                    }

                    SymbolKind::TraitConstant
                    | SymbolKind::TraitFunction
                    | SymbolKind::TraitType => {
                        let trait_id = GlobalID::new(
                            member_generic.id.target_id,
                            self.environment
                                .table()
                                .get::<Parent>(member_generic.id)
                                .parent
                                .unwrap(),
                        );

                        self.check_trait_instantiation(
                            trait_id,
                            member_generic.parent_generic_arguments.clone(),
                            resolution_span,
                        );

                        None
                    }

                    _ => None,
                };

                // extract the instantiation for the parent
                let mut parent_instantiation =
                    if let Some(adt_implementation_id) =
                        adt_implementation_check
                    {
                        let Some(instantiation) = self
                            .create_instantiation_for_adt(
                                adt_implementation_id,
                                &member_generic.parent_generic_arguments,
                                resolution_span,
                            )
                        else {
                            return;
                        };

                        instantiation
                    } else {
                        let parent_id = GlobalID::new(
                            member_generic.id.target_id,
                            self.environment
                                .table()
                                .get::<Parent>(member_generic.id)
                                .parent
                                .unwrap(),
                        );

                        let Ok(generic_parameters) =
                            self.environment
                                .table()
                                .query::<GenericParameters>(parent_id)
                        else {
                            return;
                        };

                        Instantiation::from_generic_arguments(
                            member_generic.parent_generic_arguments.clone(),
                            parent_id,
                            &generic_parameters,
                        )
                        .expect("should have no mismatched")
                    };

                let Ok(generic_parameters) =
                    self.environment
                        .table()
                        .query::<GenericParameters>(member_generic.id)
                else {
                    return;
                };

                parent_instantiation
                    .append_from_generic_arguments(
                        member_generic.member_generic_arguments.clone(),
                        member_generic.id,
                        &generic_parameters,
                    )
                    .unwrap();

                self.check_instantiation_predicates(
                    member_generic.id,
                    &parent_instantiation,
                    resolution_span,
                );
            }
        }
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    pub(super) fn check_instantiation_predicates_by_generic_arguments(
        &self,
        instantiated: GlobalID,
        generic_arguments: GenericArguments<Default>,
        instantiation_span: &Span,
    ) {
        // convert the generic arguments to an instantiation and delegate the
        // check to the `check_instantiation_predicates` method
        let Ok(generic_parameters) =
            self.environment.table().query::<GenericParameters>(instantiated)
        else {
            return;
        };

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments,
            instantiated,
            &generic_parameters,
        )
        .unwrap();

        self.check_instantiation_predicates(
            instantiated,
            &instantiation,
            instantiation_span,
        );
    }

    /// Do where clause predicates check for the given instantiation. The errors
    /// are reported to the `handler`.
    pub(super) fn check_instantiation_predicates(
        &self,
        instantiated: GlobalID,
        instantiation: &Instantiation<Default>,
        instantiation_span: &Span,
    ) {
        let Ok(where_clause) =
            self.environment.table().query::<WhereClause>(instantiated)
        else {
            return;
        };

        #[allow(clippy::significant_drop_in_scrutinee)]
        for predicate_info in &where_clause.predicates {
            let mut predicate =
                Predicate::from_other_model(predicate_info.predicate.clone());

            predicate.instantiate(instantiation);

            let errors = self
                .predicate_satisfied(predicate, predicate_info.span.clone());

            for error in errors {
                error.report(instantiation_span.clone(), self.handler);
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn check_implementation_satisfied(
        &self,
        implementation_id: GlobalID,
        instantiation: &Instantiation<Default>,
        generic_arguments: &GenericArguments<Default>,
        predicate_declaration_span: Option<Span>,
        mut is_not_general_enough: bool,
    ) -> Vec<PredicateError> {
        let mut errors = Vec::new();

        let Ok(where_clause) =
            self.environment.table().query::<WhereClause>(implementation_id)
        else {
            return errors;
        };

        // check for each predicate in the implementation
        for predicate in &where_clause.predicates {
            let mut predicate_instantiated =
                Predicate::from_other_model(predicate.predicate.clone());

            predicate_instantiated.instantiate(instantiation);

            // if found an outlives predicate where the bound (rhs) is a forall
            // lifetime, then replace it with the static lifetime; since static
            // lifetime outlives all lifetimes. However, if found on the operand
            // (lhs), then report it as `non-general-enough`.

            // check for all lifetime on the operand
            match &predicate_instantiated {
                Predicate::LifetimeOutlives(outlives) => {
                    if outlives.operand.is_forall() {
                        is_not_general_enough = true;
                        continue;
                    }
                }

                Predicate::TypeOutlives(outlives) => {
                    if RecursiveIterator::new(&outlives.operand).any(|x| {
                        x.0.as_lifetime().map_or(false, |x| x.is_forall())
                    }) {
                        is_not_general_enough = true;
                        continue;
                    }
                }

                _ => {}
            }

            // check for all lifetime on the bound and replace it with static
            match &mut predicate_instantiated {
                Predicate::LifetimeOutlives(outlives)
                    if outlives.bound.is_forall() =>
                {
                    outlives.bound = Lifetime::Static;
                }

                Predicate::TypeOutlives(outlives)
                    if outlives.bound.is_forall() =>
                {
                    outlives.bound = Lifetime::Static;
                }

                _ => {}
            }

            errors.extend(self.predicate_satisfied(
                predicate_instantiated,
                predicate.span.clone(),
            ));
        }

        if is_not_general_enough {
            errors.push(PredicateError::ImplementationIsNotGeneralEnough {
                resolved_implementation: resolution::Implementation {
                    instantiation: instantiation.clone(),
                    id: implementation_id,
                    is_not_general_enough,
                },
                generic_arguments: generic_arguments.clone(),
                predicate_declaration_span,
            });
        }

        errors
    }

    fn handle_positive_marker_satisfied(
        &self,
        result: PositiveMarkerSatisfied<Default>,
        pred_generic_arguments: &GenericArguments<Default>,
        predicate_declaration_span: Option<Span>,
    ) -> (BTreeSet<LifetimeConstraint<Default>>, Vec<PredicateError>) {
        match result {
            PositiveMarkerSatisfied::Premise
            | PositiveMarkerSatisfied::Environment
            | PositiveMarkerSatisfied::Cyclic => (BTreeSet::new(), Vec::new()),

            PositiveMarkerSatisfied::Implementation(implementation) => (
                BTreeSet::new(),
                self.check_implementation_satisfied(
                    implementation.id,
                    &implementation.instantiation,
                    pred_generic_arguments,
                    predicate_declaration_span,
                    implementation.is_not_general_enough,
                ),
            ),

            PositiveMarkerSatisfied::Congruence(btree_map) => {
                let mut constraints = BTreeSet::new();
                let mut pred_errors = Vec::new();

                for (_, result) in btree_map {
                    constraints.extend(result.constraints.iter().cloned());

                    let (new_constraints, new_pred_errors) = self
                        .handle_positive_marker_satisfied(
                            result.result.clone(),
                            pred_generic_arguments,
                            predicate_declaration_span.clone(),
                        );

                    constraints.extend(new_constraints);
                    pred_errors.extend(new_pred_errors);
                }

                (constraints, pred_errors)
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn predicate_satisfied(
        &self,
        predicate: Predicate<Default>,
        predicate_declaration_span: Option<Span>,
    ) -> Vec<PredicateError> {
        let (result, mut extra_predicate_error) = match &predicate {
            Predicate::TraitTypeCompatible(eq) => {
                let result = self.environment.compatible(
                    &Type::TraitMember(eq.lhs.clone()),
                    &eq.rhs,
                    Variance::Covariant,
                );

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

                        Err(error) => Err(error),
                    },
                    Vec::new(),
                )
            }

            Predicate::ConstantType(pred) => (
                self.environment.query(pred).map(|x| x.as_deref().cloned()),
                Vec::new(),
            ),

            Predicate::LifetimeOutlives(pred) => {
                return match self.environment.query(pred) {
                    Ok(Some(_)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(AbruptError::CyclicDependency(
                        CyclicDependencyError,
                    )) => Vec::new(),

                    Err(AbruptError::Overflow(overflow_error)) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                            overflow_error,
                        }]
                    }
                };
            }

            Predicate::TypeOutlives(pred) => {
                return match self.environment.query(pred) {
                    Ok(Some(_)) => vec![],

                    Ok(None) => {
                        vec![PredicateError::Unsatisfied {
                            predicate,
                            predicate_declaration_span,
                        }]
                    }

                    Err(AbruptError::CyclicDependency(
                        CyclicDependencyError,
                    )) => Vec::new(),

                    Err(AbruptError::Overflow(overflow_error)) => {
                        vec![PredicateError::Undecidable {
                            predicate,
                            predicate_declaration_span,
                            overflow_error,
                        }]
                    }
                };
            }

            Predicate::TupleType(pred) => (
                self.environment.query(pred).map(|x| x.as_deref().cloned()),
                Vec::new(),
            ),

            Predicate::PositiveMarker(pred) => {
                match self.environment.query(pred) {
                    Ok(None) => (Ok(None), Vec::new()),

                    Ok(Some(result)) => {
                        let (new_constraints, pred_errors) = self
                            .handle_positive_marker_satisfied(
                                result.result.clone(),
                                &pred.generic_arguments,
                                predicate_declaration_span.clone(),
                            );

                        let mut constraints = result.constraints.clone();
                        constraints.extend(new_constraints);

                        (
                            Ok(Some(Succeeded::satisfied_with(constraints))),
                            pred_errors,
                        )
                    }

                    Err(error) => (Err(error), Vec::new()),
                }
            }
            Predicate::PositiveTrait(pred) => {
                match self.environment.query(pred) {
                    Ok(None) => (Ok(None), Vec::new()),
                    Ok(Some(result)) => match result.result.clone() {
                        PositiveTraitSatisfied::Cyclic
                        | PositiveTraitSatisfied::Premise
                        | PositiveTraitSatisfied::Environment => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        PositiveTraitSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &pred.generic_arguments,
                                predicate_declaration_span.clone(),
                                implementation.is_not_general_enough,
                            ),
                        ),
                    },
                    Err(error) => (Err(error), Vec::new()),
                }
            }

            Predicate::NegativeTrait(pred) => {
                match self.environment.query(pred) {
                    Ok(None) => (Ok(None), Vec::new()),

                    Ok(Some(result)) => match result.result.clone() {
                        NegativeTraitSatisfied::Premise
                        | NegativeTraitSatisfied::UnsatisfiedPositive => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        NegativeTraitSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &pred.generic_arguments,
                                predicate_declaration_span.clone(),
                                implementation.is_not_general_enough,
                            ),
                        ),
                    },

                    Err(error) => (Err(error), Vec::new()),
                }
            }
            Predicate::NegativeMarker(negative) => {
                match self.environment.query(negative) {
                    Ok(Some(result)) => match result.result.clone() {
                        NegativeMarkerSatisfied::UnsatisfiedPositive
                        | NegativeMarkerSatisfied::Premise => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            Vec::new(),
                        ),

                        NegativeMarkerSatisfied::Implementation(
                            implementation,
                        ) => (
                            Ok(Some(Succeeded::satisfied_with(
                                result.constraints.clone(),
                            ))),
                            self.check_implementation_satisfied(
                                implementation.id,
                                &implementation.instantiation,
                                &negative.generic_arguments,
                                predicate_declaration_span.clone(),
                                implementation.is_not_general_enough,
                            ),
                        ),
                    },
                    Ok(None) => (Ok(None), Vec::new()),

                    Err(error) => (Err(error), Vec::new()),
                }
            }
        };

        match result {
            Ok(Some(Succeeded { result: Satisfied, constraints })) => {
                // if do_outlives_check is false, then we don't need to check

                for constraint in constraints {
                    match constraint {
                        LifetimeConstraint::LifetimeOutlives(pred) => {
                            match self.environment.query(&pred) {
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
                                Err(AbruptError::Overflow(overflow_error)) => {
                                    extra_predicate_error.push(
                                        PredicateError::Undecidable {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    pred,
                                                ),

                                            predicate_declaration_span: None,
                                            overflow_error,
                                        },
                                    );
                                }

                                Err(AbruptError::CyclicDependency(
                                    CyclicDependencyError,
                                ))
                                | Ok(Some(_)) => {}
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

            Err(AbruptError::CyclicDependency(CyclicDependencyError)) => {
                extra_predicate_error
            }

            Err(AbruptError::Overflow(overflow_error)) => {
                extra_predicate_error.push(PredicateError::Undecidable {
                    predicate,
                    predicate_declaration_span,
                    overflow_error,
                });

                extra_predicate_error
            }
        }
    }

    /// Do predicates check for the given type occurrences.
    #[allow(clippy::too_many_lines)]
    pub(super) fn check_type_ocurrence(
        &self,
        ty: &Type<Default>,
        instantiation_span: &Span,
    ) {
        match ty {
            Type::Error(_)
            | Type::Tuple(_)
            | Type::Phantom(_)
            | Type::Pointer(_)
            | Type::Primitive(_)
            | Type::Parameter(_)
            | Type::Symbol(_)
            | Type::TraitMember(_)
            | Type::MemberSymbol(_)
            | Type::FunctionSignature(_)
            | Type::Inference(_) => { /* no additional check */ }

            Type::Reference(reference) => {
                let outlives = Outlives::new(
                    (*reference.pointee).clone(),
                    reference.lifetime,
                );

                match self.environment.query(&outlives) {
                    Err(AbruptError::CyclicDependency(
                        CyclicDependencyError,
                    ))
                    | Ok(Some(_)) => {}

                    Ok(None) => {
                        self.handler.receive(Box::new(UnsatisfiedPredicate {
                            predicate: Predicate::TypeOutlives(outlives),
                            instantiation_span: instantiation_span.clone(),
                            predicate_declaration_span: None,
                        }));
                    }

                    Err(AbruptError::Overflow(overflow_error)) => {
                        self.handler.receive(Box::new(UndecidablePredicate {
                            predicate: Predicate::TypeOutlives(outlives),
                            predicate_declaration_span: None,
                            instantiation_span: instantiation_span.clone(),
                            overflow_error,
                        }));
                    }
                }
            }

            Type::Array(array) => {
                let expected_type = Type::Primitive(Primitive::Usize);
                let type_check =
                    TypeCheck::new(array.length.clone(), expected_type.clone());

                match self.environment.query(&type_check) {
                    Ok(Some(result)) => {
                        for LifetimeConstraint::LifetimeOutlives(outlives) in
                            &result.constraints
                        {
                            match self.environment.query(outlives) {
                                Ok(None) => {
                                    self.handler.receive(Box::new(
                                        UnsatisfiedPredicate {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    outlives.clone(),
                                                ),
                                            predicate_declaration_span: None,
                                            instantiation_span:
                                                instantiation_span.clone(),
                                        },
                                    ));
                                }

                                Err(AbruptError::CyclicDependency(
                                    CyclicDependencyError,
                                ))
                                | Ok(Some(_)) => {}

                                Err(AbruptError::Overflow(overflow_error)) => {
                                    self.handler.receive(Box::new(
                                        UndecidablePredicate {
                                            predicate:
                                                Predicate::LifetimeOutlives(
                                                    outlives.clone(),
                                                ),
                                            predicate_declaration_span: None,
                                            instantiation_span:
                                                instantiation_span.clone(),
                                            overflow_error,
                                        },
                                    ));
                                }
                            }
                        }
                    }

                    Ok(None) => {
                        self.handler.receive(Box::new(
                            ConstantArgumentTypeMismatched {
                                span: instantiation_span.clone(),
                                expected_type,
                                constant_argument: array.length.clone(),
                            },
                        ));
                    }

                    Err(AbruptError::CyclicDependency(
                        CyclicDependencyError,
                    )) => {}

                    Err(AbruptError::Overflow(overflow_error)) => {
                        self.handler.receive(Box::new(TypeSystemOverflow {
                            operation: OverflowOperation::TypeCheck,
                            overflow_span: instantiation_span.clone(),
                            overflow_error,
                        }));
                    }
                }
            }
        }
    }

    #[allow(unused)]
    fn check_unpacked_ocurrences(
        &self,
        unpacked_term: Type<Default>,
        instantiation_span: &Span,
    ) {
        let tuple_predicate = predicate::Tuple(unpacked_term);

        let errors = self.predicate_satisfied(tuple_predicate.into(), None);

        for error in errors {
            error.report(instantiation_span.clone(), self.handler);
        }
    }
}

pub(super) fn check_occurrences(
    table: &Table,
    global_id: GlobalID,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) {
    let active_premise = table.get_active_premise(global_id);
    let environment = Environment::new(
        Cow::Borrowed(&active_premise),
        table,
        normalizer::NO_OP,
    );
    let occurrences = table.get::<Occurrences>(global_id);
    let occurrences = occurrences.read();

    let checker = Checker { environment: &environment, handler };

    // check resolution occurrences
    for (resolution, span) in &occurrences.resolutions {
        checker.check_resolution_occurrence(resolution, span);
    }

    // check type occurrences
    for (ty, syn) in &occurrences.types {
        checker.check_type_ocurrence(ty, &syn.span());
    }

    // check unpacked type occurrences
    for (unpacked, syn) in &occurrences.unpacked_types {
        checker.check_unpacked_ocurrences(unpacked.clone(), &syn.span());
    }

    // check unpacked constant occurrences
    for _ in &occurrences.unpacked_constants {
        // TODO: check that the type of constant must be a tuple
    }
}

/*
impl Table<Building<RwLockContainer, Finalizer>> {
    /// Checks the well-formedness of the where clause predicates
    #[allow(unused)]
    pub fn check_where_clause(
        &self,
        id: ItemID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        if GenericID::try_from(id).is_err() {
            return;
        };

        let observer = TypeSystem::new(id, handler);
        let (_, environment_errors) = Environment::new_with(
            self.get_active_premise::<Default>(id).unwrap(),
            self,
            normalizer::NO_OP,
            &observer,
        );
        let active_premise_with_span = self
            .get_active_premise_predicates_with_span::<Default>(id)
            .unwrap();

        for error in environment_errors {
            match error {
                environment::Error::AmbiguousPredicates(predicates) => {
                    let spans = predicates
                        .iter()
                        .flat_map(|x| {
                            active_premise_with_span.get(x).unwrap().iter()
                        })
                        .cloned()
                        .collect::<Vec<_>>();

                    handler.receive(Box::new(AmbiguousPredicates {
                        predicates,
                        predicate_declaration_spans: spans,
                    }));
                }

                environment::Error::DefinintePremise(definite) => {
                    let span = active_premise_with_span.get(&definite).unwrap();

                    for span in span {
                        handler.receive(Box::new(DefinitePremisePredicate {
                            predicate: definite.clone(),
                            span: span.clone(),
                        }));
                    }
                }

                environment::Error::RecursiveTraitTypeEqualityPredicate(
                    pred,
                ) => {
                    let pred = Predicate::TraitTypeEquality(pred);
                    let spans =
                        active_premise_with_span.get(&pred).unwrap().clone();

                    handler.receive(Box::new(RecursiveTraitTypeEquality {
                        trait_type_equality: pred
                            .into_trait_type_equality()
                            .unwrap(),
                        predicate_declaration_spans: spans,
                    }));
                }

                environment::Error::Overflow(pred, _) => {
                    let spans = active_premise_with_span.get(&pred).unwrap();

                    for span in spans {
                        handler.receive(Box::new(
                            OverflowCalculatingRequirementForInstantiation {
                                instantiation_span: span.clone(),
                            },
                        ));
                    }
                }
            }
        }
    }

    /// Checks if the occurrences of symbols are valid (i.e. they satisfy the
    /// where clause predicates).
    #[allow(unused)]
    pub fn check_occurrences(
        &self,
        id: ItemID,
        occurrences: &Occurrences,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
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
                implementation_id: implementation_kind_id.into(),
            }));
        }

        for unused_ty in &self.types {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_ty.id.into(),
                implementation_id: implementation_kind_id.into(),
            }));
        }

        for unused_val in &self.constants {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_val.id.into(),
                implementation_id: implementation_kind_id.into(),
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
            r#type::Type::MemberSymbol(symbol) => {
                self.check_in_generic_arguments(
                    &symbol.parent_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &symbol.member_generic_arguments,
                );
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
                            parent: implementation_member_id.into(),
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
                            parent: implementation_member_id.into(),
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
                            parent: implementation_member_id.into(),
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
            normalizer::NO_OP,
            observer::NO_OP,
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

                Err(_) | Ok(None) => handler.receive(Box::new(
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
                implementation_member_id.into(),
                handler,
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
                query_site: implementation_member_active_premise.query_site,
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
            normalizer::NO_OP,
            observer::NO_OP,
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
                trait_member_id.into(),
                handler,
            ) {
                error.report(
                    implementation_member_sym.span().cloned().unwrap(),
                    handler,
                );
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
        ParentID: Copy + Into<ItemID>,
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
        >: Into<ItemID> + Into<GenericID>,
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

        let (environment, _) = Environment::new_with(
            premise,
            self,
            normalizer::NO_OP,
            observer::NO_OP,
        );

        environment.check_instantiation_predicates_by_generic_arguments(
            implementation_sym.implemented_id.into(),
            implementation_sym.arguments.clone(),
            implementation_sym.span().as_ref().unwrap(),
            true,
            implementation_id.into(),
            handler,
        );
    }
}
*/

#[cfg(test)]
mod test;
