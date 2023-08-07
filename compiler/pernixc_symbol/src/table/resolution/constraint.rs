use pernixc_source::Span;
use pernixc_system::diagnostic::Handler;

use super::BoundChecking;
use crate::{
    error::{
        self, LifetimeDoesNotOutlive, TraitBoundNotSatisfied, TraitTypeBoundNotSatisfied,
        TypeDoesNotOutliveLifetimeArgument,
    },
    table::Table,
    ty, GenericParameters, Genericable, GenericableID, LifetimeArgument, Substitution, WhereClause,
};

impl Table {
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    fn substitution_outlives(
        &self,
        required_lifetime_argument: LifetimeArgument,
        substitution: &Substitution,
        parent_generic_parameters: &GenericParameters,
        bound_checking: BoundChecking,
        active_where_clause: &WhereClause,
        ty: &ty::Type,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        for lifetime_parameter in &parent_generic_parameters.lifetime_parameter_order {
            let Some(lifetime_argument) = substitution
                .lifetime_arguments_by_parameter
                .get(lifetime_parameter)
                .copied()
            else {
                if bound_checking != BoundChecking::IgnoreLifetimeChecksIfElided {
                    handler.receive(error::Error::LifetimeDoesNotOutlive(
                        LifetimeDoesNotOutlive {
                            required_lifetime_argument,
                            passed_lifetime_parameter: None,
                            bound_check_span: generic_identifier_span.clone(),
                        },
                    ));
                    return false;
                }

                continue;
            };

            // static lifetimes are always valid
            let LifetimeArgument::Parameter(lifetime_parameter) = lifetime_argument else {
                continue;
            };

            if let Some(lifetime_argument_set) = active_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .get(&lifetime_parameter)
            {
                if lifetime_argument_set.contains(&required_lifetime_argument) {
                    continue;
                }
            }

            if LifetimeArgument::Parameter(lifetime_parameter) == required_lifetime_argument {
                continue;
            }

            handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                TypeDoesNotOutliveLifetimeArgument {
                    required_lifetime_argument,
                    ty: ty.clone(),
                    generics_identifier_span: generic_identifier_span.clone(),
                },
            ));
            return false;
        }

        for ty_argument in substitution.type_arguments_by_parameter.values() {
            if !self.ty_outlives(
                required_lifetime_argument,
                ty_argument,
                bound_checking,
                active_where_clause,
                generic_identifier_span,
                handler,
            ) {
                return false;
            }
        }

        true
    }

    /// Checks if the given [`ty::Type`] outlives the given [`LifetimeArgument`].
    ///
    /// # Arguments
    /// - `required_lifetime_argument`: The [`LifetimeArgument`] that must be outlived.
    /// - `ty`: The [`ty::Type`] to check.
    /// - `active_where_clause`: The active [`WhereClause`] used for checking the lifetime bounds.
    /// - `generic_identifier_span`: Specifying in which place that caused the checking.
    /// - `handler`: The [`Handler`] to handle any error.
    ///
    /// # Returns
    /// `true` if the given [`ty::Type`] outlives the given [`LifetimeArgument`], `false`
    /// otherwise.
    pub(super) fn ty_outlives(
        &self,
        required_lifetime_argument: LifetimeArgument,
        ty: &ty::Type,
        bound_checking: BoundChecking,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        if let Some(lifetime_argument_set) =
            active_where_clause.lifetime_argument_sets_by_type.get(ty)
        {
            if lifetime_argument_set.contains(&required_lifetime_argument) {
                return true;
            }
        }

        match ty {
            ty::Type::Primitive(_) | ty::Type::Enum(_) => true,

            ty::Type::Struct(struct_ty) => self.substitution_outlives(
                required_lifetime_argument,
                &struct_ty.substitution,
                self.structs
                    .get(struct_ty.struct_id)
                    .unwrap()
                    .generic_parameters(),
                bound_checking,
                active_where_clause,
                ty,
                generic_identifier_span,
                handler,
            ),

            ty::Type::Reference(reference_ty) => {
                'error: {
                    if let Some(lifetime_argument) = reference_ty.lifetime_argument {
                        if lifetime_argument != required_lifetime_argument {
                            if let LifetimeArgument::Parameter(parameter) = lifetime_argument {
                                if let Some(lifetime_argument_set) = active_where_clause
                                    .lifetime_argument_sets_by_lifetime_parameter
                                    .get(&parameter)
                                {
                                    if !lifetime_argument_set.contains(&required_lifetime_argument)
                                    {
                                        break 'error;
                                    }
                                } else {
                                    break 'error;
                                }
                            }
                        }
                    } else if bound_checking != BoundChecking::IgnoreLifetimeChecksIfElided {
                        break 'error;
                    }

                    return self.ty_outlives(
                        required_lifetime_argument,
                        &reference_ty.operand,
                        bound_checking,
                        active_where_clause,
                        generic_identifier_span,
                        handler,
                    );
                }

                handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                    TypeDoesNotOutliveLifetimeArgument {
                        required_lifetime_argument,
                        ty: ty.clone(),
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                false
            }
            ty::Type::TraitType(_) | ty::Type::Parameter(_) => {
                handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                    TypeDoesNotOutliveLifetimeArgument {
                        required_lifetime_argument,
                        ty: ty.clone(),
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                false
            }
        }
    }

    /// Checks if the given [`Substitution`] applied to a genericable symbol satisfies the
    /// [`WhereClause`] of the genericable symbol.
    ///
    /// # Arguments
    /// - `genericable_id`: The ID to the genericable symbol that the given [`Substitution`] is
    /// applied to and whose [`WhereClause`] is checked.
    /// - `substitution`: The [`Substitution`] to check.
    /// - `generic_identifier_span`: Specifying in which place that caused the checking.
    /// - `active_where_clause`: The active [`WhereClause`] used for checking all the constraints.
    /// - `handler`: The [`Handler`] to handle any error.
    ///
    /// # Returns
    /// `true` if the given [`Substitution`] satisfies the [`WhereClause`] of the genericable
    /// symbol, `false` otherwise.
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub(in crate::table) fn check_where_clause(
        &self,
        genericable_id: GenericableID,
        substitution: &Substitution,
        bound_checking: BoundChecking,
        generic_identifier_span: &Span,
        active_where_clause: &WhereClause,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        let Some(genericable_where_clause) =
            self.get_genericable(genericable_id).unwrap().where_clause()
        else {
            return true;
        };

        let Some(genericable_where_clause) = self.substitute_where_clause(
            genericable_where_clause,
            substitution,
            bound_checking,
            active_where_clause,
            generic_identifier_span,
            handler,
        ) else {
            return false;
        };

        let mut found_error = false;

        // perform lifetime bound checking
        for (required_lifetime_parameter, required_lifetime_argument_set) in
            genericable_where_clause.lifetime_argument_sets_by_lifetime_parameter
        {
            let Some(active_lifetime_argument_set) = active_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .get(&required_lifetime_parameter)
            else {
                found_error = !required_lifetime_argument_set.is_empty();
                for bound_lifetime_argument in required_lifetime_argument_set {
                    handler.receive(error::Error::LifetimeDoesNotOutlive(
                        LifetimeDoesNotOutlive {
                            passed_lifetime_parameter: Some(required_lifetime_parameter),
                            required_lifetime_argument: bound_lifetime_argument,
                            bound_check_span: generic_identifier_span.clone(),
                        },
                    ));
                }
                continue;
            };

            for bound_lifetime_argument in required_lifetime_argument_set {
                if !active_lifetime_argument_set.contains(&bound_lifetime_argument) {
                    found_error = true;
                    handler.receive(error::Error::LifetimeDoesNotOutlive(
                        LifetimeDoesNotOutlive {
                            passed_lifetime_parameter: Some(required_lifetime_parameter),
                            required_lifetime_argument: bound_lifetime_argument,
                            bound_check_span: generic_identifier_span.clone(),
                        },
                    ));
                }
            }
        }

        // perform trait bound checking
        for required_trait_bound in genericable_where_clause.trait_bounds {
            if required_trait_bound.substitution.is_concrete_substitution() {
                continue;
            }

            if !active_where_clause
                .trait_bounds
                .contains(&required_trait_bound)
            {
                found_error = true;

                handler.receive(error::Error::TraitBoundNotSatisfied(
                    TraitBoundNotSatisfied {
                        required_trait_bound_string: self
                            .get_qualified_name_with_substitution(
                                required_trait_bound.trait_id.into(),
                                &required_trait_bound.substitution,
                            )
                            .unwrap(),
                        generic_identifier_span: generic_identifier_span.clone(),
                    },
                ));
            }
        }

        // perform trait type bound checking
        for (required_trait_type, required_ty) in genericable_where_clause.types_by_trait_type {
            let Some(active_ty) = active_where_clause
                .types_by_trait_type
                .get(&required_trait_type)
            else {
                found_error = true;
                handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                    TraitTypeBoundNotSatisfied {
                        required_type: required_ty,
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                continue;
            };

            if active_ty != &required_ty {
                found_error = true;
                handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                    TraitTypeBoundNotSatisfied {
                        required_type: required_ty,
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
            }
        }

        // performs type lifetime bound checking
        for (ty, lifetime_argument_set) in genericable_where_clause.lifetime_argument_sets_by_type {
            for lifetime_argument in lifetime_argument_set {
                if !self.ty_outlives(
                    lifetime_argument,
                    &ty,
                    bound_checking,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    found_error = true;
                }
            }
        }

        !found_error
    }
}
