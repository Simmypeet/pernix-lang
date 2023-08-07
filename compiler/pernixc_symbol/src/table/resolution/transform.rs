use std::collections::HashSet;

use pernixc_source::Span;
use pernixc_system::diagnostic::Handler;

use super::{BoundChecking, Generics, Resolution, UnresolvedTraitType};
use crate::{
    error::{self, TraitTypeBoundNotSatisfied},
    table::Table,
    ty, LifetimeArgument, Substitution, TraitBound, WhereClause,
};

impl Table {
    /// Applies a substitution on the given [`WhereClause`].
    ///
    /// - If trait type resolution occurs, the trait type bound will be checked against the
    /// required type bound. If the trait type bound does not satisfy the required type bound,
    /// an error will be emitted.
    ///
    /// - If lifetime bounds' operands are substituted into a static lifetime, the newly substituted
    /// where clause will not contain those lifetime bounds.
    ///
    /// # Arguemnts
    /// - `where_clause`: The [`WhereClause`] to apply the substitution on.
    /// - `substitution`: The [`Substitution`] to apply.
    /// - `active_where_clause`: The active [`WhereClause`], used to check for constraint
    ///   requirements.
    /// - `generic_identifier_span`: Specifying in which place that caused the substitution.
    /// - `handler`: The [`Handler`] to handle any error.
    #[allow(clippy::too_many_lines)]
    pub(super) fn substitute_where_clause(
        &self,
        where_clause: &WhereClause,
        substitution: &Substitution,
        bound_checking: BoundChecking,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<WhereClause> {
        let mut result_where_clause = WhereClause::default();

        for (lifetime_parameter, lifetime_argument_set) in
            &where_clause.lifetime_argument_sets_by_lifetime_parameter
        {
            let lifetime_parameter_to_insert = match substitution
                .lifetime_arguments_by_parameter
                .get(lifetime_parameter)
            {
                Some(LifetimeArgument::Parameter(parameter)) => *parameter,
                Some(LifetimeArgument::Static) => continue,
                None => *lifetime_parameter,
            };

            let result_lifetime_argument_set = result_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .entry(lifetime_parameter_to_insert)
                .or_insert_with(HashSet::new);

            for lifetime_argument in lifetime_argument_set {
                match lifetime_argument {
                    LifetimeArgument::Static => {
                        result_lifetime_argument_set.insert(LifetimeArgument::Static);
                    }
                    LifetimeArgument::Parameter(parameter) => {
                        result_lifetime_argument_set.insert(
                            substitution
                                .lifetime_arguments_by_parameter
                                .get(parameter)
                                .copied()
                                .map_or(LifetimeArgument::Parameter(*parameter), |lt| lt),
                        );
                    }
                }
            }
        }

        for (ty, lifetime_argument_set) in &where_clause.lifetime_argument_sets_by_type {
            let aliased = self.substitute_type(
                ty,
                substitution,
                Some(bound_checking),
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;
            let result_lifetime_argument_set = result_where_clause
                .lifetime_argument_sets_by_type
                .entry(aliased)
                .or_insert_with(HashSet::default);

            for lifetime_argument in lifetime_argument_set {
                match lifetime_argument {
                    LifetimeArgument::Static => {
                        result_lifetime_argument_set.insert(LifetimeArgument::Static);
                    }
                    LifetimeArgument::Parameter(parameter) => {
                        result_lifetime_argument_set.insert(
                            substitution
                                .lifetime_arguments_by_parameter
                                .get(parameter)
                                .copied()
                                .map_or(LifetimeArgument::Parameter(*parameter), |lt| lt),
                        );
                    }
                }
            }
        }

        for trait_bound in &where_clause.trait_bounds {
            let mut new_substitution = trait_bound.substitution.clone();
            if !self.apply_substitution_on_arguments_in_place(
                &mut new_substitution,
                substitution,
                Some(bound_checking),
                active_where_clause,
                generic_identifier_span,
                handler,
            ) {
                return None;
            }

            result_where_clause.trait_bounds.insert(TraitBound {
                trait_id: trait_bound.trait_id,
                substitution: new_substitution,
            });
        }

        for (trait_type, ty) in &where_clause.types_by_trait_type {
            let aliased_ty_bound = self.substitute_type(
                ty,
                substitution,
                Some(bound_checking),
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;
            let aliased_trait_ty = self.substitute_type(
                &ty::Type::TraitType(trait_type.clone()),
                substitution,
                Some(bound_checking),
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;

            match aliased_trait_ty {
                ty::Type::TraitType(trait_ty) => {
                    result_where_clause
                        .types_by_trait_type
                        .insert(trait_ty, aliased_ty_bound);
                }
                ty => {
                    if ty != aliased_ty_bound {
                        handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                            TraitTypeBoundNotSatisfied {
                                required_type: ty,
                                generics_identifier_span: generic_identifier_span.clone(),
                            },
                        ));
                        return None;
                    }
                }
            }
        }

        Some(result_where_clause)
    }

    /// Applies a substitution on the given [`ty::Type`] and returns the newly substituted type.
    ///
    /// # Arguments
    /// - `alias`: The [`ty::Type`] to apply the substitution on.
    /// - `substitution`: The [`Substitution`] to apply.
    /// - `bound_checking`: Specifying how to handle trait resolution that might occur.
    /// - `active_where_clause`: The active [`WhereClause`], used to check for constraint
    ///  requirements.
    /// - `generic_identifier_span`: Specifying in which place that caused the substitution.
    /// - `handler`: The [`Handler`] to handle any error.
    pub(super) fn substitute_type(
        &self,
        alias: &ty::Type,
        substitution: &Substitution,
        bound_checking: Option<BoundChecking>,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<ty::Type> {
        let mut cloned_ty = alias.clone();
        if !self.substitute_type_in_place(
            &mut cloned_ty,
            substitution,
            bound_checking,
            active_where_clause,
            generic_identifier_span,
            handler,
        ) {
            return None;
        }
        Some(cloned_ty)
    }

    /// Applies a substitution on the given [`ty::Type`] in place.
    ///
    /// # Arguments
    /// - `ty`: The [`ty::Type`] to apply the substitution on.
    /// - `substitution`: The [`Substitution`] to apply.
    /// - `bound_checking`: Specifying how to handle trait resolution that might occur.
    /// - `active_where_clause`: The active [`WhereClause`], used to check for constraint
    /// requirements.
    /// - `generic_identifier_span`: Specifying in which place that caused the substitution.
    /// - `handler`: The [`Handler`] to handle any error.
    ///
    /// # Returns
    /// `true` if all the substitutions were successful, `false` otherwise.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub(super) fn substitute_type_in_place(
        &self,
        ty: &mut ty::Type,
        substitution: &Substitution,
        bound_checking: Option<BoundChecking>,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        match ty {
            ty::Type::Struct(struct_ty) => {
                if !self.apply_substitution_on_arguments_in_place(
                    &mut struct_ty.substitution,
                    substitution,
                    bound_checking,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    return false;
                }

                if let Some(bound_checking) = bound_checking {
                    if !self.check_where_clause(
                        struct_ty.struct_id.into(),
                        &struct_ty.substitution,
                        bound_checking,
                        generic_identifier_span,
                        active_where_clause,
                        handler,
                    ) {
                        return false;
                    }
                }

                true
            }
            ty::Type::Reference(reference_ty) => {
                if let Some(LifetimeArgument::Parameter(lt)) = reference_ty.lifetime_argument {
                    reference_ty.lifetime_argument = substitution
                        .lifetime_arguments_by_parameter
                        .get(&lt)
                        .copied();
                }

                self.substitute_type_in_place(
                    &mut reference_ty.operand,
                    substitution,
                    bound_checking,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                )
            }
            ty::Type::Parameter(type_parameter) => {
                if let Some(substitution) =
                    substitution.type_arguments_by_parameter.get(type_parameter)
                {
                    *ty = substitution.clone();
                }

                true
            }
            ty::Type::TraitType(trait_type) => {
                if !self.apply_substitution_on_arguments_in_place(
                    &mut trait_type.trait_substitution,
                    substitution,
                    bound_checking,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) || !self.apply_substitution_on_arguments_in_place(
                    &mut trait_type.trait_type_substitution,
                    substitution,
                    bound_checking,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    return false;
                }

                if let Some(bound_checking) = bound_checking {
                    if !self.check_where_clause(
                        self.trait_types[trait_type.trait_type_id]
                            .parent_trait_id
                            .into(),
                        &trait_type.trait_substitution,
                        bound_checking,
                        generic_identifier_span,
                        active_where_clause,
                        handler,
                    ) {
                        return false;
                    }
                }

                if trait_type.trait_substitution.is_concrete_substitution() {
                    let Some(active_implements) = self.resolve_trait_implements(
                        self.trait_types[trait_type.trait_type_id].parent_trait_id,
                        &trait_type.trait_substitution,
                        generic_identifier_span,
                        bound_checking,
                        active_where_clause,
                        handler,
                    ) else {
                        return false;
                    };

                    let Some(new_ty) = self.substitute_type(
                        &self.implements_types[self.implements[active_implements.implements_id]
                            .implements_types_by_trait_type[&trait_type.trait_type_id]]
                            .alias,
                        &Substitution::coombine(
                            &trait_type.trait_substitution,
                            &Self::transform_trait_member_substitution_to_implements_substitution(
                                trait_type.trait_type_substitution.clone(),
                                &self.trait_types[trait_type.trait_type_id].generic_parameters,
                                &self.implements_types[self.implements
                                    [active_implements.implements_id]
                                    .implements_types_by_trait_type[&trait_type.trait_type_id]]
                                    .generic_parameters,
                            ),
                        ),
                        bound_checking,
                        active_where_clause,
                        generic_identifier_span,
                        handler,
                    ) else {
                        return false;
                    };

                    *ty = new_ty;
                }

                true
            }
            _ => true,
        }
    }

    /// Applies the substitution on the given [`Substitution`]'s arguments in place.
    ///
    /// # Arguments
    /// - `substitution_operand`: The [`Substitution`] to apply the substitution on.
    /// - `other`: The [`Substitution`] to apply.
    /// - `bound_checking`: Specifying how to handle trait resolution that might occur.
    /// - `active_where_clause`: The active [`WhereClause`], used to check for constraint
    /// requirements.
    /// - `generic_identifier_span`: Specifying in which place that caused the substitution.
    /// - `handler`: The [`Handler`] to handle any error.
    ///
    /// # Returns
    /// `true` if all substitutions were successful, `false` otherwise.
    #[must_use]
    fn apply_substitution_on_arguments_in_place(
        &self,
        substitution_operand: &mut Substitution,
        other: &Substitution,
        bound_checking: Option<BoundChecking>,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        // FALSE POSITIVE
        #[allow(clippy::needless_collect)]
        for argument in substitution_operand
            .lifetime_arguments_by_parameter
            .values()
            .copied::<_>()
            .collect::<Vec<_>>()
        {
            if let LifetimeArgument::Parameter(id) = argument {
                if let Some(substitution) = other.lifetime_arguments_by_parameter.get(&id) {
                    substitution_operand
                        .lifetime_arguments_by_parameter
                        .insert(id, *substitution);
                } else {
                    substitution_operand
                        .lifetime_arguments_by_parameter
                        .remove(&id);
                }
            }
        }

        for argument in substitution_operand
            .type_arguments_by_parameter
            .values_mut()
        {
            if !self.substitute_type_in_place(
                argument,
                other,
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

    pub(super) fn substitute_type_as_resolution(
        &self,
        alias: &ty::Type,
        substitution: &Substitution,
        bound_checking: Option<BoundChecking>,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<Resolution> {
        Some(
            match self.substitute_type(
                alias,
                substitution,
                bound_checking,
                active_where_clause,
                generic_identifier_span,
                handler,
            )? {
                ty::Type::Enum(enum_id) => Resolution::Enum(enum_id),
                ty::Type::Struct(struct_ty) => Resolution::Struct(Generics {
                    symbol: struct_ty.struct_id,
                    substitution: struct_ty.substitution,
                }),
                ty::Type::Primitive(primtive) => Resolution::Primitive(primtive),
                ty::Type::Reference(reference) => Resolution::Reference(reference),
                ty::Type::Parameter(ty_parameter) => Resolution::TypeParameter(ty_parameter),
                ty::Type::TraitType(trait_ty) => Resolution::TraitType(UnresolvedTraitType {
                    trait_resolution: Generics {
                        substitution: trait_ty.trait_substitution,
                        symbol: self.trait_types[trait_ty.trait_type_id].parent_trait_id,
                    },
                    member_resolution: Generics {
                        symbol: trait_ty.trait_type_id,
                        substitution: trait_ty.trait_type_substitution,
                    },
                }),
            },
        )
    }
}
