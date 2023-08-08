use std::{cmp::Ordering, collections::HashMap};

use pernixc_source::Span;
use pernixc_system::{
    arena,
    diagnostic::{Dummy, Handler},
};

use super::{BoundChecking, Implements};
use crate::{error, table::Table, ty, LifetimeArgument, Substitution, WhereClause};

impl Table {
    /// Resolves for the trait implements that most fits the given substitution.
    ///
    /// # Arguments
    /// - `trait_id`: The trait to resolve the implements for.
    /// - `substitution`: The substitution to resolve the implements for.
    /// - `generic_identifier_span`: The span where the trait resolution is happening.
    /// - `active_where_clause`: The [`WhereClause`] used for checking the constraints in the
    /// trait resolution.
    /// - `handler`: The [`Handler`] used for reporting errors.
    pub(super) fn resolve_trait_implements(
        &self,
        trait_id: arena::ID<crate::Trait>,
        substitution: &Substitution,
        trait_resolution_span: &Span,
        bound_checking: Option<BoundChecking>,
        active_where_clause: &WhereClause,
        handler: &impl Handler<error::Error>,
    ) -> Option<Implements> {
        let mut implements_ids_by_deduced_substitution = HashMap::new();

        for implements_id in self.traits[trait_id].implements.iter().copied() {
            let Some(deduced_substitution) = self.get_deduced_substitution(
                trait_resolution_span,
                &self.implements[implements_id].substitution,
                substitution,
                active_where_clause,
            ) else {
                continue;
            };

            assert!(implements_ids_by_deduced_substitution
                .insert(implements_id, deduced_substitution)
                .is_none());
        }

        let implements = implements_ids_by_deduced_substitution
            .keys()
            .copied()
            .reduce(|lhs, rhs| {
                match compare(
                    &self.implements[lhs].substitution,
                    &self.implements[rhs].substitution,
                ) {
                    Comparison::MoreSpecialized => lhs,
                    Comparison::LessSpecialized => rhs,
                    Comparison::Incompatible | Comparison::Ambiguous => {
                        unreachable!()
                    }
                }
            });

        implements.map_or_else(
            || {
                handler.receive(error::Error::NoImplementsFound(error::NoImplementsFound {
                    span: trait_resolution_span.clone(),
                }));
                None
            },
            |implements_id| {
                let deduced_substitution = implements_ids_by_deduced_substitution
                    .remove(&implements_id)
                    .unwrap();

                if let Some(bound_checking) = bound_checking {
                    if !self.check_where_clause(
                        implements_id.into(),
                        &deduced_substitution,
                        bound_checking,
                        trait_resolution_span,
                        active_where_clause,
                        &Dummy,
                    ) {
                        return None;
                    }
                }

                Some(Implements {
                    implements_id,
                    deduced_substitution,
                })
            },
        )
    }

    fn deduce_in_type(
        implements_ty: &ty::Type,
        trait_ty: &ty::Type,
        (mut previous_substitution, mut trait_type_substitution): (
            Substitution,
            HashMap<ty::TraitType, ty::Type>,
        ),
    ) -> Option<(Substitution, HashMap<ty::TraitType, ty::Type>)> {
        match (implements_ty, trait_ty) {
            (ty::Type::Parameter(implements_ty), trait_ty) => {
                if let Some(previous_ty) = previous_substitution
                    .type_arguments_by_parameter
                    .insert(*implements_ty, trait_ty.clone())
                {
                    if previous_ty != *trait_ty {
                        return None;
                    }
                }

                Some((previous_substitution, trait_type_substitution))
            }
            (ty::Type::TraitType(implements_ty), trait_ty) => {
                if let Some(previous_ty) =
                    trait_type_substitution.insert(implements_ty.clone(), trait_ty.clone())
                {
                    if previous_ty != *trait_ty {
                        return None;
                    }
                }

                Some((previous_substitution, trait_type_substitution))
            }
            (ty::Type::Reference(implements_ty), ty::Type::Reference(trait_ty)) => {
                if implements_ty.qualifier != trait_ty.qualifier {
                    return None;
                }

                if let (LifetimeArgument::Parameter(lifetime_parameter), Some(lifetime_argument)) = (
                    implements_ty.lifetime_argument.unwrap(),
                    trait_ty.lifetime_argument,
                ) {
                    if let Some(previous_lifetime_argument) = previous_substitution
                        .lifetime_arguments_by_parameter
                        .insert(lifetime_parameter, lifetime_argument)
                    {
                        if previous_lifetime_argument != lifetime_argument {
                            return None;
                        }
                    }
                }

                Self::deduce_in_type(
                    &implements_ty.operand,
                    &trait_ty.operand,
                    (previous_substitution, trait_type_substitution),
                )
            }
            (ty::Type::Struct(implements_ty), ty::Type::Struct(trait_ty)) => {
                if implements_ty.struct_id != trait_ty.struct_id {
                    return None;
                }

                Self::deduce_in_substitution(
                    &implements_ty.substitution,
                    &trait_ty.substitution,
                    (previous_substitution, trait_type_substitution),
                )
            }
            (implements_ty, trait_ty) => {
                if implements_ty != trait_ty {
                    return None;
                }

                Some((previous_substitution, trait_type_substitution))
            }
        }
    }

    fn deduce_in_substitution(
        implements_substitution: &Substitution,
        trait_substitution: &Substitution,
        (mut previous_substitution, mut trait_type_substitution): (
            Substitution,
            HashMap<ty::TraitType, ty::Type>,
        ),
    ) -> Option<(Substitution, HashMap<ty::TraitType, ty::Type>)> {
        assert!(
            implements_substitution.type_arguments_by_parameter.len()
                == trait_substitution.type_arguments_by_parameter.len()
                && implements_substitution
                    .lifetime_arguments_by_parameter
                    .len()
                    == trait_substitution.lifetime_arguments_by_parameter.len()
        );

        for (implements_lt, trait_lt) in implements_substitution
            .lifetime_arguments_by_parameter
            .keys()
            .map(|key| {
                (
                    *implements_substitution
                        .lifetime_arguments_by_parameter
                        .get(key)
                        .unwrap(),
                    *trait_substitution
                        .lifetime_arguments_by_parameter
                        .get(key)
                        .unwrap(),
                )
            })
        {
            if let (LifetimeArgument::Parameter(implements_lt_parameter), trait_lt_argument) =
                (implements_lt, trait_lt)
            {
                if let Some(previous_lt_argument) = previous_substitution
                    .lifetime_arguments_by_parameter
                    .insert(implements_lt_parameter, trait_lt_argument)
                {
                    if previous_lt_argument != trait_lt_argument {
                        return None;
                    }
                }
            }
        }

        for (implements_ty, trait_ty) in implements_substitution
            .type_arguments_by_parameter
            .keys()
            .map(|key| {
                (
                    implements_substitution
                        .type_arguments_by_parameter
                        .get(key)
                        .unwrap(),
                    trait_substitution
                        .type_arguments_by_parameter
                        .get(key)
                        .unwrap(),
                )
            })
        {
            let (previous_substitution_new, trait_type_substitution_new) = Self::deduce_in_type(
                implements_ty,
                trait_ty,
                (previous_substitution, trait_type_substitution),
            )?;

            previous_substitution = previous_substitution_new;
            trait_type_substitution = trait_type_substitution_new;
        }

        Some((previous_substitution, trait_type_substitution))
    }

    fn get_deduced_substitution(
        &self,
        generic_identifier_span: &Span,
        implement_substitution: &Substitution,
        trait_substitution: &Substitution,
        active_where_clause: &WhereClause,
    ) -> Option<Substitution> {
        let (deduced_substitution, trait_type_substitution) = Self::deduce_in_substitution(
            implement_substitution,
            trait_substitution,
            (Substitution::default(), HashMap::default()),
        )?;

        for (trait_ty, required_ty) in trait_type_substitution {
            let trait_ty = ty::Type::TraitType(trait_ty);
            let Some(aliased_ty) = self.substitute_type(
                &trait_ty,
                &deduced_substitution,
                None,
                active_where_clause,
                generic_identifier_span,
                &Dummy,
            ) else {
                return None;
            };

            if aliased_ty != required_ty {
                return None;
            }
        }

        Some(deduced_substitution)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::table) enum Comparison {
    Incompatible,
    MoreSpecialized,
    LessSpecialized,
    Ambiguous,
}

/// Compares between two substitution to determine which one is more specialized.
///
/// # Returns
/// - [`Comparison::Incompatible`] if the two substitutions are incompatible.
/// - [`Comparison::MoreSpecialized`] if the `lhs` is more specialized than the `rhs`.
/// - [`Comparison::LessSpecialized`] if the `rhs` is more specialized than the `lhs`.
pub(in crate::table) fn compare(
    lhs_implements_substitution: &Substitution,
    rhs_implements_substitution: &Substitution,
) -> Comparison {
    assert_eq!(
        lhs_implements_substitution
            .type_arguments_by_parameter
            .len(),
        rhs_implements_substitution
            .type_arguments_by_parameter
            .len()
    );

    let lhs_to_rhs = get_mappings(lhs_implements_substitution, rhs_implements_substitution);
    let rhs_to_lhs = get_mappings(rhs_implements_substitution, lhs_implements_substitution);

    match (lhs_to_rhs, rhs_to_lhs) {
        (None, None) => Comparison::Incompatible,
        (None, Some(rhs_to_lhs)) => {
            assert!(!rhs_to_lhs.is_empty());
            Comparison::MoreSpecialized
        }
        (Some(lhs_to_rhs), None) => {
            assert!(!lhs_to_rhs.is_empty());
            Comparison::LessSpecialized
        }
        (Some(lhs_to_rhs), Some(rhs_to_lhs)) => match lhs_to_rhs.len().cmp(&rhs_to_lhs.len()) {
            Ordering::Less => Comparison::MoreSpecialized,
            Ordering::Equal => Comparison::Ambiguous,
            Ordering::Greater => Comparison::LessSpecialized,
        },
    }
}

// get mappings from lhs -> rhs
fn get_mappings(
    lhs_implements_substitution: &Substitution,
    rhs_implements_substitution: &Substitution,
) -> Option<HashMap<ty::Type, ty::Type>> {
    let mut mappings = HashMap::new();

    for ty_parameter_key in lhs_implements_substitution
        .type_arguments_by_parameter
        .keys()
    {
        let lhs_ty = lhs_implements_substitution
            .type_arguments_by_parameter
            .get(ty_parameter_key)
            .unwrap();
        let rhs_ty = rhs_implements_substitution
            .type_arguments_by_parameter
            .get(ty_parameter_key)
            .unwrap();

        mappings = get_mappings_on_type(lhs_ty, rhs_ty, mappings)?;
    }

    Some(mappings)
}

// maps from lhs_type -> rhs_type
fn get_mappings_on_type(
    lhs_type: &ty::Type,
    rhs_ty: &ty::Type,
    mut previous_mappings: HashMap<ty::Type, ty::Type>,
) -> Option<HashMap<ty::Type, ty::Type>> {
    match (lhs_type, rhs_ty) {
        (ty::Type::Parameter(lhs_ty_parameter), rhs_ty) => {
            if let Some(previous_ty) =
                previous_mappings.insert(ty::Type::Parameter(*lhs_ty_parameter), rhs_ty.clone())
            {
                if previous_ty != *rhs_ty {
                    return None;
                }
            }
        }
        (ty::Type::TraitType(lhs_ty_parameter), rhs_ty) => {
            if let Some(previous_ty) = previous_mappings.insert(
                ty::Type::TraitType(lhs_ty_parameter.clone()),
                rhs_ty.clone(),
            ) {
                if previous_ty != *rhs_ty {
                    return None;
                }
            }
        }
        (ty::Type::Reference(lhs_ty_reference), ty::Type::Reference(rhs_ty_reference)) => {
            if lhs_ty_reference.qualifier != rhs_ty_reference.qualifier {
                return None;
            }

            previous_mappings = get_mappings_on_type(
                &lhs_ty_reference.operand,
                &rhs_ty_reference.operand,
                previous_mappings,
            )?;
        }
        (ty::Type::Struct(lhs_ty_struct), ty::Type::Struct(rhs_ty_struct)) => {
            if lhs_ty_struct.struct_id != rhs_ty_struct.struct_id {
                return None;
            }

            assert_eq!(
                lhs_ty_struct.substitution.type_arguments_by_parameter.len(),
                rhs_ty_struct.substitution.type_arguments_by_parameter.len()
            );

            for ty_parameter_key in lhs_ty_struct
                .substitution
                .type_arguments_by_parameter
                .keys()
            {
                let lhs_ty =
                    &lhs_ty_struct.substitution.type_arguments_by_parameter[ty_parameter_key];
                let rhs_ty =
                    &rhs_ty_struct.substitution.type_arguments_by_parameter[ty_parameter_key];

                previous_mappings = get_mappings_on_type(lhs_ty, rhs_ty, previous_mappings)?;
            }
        }
        (lhs_ty, rhs_ty) => {
            if lhs_ty != rhs_ty {
                return None;
            }
        }
    }

    Some(previous_mappings)
}
