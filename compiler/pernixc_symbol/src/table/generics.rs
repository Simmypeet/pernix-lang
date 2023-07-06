use std::collections::HashMap;

use derive_more::From;
use itertools::Itertools;
use pernixc_system::{arena, diagnostic::Handler};

use super::Table;
use crate::{
    error,
    ty::{self, Type},
    GenericableID, Implements, LifetimeArgument, Substitution, Trait, TypeParameter, WhereClause,
};

/// Used to determine the specialize relationship between two generic parameters.
pub(super) enum Specialization {
    Incompatible,
    Ambiguous,
    MoreGeneric,
    MoreSpecific,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, thiserror::Error)]
pub(super) enum Error {
    #[error("{0}")]
    InvalidID(arena::Error),

    #[error("Length between two type iterators mismatched.")]
    LengthMismatch,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, From, thiserror::Error)]
#[error("Mapping between two types is incompatible.")]
struct IncompatibleError;

impl Specialization {
    pub(super) fn compare<'a>(
        base: impl Iterator<Item = &'a Type> + Clone,
        operand: impl Iterator<Item = &'a Type> + Clone,
    ) -> Result<Self, Error> {
        let base_map_operand_count = 'a: {
            let mut mappings = HashMap::new();
            for i in base.clone().zip_longest(operand.clone()) {
                if let itertools::EitherOrBoth::Both(base, operand) = i {
                    match Self::map(std::iter::once(operand), std::iter::once(base), mappings) {
                        Ok(v) => mappings = v,
                        Err(_) => break 'a None,
                    }
                } else {
                    return Err(Error::LengthMismatch);
                }
            }

            Some(mappings.len())
        };

        let operand_map_base_count = 'a: {
            let mut mappings = HashMap::new();
            for i in operand.zip_longest(base) {
                if let itertools::EitherOrBoth::Both(operand, base) = i {
                    match Self::map(std::iter::once(base), std::iter::once(operand), mappings) {
                        Ok(v) => mappings = v,
                        Err(_) => break 'a None,
                    }
                } else {
                    return Err(Error::LengthMismatch);
                }
            }

            Some(mappings.len())
        };

        match (operand_map_base_count, base_map_operand_count) {
            (None, None) => Ok(Self::Incompatible),
            (Some(_), None) => Ok(Self::MoreGeneric),
            (None, Some(_)) => Ok(Self::MoreSpecific),
            (operand_map_base_count, base_map_operand_count) => {
                match operand_map_base_count.cmp(&base_map_operand_count) {
                    std::cmp::Ordering::Less => Ok(Self::MoreSpecific),
                    std::cmp::Ordering::Equal => Ok(Self::Ambiguous),
                    std::cmp::Ordering::Greater => Ok(Self::MoreGeneric),
                }
            }
        }
    }

    fn map<'a>(
        base: impl Iterator<Item = &'a Type>,
        operand: impl Iterator<Item = &'a Type>,
        mut generic_parameter_mappings: HashMap<arena::ID<TypeParameter>, &'a Type>,
    ) -> Result<HashMap<arena::ID<TypeParameter>, &'a Type>, IncompatibleError> {
        for i in base.zip_longest(operand) {
            if let itertools::EitherOrBoth::Both(base, operand) = i {
                match (base, operand) {
                    (Type::Parameter(type_parameter_id), _) => {
                        if let Some(ty) = generic_parameter_mappings.get(type_parameter_id).copied()
                        {
                            if ty == operand {
                                continue;
                            }
                            return Err(IncompatibleError);
                        }

                        generic_parameter_mappings.insert(*type_parameter_id, operand);
                        continue;
                    }
                    (Type::Struct(base), Type::Struct(operand)) => {
                        if base.struct_id != operand.struct_id {
                            return Err(IncompatibleError);
                        }

                        generic_parameter_mappings = Self::map(
                            base.substitution.type_arguments_by_parameter.values(),
                            operand.substitution.type_arguments_by_parameter.values(),
                            generic_parameter_mappings,
                        )?;
                    }
                    (Type::TraitType(lhs), Type::TraitType(rhs)) => {
                        if lhs.trait_type_id != rhs.trait_type_id {
                            return Err(IncompatibleError);
                        }

                        generic_parameter_mappings = Self::map(
                            lhs.substitution.type_arguments_by_parameter.values(),
                            rhs.substitution.type_arguments_by_parameter.values(),
                            generic_parameter_mappings,
                        )?;
                    }
                    (Type::Reference(lhs), Type::Reference(rhs)) => {
                        if lhs.qualifier != rhs.qualifier {
                            return Err(IncompatibleError);
                        }

                        generic_parameter_mappings = Self::map(
                            std::iter::once(&*lhs.operand),
                            std::iter::once(&*rhs.operand),
                            generic_parameter_mappings,
                        )?;
                    }
                    _ => todo!(),
                }
            } else {
                return Err(IncompatibleError);
            }
        }

        Ok(generic_parameter_mappings)
    }
}

impl Table {
    #[allow(clippy::too_many_lines)]
    pub(super) fn outlives(
        &self,
        active_where_clause: &WhereClause,
        ty: &ty::Type,
        lifetime_argument: LifetimeArgument,
    ) -> Result<bool, arena::Error> {
        match ty {
            Type::Struct(struct_ty) => {
                let struct_symbol = self.structs.get_ok_or(struct_ty.struct_id)?;

                for field_symbol in struct_symbol.field_order.iter().map(|x| &self.fields[*x]) {
                    let mut field_ty = field_symbol.ty.clone();
                    struct_ty.substitution.apply_type(&mut field_ty);

                    if !self.outlives(active_where_clause, &field_ty, lifetime_argument)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            Type::Primitive(..) => Ok(true),
            Type::Reference(reference_ty) => {
                if reference_ty.lifetime_argument == lifetime_argument {
                    return self.outlives(
                        active_where_clause,
                        &reference_ty.operand,
                        lifetime_argument,
                    );
                }

                match (reference_ty.lifetime_argument, lifetime_argument) {
                    (LifetimeArgument::Static, _) => self.outlives(
                        active_where_clause,
                        &reference_ty.operand,
                        lifetime_argument,
                    ),
                    (
                        LifetimeArgument::Parameter(reference_ty_lifetime_parameter),
                        lifetime_argument,
                    ) => {
                        let Some(lifetime_argument_bound_set) = active_where_clause
                            .lifetime_argument_sets_by_lifetime_parameter
                            .get(&reference_ty_lifetime_parameter)
                        else {
                            return Ok(false);
                        };

                        if lifetime_argument_bound_set.contains(&lifetime_argument) {
                            self.outlives(
                                active_where_clause,
                                &reference_ty.operand,
                                lifetime_argument,
                            )
                        } else {
                            Ok(false)
                        }
                    }
                }
            }
            Type::Parameter(type_parameter) => {
                let Some(lifetime_argument_bound_set) = active_where_clause
                    .lifetime_argument_sets_by_type_parameter
                    .get(type_parameter)
                else {
                    return Ok(false);
                };

                if lifetime_argument_bound_set.contains(&lifetime_argument) {
                    return Ok(true);
                }

                for lifetime_argument_bound in lifetime_argument_bound_set {
                    let LifetimeArgument::Parameter(lifetime_parameter_bound) =
                        lifetime_argument_bound else {
                        continue;
                    };

                    let Some(inner_lifetime_argument_bound_set) = active_where_clause
                        .lifetime_argument_sets_by_lifetime_parameter
                        .get(lifetime_parameter_bound)
                    else {
                        return Ok(false);
                    };

                    if inner_lifetime_argument_bound_set.contains(&lifetime_argument) {
                        return Ok(true);
                    }
                }

                Ok(false)
            }

            Type::TraitType(trait_type) => {
                let Some(lifetime_argument_bound_set) = active_where_clause
                    .lifetime_argument_sets_by_trait_type
                    .get(trait_type)
                else {
                    return Ok(false);
                };

                if lifetime_argument_bound_set.contains(&lifetime_argument) {
                    return Ok(true);
                }

                for lifetime_argument_bound in lifetime_argument_bound_set {
                    let LifetimeArgument::Parameter(lifetime_parameter_bound) =
                        lifetime_argument_bound else {
                        continue;
                    };

                    let Some(inner_lifetime_argument_bound_set) = active_where_clause
                        .lifetime_argument_sets_by_lifetime_parameter
                        .get(lifetime_parameter_bound)
                    else {
                        return Ok(false);
                    };

                    if inner_lifetime_argument_bound_set.contains(&lifetime_argument) {
                        return Ok(true);
                    }
                }

                Ok(false)
            }
        }
    }
}

pub(super) enum TraitResolution {
    Resolved(arena::ID<Implements>),
    ConstraintSatisfied,
}

impl ty::Type {
    fn contains_type_parameter(&self) -> bool {
        match self {
            Self::Struct(struct_ty) => struct_ty
                .substitution
                .type_arguments_by_parameter
                .values()
                .any(Self::contains_type_parameter),
            Self::Primitive(_) => false,
            Self::Reference(reference_ty) => reference_ty.operand.contains_type_parameter(),
            Self::Parameter(_) => true,
            Self::TraitType(trait_ty) => trait_ty
                .substitution
                .type_arguments_by_parameter
                .values()
                .any(Self::contains_type_parameter),
        }
    }
}

impl Table {
    fn where_clause_satisfied(
        &self,
        genericable_id: GenericableID,
        substitution: &Substitution,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), Error> {
        todo!()
    }

    fn resolve_trait(
        &self,
        trait_id: arena::ID<Trait>,
        substitution: &Substitution,
        handler: &impl Handler<error::Error>,
    ) -> Result<TraitResolution, Error> {
        // check if the where clause satisfied
        self.where_clause_satisfied(trait_id.into(), substitution, handler)?;

        // if in the substitution there is a type parameter, then the trait is not resolved.
        let requires_concrete_resolution = !substitution
            .type_arguments_by_parameter
            .values()
            .any(ty::Type::contains_type_parameter);

        if requires_concrete_resolution {
            todo!()
        } else {
            Ok(TraitResolution::ConstraintSatisfied)
        }
    }
}
