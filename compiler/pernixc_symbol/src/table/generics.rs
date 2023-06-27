use std::collections::HashMap;

use derive_more::From;
use itertools::Itertools;
use pernixc_system::arena;

use crate::{ty::Type, TypeParameter};

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
