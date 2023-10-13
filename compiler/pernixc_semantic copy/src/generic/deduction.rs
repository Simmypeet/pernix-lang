use std::collections::{hash_map::Entry, HashMap};

use super::AssociatedBounds;
use crate::{
    constant,
    symbol::{LocalSubstitution, Substitution},
    table::Table,
    ty::{self, Lifetime},
};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct Deduction {
    substitution: Substitution,
    trait_associated_type_deductions: HashMap<ty::TraitAssociated, ty::Type>,
    trait_associated_constant_deductions: HashMap<constant::TraitAssociated, constant::Constant>,
}

impl Deduction {
    fn combine(mut self, other: Self) -> Option<Self> {
        for (trait_associated, ty) in other.trait_associated_type_deductions {
            match self
                .trait_associated_type_deductions
                .entry(trait_associated)
            {
                Entry::Occupied(entry) => {
                    if entry.get() != &ty {
                        return None;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(ty);
                }
            }
        }

        for (trait_associated, constant) in other.trait_associated_constant_deductions {
            match self
                .trait_associated_constant_deductions
                .entry(trait_associated)
            {
                Entry::Occupied(entry) => {
                    if entry.get() != &constant {
                        return None;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(constant);
                }
            }
        }

        for (parameter, ty) in other.substitution.type_substitutions {
            match self.substitution.type_substitutions.entry(parameter) {
                Entry::Occupied(entry) => {
                    if entry.get() != &ty {
                        return None;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(ty);
                }
            }
        }

        for (parameter, constant) in other.substitution.constant_substitutions {
            match self.substitution.constant_substitutions.entry(parameter) {
                Entry::Occupied(entry) => {
                    if entry.get() != &constant {
                        return None;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(constant);
                }
            }
        }

        for (parameter, lifetime) in other.substitution.lifetime_substitutions {
            match self.substitution.lifetime_substitutions.entry(parameter) {
                Entry::Occupied(entry) => {
                    if entry.get() != &lifetime {
                        return None;
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(lifetime);
                }
            }
        }

        Some(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Error {
    NonUnifiable,
    InvalidArguments,
}

impl Table {
    pub(super) fn get_deduced_substitution(
        &self,
        implements_substitution: &LocalSubstitution,
        trait_substitution: &LocalSubstitution,
        active_associated_bounds: &AssociatedBounds,
    ) -> Result<LocalSubstitution, Error> {
        let deduction = deduce_in_local_substitution(implements_substitution, trait_substitution)?;

        for (trait_associated_ty, bound) in deduction.trait_associated_type_deductions {
            let trait_associated_ty = ty::Type::TraitAssociated(trait_associated_ty);

            let aliased = self
                .substitute_type(
                    trait_associated_ty,
                    &deduction.substitution,
                    active_associated_bounds,
                )
                .map_err(|x| match x {
                    crate::generic::SubstitutionError::InvalidArguments => Error::InvalidArguments,
                    crate::generic::SubstitutionError::IncompleteImplements
                    | crate::generic::SubstitutionError::TraitImplementsNotFound(_) => {
                        Error::NonUnifiable
                    }
                })?;

            if aliased != bound {
                return Err(Error::NonUnifiable);
            }
        }

        Ok(deduction
            .substitution
            .try_into_local()
            .expect("should've been able to convert"))
    }
}

#[allow(clippy::too_many_lines)]
pub(super) fn dedcue_in_type(
    implements_ty: &ty::Type,
    trait_ty: &ty::Type,
) -> Result<Deduction, Error> {
    match (implements_ty, trait_ty) {
        (ty::Type::Parameter(implements_ty_parameter), trait_ty) => {
            let mut deduction = Deduction::default();

            deduction
                .substitution
                .type_substitutions
                .insert(*implements_ty_parameter, trait_ty.clone());

            Ok(deduction)
        }

        (ty::Type::TraitAssociated(implements_trait_associated), trait_ty) => {
            let mut deduction = Deduction::default();

            deduction
                .trait_associated_type_deductions
                .insert(implements_trait_associated.clone(), trait_ty.clone());

            Ok(deduction)
        }

        // unifying two enums
        (ty::Type::Enum(implements_ty), ty::Type::Enum(trait_ty)) => {
            if implements_ty.enum_ref != trait_ty.enum_ref {
                return Err(Error::NonUnifiable);
            }

            deduce_in_local_substitution(&dynements_ty.substitution, &trait_ty.substitution)
        }

        // unifying two references
        (ty::Type::Reference(implements_ty), ty::Type::Reference(trait_ty)) => {
            if implements_ty.qualifier != trait_ty.qualifier {
                return Err(Error::NonUnifiable);
            }

            let mut deduction = dedcue_in_type(&dynements_ty.operand, &trait_ty.operand)?;

            if let Lifetime::Parameter(parameter) = implements_ty.lifetime {
                if let Some(previous_lifetime) = deduction
                    .substitution
                    .lifetime_substitutions
                    .insert(parameter, trait_ty.lifetime)
                {
                    // collision between lifetime deduction
                    if previous_lifetime != trait_ty.lifetime {
                        return Err(Error::NonUnifiable);
                    }
                }
            }

            Ok(deduction)
        }

        // unifying two pointers
        (ty::Type::Pointer(implements_ty), ty::Type::Pointer(trait_ty)) => {
            if implements_ty.qualifier != trait_ty.qualifier {
                return Err(Error::NonUnifiable);
            }

            dedcue_in_type(&dynements_ty.operand, &trait_ty.operand)
        }

        // unifying two structs
        (ty::Type::Struct(implements_ty), ty::Type::Struct(trait_ty)) => {
            if implements_ty.struct_ref != trait_ty.struct_ref {
                return Err(Error::NonUnifiable);
            }

            deduce_in_local_substitution(&dynements_ty.substitution, &trait_ty.substitution)
        }
        // unifying two tuples
        (ty::Type::Tuple(implements_ty), ty::Type::Tuple(trait_ty)) => {
            let unpacked_count = implements_ty
                .elements
                .iter()
                .filter(|x| x.is_unpacked())
                .count();

            match unpacked_count {
                // no unpacked elements case
                0 => {
                    if implements_ty.elements.len() != trait_ty.elements.len() {
                        return Err(Error::NonUnifiable);
                    }

                    let mut deduction = Deduction::default();

                    for (implements_element, trait_element) in
                        implements_ty.elements.iter().zip(trait_ty.elements.iter())
                    {
                        let (
                            ty::TupleElement::Regular(implements_element),
                            ty::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(dedcue_in_type(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    Ok(deduction)
                }

                // one unpacked element case
                1 => {
                    // impossible to unify
                    if implements_ty.elements.len() > trait_ty.elements.len() + 1 {
                        return Err(Error::NonUnifiable);
                    }

                    let mut deduction = Deduction::default();

                    let unpacked_position = implements_ty
                        .elements
                        .iter()
                        .position(ty::TupleElement::is_unpacked)
                        .unwrap();

                    let head_range = 0..unpacked_position;
                    let implements_tail_range =
                        (unpacked_position + 1)..implements_ty.elements.len();
                    let trait_tail_range = (trait_ty.elements.len()
                        - implements_tail_range.clone().count())
                        ..trait_ty.elements.len();
                    let trait_unpack_range = unpacked_position..trait_tail_range.start;

                    // unify head
                    for (implements_element, trait_element) in implements_ty.elements
                        [head_range.clone()]
                    .iter()
                    .zip(&trait_ty.elements[head_range])
                    {
                        let (
                            ty::TupleElement::Regular(implements_element),
                            ty::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(dedcue_in_type(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    // unify tail
                    for (implements_element, trait_element) in implements_ty.elements
                        [implements_tail_range]
                        .iter()
                        .zip(&trait_ty.elements[trait_tail_range])
                    {
                        let (
                            ty::TupleElement::Regular(implements_element),
                            ty::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(dedcue_in_type(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    // unify unpacked
                    let packed_tuple = || -> ty::Type {
                        ty::Type::Tuple(ty::Tuple {
                            elements: trait_ty.elements[trait_unpack_range.clone()].to_vec(),
                        })
                    };

                    match implements_ty.elements[unpacked_position]
                        .as_unpacked()
                        .unwrap()
                    {
                        ty::Unpacked::Parameter(parameter) => {
                            if let Some(previous_ty) = deduction
                                .substitution
                                .type_substitutions
                                .insert(*parameter, packed_tuple())
                            {
                                // collision between type deduction
                                if previous_ty != packed_tuple() {
                                    return Err(Error::NonUnifiable);
                                }
                            }
                        }
                        ty::Unpacked::TraitAssociated(trait_associated) => {
                            if let Some(previous_ty) = deduction
                                .trait_associated_type_deductions
                                .insert(trait_associated.clone(), packed_tuple())
                            {
                                // collision between trait associated type deduction
                                if previous_ty != packed_tuple() {
                                    return Err(Error::NonUnifiable);
                                }
                            }
                        }
                    }

                    Ok(deduction)
                }

                _ => Err(Error::InvalidArguments),
            }
        }
        // unify two arrays
        (ty::Type::Array(implements_ty), ty::Type::Array(trait_ty)) => {
            let deduction = deduce_in_constant(&dynements_ty.size, &trait_ty.size)?;

            deduction
                .combine(dedcue_in_type(
                    &dynements_ty.element_ty,
                    &trait_ty.element_ty,
                )?)
                .ok_or(Error::NonUnifiable)
        }

        (implements_ty, trait_ty) => {
            if implements_ty != trait_ty {
                return Err(Error::NonUnifiable);
            }

            Ok(Deduction::default())
        }
    }
}

#[allow(clippy::too_many_lines)]
pub(super) fn deduce_in_constant(
    implements_constant: &constant::Constant,
    trait_constant: &constant::Constant,
) -> Result<Deduction, Error> {
    match (implements_constant, trait_constant) {
        (constant::Constant::Parameter(implements_constant_parameter), trait_constant) => {
            let mut deduction = Deduction::default();

            deduction
                .substitution
                .constant_substitutions
                .insert(*implements_constant_parameter, trait_constant.clone());

            Ok(deduction)
        }

        (constant::Constant::TraitAssociated(implements_trait_asssociated), trait_constant) => {
            let mut deduction = Deduction::default();

            deduction
                .trait_associated_constant_deductions
                .insert(implements_trait_asssociated.clone(), trait_constant.clone());

            Ok(deduction)
        }

        // unify two structs
        (constant::Constant::Struct(implements_constant), constant::Constant::Struct(trait_ty)) => {
            let mut deduction = dedcue_in_type(
                &ty::Type::Struct(implements_constant.struct_ty.clone()),
                &ty::Type::Struct(trait_ty.struct_ty.clone()),
            )?;

            if implements_constant.fields.len() != trait_ty.fields.len() {
                return Err(Error::InvalidArguments);
            }

            for (implements_field, trait_field) in implements_constant
                .fields
                .iter()
                .zip(trait_ty.fields.iter())
            {
                deduction = deduction
                    .combine(deduce_in_constant(implements_field, trait_field)?)
                    .ok_or(Error::NonUnifiable)?;
            }

            Ok(deduction)
        }

        // unifying two enums
        (constant::Constant::Enum(implements_constant), constant::Constant::Enum(trait_ty)) => {
            let deduction = dedcue_in_type(
                &ty::Type::Enum(implements_constant.enum_ty.clone()),
                &ty::Type::Enum(trait_ty.enum_ty.clone()),
            )?;

            if implements_constant.local_variant_ref != trait_ty.local_variant_ref {
                return Err(Error::NonUnifiable);
            }

            match (&dynements_constant.value, &trait_ty.value) {
                (Some(implements_value), Some(trait_value)) => deduction
                    .combine(deduce_in_constant(implements_value, trait_value)?)
                    .ok_or(Error::NonUnifiable),

                (None, None) => Ok(deduction),

                _ => Err(Error::InvalidArguments),
            }
        }

        // unifying two arrays
        (
            constant::Constant::Array(implements_constant),
            constant::Constant::Array(trait_constant),
        ) => {
            let mut deduction = dedcue_in_type(
                &dynements_constant.element_ty,
                &dynements_constant.element_ty,
            )?;

            if implements_constant.elements.len() != trait_constant.elements.len() {
                return Err(Error::InvalidArguments);
            }

            for (implements_element, trait_element) in implements_constant
                .elements
                .iter()
                .zip(trait_constant.elements.iter())
            {
                deduction = deduction
                    .combine(deduce_in_constant(implements_element, trait_element)?)
                    .ok_or(Error::NonUnifiable)?;
            }

            Ok(deduction)
        }

        // unifying two tuples
        (
            constant::Constant::Tuple(implements_constant),
            constant::Constant::Tuple(trait_constant),
        ) => {
            let unpacked_count = implements_constant
                .elements
                .iter()
                .filter(|x| x.is_unpacked())
                .count();

            match unpacked_count {
                // no unpacked elements case
                0 => {
                    if implements_constant.elements.len() != trait_constant.elements.len() {
                        return Err(Error::NonUnifiable);
                    }

                    let mut deduction = Deduction::default();

                    for (implements_element, trait_element) in implements_constant
                        .elements
                        .iter()
                        .zip(trait_constant.elements.iter())
                    {
                        let (
                            constant::TupleElement::Regular(implements_element),
                            constant::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(deduce_in_constant(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    Ok(deduction)
                }

                // one unpacked element case
                1 => {
                    // impossible to unify
                    if implements_constant.elements.len() > trait_constant.elements.len() + 1 {
                        return Err(Error::NonUnifiable);
                    }

                    let mut deduction = Deduction::default();

                    let unpacked_position = implements_constant
                        .elements
                        .iter()
                        .position(constant::TupleElement::is_unpacked)
                        .unwrap();

                    let head_range = 0..unpacked_position;
                    let implements_tail_range =
                        (unpacked_position + 1)..implements_constant.elements.len();
                    let trait_tail_range = (trait_constant.elements.len()
                        - implements_tail_range.clone().count())
                        ..trait_constant.elements.len();
                    let trait_unpack_range = unpacked_position..trait_tail_range.start;

                    // unify head
                    for (implements_element, trait_element) in implements_constant.elements
                        [head_range.clone()]
                    .iter()
                    .zip(&trait_constant.elements[head_range])
                    {
                        let (
                            constant::TupleElement::Regular(implements_element),
                            constant::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(deduce_in_constant(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    // unify tail
                    for (implements_element, trait_element) in implements_constant.elements
                        [implements_tail_range]
                        .iter()
                        .zip(&trait_constant.elements[trait_tail_range])
                    {
                        let (
                            constant::TupleElement::Regular(implements_element),
                            constant::TupleElement::Regular(trait_element),
                        ) = (implements_element, trait_element)
                        else {
                            unreachable!()
                        };

                        deduction = deduction
                            .combine(deduce_in_constant(implements_element, trait_element)?)
                            .ok_or(Error::NonUnifiable)?;
                    }

                    // unify unpacked
                    let packed_tuple = || -> constant::Constant {
                        constant::Constant::Tuple(constant::Tuple {
                            elements: trait_constant.elements[trait_unpack_range.clone()].to_vec(),
                        })
                    };

                    match implements_constant.elements[unpacked_position]
                        .as_unpacked()
                        .unwrap()
                    {
                        constant::Unpacked::Parameter(parameter) => {
                            if let Some(previous_ty) = deduction
                                .substitution
                                .constant_substitutions
                                .insert(*parameter, packed_tuple())
                            {
                                // collision between type deduction
                                if previous_ty != packed_tuple() {
                                    return Err(Error::NonUnifiable);
                                }
                            }
                        }
                        constant::Unpacked::TraitAssociated(trait_associated) => {
                            if let Some(previous_ty) = deduction
                                .trait_associated_constant_deductions
                                .insert(trait_associated.clone(), packed_tuple())
                            {
                                // collision between trait associated type deduction
                                if previous_ty != packed_tuple() {
                                    return Err(Error::NonUnifiable);
                                }
                            }
                        }
                    }

                    Ok(deduction)
                }

                _ => Err(Error::InvalidArguments),
            }
        }

        // equality check
        (implements_constant, trait_constant) => {
            if implements_constant != trait_constant {
                return Err(Error::NonUnifiable);
            }

            Ok(Deduction::default())
        }
    }
}

pub(super) fn deduce_in_local_substitution(
    implements_substitution: &LocalSubstitution,
    trait_substitution: &LocalSubstitution,
) -> Result<Deduction, Error> {
    if implements_substitution.types.len() != trait_substitution.types.len()
        || implements_substitution.constants.len() != trait_substitution.constants.len()
        || implements_substitution.lifetimes.len() != trait_substitution.lifetimes.len()
    {
        return Err(Error::InvalidArguments);
    }

    let mut deduction = Deduction::default();

    // deduce lifetime
    for (implements_lt, trait_lt) in implements_substitution
        .lifetimes
        .iter()
        .zip(trait_substitution.lifetimes.iter())
    {
        let Lifetime::Parameter(implements_lt_parameter) = implements_lt else {
            continue;
        };

        let Some(previous_lt_argument) = deduction
            .substitution
            .lifetime_substitutions
            .insert(*implements_lt_parameter, *trait_lt)
        else {
            continue;
        };

        // collision between lifetime deduction
        if previous_lt_argument != *trait_lt {
            return Err(Error::NonUnifiable);
        }
    }

    // deduce types
    for (implements_ty, trait_ty) in implements_substitution
        .types
        .iter()
        .zip(trait_substitution.types.iter())
    {
        deduction = deduction
            .combine(dedcue_in_type(implements_ty, trait_ty)?)
            .ok_or(Error::NonUnifiable)?;
    }

    // deduce constants
    for (implements_constant, trait_constant) in implements_substitution
        .constants
        .iter()
        .zip(trait_substitution.constants.iter())
    {
        deduction = deduction
            .combine(deduce_in_constant(implements_constant, trait_constant)?)
            .ok_or(Error::NonUnifiable)?;
    }

    Ok(deduction)
}

#[cfg(test)]
mod tests;
