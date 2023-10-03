//! Contains code related to generic and trait resolution.

use std::collections::HashMap;

use super::Table;
use crate::{
    constant,
    symbol::{
        ImplementsSignature, LocalImplementsRef, LocalSubstitution, Substitution, TraitConstant,
        TraitRef,
    },
    ty::{self, Lifetime},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct TraitInstantiation {
    pub trait_ref: TraitRef,
    pub substitution: LocalSubstitution,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("the generic argument substitution passed to the function is not concrete")]
    NonConcreteSubstitutionError,

    #[error("the input passed into the function is invalid for the table")]
    InvalidInput,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
struct Deduction {
    substitution: Substitution,
    trait_associated_type_deductions: HashMap<ty::TraitAssociated, ty::Type>,
    trait_associated_constant_deductions: HashMap<constant::TraitAssociated, constant::Constant>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementsKeys {
    Negative(usize),
    Positive(LocalImplementsRef),
}

impl Table {
    /// Resolves a trait substitution to a concrete implements.
    ///
    /// The bounds are not checked as the bound doesn't effect the trait resolution mechanism.
    /// Therefore, you might need to manually check whether the bounds of the resolved implements
    /// are satisfied.
    ///
    /// # Returns
    ///
    /// `Some` if the trait resolution succeeded, `None` if no trait implements was found.
    ///
    /// # Errors
    ///
    /// [`Error::NonConcreteSubstitutionError`] is returned if the trait substitution is not
    /// concrete.
    /// [`Error::InvalidInput`] is returned if the input passed to the function was invalid.
    pub fn resolve_trait_implements(
        &self,
        trait_instantiation: &TraitInstantiation,
    ) -> Result<Option<super::Implements>, Error> {
        if !trait_instantiation.substitution.is_concrete() {
            return Err(Error::NonConcreteSubstitutionError);
        }

        let trait_sym = self
            .traits
            .get(trait_instantiation.trait_ref.0)
            .ok_or(Error::InvalidInput)?;

        let mut deduced_substitutions_by_implements_key = HashMap::new();

        let positives = trait_sym
            .implements
            .iter()
            .enumerate()
            .map(|(index, implements)| {
                (
                    ImplementsKeys::Positive(LocalImplementsRef(index)),
                    &implements.signature,
                )
            });

        let negatives = trait_sym.negative_implements.iter().enumerate().map(
            |(index, implements_signature)| (ImplementsKeys::Negative(index), implements_signature),
        );

        for (implements_key, signature) in positives.chain(negatives) {
            let Some(deduced_substitution) =
                self.get_deduced_substitution(&trait_instantiation.substitution, signature)
            else {
                continue;
            };

            assert!(deduced_substitutions_by_implements_key
                .insert(implements_key, deduced_substitution)
                .is_none());
        }

        todo!()
    }

    fn get_deduced_substitution(
        &self,
        trait_substitution: &LocalSubstitution,
        implements_signature: &ImplementsSignature,
    ) -> Option<Substitution> {
        todo!()
    }

    #[allow(clippy::too_many_lines)]
    fn dedcue_in_type(
        implements_ty: &ty::Type,
        trait_ty: &ty::Type,
        mut previous_deduction: Deduction,
    ) -> Result<Option<Deduction>, Error> {
        match (implements_ty, trait_ty) {
            (ty::Type::Parameter(implements_ty_parameter), trait_ty) => {
                let Some(previous_ty) = previous_deduction
                    .substitution
                    .type_substitutions
                    .insert(*implements_ty_parameter, trait_ty.clone())
                else {
                    return Ok(Some(previous_deduction));
                };

                // collision between type deduction
                if previous_ty != *trait_ty {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }

            (ty::Type::TraitAssociated(implements_trait_associated), trait_ty) => {
                let Some(previous_ty) = previous_deduction
                    .trait_associated_type_deductions
                    .insert(implements_trait_associated.clone(), trait_ty.clone())
                else {
                    return Ok(Some(previous_deduction));
                };

                // collision between trait associated type deduction
                if previous_ty != *trait_ty {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }

            // unifying two enums
            (ty::Type::Enum(implements_ty), ty::Type::Enum(trait_ty)) => {
                if implements_ty.enum_ref != trait_ty.enum_ref {
                    return Ok(None);
                }

                Self::deduce_in_substitution(
                    &implements_ty.substitution,
                    &trait_ty.substitution,
                    previous_deduction,
                )
            }

            // unifying two references
            (ty::Type::Reference(implements_ty), ty::Type::Reference(trait_ty)) => {
                if implements_ty.qualifier != trait_ty.qualifier {
                    return Ok(None);
                }

                if let Lifetime::Parameter(implements_lt_parameter) = implements_ty.lifetime {
                    if let Some(previous_lifetime_argument) = previous_deduction
                        .substitution
                        .lifetime_substitutions
                        .insert(implements_lt_parameter, trait_ty.lifetime)
                    {
                        // deduction collision
                        if previous_lifetime_argument != trait_ty.lifetime {
                            return Ok(None);
                        }
                    }
                };

                Self::dedcue_in_type(
                    &implements_ty.operand,
                    &trait_ty.operand,
                    previous_deduction,
                )
            }

            // unifying two pointers
            (ty::Type::Pointer(implements_ty), ty::Type::Pointer(trait_ty)) => {
                if implements_ty.qualifier != trait_ty.qualifier {
                    return Ok(None);
                }

                Self::dedcue_in_type(
                    &implements_ty.operand,
                    &trait_ty.operand,
                    previous_deduction,
                )
            }

            // unifying two structs
            (ty::Type::Struct(implements_ty), ty::Type::Struct(trait_ty)) => {
                if implements_ty.struct_ref != trait_ty.struct_ref {
                    return Ok(None);
                }

                Self::deduce_in_substitution(
                    &implements_ty.substitution,
                    &trait_ty.substitution,
                    previous_deduction,
                )
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
                            return Ok(None);
                        }

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

                            previous_deduction = match Self::dedcue_in_type(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
                        }

                        Ok(Some(previous_deduction))
                    }

                    // one unpacked element case
                    1 => {
                        // impossible to unify
                        if implements_ty.elements.len() > trait_ty.elements.len() + 1 {
                            return Ok(None);
                        }

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

                            previous_deduction = match Self::dedcue_in_type(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
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

                            previous_deduction = match Self::dedcue_in_type(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
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
                                if let Some(previous_ty) = previous_deduction
                                    .substitution
                                    .type_substitutions
                                    .insert(*parameter, packed_tuple())
                                {
                                    // collision between type deduction
                                    if previous_ty != packed_tuple() {
                                        return Ok(None);
                                    }
                                }
                            }
                            ty::Unpacked::TraitAssociated(trait_associated) => {
                                if let Some(previous_ty) = previous_deduction
                                    .trait_associated_type_deductions
                                    .insert(trait_associated.clone(), packed_tuple())
                                {
                                    // collision between trait associated type deduction
                                    if previous_ty != packed_tuple() {
                                        return Ok(None);
                                    }
                                }
                            }
                        }

                        Ok(Some(previous_deduction))
                    }

                    _ => Err(Error::InvalidInput),
                }
            }
            // unify two arrays
            (ty::Type::Array(implements_ty), ty::Type::Array(trait_ty)) => {
                previous_deduction = match Self::deduce_in_constant(
                    &implements_ty.size,
                    &trait_ty.size,
                    previous_deduction,
                )? {
                    Some(deduction) => deduction,
                    None => return Ok(None),
                };

                Self::dedcue_in_type(
                    &implements_ty.element_ty,
                    &trait_ty.element_ty,
                    previous_deduction,
                )
            }

            (implements_ty, trait_ty) => {
                if implements_ty != trait_ty {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn deduce_in_constant(
        implements_constant: &constant::Constant,
        trait_constant: &constant::Constant,
        mut previous_deduction: Deduction,
    ) -> Result<Option<Deduction>, Error> {
        match (implements_constant, trait_constant) {
            (constant::Constant::Parameter(implements_constant_parameter), trait_constant) => {
                let Some(previous_constant) = previous_deduction
                    .substitution
                    .constant_substitutions
                    .insert(*implements_constant_parameter, trait_constant.clone())
                else {
                    return Ok(Some(previous_deduction));
                };

                // collision between constant deduction
                if previous_constant != *trait_constant {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }

            (constant::Constant::TraitAssociated(implements_trait_asssociated), trait_constant) => {
                let Some(previous_constant) = previous_deduction
                    .trait_associated_constant_deductions
                    .insert(implements_trait_asssociated.clone(), trait_constant.clone())
                else {
                    return Ok(Some(previous_deduction));
                };

                // collision between trait associated constant deduction
                if previous_constant != *trait_constant {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }

            // unify two structs
            (
                constant::Constant::Struct(implements_constant),
                constant::Constant::Struct(trait_ty),
            ) => {
                previous_deduction = match Self::dedcue_in_type(
                    &ty::Type::Struct(implements_constant.struct_ty.clone()),
                    &ty::Type::Struct(trait_ty.struct_ty.clone()),
                    previous_deduction,
                )? {
                    Some(some) => some,
                    None => return Ok(None),
                };

                if implements_constant.fields.len() != trait_ty.fields.len() {
                    return Err(Error::InvalidInput);
                }

                for (implements_field, trait_field) in implements_constant
                    .fields
                    .iter()
                    .zip(trait_ty.fields.iter())
                {
                    previous_deduction = match Self::deduce_in_constant(
                        implements_field,
                        trait_field,
                        previous_deduction,
                    )? {
                        Some(some) => some,
                        None => return Ok(None),
                    };
                }

                Ok(Some(previous_deduction))
            }

            // unifying two enums
            (constant::Constant::Enum(implements_constant), constant::Constant::Enum(trait_ty)) => {
                previous_deduction = match Self::dedcue_in_type(
                    &ty::Type::Enum(implements_constant.enum_ty.clone()),
                    &ty::Type::Enum(trait_ty.enum_ty.clone()),
                    previous_deduction,
                )? {
                    Some(some) => some,
                    None => return Ok(None),
                };

                if implements_constant.local_variant_ref != trait_ty.local_variant_ref {
                    return Ok(None);
                }

                match (&implements_constant.value, &trait_ty.value) {
                    (Some(implements_value), Some(trait_value)) => {
                        Self::deduce_in_constant(implements_value, trait_value, previous_deduction)
                    }

                    (None, None) => Ok(Some(previous_deduction)),

                    _ => Err(Error::InvalidInput),
                }
            }

            // unifying two arrays
            (
                constant::Constant::Array(implements_constant),
                constant::Constant::Array(trait_constant),
            ) => {
                previous_deduction = match Self::dedcue_in_type(
                    &implements_constant.element_ty,
                    &trait_constant.element_ty,
                    previous_deduction,
                )? {
                    Some(some) => some,
                    None => return Ok(None),
                };

                if implements_constant.elements.len() != trait_constant.elements.len() {
                    return Ok(None);
                }

                for (implements_element, trait_element) in implements_constant
                    .elements
                    .iter()
                    .zip(trait_constant.elements.iter())
                {
                    previous_deduction = match Self::deduce_in_constant(
                        implements_element,
                        trait_element,
                        previous_deduction,
                    )? {
                        Some(some) => some,
                        None => return Ok(None),
                    };
                }

                Ok(Some(previous_deduction))
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
                            return Ok(None);
                        }

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

                            previous_deduction = match Self::deduce_in_constant(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
                        }

                        Ok(Some(previous_deduction))
                    }

                    // one unpacked element case
                    1 => {
                        // impossible to unify
                        if implements_constant.elements.len() > trait_constant.elements.len() + 1 {
                            return Ok(None);
                        }

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

                            previous_deduction = match Self::deduce_in_constant(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
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

                            previous_deduction = match Self::deduce_in_constant(
                                implements_element,
                                trait_element,
                                previous_deduction,
                            )? {
                                Some(deduction) => deduction,
                                None => return Ok(None),
                            };
                        }

                        // unify unpacked
                        let packed_tuple = || -> constant::Constant {
                            constant::Constant::Tuple(constant::Tuple {
                                elements: trait_constant.elements[trait_unpack_range.clone()]
                                    .to_vec(),
                            })
                        };

                        match implements_constant.elements[unpacked_position]
                            .as_unpacked()
                            .unwrap()
                        {
                            constant::Unpacked::Parameter(parameter) => {
                                if let Some(previous_ty) = previous_deduction
                                    .substitution
                                    .constant_substitutions
                                    .insert(*parameter, packed_tuple())
                                {
                                    // collision between type deduction
                                    if previous_ty != packed_tuple() {
                                        return Ok(None);
                                    }
                                }
                            }
                            constant::Unpacked::TraitAssociated(trait_associated) => {
                                if let Some(previous_ty) = previous_deduction
                                    .trait_associated_constant_deductions
                                    .insert(trait_associated.clone(), packed_tuple())
                                {
                                    // collision between trait associated type deduction
                                    if previous_ty != packed_tuple() {
                                        return Ok(None);
                                    }
                                }
                            }
                        }

                        Ok(Some(previous_deduction))
                    }

                    _ => Err(Error::InvalidInput),
                }
            }

            // equality check
            (implements_constant, trait_constant) => {
                if implements_constant != trait_constant {
                    return Ok(None);
                }

                Ok(Some(previous_deduction))
            }
        }
    }

    fn deduce_in_substitution(
        implements_substitution: &LocalSubstitution,
        trait_substitution: &LocalSubstitution,
        mut previous_deduction: Deduction,
    ) -> Result<Option<Deduction>, Error> {
        if implements_substitution.types.len() != trait_substitution.types.len()
            || implements_substitution.constants.len() != trait_substitution.constants.len()
            || implements_substitution.lifetimes.len() != trait_substitution.lifetimes.len()
        {
            return Err(Error::InvalidInput);
        }

        // deduce lifetime
        for (implements_lt, trait_lt) in implements_substitution
            .lifetimes
            .iter()
            .zip(trait_substitution.lifetimes.iter())
        {
            let Lifetime::Parameter(implements_lt_parameter) = implements_lt else {
                continue;
            };

            let Some(previous_lt_argument) = previous_deduction
                .substitution
                .lifetime_substitutions
                .insert(*implements_lt_parameter, *trait_lt)
            else {
                continue;
            };

            // collision between lifetime deduction
            if previous_lt_argument != *trait_lt {
                return Ok(None);
            }
        }

        // deduce types
        for (implements_ty, trait_ty) in implements_substitution
            .types
            .iter()
            .zip(trait_substitution.types.iter())
        {
            previous_deduction =
                match Self::dedcue_in_type(implements_ty, trait_ty, previous_deduction)? {
                    Some(deduction) => deduction,
                    None => return Ok(None),
                };
        }

        // deduce constants
        for (implements_constant, trait_constant) in implements_substitution
            .constants
            .iter()
            .zip(trait_substitution.constants.iter())
        {
            previous_deduction = match Self::deduce_in_constant(
                implements_constant,
                trait_constant,
                previous_deduction,
            )? {
                Some(deduction) => deduction,
                None => return Ok(None),
            };
        }

        Ok(Some(previous_deduction))
    }
}

#[cfg(test)]
mod tests;
