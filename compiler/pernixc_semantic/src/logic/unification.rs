//! Contains the code for unifying terms.

use std::{borrow::Cow, collections::hash_map::Entry};

use super::Substitution;
use crate::entity::{
    constant::{self, Constant},
    r#type::{self, Type},
    region::Region,
    GenericArguments, Model,
};

/// The unifier has already mapped into a particular term but it has to unify with another non-equal
/// term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConflictError<Substitution, Term> {
    /// The unifier that has already mapped into a particular term.
    pub unifier: Substitution,

    /// The term that the unifier has already mapped into.
    pub existing: Term,

    /// The term that the unifier has to unify with.
    pub target: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Error<'a, S: Model> {
    /// These two types can't be unified.
    Type(Cow<'a, Type<S>>, Cow<'a, Type<S>>),

    /// These two constants can't be unified.
    Constant(Cow<'a, Constant<S>>, Cow<'a, Constant<S>>),

    /// These two regions can't be unified.
    Region(&'a Region<S>, &'a Region<S>),

    /// These two generic arguments can't be unified.
    GenericArguments(&'a GenericArguments<S>, &'a GenericArguments<S>),

    TypeConflict(ConflictError<Cow<'a, Type<S>>, Cow<'a, Type<S>>>),
    ConstantConflict(ConflictError<Cow<'a, Constant<S>>, Cow<'a, Constant<S>>>),
    RegionConflict(ConflictError<Cow<'a, Region<S>>, Cow<'a, Region<S>>>),
}

/// Describes which types of terms can be unified.
pub trait Config:
    std::fmt::Debug + Clone + Copy + PartialEq + Eq + std::hash::Hash + Default + Send + Sync + 'static
{
    /// The system that the terms are in.
    type Model: Model;

    /// Determine whether can [`r#type::Primitive`] can be mapped to another type.
    const MAP_PRIMITIVE_TYPE: bool;

    /// Determine whether can [`System::TypeInference`] can be mapped to another type.
    const MAP_TYPE_INFERENCE: bool;

    /// Determine whether can [`r#type::Algebraic`] can be mapped to another type.
    const MAP_ALGRBRAICE_TYPE: bool;

    /// Determine whether can [`r#type::Pointer`] can be mapped to another type.
    const MAP_POINTER_TYPE: bool;

    /// Determine whether can [`r#type::Reference`] can be mapped to another type.
    const MAP_REFERENCE_TYPE: bool;

    /// Determine whether can [`r#type::Array`] can be mapped to another type.
    const MAP_ARRAY_TYPE: bool;

    /// Determine whether can [`r#type::TraitMember`] can be mapped to another type.
    const MAP_TRAIT_TYPE: bool;

    /// Determine whether can [`TypeParameterID`] can be mapped to another type.
    const MAP_TYPE_PARAMETER: bool;

    /// Determine whether can [`r#type::Tuple`] can be mapped to another type.
    const MAP_TUPLE_TYPE: bool;

    /// Determine whether can [`constant::Primitive`] can be mapped to another constant.
    const MAP_PRIMITIVE_CONSTANT: bool;

    /// Determine whether can [`System::ConstantInference`] can be mapped to another constant.
    const MAP_CONSTANT_INFERENCE: bool;

    /// Determine whether can [`constant::Struct`] can be mapped to another constant.
    const MAP_STRUCT_CONSTANT: bool;

    /// Determine whether can [`constant::Enum`] can be mapped to another constant.
    const MAP_ENUM_CONSTANT: bool;

    /// Determine whether can [`constant::Array`] can be mapped to another constant.
    const MAP_ARRAY_CONSTANT: bool;

    /// Determine whether can [`ConstantParameterID`] can be mapped to another constant.
    const MAP_CONSTANT_PARAMETER: bool;

    /// Determine whether can [`constant::TraitMember`] can be mapped to another constant.
    const MAP_TRAIT_CONSTANT: bool;

    /// Determine whether can [`constant::Tuple`] can be mapped to another constant.
    const MAP_TUPLE_CONSTANT: bool;

    /// Determine whether can [`LifetimeParameterID`] can be mapped to another region.
    const MAP_LIFETIME_PARAMETER: bool;

    /// Determine whether can [`Region::Context`] can be mapped to another region.
    const MAP_REGION_CONTEXT: bool;

    /// Determine whether can [`Region::Static`] can be mapped to another region.
    const MAP_STATIC_REGION: bool;
}

/// Is a struct that implements [`Config`] which allows unification of term variables and
/// trait member variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct VariableSubstitution<S>(std::marker::PhantomData<S>);

impl<S: Model> Config for VariableSubstitution<S> {
    type Model = S;

    const MAP_ALGRBRAICE_TYPE: bool = false;
    const MAP_ARRAY_CONSTANT: bool = false;
    const MAP_ARRAY_TYPE: bool = false;
    const MAP_CONSTANT_INFERENCE: bool = false;
    const MAP_CONSTANT_PARAMETER: bool = true;
    const MAP_ENUM_CONSTANT: bool = false;
    const MAP_LIFETIME_PARAMETER: bool = true;
    const MAP_POINTER_TYPE: bool = false;
    const MAP_PRIMITIVE_CONSTANT: bool = false;
    const MAP_PRIMITIVE_TYPE: bool = false;
    const MAP_REFERENCE_TYPE: bool = false;
    const MAP_REGION_CONTEXT: bool = false;
    const MAP_STATIC_REGION: bool = false;
    const MAP_STRUCT_CONSTANT: bool = false;
    const MAP_TRAIT_CONSTANT: bool = true;
    const MAP_TRAIT_TYPE: bool = true;
    const MAP_TUPLE_CONSTANT: bool = false;
    const MAP_TUPLE_TYPE: bool = false;
    const MAP_TYPE_INFERENCE: bool = false;
    const MAP_TYPE_PARAMETER: bool = true;
}

macro_rules! tuple_unifiable_function {
    ($name:ident, $domain:ident) => {
        fn $name<S: Model>(unifier: &$domain::Tuple<S>, target: &$domain::Tuple<S>) -> bool {
            if target.elements.iter().filter(|x| x.is_unpacked()).count() > 0 {
                return false;
            }

            match unifier.elements.iter().filter(|x| x.is_unpacked()).count() {
                0 => unifier.elements.len() == target.elements.len(),
                1 => unifier.elements.len() <= target.elements.len() + 1,
                _ => false,
            }
        }
    };
}

tuple_unifiable_function!(tuple_type_unifiable, r#type);
tuple_unifiable_function!(tuple_constant_unifiable, constant);

macro_rules! tuple_unify_function {
    ($name:ident, $domain:ident, $kind:ident, $param_flag:ident, $trai_member_flag:ident, $map_name: ident, $err_name: ident) => {
        fn $name<'a, T: Config>(
            unifier: &'a $domain::Tuple<T::Model>,
            target: &'a $domain::Tuple<T::Model>,
            mut existing: Substitution<'a, T::Model>,
        ) -> Result<Substitution<'a, T::Model>, Error<'a, T::Model>> {
            let unpacked_count = unifier.elements.iter().filter(|x| x.is_unpacked()).count();

            match unpacked_count {
                // no unpacked elements case
                0 => {
                    for (unifier_element, target_element) in
                        unifier.elements.iter().zip(target.elements.iter())
                    {
                        let unifier_element = unifier_element.as_regular().unwrap();
                        let target_element = target_element.as_regular().unwrap();

                        existing =
                            $kind::unify_internal::<T>(unifier_element, target_element, existing)?;
                    }

                    Ok(existing)
                }

                // one unpacked element case
                1 => {
                    let unpacked_position = unifier
                        .elements
                        .iter()
                        .position($domain::TupleElement::is_unpacked)
                        .unwrap();

                    let head_range = 0..unpacked_position;
                    let unifier_tail_range = (unpacked_position + 1)..unifier.elements.len();
                    let target_tail_range = (target.elements.len()
                        - unifier_tail_range.clone().count())
                        ..target.elements.len();
                    let target_unpack_range = unpacked_position..target_tail_range.start;

                    // unify head
                    for (unifier_element, target_element) in unifier.elements[head_range.clone()]
                        .iter()
                        .zip(&target.elements[head_range])
                    {
                        let unifier_element = unifier_element.as_regular().unwrap();
                        let target_element = target_element.as_regular().unwrap();

                        existing =
                            $kind::unify_internal::<T>(unifier_element, target_element, existing)?;
                    }

                    // unify tail
                    for (unifier_element, target_element) in unifier.elements[unifier_tail_range]
                        .iter()
                        .zip(&target.elements[target_tail_range])
                    {
                        let unifier_element = unifier_element.as_regular().unwrap();
                        let target_element = target_element.as_regular().unwrap();

                        existing =
                            $kind::unify_internal::<T>(unifier_element, target_element, existing)?;
                    }

                    let target_unpack = Cow::Owned($kind::Tuple($domain::Tuple {
                        elements: target.elements[target_unpack_range].to_vec(),
                    }));

                    let unpacked = unifier.elements[unpacked_position].as_unpacked().unwrap();
                    match unpacked {
                        $domain::Unpacked::Parameter(parameter) => {
                            let parameter: Cow<'a, $kind<T::Model>> =
                                Cow::Owned($kind::Parameter(*parameter));

                            if T::$param_flag {
                                match existing.$map_name.entry(parameter.clone()) {
                                    Entry::Occupied(entry) => {
                                        if **entry.get() != *target_unpack {
                                            return Err(Error::$err_name(ConflictError {
                                                unifier: parameter,
                                                existing: entry.remove(),
                                                target: target_unpack,
                                            }));
                                        }
                                    }
                                    Entry::Vacant(entry) => {
                                        entry.insert(target_unpack);
                                    }
                                }
                            } else {
                                return Err(Error::$kind(parameter, target_unpack));
                            }
                        }
                        $domain::Unpacked::TraitMember(trait_member) => {
                            let trait_member: Cow<'a, $kind<T::Model>> =
                                Cow::Owned($kind::TraitMember(trait_member.clone()));

                            if T::$trai_member_flag {
                                match existing.$map_name.entry(trait_member.clone()) {
                                    Entry::Occupied(entry) => {
                                        if **entry.get() != *target_unpack {
                                            return Err(Error::$err_name(ConflictError {
                                                unifier: trait_member,
                                                existing: entry.remove(),
                                                target: target_unpack,
                                            }));
                                        }
                                    }
                                    Entry::Vacant(entry) => {
                                        entry.insert(target_unpack);
                                    }
                                }
                            } else {
                                return Err(Error::$kind(trait_member, target_unpack));
                            }
                        }
                    }

                    Ok(existing)
                }

                _ => unreachable!(),
            }
        }
    };
}

tuple_unify_function!(
    unify_tuple_constant,
    constant,
    Constant,
    MAP_CONSTANT_PARAMETER,
    MAP_TRAIT_CONSTANT,
    constants,
    ConstantConflict
);
tuple_unify_function!(
    unify_tuple_type,
    r#type,
    Type,
    MAP_TYPE_PARAMETER,
    MAP_TRAIT_CONSTANT,
    types,
    TypeConflict
);

impl<S: Model> Type<S> {
    /// Unifies two type terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms can't be unified.
    pub fn unify<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::unify_internal::<T>(unifier, target, Substitution::default())
    }

    fn unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        // early unification
        let early_unify = match unifier {
            Self::Primitive(_) => T::MAP_PRIMITIVE_TYPE,
            Self::Inference(_) => T::MAP_TYPE_INFERENCE,
            Self::Algebraic(_) => T::MAP_ALGRBRAICE_TYPE,
            Self::Pointer(_) => T::MAP_POINTER_TYPE,
            Self::Reference(_) => T::MAP_REFERENCE_TYPE,
            Self::Array(_) => T::MAP_ARRAY_TYPE,
            Self::TraitMember(_) => T::MAP_TRAIT_TYPE,
            Self::Parameter(_) => T::MAP_TYPE_PARAMETER,
            Self::Tuple(_) => T::MAP_TUPLE_TYPE,
        };

        if early_unify {
            match existing.types.entry(Cow::Borrowed(unifier)) {
                Entry::Occupied(entry) => {
                    if **entry.get() != *target {
                        return Err(Error::TypeConflict(ConflictError {
                            unifier: Cow::Borrowed(unifier),
                            existing: entry.remove(),
                            target: Cow::Borrowed(target),
                        }));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(Cow::Borrowed(target));
                }
            }

            return Ok(existing);
        }

        Self::sub_structural_unify_internal::<T>(unifier, target, existing)
    }

    fn sub_structural_unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        // sub-structural unification
        match (unifier, target) {
            (Self::Algebraic(unifier), Self::Algebraic(target)) if unifier.kind == target.kind => {
                GenericArguments::unify_internal::<T>(
                    &unifier.generic_arguments,
                    &target.generic_arguments,
                    existing,
                )
            }
            (Self::Pointer(unifier), Self::Pointer(target))
                if unifier.qualifier == target.qualifier =>
            {
                Self::unify_internal::<T>(&unifier.pointee, &target.pointee, existing)
            }
            (Self::Reference(unifier), Self::Reference(target))
                if unifier.qualifier == target.qualifier =>
            {
                existing = Region::unify_internal::<T>(&unifier.region, &target.region, existing)?;
                Self::unify_internal::<T>(&unifier.pointee, &target.pointee, existing)
            }
            (Self::Array(unifier), Self::Array(target)) => {
                existing =
                    Constant::unify_internal::<T>(&unifier.length, &target.length, existing)?;
                Self::unify_internal::<T>(&unifier.element, &target.element, existing)
            }
            (Self::TraitMember(unifier), Self::TraitMember(target))
                if unifier.trait_type_id == target.trait_type_id =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.trait_generic_arguments,
                    &target.trait_generic_arguments,
                    existing,
                )?;
                GenericArguments::unify_internal::<T>(
                    &unifier.member_generic_arguments,
                    &target.member_generic_arguments,
                    existing,
                )
            }
            (Self::Tuple(unifier), Self::Tuple(target))
                if tuple_type_unifiable(unifier, target) =>
            {
                unify_tuple_type::<T>(unifier, target, existing)
            }

            (unifier, target) => {
                if unifier == target {
                    Ok(existing)
                } else {
                    Err(Error::Type(Cow::Borrowed(unifier), Cow::Borrowed(target)))
                }
            }
        }
    }
}

impl<S: Model> Constant<S> {
    /// Unifies two constant terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms can't be unified.
    pub fn unify<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::unify_internal::<VariableSubstitution<S>>(unifier, target, Substitution::default())
    }

    fn unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        let early_unify = match unifier {
            Self::Primitive(_) if T::MAP_PRIMITIVE_CONSTANT => todo!(),
            Self::Inference(_) if T::MAP_CONSTANT_INFERENCE => todo!(),
            Self::Struct(_) if T::MAP_STRUCT_CONSTANT => todo!(),
            Self::Enum(_) if T::MAP_ENUM_CONSTANT => todo!(),
            Self::Array(_) if T::MAP_ARRAY_CONSTANT => todo!(),
            Self::Parameter(_) if T::MAP_CONSTANT_PARAMETER => todo!(),
            Self::TraitMember(_) if T::MAP_TRAIT_CONSTANT => todo!(),
            Self::Tuple(_) if T::MAP_TUPLE_CONSTANT => todo!(),
            _ => false,
        };

        if early_unify {
            match existing.constants.entry(Cow::Borrowed(unifier)) {
                Entry::Occupied(entry) => {
                    if **entry.get() != *target {
                        return Err(Error::ConstantConflict(ConflictError {
                            unifier: Cow::Borrowed(unifier),
                            existing: entry.remove(),
                            target: Cow::Borrowed(target),
                        }));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(Cow::Borrowed(target));
                }
            }

            return Ok(existing);
        }

        Self::sub_structural_unify_internal::<T>(unifier, target, existing)
    }

    fn sub_structural_unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        match (unifier, target) {
            (Self::Struct(unifier), Self::Struct(target))
                if unifier.struct_id == target.struct_id
                    && unifier.fields.len() == target.fields.len() =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.generic_arguments,
                    &target.generic_arguments,
                    existing,
                )?;

                for (unifier, target) in unifier.fields.iter().zip(target.fields.iter()) {
                    existing = Self::unify_internal::<T>(unifier, target, existing)?;
                }

                Ok(existing)
            }

            (Self::Enum(unifier), Self::Enum(target))
                if unifier.variant_id == target.variant_id
                    && unifier.associated_value.is_some() == target.associated_value.is_some() =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.generic_arguments,
                    &target.generic_arguments,
                    existing,
                )?;

                match (&unifier.associated_value, &target.associated_value) {
                    (None, None) => Ok(existing),
                    (Some(unifier), Some(target)) => {
                        Self::unify_internal::<T>(unifier, target, existing)
                    }
                    (_, _) => unreachable!(),
                }
            }

            (Self::Array(unifier), Self::Array(target))
                if unifier.elements.len() == target.elements.len() =>
            {
                existing =
                    Type::unify_internal::<T>(&unifier.element_ty, &target.element_ty, existing)?;

                for (unifier, target) in unifier.elements.iter().zip(target.elements.iter()) {
                    existing = Self::unify_internal::<T>(unifier, target, existing)?;
                }

                Ok(existing)
            }

            (Self::TraitMember(unifier), Self::TraitMember(target))
                if unifier.trait_constant_id == target.trait_constant_id =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.trait_substitution,
                    &target.trait_substitution,
                    existing,
                )?;

                Ok(existing)
            }

            (Self::Tuple(unifier), Self::Tuple(target))
                if tuple_constant_unifiable(unifier, target) =>
            {
                unify_tuple_constant::<T>(unifier, target, existing)
            }

            (unifier, target) => {
                if unifier == target {
                    Ok(existing)
                } else {
                    Err(Error::Constant(
                        Cow::Borrowed(unifier),
                        Cow::Borrowed(target),
                    ))
                }
            }
        }
    }
}

impl<S: Model> Region<S> {
    fn unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        let unify = match unifier {
            Self::Static if T::MAP_STATIC_REGION => true,
            Self::Named(_) if T::MAP_LIFETIME_PARAMETER => true,
            Self::Context(_) if T::MAP_REGION_CONTEXT => true,
            _ => false,
        };

        if unify {
            match existing.regions.entry(Cow::Borrowed(unifier)) {
                Entry::Occupied(entry) => {
                    if **entry.get() != *target {
                        return Err(Error::RegionConflict(ConflictError {
                            unifier: Cow::Borrowed(unifier),
                            existing: entry.remove(),
                            target: Cow::Borrowed(target),
                        }));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(Cow::Borrowed(target));
                }
            }

            Ok(existing)
        } else if unifier == target {
            Ok(existing)
        } else {
            Err(Error::Region(unifier, target))
        }
    }
}

impl<S: Model> GenericArguments<S> {
    fn unify_internal<'a, T: Config<Model = S>>(
        unifier: &'a Self,
        target: &'a Self,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        if unifier.types.len() != target.types.len()
            || unifier.constants.len() != target.constants.len()
            || unifier.regions.len() != target.regions.len()
        {
            return Err(Error::GenericArguments(unifier, target));
        }

        for (unifier, target) in unifier.types.iter().zip(target.types.iter()) {
            existing = Type::unify_internal::<T>(unifier, target, existing)?;
        }

        for (unifier, target) in unifier.constants.iter().zip(target.constants.iter()) {
            existing = Constant::unify_internal::<T>(unifier, target, existing)?;
        }

        for (unifier, target) in unifier.regions.iter().zip(target.regions.iter()) {
            existing = Region::unify_internal::<T>(unifier, target, existing)?;
        }

        Ok(existing)
    }
}

#[cfg(test)]
mod tests;
