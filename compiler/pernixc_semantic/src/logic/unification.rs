//! Contains the code for unifying terms.

use std::{borrow::Cow, collections::hash_map::Entry};

use super::{Mapping, QueryRecords, Substitution};
use crate::{
    entity::{
        constant::{self, Constant},
        r#type::{self, Type},
        region::Region,
        GenericArguments, Model,
    },
    table::Table,
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
pub trait Config<S: Model>:
    std::fmt::Debug + Clone + Copy + PartialEq + Eq + std::hash::Hash + Default + Send + Sync + 'static
{
    /// Determines whether a particular type can be mapped into another given type.
    fn type_mappable(&self, unifier: &Type<S>, target: &Type<S>) -> bool;

    /// Determines whether a particular constant can be mapped into another given constant.
    fn constant_mappable(&self, unifier: &Constant<S>, target: &Constant<S>) -> bool;

    /// Determines whether a particular region can be mapped into another given region.
    fn region_mappable(&self, unifier: &Region<S>, target: &Region<S>) -> bool;
}

/// Is a struct that implements [`Config`] which allows unification of all terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct All;

impl<S: Model> Config<S> for All {
    fn type_mappable(&self, _: &Type<S>, _: &Type<S>) -> bool { true }

    fn constant_mappable(&self, _: &Constant<S>, _: &Constant<S>) -> bool { true }

    fn region_mappable(&self, _: &Region<S>, _: &Region<S>) -> bool { true }
}

/// Is a struct that implements [`Config`] which allows unification of parameters and trait members
/// terms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Indefinite;

impl<S: Model> Config<S> for Indefinite {
    fn type_mappable(&self, unifier: &Type<S>, _: &Type<S>) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn constant_mappable(&self, unifier: &Constant<S>, _: &Constant<S>) -> bool {
        unifier.is_parameter() || unifier.is_trait_member()
    }

    fn region_mappable(&self, unifier: &Region<S>, _: &Region<S>) -> bool { unifier.is_named() }
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
    ($name:ident, $domain:ident, $kind:ident, $map_name: ident, $err_name: ident, $mappable_func: ident) => {
        #[allow(clippy::too_many_lines)]
        fn $name<'a, S: Model, T: Config<S>>(
            unifier: &'a $domain::Tuple<S>,
            target: &'a $domain::Tuple<S>,
            config: &T,
            mut existing: Substitution<'a, S>,
        ) -> Result<Substitution<'a, S>, Error<'a, S>> {
            let unpacked_count = unifier.elements.iter().filter(|x| x.is_unpacked()).count();

            match unpacked_count {
                // no unpacked elements case
                0 => {
                    for (unifier_element, target_element) in
                        unifier.elements.iter().zip(target.elements.iter())
                    {
                        let unifier_element = unifier_element.as_regular().unwrap();
                        let target_element = target_element.as_regular().unwrap();

                        existing = $kind::unify_internal::<T>(
                            unifier_element,
                            target_element,
                            config,
                            existing,
                        )?;
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

                        existing = $kind::unify_internal::<T>(
                            unifier_element,
                            target_element,
                            config,
                            existing,
                        )?;
                    }

                    // unify tail
                    for (unifier_element, target_element) in unifier.elements[unifier_tail_range]
                        .iter()
                        .zip(&target.elements[target_tail_range])
                    {
                        let unifier_element = unifier_element.as_regular().unwrap();
                        let target_element = target_element.as_regular().unwrap();

                        existing = $kind::unify_internal::<T>(
                            unifier_element,
                            target_element,
                            config,
                            existing,
                        )?;
                    }

                    let target_unpack: Cow<'a, $kind<S>> =
                        Cow::Owned($kind::Tuple($domain::Tuple {
                            elements: target.elements[target_unpack_range].to_vec(),
                        }));

                    let unpacked = unifier.elements[unpacked_position].as_unpacked().unwrap();
                    match unpacked {
                        $domain::Unpacked::Parameter(parameter) => {
                            let parameter: Cow<'a, $kind<S>> =
                                Cow::Owned($kind::Parameter(*parameter));

                            if config.$mappable_func(parameter.as_ref(), target_unpack.as_ref()) {
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
                            let trait_member: Cow<'a, $kind<S>> =
                                Cow::Owned($kind::TraitMember(trait_member.clone()));

                            if config.$mappable_func(trait_member.as_ref(), target_unpack.as_ref())
                            {
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
    constants,
    ConstantConflict,
    constant_mappable
);
tuple_unify_function!(
    unify_tuple_type,
    r#type,
    Type,
    types,
    TypeConflict,
    type_mappable
);

impl<S: Model> Type<S> {
    /// Unifies two type terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms can't be unified.
    pub fn unify<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::unify_internal::<T>(unifier, target, config, Substitution::default())
    }

    /// Sub-structurally unifies two type terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms can't be sub-structurally unified.
    pub fn sub_structural_unify<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::sub_structural_unify_internal::<T>(unifier, target, config, Substitution::default())
    }

    fn unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        if config.type_mappable(unifier, target) {
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

        Self::sub_structural_unify_internal::<T>(unifier, target, config, existing)
    }

    fn sub_structural_unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        // sub-structural unification
        match (unifier, target) {
            (Self::Algebraic(unifier), Self::Algebraic(target)) if unifier.kind == target.kind => {
                GenericArguments::unify_internal::<T>(
                    &unifier.generic_arguments,
                    &target.generic_arguments,
                    config,
                    existing,
                )
            }
            (Self::Pointer(unifier), Self::Pointer(target))
                if unifier.qualifier == target.qualifier =>
            {
                Self::unify_internal::<T>(&unifier.pointee, &target.pointee, config, existing)
            }
            (Self::Reference(unifier), Self::Reference(target))
                if unifier.qualifier == target.qualifier =>
            {
                existing =
                    Region::unify_internal::<T>(&unifier.region, &target.region, config, existing)?;
                Self::unify_internal::<T>(&unifier.pointee, &target.pointee, config, existing)
            }
            (Self::Array(unifier), Self::Array(target)) => {
                existing = Constant::unify_internal::<T>(
                    &unifier.length,
                    &target.length,
                    config,
                    existing,
                )?;
                Self::unify_internal::<T>(&unifier.element, &target.element, config, existing)
            }
            (Self::TraitMember(unifier), Self::TraitMember(target))
                if unifier.trait_type_id == target.trait_type_id =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.trait_generic_arguments,
                    &target.trait_generic_arguments,
                    config,
                    existing,
                )?;
                GenericArguments::unify_internal::<T>(
                    &unifier.member_generic_arguments,
                    &target.member_generic_arguments,
                    config,
                    existing,
                )
            }
            (Self::Tuple(unifier), Self::Tuple(target))
                if tuple_type_unifiable(unifier, target) =>
            {
                unify_tuple_type::<S, T>(unifier, target, config, existing)
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
    pub fn unify<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::unify_internal::<T>(unifier, target, config, Substitution::default())
    }

    /// Sub-structurally unifies two constant terms.
    ///
    /// # Errors
    ///
    /// Returns an error if the two terms can't be sub-structurally unified.
    pub fn sub_structural_unify<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        Self::sub_structural_unify_internal::<T>(unifier, target, config, Substitution::default())
    }

    fn unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        if config.constant_mappable(unifier, target) {
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

        Self::sub_structural_unify_internal::<T>(unifier, target, config, existing)
    }

    fn sub_structural_unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
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
                    config,
                    existing,
                )?;

                for (unifier, target) in unifier.fields.iter().zip(target.fields.iter()) {
                    existing = Self::unify_internal::<T>(unifier, target, config, existing)?;
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
                    config,
                    existing,
                )?;

                match (&unifier.associated_value, &target.associated_value) {
                    (None, None) => Ok(existing),
                    (Some(unifier), Some(target)) => {
                        Self::unify_internal::<T>(unifier, target, config, existing)
                    }
                    (_, _) => unreachable!(),
                }
            }

            (Self::Array(unifier), Self::Array(target))
                if unifier.elements.len() == target.elements.len() =>
            {
                existing = Type::unify_internal::<T>(
                    &unifier.element_ty,
                    &target.element_ty,
                    config,
                    existing,
                )?;

                for (unifier, target) in unifier.elements.iter().zip(target.elements.iter()) {
                    existing = Self::unify_internal::<T>(unifier, target, config, existing)?;
                }

                Ok(existing)
            }

            (Self::TraitMember(unifier), Self::TraitMember(target))
                if unifier.trait_constant_id == target.trait_constant_id =>
            {
                existing = GenericArguments::unify_internal::<T>(
                    &unifier.trait_arguments,
                    &target.trait_arguments,
                    config,
                    existing,
                )?;

                Ok(existing)
            }

            (Self::Tuple(unifier), Self::Tuple(target))
                if tuple_constant_unifiable(unifier, target) =>
            {
                unify_tuple_constant::<S, T>(unifier, target, config, existing)
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
    fn unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        if config.region_mappable(unifier, target) {
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
    fn unify_internal<'a, T: Config<S>>(
        unifier: &'a Self,
        target: &'a Self,
        config: &T,
        mut existing: Substitution<'a, S>,
    ) -> Result<Substitution<'a, S>, Error<'a, S>> {
        if unifier.types.len() != target.types.len()
            || unifier.constants.len() != target.constants.len()
            || unifier.regions.len() != target.regions.len()
        {
            return Err(Error::GenericArguments(unifier, target));
        }

        for (unifier, target) in unifier.types.iter().zip(target.types.iter()) {
            existing = Type::unify_internal::<T>(unifier, target, config, existing)?;
        }

        for (unifier, target) in unifier.constants.iter().zip(target.constants.iter()) {
            existing = Constant::unify_internal::<T>(unifier, target, config, existing)?;
        }

        for (unifier, target) in unifier.regions.iter().zip(target.regions.iter()) {
            existing = Region::unify_internal::<T>(unifier, target, config, existing)?;
        }

        Ok(existing)
    }
}

impl<S: Model> Type<S> {
    pub(super) fn unify_with_premise_mappings<'a, 'b>(
        unifier: &'a Self,
        target: &'a Self,
        config: &impl Config<S>,
        table: &Table,
        premise_mappings: &Mapping<S>,
        query_records: &mut QueryRecords<S>,
    ) -> Mapping<'b, S> {
    }
}

#[cfg(test)]
mod tests;
