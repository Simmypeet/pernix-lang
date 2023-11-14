//! Contains the code for unifying terms.

use std::{cell::Cell, collections::hash_map::Entry};

use pernixc_base::extension::CellExt;

use super::{Mapping, QueryRecords};
use crate::{
    entity::{
        constant::{self, Constant},
        r#type::{self, Type},
        region::Region,
        GenericArguments, Model, Substitution,
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
pub enum Error<S: Model> {
    /// These two types can't be unified.
    Type(Type<S>, Type<S>),

    /// These two constants can't be unified.
    Constant(Constant<S>, Constant<S>),

    /// These two regions can't be unified.
    Region(Region<S>, Region<S>),

    /// These two generic arguments can't be unified.
    GenericArguments(GenericArguments<S>, GenericArguments<S>),

    TypeConflict(ConflictError<Type<S>, Type<S>>),
    RegionConflict(ConflictError<Region<S>, Region<S>>),
    ConstantConflict(ConflictError<Constant<S>, Constant<S>>),
}

/// Describes which types of terms can be unified.
pub trait Config<S: Model> {
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

macro_rules! tuple_unifiable_body {
    ($domain:ident) => {
        fn tuple_unifiable(unifier: &$domain::Tuple<S>, target: &$domain::Tuple<S>) -> bool {
            match unifier.elements.iter().filter(|x| x.is_unpacked()).count() {
                0 => {
                    unifier.elements.len() == target.elements.len()
                        && !target.elements.iter().any(|x| x.is_unpacked())
                }
                1 => {
                    if unifier.elements.len() > target.elements.len() + 1 {
                        return false;
                    }

                    let unpacked_position = unifier
                        .elements
                        .iter()
                        .position($domain::TupleElement::is_unpacked)
                        .unwrap();

                    let tail_to_unpack_count = unifier.elements.len() - unpacked_position - 1;

                    if target.elements[..unpacked_position]
                        .iter()
                        .any(|x| x.is_unpacked())
                    {
                        return false;
                    }

                    if target.elements[target.elements.len() - tail_to_unpack_count..]
                        .iter()
                        .any(|x| x.is_unpacked())
                    {
                        return false;
                    }

                    true
                }
                _ => false,
            }
        }
    };
}

macro_rules! tuple_unify_body {
    ($domain:ident, $map_name: ident, $err_name: ident, $mappable_func: ident) => {
        #[allow(clippy::too_many_lines)]
        fn tuple_unify(
            lhs: &$domain::Tuple<S>,
            rhs: &$domain::Tuple<S>,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
            config: &impl Config<S>,
            mut existing: Substitution<S>,
        ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
            let unpacked_count = lhs.elements.iter().filter(|x| x.is_unpacked()).count();

            match unpacked_count {
                // no unpacked elements case
                0 => {
                    for (lhs_element, rhs_element) in lhs.elements.iter().zip(rhs.elements.iter()) {
                        let lhs_element = lhs_element.as_regular().unwrap();
                        let rhs_element = rhs_element.as_regular().unwrap();

                        existing = Self::unify_internal(
                            lhs_element,
                            rhs_element,
                            premise_mapping,
                            table,
                            records,
                            config,
                            existing,
                        )?;
                    }

                    Ok(existing)
                }

                // one unpacked element case
                1 => {
                    let unpacked_position = lhs
                        .elements
                        .iter()
                        .position($domain::TupleElement::is_unpacked)
                        .unwrap();

                    let head_range = 0..unpacked_position;
                    let lhs_tail_range = (unpacked_position + 1)..lhs.elements.len();
                    let rhs_tail_range =
                        (rhs.elements.len() - lhs_tail_range.clone().count())..rhs.elements.len();
                    let rhs_unpack_range = unpacked_position..rhs_tail_range.start;

                    // unify head
                    for (lhs_element, rhs_element) in lhs.elements[head_range.clone()]
                        .iter()
                        .zip(&rhs.elements[head_range])
                    {
                        let lhs_element = lhs_element.as_regular().unwrap();
                        let rhs_element = rhs_element.as_regular().unwrap();

                        existing = Self::unify_internal(
                            lhs_element,
                            rhs_element,
                            premise_mapping,
                            table,
                            records,
                            config,
                            existing,
                        )?;
                    }

                    // unify tail
                    for (lhs_element, rhs_element) in lhs.elements[lhs_tail_range]
                        .iter()
                        .zip(&rhs.elements[rhs_tail_range])
                    {
                        let lhs_element = lhs_element.as_regular().unwrap();
                        let rhs_element = rhs_element.as_regular().unwrap();

                        existing = Self::unify_internal(
                            lhs_element,
                            rhs_element,
                            premise_mapping,
                            table,
                            records,
                            config,
                            existing,
                        )?;
                    }

                    let rhs_unpack = Self::Tuple($domain::Tuple {
                        elements: rhs.elements[rhs_unpack_range].to_vec(),
                    });

                    let unpacked = lhs.elements[unpacked_position].as_unpacked().unwrap();
                    match unpacked {
                        $domain::Unpacked::Parameter(parameter) => {
                            let parameter = Self::Parameter(*parameter);

                            existing = Self::unify_internal(
                                &parameter,
                                &rhs_unpack,
                                premise_mapping,
                                table,
                                records,
                                config,
                                existing,
                            )?;
                        }
                        $domain::Unpacked::TraitMember(trait_member) => {
                            let trait_member = Self::TraitMember(trait_member.clone());

                            existing = Self::unify_internal(
                                &trait_member,
                                &rhs_unpack,
                                premise_mapping,
                                table,
                                records,
                                config,
                                existing,
                            )?;
                        }
                    }

                    Ok(existing)
                }

                _ => unreachable!(),
            }
        }
    };
}

macro_rules! unify_internal_body {
    ($record_set:ident, $mappable_func:ident, $sub:ident, $err:ident) => {
        pub(super) fn unify_internal(
            lhs: &Self,
            rhs: &Self,
            premise_mapping: &Mapping<S>,
            table: &Table,
            records: &Cell<QueryRecords<S>>,
            config: &impl Config<S>,
            mut existing: Substitution<S>,
        ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
            let terms = (lhs.clone(), rhs.clone());

            // avoid recursion
            if records.visit(|x| x.$record_set.contains(&terms)) {
                return Ok(existing);
            }

            records.visit_mut(|x| x.$record_set.insert(terms.clone()));

            // try to unify
            if config.$mappable_func(lhs, rhs) {
                match existing.$sub.entry(lhs.clone()) {
                    Entry::Occupied(entry) => {
                        if !Self::equals_internal(entry.get(), rhs, premise_mapping, table, records)
                        {
                            records.visit_mut(|x| x.$record_set.remove(&terms));

                            let existing_term = entry.remove();
                            return Err((
                                existing,
                                Error::$err(ConflictError {
                                    unifier: lhs.clone(),
                                    target: rhs.clone(),
                                    existing: existing_term,
                                }),
                            ));
                        }
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(rhs.clone());
                    }
                }

                records.visit_mut(|x| x.$record_set.remove(&terms));
                return Ok(existing);
            }

            // total substitution count
            let total_count =
                existing.types.len() + existing.constants.len() + existing.regions.len();

            let error = match Self::sub_structural_unify_internal(
                lhs,
                rhs,
                premise_mapping,
                table,
                records,
                config,
                existing,
            ) {
                Ok(existing) => {
                    records.visit_mut(|x| x.$record_set.remove(&terms));
                    return Ok(existing);
                }
                Err((previous, error)) => {
                    existing = previous;
                    error
                }
            };

            for (lhs_mapping, rhs_mappings) in &premise_mapping.$sub {
                if !Self::equals_internal(rhs, lhs_mapping, premise_mapping, table, records) {
                    continue;
                }

                for rhs_mapping in rhs_mappings {
                    match Self::unify_internal(
                        lhs,
                        rhs_mapping,
                        premise_mapping,
                        table,
                        records,
                        config,
                        existing,
                    ) {
                        Ok(ok) => {
                            // no new substitutions, keep going
                            if total_count < ok.types.len() + ok.constants.len() + ok.regions.len()
                            {
                                records.visit_mut(|x| x.$record_set.remove(&terms));
                                return Ok(ok);
                            }

                            existing = ok;
                        }
                        Err((previous, _)) => {
                            existing = previous;
                        }
                    }
                }
            }

            records.visit_mut(|x| x.$record_set.remove(&terms));
            Err((existing, error))
        }
    };
}

impl<S: Model> Type<S> {
    unify_internal_body!(type_unifies, type_mappable, types, TypeConflict);

    tuple_unify_body!(r#type, types, Type, type_mappable);

    tuple_unifiable_body!(r#type);

    /// Unifies two types.
    ///
    /// # Parameters
    ///
    /// - `lhs`: The source type to unify.
    /// - `rhs`: The destination type to unify.
    /// - `premise_mapping`: The mapping that is used as a premise.
    /// - `table`: The table that is used for normalization.
    /// - `config`: The configuration that determines which types of terms can be unified.
    ///
    /// # Returns
    ///
    /// Returns a [`Substitution`] that can be applied to the `lhs` type to make it equals to `rhs`.
    ///
    /// # Errors
    ///
    /// Returns an error if the two types can't be unified.
    pub fn unify(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        config: &impl Config<S>,
    ) -> Result<Substitution<S>, Error<S>> {
        Self::unify_internal(
            lhs,
            rhs,
            premise_mapping,
            table,
            &Cell::new(QueryRecords::default()),
            config,
            Substitution::default(),
        )
        .map_err(|(_, error)| error)
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn sub_structural_unify_internal(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
        config: &impl Config<S>,
        mut existing: Substitution<S>,
    ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
        // sub-structural unification
        match (lhs, rhs) {
            (Self::Algebraic(lhs), Self::Algebraic(rhs)) if lhs.kind == rhs.kind => {
                GenericArguments::unify_internal(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }
            (Self::Pointer(lhs), Self::Pointer(rhs)) if lhs.qualifier == rhs.qualifier => {
                Self::unify_internal(
                    &lhs.pointee,
                    &rhs.pointee,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }
            (Self::Reference(lhs), Self::Reference(rhs)) if lhs.qualifier == rhs.qualifier => {
                existing = Region::unify_internal(
                    &lhs.region,
                    &rhs.region,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                Self::unify_internal(
                    &lhs.pointee,
                    &rhs.pointee,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }
            (Self::Array(lhs), Self::Array(rhs)) => {
                existing = Self::unify_internal(
                    &lhs.element,
                    &rhs.element,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                Constant::unify_internal(
                    &lhs.length,
                    &rhs.length,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }
            (Self::TraitMember(lhs), Self::TraitMember(rhs))
                if lhs.trait_type_id == rhs.trait_type_id =>
            {
                existing = GenericArguments::unify_internal(
                    &lhs.trait_generic_arguments,
                    &rhs.trait_generic_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                GenericArguments::unify_internal(
                    &lhs.member_generic_arguments,
                    &rhs.member_generic_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }
            (Self::Tuple(lhs), Self::Tuple(rhs)) if Self::tuple_unifiable(lhs, rhs) => {
                Self::tuple_unify(lhs, rhs, premise_mapping, table, records, config, existing)
            }
            (_, _) => {
                if Self::equals_internal(lhs, rhs, premise_mapping, table, records) {
                    Ok(existing)
                } else {
                    Err((existing, Error::Type(lhs.clone(), rhs.clone())))
                }
            }
        }
    }
}

impl<S: Model> Constant<S> {
    unify_internal_body!(
        constant_unifies,
        constant_mappable,
        constants,
        ConstantConflict
    );

    tuple_unify_body!(constant, constants, Constant, constant_mappable);

    tuple_unifiable_body!(constant);

    pub(super) fn sub_structural_unify_internal(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
        config: &impl Config<S>,
        mut existing: Substitution<S>,
    ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
        match (lhs, rhs) {
            (Self::Struct(lhs), Self::Struct(rhs))
                if lhs.struct_id == rhs.struct_id && lhs.fields.len() == rhs.fields.len() =>
            {
                existing = GenericArguments::unify_internal(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                for (lhs_field, rhs_field) in lhs.fields.iter().zip(&rhs.fields) {
                    existing = Self::unify_internal(
                        lhs_field,
                        rhs_field,
                        premise_mapping,
                        table,
                        records,
                        config,
                        existing,
                    )?;
                }

                Ok(existing)
            }

            (Self::Enum(lhs), Self::Enum(rhs))
                if lhs.variant_id == rhs.variant_id
                    && lhs.associated_value.is_some() == rhs.associated_value.is_some() =>
            {
                existing = GenericArguments::unify_internal(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                match (&lhs.associated_value, &rhs.associated_value) {
                    (Some(lhs), Some(rhs)) => Self::unify_internal(
                        lhs,
                        rhs,
                        premise_mapping,
                        table,
                        records,
                        config,
                        existing,
                    ),
                    (None, None) => Ok(existing),
                    (_, _) => unreachable!(),
                }
            }

            (Self::Array(lhs), Self::Array(rhs)) if lhs.elements.len() == rhs.elements.len() => {
                existing = Type::unify_internal(
                    &lhs.element_ty,
                    &rhs.element_ty,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )?;

                for (lhs_element, rhs_element) in lhs.elements.iter().zip(&rhs.elements) {
                    existing = Self::unify_internal(
                        lhs_element,
                        rhs_element,
                        premise_mapping,
                        table,
                        records,
                        config,
                        existing,
                    )?;
                }

                Ok(existing)
            }

            (Self::TraitMember(lhs), Self::TraitMember(rhs))
                if lhs.trait_constant_id == rhs.trait_constant_id =>
            {
                GenericArguments::unify_internal(
                    &lhs.trait_arguments,
                    &rhs.trait_arguments,
                    premise_mapping,
                    table,
                    records,
                    config,
                    existing,
                )
            }

            (Self::Tuple(lhs), Self::Tuple(rhs)) if Self::tuple_unifiable(lhs, rhs) => {
                Self::tuple_unify(lhs, rhs, premise_mapping, table, records, config, existing)
            }

            (_, _) => {
                if Self::equals_internal(lhs, rhs, premise_mapping, table, records) {
                    Ok(existing)
                } else {
                    Err((existing, Error::Constant(lhs.clone(), rhs.clone())))
                }
            }
        }
    }
}

impl<S: Model> Region<S> {
    pub(super) fn unify_internal(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
        config: &impl Config<S>,
        mut existing: Substitution<S>,
    ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
        if config.region_mappable(lhs, rhs) {
            match existing.regions.entry(lhs.clone()) {
                Entry::Occupied(entry) => {
                    if !Self::equals_internal(entry.get(), rhs, premise_mapping, table, records) {
                        let existing_term = entry.remove();
                        return Err((
                            existing,
                            Error::RegionConflict(ConflictError {
                                unifier: lhs.clone(),
                                existing: existing_term,
                                target: rhs.clone(),
                            }),
                        ));
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(rhs.clone());
                }
            }

            Ok(existing)
        } else if Self::equals_internal(lhs, rhs, premise_mapping, table, records) {
            Ok(existing)
        } else {
            Err((existing, Error::Region(lhs.clone(), rhs.clone())))
        }
    }
}

impl<S: Model> GenericArguments<S> {
    /// Unifies two generic arguments.
    ///
    /// # Errors
    ///
    /// Returns an error if the two generic arguments can't be unified.
    pub fn unify(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        config: &impl Config<S>,
    ) -> Result<Substitution<S>, Error<S>> {
        Self::unify_internal(
            lhs,
            rhs,
            premise_mapping,
            table,
            &Cell::new(QueryRecords::default()),
            config,
            Substitution::default(),
        )
        .map_err(|(_, error)| error)
    }

    pub(super) fn unify_internal(
        lhs: &Self,
        rhs: &Self,
        premise_mapping: &Mapping<S>,
        table: &Table,
        records: &Cell<QueryRecords<S>>,
        config: &impl Config<S>,
        mut existing: Substitution<S>,
    ) -> Result<Substitution<S>, (Substitution<S>, Error<S>)> {
        if lhs.types.len() != rhs.types.len()
            || lhs.constants.len() != rhs.constants.len()
            || lhs.regions.len() != rhs.regions.len()
        {
            return Err((existing, Error::GenericArguments(lhs.clone(), rhs.clone())));
        }

        for (lhs, rhs) in lhs.types.iter().zip(&rhs.types) {
            existing =
                Type::unify_internal(lhs, rhs, premise_mapping, table, records, config, existing)?;
        }

        for (lhs, rhs) in lhs.constants.iter().zip(&rhs.constants) {
            existing = Constant::unify_internal(
                lhs,
                rhs,
                premise_mapping,
                table,
                records,
                config,
                existing,
            )?;
        }

        for (lhs, rhs) in lhs.regions.iter().zip(&rhs.regions) {
            existing = Region::unify_internal(
                lhs,
                rhs,
                premise_mapping,
                table,
                records,
                config,
                existing,
            )?;
        }

        Ok(existing)
    }
}

#[cfg(test)]
mod tests;
