//! Contains the implementation of the order of the generic arguments.
//!
//! This is primarily used to determine the specialization of the
//! implementation of the trait.

use std::collections::{BTreeMap, BTreeSet};

use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type, Model,
};

use super::{
    mapping::Mapping,
    normalizer::Normalizer,
    unification::{self, Log, Unification},
    Error, Satisfied, Succeeded,
};
use crate::{environment::Environment, term::Term};

/// The order in terms of specificity of the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

fn type_predicate<M: Model>(x: &Type<M>) -> bool {
    x.is_parameter() || matches!(x, Type::TraitMember(_))
}

fn constant_predicate<M: Model>(x: &Constant<M>) -> bool { x.is_parameter() }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct CompatiblePredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, Error> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        to: &Type<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, Error> {
        Ok((type_predicate(from) || type_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Constant<M>,
        to: &Constant<M>,
        _: &[Log<M>],
        _: &[Log<M>],
    ) -> Result<Option<Succeeded<Satisfied, M>>, Error> {
        Ok((constant_predicate(from) || constant_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

fn append_mapping<T: Term>(
    mapping: &mut Mapping<T::Model>,
    this: &[T],
    other: &[T],
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<bool, Error> {
    for (this_term, other_term) in this.iter().zip(other.iter()) {
        let Some(unifier) = environment.query(&Unification::new(
            this_term.clone(),
            other_term.clone(),
            CompatiblePredicate,
        ))?
        else {
            return Ok(false);
        };

        mapping.append_from_unifier(unifier.result.clone());
    }

    Ok(true)
}

fn matching_copmatible<T: Term>(
    matching: BTreeMap<T, BTreeSet<T>>,
    filter: impl Fn(&T) -> bool,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<bool, Error> {
    for (key, matching) in matching {
        if !filter(&key) {
            continue;
        }

        let mut outer_iter = matching.iter();

        while let Some(outer) = outer_iter.next() {
            for inner in outer_iter.clone() {
                if environment
                    .query(&Unification::new(
                        inner.clone(),
                        outer.clone(),
                        CompatiblePredicate,
                    ))?
                    .is_some()
                {
                    return Ok(false);
                }
            }
        }
    }

    Ok(true)
}

fn get_generic_arguments_matching_count<M: Model>(
    this: &GenericArguments<M>,
    other: &GenericArguments<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<Option<usize>, Error> {
    let mut mapping = Mapping::default();

    if !append_mapping(&mut mapping, &this.types, &other.types, environment)?
        || !append_mapping(
            &mut mapping,
            &this.constants,
            &other.constants,
            environment,
        )?
    {
        return Ok(None);
    }

    let count = mapping.types.keys().filter(|x| type_predicate(x)).count()
        + mapping.constants.keys().filter(|x| constant_predicate(x)).count();

    if !matching_copmatible(
        mapping.types,
        |ty| type_predicate(ty),
        environment,
    )? || !matching_copmatible(
        mapping.constants,
        |val| constant_predicate(val),
        environment,
    )? {
        return Ok(None);
    }

    Ok(Some(count))
}

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Determines the order of the generic arguments.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    pub fn order(
        &self,
        this: &GenericArguments<M>,
        other: &GenericArguments<M>,
    ) -> Result<Order, Error> {
        if this.lifetimes.len() != other.lifetimes.len()
            || this.types.len() != other.types.len()
            || this.constants.len() != other.constants.len()
        {
            return Ok(Order::Incompatible);
        }

        let this_to_other =
            get_generic_arguments_matching_count(this, other, self)?;
        let other_to_this =
            get_generic_arguments_matching_count(other, this, self)?;

        match (this_to_other, other_to_this) {
            (None, None) => Ok(Order::Incompatible),
            (None, Some(_)) => Ok(Order::MoreSpecific),
            (Some(_), None) => Ok(Order::MoreGeneral),
            (Some(self_to_other), Some(other_to_self)) => {
                Ok(match self_to_other.cmp(&other_to_self) {
                    std::cmp::Ordering::Less => Order::MoreSpecific,
                    std::cmp::Ordering::Equal => Order::Ambiguous,
                    std::cmp::Ordering::Greater => Order::MoreGeneral,
                })
            }
        }
    }
}

#[cfg(test)]
mod test;
