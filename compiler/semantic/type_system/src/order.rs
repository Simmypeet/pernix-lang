//! Contains the implementation of the order of the generic arguments.
//!
//! This is primarily used to determine the specialization of the
//! implementation of the trait.

use std::collections::{BTreeMap, BTreeSet};

use pernixc_term::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type,
};

use crate::{
    environment::Environment,
    mapping::Mapping,
    normalizer::Normalizer,
    term::Term,
    unification::{self, Log, Unification},
    Error, Satisfied, Succeeded,
};

/// The order in terms of specificity of the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

fn type_predicate(x: &Type) -> bool {
    x.is_parameter() || matches!(x, Type::TraitMember(_))
}

fn constant_predicate(x: &Constant) -> bool { x.is_parameter() }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct CompatiblePredicate;

impl unification::Predicate<Lifetime> for CompatiblePredicate {
    fn unifiable(
        &self,
        _: &Lifetime,
        _: &Lifetime,
        _: &[Log],
        _: &[Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Type> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Type,
        to: &Type,
        _: &[Log],
        _: &[Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok((type_predicate(from) || type_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

impl unification::Predicate<Constant> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Constant,
        to: &Constant,
        _: &[Log],
        _: &[Log],
    ) -> Result<Option<Succeeded<Satisfied>>, Error> {
        Ok((constant_predicate(from) || constant_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

async fn append_mapping<T: Term>(
    mapping: &mut Mapping,
    this: &[T],
    other: &[T],
    environment: &Environment<'_, impl Normalizer>,
) -> Result<bool, Error> {
    for (this_term, other_term) in this.iter().zip(other.iter()) {
        let Some(unifier) = environment
            .query(&Unification::new(
                this_term.clone(),
                other_term.clone(),
                CompatiblePredicate,
            ))
            .await?
        else {
            return Ok(false);
        };

        mapping.append_from_unifier(unifier.result.clone());
    }

    Ok(true)
}

async fn matching_copmatible<T: Term>(
    matching: BTreeMap<T, BTreeSet<T>>,
    filter: impl Fn(&T) -> bool,
    environment: &Environment<'_, impl Normalizer>,
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
                    ))
                    .await?
                    .is_some()
                {
                    return Ok(false);
                }
            }
        }
    }

    Ok(true)
}

async fn get_generic_arguments_matching_count(
    this: &GenericArguments,
    other: &GenericArguments,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<usize>, Error> {
    let mut mapping = Mapping::default();

    if !append_mapping(&mut mapping, &this.types, &other.types, environment)
        .await?
        || !append_mapping(
            &mut mapping,
            &this.constants,
            &other.constants,
            environment,
        )
        .await?
    {
        return Ok(None);
    }

    let count = mapping.types.keys().filter(|x| type_predicate(x)).count()
        + mapping.constants.keys().filter(|x| constant_predicate(x)).count();

    if !matching_copmatible(mapping.types, type_predicate, environment).await?
        || !matching_copmatible(
            mapping.constants,
            constant_predicate,
            environment,
        )
        .await?
    {
        return Ok(None);
    }

    Ok(Some(count))
}

impl<N: Normalizer> Environment<'_, N> {
    /// Determines the order of the generic arguments.
    pub async fn order(
        &self,
        this: &GenericArguments,
        other: &GenericArguments,
    ) -> Result<Order, Error> {
        if this.lifetimes.len() != other.lifetimes.len()
            || this.types.len() != other.types.len()
            || this.constants.len() != other.constants.len()
        {
            return Ok(Order::Incompatible);
        }

        let this_to_other =
            get_generic_arguments_matching_count(this, other, self).await?;
        let other_to_this =
            get_generic_arguments_matching_count(other, this, self).await?;

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
