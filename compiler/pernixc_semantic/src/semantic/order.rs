//! Contains the implementation of the order of the generic arguments.
//!
//! This is primarily used to determine the specialization of the
//! implementation of the trait.

use super::{
    model::Model,
    normalizer::Normalizer,
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Unification},
    Environment,
};
use crate::table::State;

/// The order in terms of specificity of the generic arguments.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Order {
    Incompatible,
    MoreGeneral,
    MoreSpecific,
    Ambiguous,
}

const fn lifetime_predicate<M: Model>(_: &Lifetime<M>) -> bool { true }

fn type_predicate<M: Model>(x: &Type<M>) -> bool {
    x.is_parameter() || matches!(x, Type::TraitMember(_))
}

fn constant_predicate<M: Model>(x: &Constant<M>) -> bool { x.is_parameter() }

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OrderUnifyingConfig;

impl<M: Model> unification::Config<M> for OrderUnifyingConfig {
    fn lifetime_unifiable(
        &mut self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(true)
    }

    fn type_unifiable(
        &mut self,
        from: &Type<M>,
        to: &Type<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(type_predicate(from) || type_predicate(to))
    }

    fn constant_unifiable(
        &mut self,
        from: &Constant<M>,
        to: &Constant<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(constant_predicate(from) || constant_predicate(to))
    }
}

fn get_arguments_matching_count<T: Term>(
    this: &[T],
    other: &[T],
    predicate: &impl Fn(&T) -> bool,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    session: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Option<usize>, ExceedLimitError> {
    let mut count = 0;

    for (lifetime, other_lifetime) in this.iter().zip(other.iter()) {
        let Some(unification) = unification::unify(
            lifetime,
            other_lifetime,
            &mut OrderUnifyingConfig,
            environment,
            session,
        )?
        else {
            return Ok(None);
        };

        count += get_unification_matching_count(&unification, predicate);
    }

    Ok(Some(count))
}

fn get_generic_arguments_matching_count<M: Model>(
    this: &GenericArguments<M>,
    other: &GenericArguments<M>,
    environment: &Environment<M, impl State, impl Normalizer<M>>,
    limit: &mut Limit<
        impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    >,
) -> Result<Option<usize>, ExceedLimitError> {
    let (Some(lifetime_count), Some(ty_count), Some(constant_count)) = (
        get_arguments_matching_count(
            &this.lifetimes,
            &other.lifetimes,
            &lifetime_predicate,
            environment,
            limit,
        )?,
        get_arguments_matching_count(
            &this.types,
            &other.types,
            &type_predicate,
            environment,
            limit,
        )?,
        get_arguments_matching_count(
            &this.constants,
            &other.constants,
            &constant_predicate,
            environment,
            limit,
        )?,
    ) else {
        return Ok(None);
    };

    Ok(Some(lifetime_count + ty_count + constant_count))
}

fn get_unification_matching_count<T: Term>(
    unification: &Unification<T>,
    predicate: impl Fn(&T) -> bool,
) -> usize {
    match &unification.r#match {
        unification::Matching::Unifiable(from, _) => {
            usize::from(predicate(from))
        }
        unification::Matching::Substructural(substructural) => {
            substructural
                .lifetimes
                .values()
                .map(|x| get_unification_matching_count(x, lifetime_predicate))
                .sum::<usize>()
                + substructural
                    .types
                    .values()
                    .map(|x| get_unification_matching_count(x, type_predicate))
                    .sum::<usize>()
                + substructural
                    .constants
                    .values()
                    .map(|x| {
                        get_unification_matching_count(x, constant_predicate)
                    })
                    .sum::<usize>()
        }
        unification::Matching::Equality => 0,
    }
}

impl<M: Model> GenericArguments<M> {
    /// Determines the order of the generic arguments.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn order(
        &self,
        other: &Self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<Order, ExceedLimitError> {
        if self.lifetimes.len() != other.lifetimes.len()
            || self.types.len() != other.types.len()
            || self.constants.len() != other.constants.len()
        {
            return Ok(Order::Incompatible);
        }

        let self_to_other = get_generic_arguments_matching_count(
            self,
            other,
            environment,
            limit,
        )?;
        let other_to_self = get_generic_arguments_matching_count(
            other,
            self,
            environment,
            limit,
        )?;

        match (self_to_other, other_to_self) {
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
mod tests;
