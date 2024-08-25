//! Contains the implementation of the order of the generic arguments.
//!
//! This is primarily used to determine the specialization of the
//! implementation of the trait.

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use super::{
    mapping::Mapping,
    model::Model,
    normalizer::Normalizer,
    observer::Observer,
    query::Context,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Log, Unification},
    Environment, Output, OverflowError, Satisfied, Succeeded,
};
use crate::{symbol::table::State, type_system::Compute};

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
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        to: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok((type_predicate(from) || type_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for CompatiblePredicate {
    fn unifiable(
        &self,
        from: &Constant<M>,
        to: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok((constant_predicate(from) || constant_predicate(to))
            .then_some(Succeeded::satisfied()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct OrderPredicate;

impl<M: Model> unification::Predicate<Lifetime<M>> for OrderPredicate {
    fn unifiable(
        &self,
        _: &Lifetime<M>,
        _: &Lifetime<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(Some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Type<M>> for OrderPredicate {
    fn unifiable(
        &self,
        from: &Type<M>,
        _: &Type<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(type_predicate(from).then_some(Succeeded::satisfied()))
    }
}

impl<M: Model> unification::Predicate<Constant<M>> for OrderPredicate {
    fn unifiable(
        &self,
        from: &Constant<M>,
        _: &Constant<M>,
        _: &Vec<Log<M>>,
        _: &Vec<Log<M>>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        Ok(constant_predicate(from).then_some(Succeeded::satisfied()))
    }
}

fn append_mapping<T: Term, S: State>(
    mapping: &mut Mapping<T::Model>,
    this: &[T],
    other: &[T],
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
) -> Result<bool, OverflowError> {
    for (this_term, other_term) in this.iter().zip(other.iter()) {
        let Some(unifier) = Unification::new(
            this_term.clone(),
            other_term.clone(),
            Arc::new(OrderPredicate),
        )
        .query_with_context(environment, context)?
        else {
            return Ok(false);
        };

        mapping.append_from_unifier(unifier.result);
    }

    Ok(true)
}

fn matching_copmatible<T: Term, S: State>(
    matching: BTreeMap<T, BTreeSet<T>>,
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
) -> Result<bool, OverflowError> {
    let compatible = Arc::new(CompatiblePredicate);

    for (_, matching) in matching {
        let mut outer_iter = matching.iter();

        while let Some(outer) = outer_iter.next() {
            let mut inner_iter = outer_iter.clone();

            while let Some(inner) = inner_iter.next() {
                if Unification::new(
                    inner.clone(),
                    outer.clone(),
                    compatible.clone(),
                )
                .query_with_context(environment, context)?
                .is_none()
                {
                    return Ok(false);
                }
            }
        }
    }

    Ok(true)
}

fn get_generic_arguments_matching_count<M: Model, S: State>(
    this: &GenericArguments<M>,
    other: &GenericArguments<M>,
    environment: &Environment<M, S, impl Normalizer<M, S>, impl Observer<M, S>>,
    context: &mut Context<M>,
) -> Result<Option<usize>, OverflowError> {
    let mut mapping = Mapping::default();

    if !append_mapping(
        &mut mapping,
        &this.types,
        &other.types,
        environment,
        context,
    )? || !append_mapping(
        &mut mapping,
        &this.constants,
        &other.constants,
        environment,
        context,
    )? {
        return Ok(None);
    }

    let count = mapping.types.len() + mapping.constants.len();

    if !matching_copmatible(mapping.types, environment, context)?
        || !matching_copmatible(mapping.constants, environment, context)?
    {
        return Ok(None);
    }

    Ok(Some(count))
}

impl<M: Model> GenericArguments<M> {
    /// Determines the order of the generic arguments.
    ///
    /// # Errors
    ///
    /// See  for more information.
    pub fn order<S: State>(
        &self,
        other: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
    ) -> Result<Order, OverflowError> {
        self.order_with_context(other, environment, &mut Context::new())
    }

    /// Determines the order of the generic arguments.
    ///
    /// # Errors
    ///
    /// See [`OverflowError`] for more information.
    pub fn order_with_context<S: State>(
        &self,
        other: &Self,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        context: &mut Context<M>,
    ) -> Result<Order, OverflowError> {
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
            context,
        )?;
        let other_to_self = get_generic_arguments_matching_count(
            other,
            self,
            environment,
            context,
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

// TODO: Add test
// #[cfg(test)]
// mod tests;
