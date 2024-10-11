//! Implements the logic for equality checking.

use super::{
    matching::Matching, normalizer::Normalizer, observer::Observer,
    query::Context, term::Term, Compute, Environment, Output, OverflowError,
    Satisfied, Succeeded,
};
use crate::symbol::table::{self, DisplayObject, State};

/// A query for checking strict equality
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Equality<T, U = T> {
    pub lhs: T,
    pub rhs: U,
}

impl<T, U> Equality<T, U> {
    /// Creates a new equality query.
    #[must_use]
    pub const fn new(lhs: T, rhs: U) -> Self { Self { lhs, rhs } }
}

impl<S: State, T: table::Display<S>, U: table::Display<S>> table::Display<S>
    for Equality<T, U>
{
    fn fmt(
        &self,
        table: &table::Table<S>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{} = {}",
            DisplayObject { display: &self.lhs, table },
            DisplayObject { display: &self.rhs, table }
        )
    }
}

#[allow(clippy::mismatching_type_param_order)]
impl<T: Term> Compute for Equality<T, T> {
    type Error = OverflowError;
    type Parameter = ();

    #[allow(private_bounds, private_interfaces)]
    fn implementation<S: State>(
        &self,
        environment: &Environment<
            Self::Model,
            S,
            impl Normalizer<Self::Model, S>,
            impl Observer<Self::Model, S>,
        >,
        context: &mut Context<Self::Model>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        // trivially satisfied
        if self.lhs == self.rhs {
            return Ok(Some(Succeeded::satisfied()));
        }

        if let Some(result) =
            equals_without_mapping(&self.lhs, &self.rhs, environment, context)?
        {
            return Ok(Some(result));
        }

        for predicate in &environment.premise.predicates {
            let Some(equality_predicate) =
                T::as_trait_member_equality_predicate(predicate)
            else {
                continue;
            };

            let trait_member_term = T::from(equality_predicate.lhs.clone());

            if self.lhs.as_trait_member().is_some() {
                if let Some(result) = equals_without_mapping(
                    &self.lhs,
                    &trait_member_term,
                    environment,
                    context,
                )? {
                    if let Some(inner_result) = Self::new(
                        equality_predicate.rhs.clone(),
                        self.rhs.clone(),
                    )
                    .query_with_context(environment, context)?
                    {
                        return Ok(Some(result.combine(inner_result)));
                    }
                }
            }

            if self.rhs.as_trait_member().is_some() {
                if let Some(result) = equals_without_mapping(
                    &trait_member_term,
                    &self.rhs,
                    environment,
                    context,
                )? {
                    if let Some(inner_result) = Self::new(
                        self.lhs.clone(),
                        equality_predicate.rhs.clone(),
                    )
                    .query_with_context(environment, context)?
                    {
                        return Ok(Some(result.combine(inner_result)));
                    }
                }
            }
        }

        Ok(None)
    }
}

fn equals_by_unification<T: Term, S: State>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
) -> Result<Output<Satisfied, T::Model>, OverflowError> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut satisfied = Succeeded::default();

    for Matching { lhs, rhs, .. } in matching.lifetimes {
        let Some(result) =
            Equality::new(lhs, rhs).query_with_context(environment, context)?
        else {
            return Ok(None);
        };

        satisfied = satisfied.combine(result);
    }

    for Matching { lhs, rhs, .. } in matching.types {
        let Some(result) =
            Equality::new(lhs, rhs).query_with_context(environment, context)?
        else {
            return Ok(None);
        };

        satisfied = satisfied.combine(result);
    }

    for Matching { lhs, rhs, .. } in matching.constants {
        let Some(result) =
            Equality::new(lhs, rhs).query_with_context(environment, context)?
        else {
            return Ok(None);
        };

        satisfied = satisfied.combine(result);
    }

    Ok(Some(satisfied))
}

fn equals_by_normalization<T: Term, S: State>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
) -> Result<Output<Satisfied, T::Model>, OverflowError> {
    if let Some(lhs) = lhs.normalize(environment, context)? {
        if let Some(mut result) = Equality::new(lhs.result, rhs.clone())
            .query_with_context(environment, context)?
        {
            result.constraints.extend(lhs.constraints);
            return Ok(Some(result));
        }
    }

    if let Some(rhs) = rhs.normalize(environment, context)? {
        if let Some(mut result) = Equality::new(lhs.clone(), rhs.result)
            .query_with_context(environment, context)?
        {
            result.constraints.extend(rhs.constraints);
            return Ok(Some(result));
        }
    }

    Ok(None)
}

fn equals_without_mapping<T: Term, S: State>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<
        T::Model,
        S,
        impl Normalizer<T::Model, S>,
        impl Observer<T::Model, S>,
    >,
    context: &mut Context<T::Model>,
) -> Result<Output<Satisfied, T::Model>, OverflowError> {
    if lhs == rhs {
        return Ok(Some(Succeeded::satisfied()));
    }

    if let Some(result) = equals_by_unification(lhs, rhs, environment, context)?
    {
        return Ok(Some(result));
    }

    if let Some(result) =
        equals_by_normalization(lhs, rhs, environment, context)?
    {
        return Ok(Some(result));
    }

    Ok(None)
}

#[cfg(test)]
pub(super) mod tests;
