//! Implements the logic for equality checking.

use std::sync::Arc;

use pernixc_table::{DisplayObject, Table};
use pernixc_term::matching::Matching;
use serde::{Deserialize, Serialize};

use crate::{
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    Error, Satisfied, Succeeded,
};

/// A query for checking strict equality
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
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

impl<T: pernixc_table::Display, U: pernixc_table::Display>
    pernixc_table::Display for Equality<T, U>
{
    fn fmt(
        &self,
        table: &Table,
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
impl<T: Term> Query for Equality<T, T> {
    type Model = T::Model;
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
    type Error = Error;

    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        // trivially satisfied
        if self.lhs == self.rhs {
            return Ok(Some(Arc::new(Succeeded::satisfied())));
        }

        if let Some(result) =
            equals_without_mapping(&self.lhs, &self.rhs, environment)?
        {
            return Ok(Some(Arc::new(result)));
        }

        for predicate in &environment.premise().predicates {
            let Some(equality_predicate) =
                T::as_trait_member_compatible_predicate(predicate)
            else {
                continue;
            };

            let trait_member_term = T::from(equality_predicate.lhs.clone());

            if self.lhs.as_trait_member().is_some() {
                if let Some(mut result) = equals_without_mapping(
                    &self.lhs,
                    &trait_member_term,
                    environment,
                )? {
                    if let Some(inner_result) =
                        environment.query(&Self::new(
                            equality_predicate.rhs.clone(),
                            self.rhs.clone(),
                        ))?
                    {
                        result
                            .constraints
                            .extend(inner_result.constraints.iter().cloned());
                        return Ok(Some(Arc::new(result)));
                    }
                }
            }

            if self.rhs.as_trait_member().is_some() {
                if let Some(mut result) = equals_without_mapping(
                    &trait_member_term,
                    &self.rhs,
                    environment,
                )? {
                    if let Some(inner_result) =
                        environment.query(&Self::new(
                            self.lhs.clone(),
                            equality_predicate.rhs.clone(),
                        ))?
                    {
                        result
                            .constraints
                            .extend(inner_result.constraints.iter().cloned());

                        return Ok(Some(Arc::new(result)));
                    }
                }
            }
        }

        Ok(None)
    }
}

fn equals_by_unification<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<Option<Succeeded<Satisfied, T::Model>>, super::Error> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut satisfied = Succeeded::default();

    for Matching { lhs, rhs, .. } in matching.lifetimes {
        let Some(result) = environment.query(&Equality::new(lhs, rhs))? else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for Matching { lhs, rhs, .. } in matching.types {
        let Some(result) = environment.query(&Equality::new(lhs, rhs))? else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for Matching { lhs, rhs, .. } in matching.constants {
        let Some(result) = environment.query(&Equality::new(lhs, rhs))? else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    Ok(Some(satisfied))
}

fn equals_by_normalization<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<Option<Succeeded<Satisfied, T::Model>>, super::Error> {
    if let Some(Succeeded { result: eq, mut constraints }) =
        lhs.normalize(environment)?
    {
        if let Some(result) =
            environment.query(&Equality::new(eq, rhs.clone()))?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::satisfied_with(constraints)));
        }
    }

    if let Some(Succeeded { result: eq, mut constraints }) =
        rhs.normalize(environment)?
    {
        if let Some(result) =
            environment.query(&Equality::new(lhs.clone(), eq))?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::satisfied_with(constraints)));
        }
    }

    Ok(None)
}

fn equals_without_mapping<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<Option<Succeeded<Satisfied, T::Model>>, super::Error> {
    if lhs == rhs {
        return Ok(Some(Succeeded::satisfied()));
    }

    if let Some(result) = equals_by_unification(lhs, rhs, environment)? {
        return Ok(Some(result));
    }

    if let Some(result) = equals_by_normalization(lhs, rhs, environment)? {
        return Ok(Some(result));
    }

    Ok(None)
}

#[cfg(test)]
pub(super) mod test;
