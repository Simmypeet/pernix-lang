//! Implements the logic for equality checking.

use std::sync::Arc;

use pernixc_term::matching::Matching;

use crate::{
    environment::{Environment, Query},
    normalizer::Normalizer,
    term::Term,
    Error, Satisfied, Succeeded,
};

/// A query for checking strict equality
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
#[allow(missing_docs)]
pub struct Equality<T, U = T> {
    pub lhs: T,
    pub rhs: U,
}

#[allow(clippy::mismatching_type_param_order)]
impl<T: Term> Query for Equality<T, T> {
    type Parameter = ();
    type InProgress = ();
    type Result = Succeeded<Satisfied>;
    type Error = Error;

    async fn query(
        &self,
        environment: &Environment<'_, impl Normalizer>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        if let Some(result) =
            equals_without_mapping(&self.lhs, &self.rhs, environment).await?
        {
            return Ok(Some(Arc::new(result)));
        }

        for predicate in &environment.premise().predicates {
            let Some(equality_predicate) =
                T::as_trait_member_compatible_predicate(predicate)
            else {
                continue;
            };

            let trait_member_term: T = equality_predicate.lhs.clone().into();

            if self.lhs.as_trait_member().is_some() {
                if let Some(mut result) = equals_without_mapping(
                    &self.lhs,
                    &trait_member_term,
                    environment,
                )
                .await?
                {
                    if let Some(inner_result) =
                        Box::pin(environment.query(&Self::new(
                            equality_predicate.rhs.clone(),
                            self.rhs.clone(),
                        )))
                        .await?
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
                )
                .await?
                {
                    if let Some(inner_result) =
                        Box::pin(environment.query(&Self::new(
                            self.lhs.clone(),
                            equality_predicate.rhs.clone(),
                        )))
                        .await?
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

async fn equals_by_unification<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, super::Error> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut satisfied = Succeeded::default();

    for Matching { lhs, rhs, .. } in matching.lifetimes {
        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for Matching { lhs, rhs, .. } in matching.types {
        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for Matching { lhs, rhs, .. } in matching.constants {
        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    Ok(Some(satisfied))
}

async fn equals_by_normalization<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, super::Error> {
    if let Some(Succeeded { result: eq, mut constraints }) =
        lhs.normalize(environment).await?
    {
        if let Some(result) =
            environment.query(&Equality::new(eq, rhs.clone())).await?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::satisfied_with(constraints)));
        }
    }

    if let Some(Succeeded { result: eq, mut constraints }) =
        rhs.normalize(environment).await?
    {
        if let Some(result) =
            environment.query(&Equality::new(lhs.clone(), eq)).await?
        {
            constraints.extend(result.constraints.iter().cloned());
            return Ok(Some(Succeeded::satisfied_with(constraints)));
        }
    }

    Ok(None)
}

async fn equals_without_mapping<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<'_, impl Normalizer>,
) -> Result<Option<Succeeded<Satisfied>>, super::Error> {
    // trivially satisfied
    if lhs == rhs {
        return Ok(Some(Succeeded::satisfied()));
    }

    Box::pin(async move {
        if let Some(result) =
            equals_by_unification(lhs, rhs, environment).await?
        {
            return Ok(Some(result));
        }

        if let Some(result) =
            equals_by_normalization(lhs, rhs, environment).await?
        {
            return Ok(Some(result));
        }

        Ok(None)
    })
    .await
}

#[cfg(test)]
mod test;
