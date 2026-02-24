//! Implements the logic for equality checking.

use std::sync::Arc;

use crate::{
    Satisfied, Succeeded,
    environment::{BoxedFuture, Environment, Query, QueryResult},
    normalizer::Normalizer,
    term::Term,
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
    type InProgress = ();
    type Result = Option<Arc<Succeeded<Satisfied>>>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            if let Some(result) =
                equals_without_mapping(&self.lhs, &self.rhs, environment)
                    .await?
            {
                return Ok(Some(Arc::new(result)));
            }

            for predicate in &environment.premise().predicates {
                let Some(equality_predicate) =
                    T::as_instance_associated_equality_predicate(predicate)
                else {
                    continue;
                };

                let trait_member_term: T =
                    equality_predicate.lhs.clone().into();

                if self.lhs.as_instance_associated().is_some()
                    && let Some(mut result) = equals_without_mapping(
                        &self.lhs,
                        &trait_member_term,
                        environment,
                    )
                    .await?
                    && let Some(inner_result) = environment
                        .query(&Self::new(
                            equality_predicate.rhs.clone(),
                            self.rhs.clone(),
                        ))
                        .await?
                {
                    result
                        .constraints
                        .extend(inner_result.constraints.iter().cloned());
                    return Ok(Some(Arc::new(result)));
                }

                if self.rhs.as_instance_associated().is_some()
                    && let Some(mut result) = equals_without_mapping(
                        &trait_member_term,
                        &self.rhs,
                        environment,
                    )
                    .await?
                    && let Some(inner_result) = environment
                        .query(&Self::new(
                            self.lhs.clone(),
                            equality_predicate.rhs.clone(),
                        ))
                        .await?
                {
                    result
                        .constraints
                        .extend(inner_result.constraints.iter().cloned());

                    return Ok(Some(Arc::new(result)));
                }
            }

            Ok(None)
        })
    }

    fn on_cyclic(
        &self,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[crate::environment::Call<
            crate::environment::DynArc,
            crate::environment::DynArc,
        >],
    ) -> Self::Result {
        None
    }
}

async fn equals_by_unification<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Satisfied>>> {
    let Some(matching) = lhs.substructural_match(rhs) else {
        return Ok(None);
    };

    let mut satisfied = Succeeded::default();
    let (lifetimes, types, constants, instances) = matching.destructure();

    for matching in lifetimes {
        let (lhs, rhs, _, _) = matching.destructure();

        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for matching in types {
        let (lhs, rhs, _, _) = matching.destructure();

        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for matching in constants {
        let (lhs, rhs, _, _) = matching.destructure();

        let Some(result) = environment.query(&Equality::new(lhs, rhs)).await?
        else {
            return Ok(None);
        };

        satisfied.constraints.extend(result.constraints.iter().cloned());
    }

    for matching in instances {
        let (lhs, rhs, _, _) = matching.destructure();

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
) -> QueryResult<Option<Succeeded<Satisfied>>> {
    if let Some(Succeeded { result: eq, mut constraints }) =
        lhs.normalize(environment).await?
        && let Some(result) =
            environment.query(&Equality::new(eq, rhs.clone())).await?
    {
        constraints.extend(result.constraints.iter().cloned());
        return Ok(Some(Succeeded::satisfied_with(constraints)));
    }

    if let Some(Succeeded { result: eq, mut constraints }) =
        rhs.normalize(environment).await?
        && let Some(result) =
            environment.query(&Equality::new(lhs.clone(), eq)).await?
    {
        constraints.extend(result.constraints.iter().cloned());
        return Ok(Some(Succeeded::satisfied_with(constraints)));
    }

    Ok(None)
}

async fn equals_without_mapping<T: Term>(
    lhs: &T,
    rhs: &T,
    environment: &Environment<'_, impl Normalizer>,
) -> QueryResult<Option<Succeeded<Satisfied>>> {
    // trivially satisfied
    if lhs == rhs {
        return Ok(Some(Succeeded::satisfied()));
    }

    if let Some(result) = equals_by_unification(lhs, rhs, environment).await? {
        return Ok(Some(result));
    }

    if let Some(result) = equals_by_normalization(lhs, rhs, environment).await?
    {
        return Ok(Some(result));
    }

    Ok(None)
}

impl<N: Normalizer> Environment<'_, N> {
    /// Checks if two terms are strictly equals.
    pub async fn equals<T: Term>(
        &self,
        lhs: T,
        rhs: T,
    ) -> QueryResult<Option<Arc<Succeeded<Satisfied>>>> {
        self.query(&Equality::new(lhs, rhs)).await
    }
}

#[cfg(test)]
pub mod test;
