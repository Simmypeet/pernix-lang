//! A module for retrieving equivalences of a term based on the equality
//! premises.

use std::{future::Future, sync::Arc};

use pernixc_semantic_element::variance::Variance;
use pernixc_term::{
    constant::Constant, instance::Instance, lifetime::Lifetime,
    predicate::Predicate, r#type::Type,
};

use crate::{
    Succeeded,
    environment::{BoxedFuture, Environment, Query, QueryResult},
    normalizer::Normalizer,
    subtype::Subtype,
    term::Term,
};

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub trait Impl: Sized {
    #[doc(hidden)]
    fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> impl Future<Output = QueryResult<Arc<[Succeeded<Self>]>>> + Send;
}

impl Impl for Lifetime {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Arc<[Succeeded<Self>]>> {
        let mut equivalences = Vec::new();

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences.into())
    }
}

impl Impl for Type {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Arc<[Succeeded<Self>]>> {
        let mut equivalences = Vec::new();

        if self.is_instance_associated() {
            for equivalence in environment
                .premise()
                .predicates
                .iter()
                .filter_map(Predicate::as_instance_associated_type_equality)
            {
                let lhs = Self::InstanceAssociated(equivalence.lhs.clone());
                let rhs = &equivalence.rhs;

                if let Some(result) = environment
                    .query(&Subtype::new(
                        self.clone(),
                        lhs,
                        Variance::Invariant,
                    ))
                    .await?
                {
                    if !result.result.forall_lifetime_errors.is_empty() {
                        continue; // sadly
                    }

                    let mut final_result = rhs.clone();

                    // instantiate the forall lifetime
                    result
                        .result
                        .forall_lifetime_instantiations
                        .instantiate(&mut final_result);

                    equivalences.push(Succeeded {
                        result: final_result,
                        constraints: result.constraints.clone(),
                    });
                }
            }
        }

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences.into())
    }
}

impl Impl for Constant {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Arc<[Succeeded<Self>]>> {
        let mut equivalences = Vec::new();

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences.into())
    }
}

impl Impl for Instance {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> QueryResult<Arc<[Succeeded<Self>]>> {
        let mut equivalences = Vec::new();

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences.into())
    }
}

/// A query for retrieving a set of equivalences term.
///
/// This query is used for term rewriting.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, derive_new::new,
)]
pub struct Equivalences<T>(pub T);

impl<T: Term> Query for Equivalences<T> {
    type InProgress = ();
    type Result = Arc<[Succeeded<T>]>;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result> {
        Box::pin(async move {
            Impl::get_equivalences_internal(&self.0, environment).await
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
        Arc::from([])
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Retrieves the equivalences of the given term based on the equality
    /// premises and normalization.
    pub async fn get_equivalences<T: Term>(
        &self,
        term: &T,
    ) -> QueryResult<Arc<[Succeeded<T>]>> {
        // it should always return `Some` even in the cases of there's no more
        // equivalent rewrites.
        self.query(&Equivalences(term.clone())).await
    }
}
