//! A module for retrieving equivalences of a term based on the equality
//! premises.

use std::{future::Future, sync::Arc};

use pernixc_term::{
    constant::Constant, lifetime::Lifetime, predicate::Predicate, r#type::Type,
    variance::Variance,
};

use crate::{
    environment::{BoxedFuture, Environment, Query},
    normalizer::Normalizer,
    subtype::Subtype,
    term::Term,
    Error, Succeeded,
};

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub trait Impl: Sized {
    #[doc(hidden)]
    fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> impl Future<Output = Result<Vec<Succeeded<Self>>, Error>> + Send;
}

impl Impl for Lifetime {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Vec<Succeeded<Self>>, Error> {
        let mut equivalences = Vec::new();

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences)
    }
}

impl Impl for Type {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Vec<Succeeded<Self>>, Error> {
        let mut equivalences = Vec::new();

        if !self.is_trait_member() {
            return Ok(equivalences);
        }

        for equivalence in environment
            .premise()
            .predicates
            .iter()
            .filter_map(Predicate::as_trait_type_compatible)
        {
            let lhs = Self::TraitMember(equivalence.lhs.clone());
            let rhs = &equivalence.rhs;

            if let Some(result) = environment
                .query(&Subtype::new(self.clone(), lhs, Variance::Invariant))
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

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences)
    }
}

impl Impl for Constant {
    async fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> Result<Vec<Succeeded<Self>>, Error> {
        let mut equivalences = Vec::new();

        if let Some(normalization) = self.normalize(environment).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences)
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
    type Parameter = ();
    type InProgress = ();
    type Result = Vec<Succeeded<T>>;
    type Error = Error;

    fn query<'x, N: Normalizer>(
        &'x self,
        environment: &'x Environment<'x, N>,
        (): Self::Parameter,
        (): Self::InProgress,
    ) -> BoxedFuture<'x, Self::Result, Self::Error> {
        Box::pin(async move {
            Impl::get_equivalences_internal(&self.0, environment)
                .await
                .map(|x| Some(Arc::new(x)))
        })
    }
}

impl<N: Normalizer> Environment<'_, N> {
    /// Retrieves the equivalences of the given term based on the equality
    /// premises and normalization.
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub async fn get_equivalences<T: Term>(
        &self,
        term: &T,
    ) -> Result<Arc<Vec<Succeeded<T>>>, Error> {
        // it should always return `Some` even in the cases of there's no more
        // equivalent rewrites.
        Ok(self.query(&Equivalences(term.clone())).await?.unwrap_or_default())
    }
}
