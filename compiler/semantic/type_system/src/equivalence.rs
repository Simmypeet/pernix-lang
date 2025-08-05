//! A module for retrieving equivalences of a term based on the equality
//! premises.

use std::future::Future;

use pernixc_term::{
    constant::Constant, lifetime::Lifetime, predicate::Predicate, r#type::Type,
    variance::Variance,
};

use crate::{
    compatible::Compatibility, environment::Environment,
    normalizer::Normalizer, term::Term, Error, Succeeded,
};

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub trait Equivalence: Sized {
    #[doc(hidden)]
    fn get_equivalences_internal(
        &self,
        environment: &Environment<'_, impl Normalizer>,
    ) -> impl Future<Output = Result<Vec<Succeeded<Self>>, Error>>;
}

impl Equivalence for Lifetime {
    async fn get_equivalences_internal(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Vec<Succeeded<Self>>, Error> {
        Ok(Vec::new())
    }
}

impl Equivalence for Type {
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
            let lhs = &lhs;
            let rhs = &equivalence.rhs;

            if let Some(Succeeded {
                result:
                    Compatibility {
                        forall_lifetime_instantiations,
                        forall_lifetime_errors,
                    },
                constraints,
            }) =
                environment.compatible(self, lhs, Variance::Covariant).await?
            {
                let mut result = rhs.clone();

                // instantiate the forall lifetime
                forall_lifetime_instantiations.instantiate(&mut result);

                if !forall_lifetime_errors.is_empty() {
                    continue; // sadly
                }

                equivalences.push(Succeeded { result, constraints });
            }
        }

        Ok(equivalences)
    }
}

impl Equivalence for Constant {
    async fn get_equivalences_internal(
        &self,
        _: &Environment<'_, impl Normalizer>,
    ) -> Result<Vec<Succeeded<Self>>, Error> {
        Ok(Vec::new())
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
    ) -> Result<Vec<Succeeded<T>>, Error> {
        let mut equivalences = term.get_equivalences_internal(self).await?;

        if let Some(normalization) = term.normalize(self).await? {
            equivalences.push(normalization);
        }

        Ok(equivalences)
    }
}
