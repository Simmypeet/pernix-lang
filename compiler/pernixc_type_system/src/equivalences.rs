//! A module for retrieving equivalences of a term based on the equality
//! premises.

use pernixc_term::{
    constant::Constant, lifetime::Lifetime, predicate::Predicate, r#type::Type,
    variance::Variance, Model, ModelOf,
};

use crate::{
    compatible::Compatibility, environment::Environment,
    normalizer::Normalizer, term::Term, Error, Succeeded,
};

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub trait Equivalence: ModelOf + Sized {
    #[doc(hidden)]
    fn get_equivalences_internal(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Vec<Succeeded<Self, Self::Model>>, Error>;
}

impl<M: Model> Equivalence for Lifetime<M> {
    fn get_equivalences_internal(
        &self,
        _: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, Error> {
        Ok(Vec::new())
    }
}

impl<M: Model> Equivalence for Type<M> {
    fn get_equivalences_internal(
        &self,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, Error> {
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
            }) = environment.compatible(self, lhs, Variance::Covariant)?
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

impl<M: Model> Equivalence for Constant<M> {
    fn get_equivalences_internal(
        &self,
        _: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, Error> {
        Ok(Vec::new())
    }
}

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Retrieves the equivalences of the given term based on the equality
    /// premises and normalization.
    ///
    /// # Errors
    ///
    /// See [`AbruptError`] for more information.
    pub fn get_equivalences<T: Term<Model = M>>(
        &self,
        term: &T,
    ) -> Result<Vec<Succeeded<T, M>>, Error> {
        let mut equivalences = term.get_equivalences_internal(self)?;

        if let Some(normalization) = term.normalize(self)? {
            equivalences.push(normalization);
        }

        Ok(equivalences)
    }
}
