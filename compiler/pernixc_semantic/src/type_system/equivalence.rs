//! A module for retrieving equivalences of a term based on the equality
//! premises.

use super::{
    compatible::{Compatibility, Compatible},
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    variance::Variance,
    AbruptError, Environment, Succeeded,
};

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub(super) trait Equivalence: ModelOf + Sized {
    fn get_equivalences(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
    ) -> Result<Vec<Succeeded<Self, Self::Model>>, AbruptError>;
}

impl<M: Model> Equivalence for Lifetime<M> {
    fn get_equivalences(
        &self,
        _: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, AbruptError> {
        Ok(Vec::new())
    }
}

impl<M: Model> Equivalence for Type<M> {
    fn get_equivalences(
        &self,
        environment: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, AbruptError> {
        let mut equivalences = Vec::new();

        if !self.is_trait_member() {
            return Ok(equivalences);
        }

        for equivalence in environment
            .premise
            .predicates
            .iter()
            .filter_map(Predicate::as_trait_type_equality)
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
            }) = self.compatible(lhs, Variance::Covariant, environment)?
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
    fn get_equivalences(
        &self,
        _: &Environment<M, impl Normalizer<M>>,
    ) -> Result<Vec<Succeeded<Self, M>>, AbruptError> {
        Ok(Vec::new())
    }
}

/// Retrieves the equivalences of the given term based on the equality premises
/// and normalization.
///
/// # Errors
///
/// See [`OverflowError`] for more information.
pub fn get_equivalences<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl Normalizer<T::Model>>,
) -> Result<Vec<Succeeded<T, T::Model>>, AbruptError> {
    let mut equivalences = term.get_equivalences(environment)?;

    if let Some(normalization) = term.normalize(environment)? {
        equivalences.push(normalization);
    }

    Ok(equivalences)
}
