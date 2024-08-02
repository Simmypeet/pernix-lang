//! A module for retrieving equivalences of a term based on the equality
//! premises.

use super::{
    compatible::{Compatibility, Compatible},
    model::Model,
    normalizer::Normalizer,
    predicate::Predicate,
    query::Context,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    variance::Variance,
    Environment, OverflowError, Succeeded,
};
use crate::symbol::table::State;

/// A trait used for retrieving equivalences of a term based on the equality
/// premises.
pub(super) trait Equivalence: ModelOf + Sized {
    fn get_equivalences(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut Context<Self::Model>,
    ) -> Result<Vec<Succeeded<Self, Self::Model>>, OverflowError>;
}

impl<M: Model> Equivalence for Lifetime<M> {
    fn get_equivalences(
        &self,
        _: &Environment<M, impl State, impl Normalizer<M>>,
        _: &mut Context<M>,
    ) -> Result<Vec<Succeeded<Self, M>>, OverflowError> {
        Ok(Vec::new())
    }
}

impl<M: Model> Equivalence for Type<M> {
    fn get_equivalences(
        &self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        context: &mut Context<M>,
    ) -> Result<Vec<Succeeded<Self, M>>, OverflowError> {
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
            let lhs = Type::TraitMember(equivalence.lhs.clone());
            let lhs = &lhs;
            let rhs = &equivalence.rhs;

            if let Some(Succeeded {
                result:
                    Compatibility {
                        forall_lifetime_instantiations,
                        forall_lifetime_errors,
                    },
                constraints,
            }) = self.compatible_with_context(
                lhs,
                Variance::Covariant,
                environment,
                context,
            )? {
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
        _: &Environment<M, impl State, impl Normalizer<M>>,
        _: &mut Context<M>,
    ) -> Result<Vec<Succeeded<Self, M>>, OverflowError> {
        Ok(Vec::new())
    }
}

pub(super) fn get_equivalences_with_context<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
) -> Result<Vec<Succeeded<T, T::Model>>, OverflowError> {
    let mut equivalences = term.get_equivalences(environment, context)?;

    if let Some(normalization) = term.normalize(environment, context)? {
        equivalences.push(normalization);
    }

    Ok(equivalences)
}

/// Retrieves the equivalences of the given term based on the equality premises
/// and normalization.
pub fn get_equivalences<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Result<Vec<Succeeded<T, T::Model>>, OverflowError> {
    let mut context = Context::new();
    get_equivalences_with_context(term, environment, &mut context)
}