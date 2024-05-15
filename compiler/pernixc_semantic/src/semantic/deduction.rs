//! Contains the generic arguments deduction logic.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use super::{
    equality,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    model::Model,
    normalizer::Normalizer,
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification, Environment,
};
use crate::table::State;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DeductionUnifyingConfig;

impl<M: Model> unification::Config<M> for DeductionUnifyingConfig {
    fn lifetime_unifiable(
        &mut self,
        from: &Lifetime<M>,
        _: &Lifetime<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(from.is_parameter())
    }

    fn type_unifiable(
        &mut self,
        from: &Type<M>,
        _: &Type<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(matches!(from, Type::Parameter(_) | Type::TraitMember(_)))
    }

    fn constant_unifiable(
        &mut self,
        from: &Constant<M>,
        _: &Constant<M>,
    ) -> Result<bool, ExceedLimitError> {
        Ok(matches!(from, Constant::Parameter(_)))
    }
}

fn unify<T: Term>(
    lhs: &[T],
    rhs: &[T],
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
    mut existing: Mapping<T::Model>,
) -> Result<Option<Mapping<T::Model>>, ExceedLimitError> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = unification::unify(
            lhs,
            rhs,
            &mut DeductionUnifyingConfig,
            environment,
            limit,
        )?
        else {
            return Ok(None);
        };

        existing.append_from_unification(new);
    }

    Ok(Some(existing))
}

fn extract<K: Eq + Hash, V>(
    map: HashMap<K, V>,
    mut predicate: impl FnMut(&K) -> bool,
) -> (HashMap<K, V>, HashMap<K, V>) {
    let mut positive = HashMap::new();
    let mut negative = HashMap::new();

    for (key, value) in map {
        if predicate(&key) {
            positive.insert(key, value);
        } else {
            negative.insert(key, value);
        }
    }

    (positive, negative)
}

fn mapping_equals<T: Term>(
    unification: HashMap<T, HashSet<T>>,
    substitution: &Instantiation<T::Model>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<bool, ExceedLimitError> {
    for (mut key, values) in unification {
        instantiation::instantiate(&mut key, substitution);

        for value in values {
            if !equality::equals(&key, &value, environment, limit)? {
                return Ok(false);
            }
        }
    }

    Ok(true)
}

#[allow(clippy::type_complexity)]
fn from_unification_to_substitution<T: Term>(
    unification: HashMap<T, HashSet<T>>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<Option<HashMap<T, T>>, ExceedLimitError> {
    let mut result = HashMap::new();
    for (key, values) in unification {
        let mut values = values.into_iter();

        let sampled = values.next().expect("should at least have one element");

        for value in values {
            if !equality::equals(&sampled, &value, environment, limit)? {
                return Ok(None);
            }
        }

        assert!(result.insert(key, sampled).is_none());
    }

    Ok(Some(result))
}

impl<M: Model> GenericArguments<M> {
    /// Performs generic parameter deduction.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn deduce(
        &self,
        another: &Self,
        environment: &Environment<M, impl State, impl Normalizer<M>>,
        limit: &mut Limit<
            impl Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
        >,
    ) -> Result<Option<Instantiation<M>>, ExceedLimitError> {
        // arity check
        if self.lifetimes.len() != another.lifetimes.len()
            || self.types.len() != another.types.len()
            || self.constants.len() != another.constants.len()
        {
            return Ok(None);
        }

        // unify all kinds of generic arguments
        let Some(unification) = unify(
            &self.lifetimes,
            &another.lifetimes,
            environment,
            limit,
            Mapping::default(),
        )?
        else {
            return Ok(None);
        };
        let Some(unification) = unify(
            &self.types,
            &another.types,
            environment,
            limit,
            unification,
        )?
        else {
            return Ok(None);
        };
        let Some(unification) = unify(
            &self.constants,
            &another.constants,
            environment,
            limit,
            unification,
        )?
        else {
            return Ok(None);
        };

        // separate out the unification between generic parameters and trait
        // members
        let (base_unification, trait_type_map, trait_constant_map) = {
            let (type_param_map, trait_type_map) =
                extract(unification.types, Type::is_parameter);
            let (constant_param_map, trait_constant_map) =
                extract(unification.constants, Constant::is_parameter);

            let Some(lifetimes) = from_unification_to_substitution(
                unification.lifetimes,
                environment,
                limit,
            )?
            else {
                return Ok(None);
            };
            let Some(types) = from_unification_to_substitution(
                type_param_map,
                environment,
                limit,
            )?
            else {
                return Ok(None);
            };
            let Some(constants) = from_unification_to_substitution(
                constant_param_map,
                environment,
                limit,
            )?
            else {
                return Ok(None);
            };
            (
                Instantiation { lifetimes, types, constants },
                trait_type_map,
                trait_constant_map,
            )
        };

        if !mapping_equals(
            trait_type_map,
            &base_unification,
            environment,
            limit,
        )? || !mapping_equals(
            trait_constant_map,
            &base_unification,
            environment,
            limit,
        )? {
            return Ok(None);
        }

        Ok(Some(base_unification))
    }
}
