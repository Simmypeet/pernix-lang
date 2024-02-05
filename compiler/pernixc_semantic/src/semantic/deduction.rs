//! Contains the generic arguments deduction logic.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use super::{
    equality,
    instantiation::{self, Instantiation},
    mapping::Mapping,
    session::{ExceedLimitError, Limit, Session},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification, Premise, Semantic,
};
use crate::{
    arena::ID,
    semantic::term::{constant, r#type, MemberSymbol},
    symbol::{GenericID, MemberID},
    table::{State, Table},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DeductionUnifyingConfig;

impl unification::Config for DeductionUnifyingConfig {
    fn lifetime_unifiable(&mut self, from: &Lifetime, _: &Lifetime) -> bool {
        from.is_parameter()
    }

    fn type_unifiable(&mut self, from: &Type, _: &Type) -> bool {
        matches!(
            from,
            Type::Parameter(_)
                | Type::MemberSymbol(MemberSymbol {
                    id: r#type::MemberSymbolKindID::Trait(_),
                    ..
                })
        )
    }

    fn constant_unifiable(&mut self, from: &Constant, _: &Constant) -> bool {
        matches!(
            from,
            Constant::Parameter(_)
                | Constant::MemberSymbol(MemberSymbol {
                    id: constant::MemberSymbolKindID::Trait(_),
                    ..
                })
        )
    }
}

fn unify<
    T: Term,
    S: Semantic<T> + Semantic<Type> + Semantic<Lifetime> + Semantic<Constant>,
    R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>,
>(
    lhs: &[T],
    rhs: &[T],
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
    mut existing: Mapping,
) -> Result<Option<Mapping>, ExceedLimitError> {
    for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
        let Some(new) = unification::unify(
            lhs,
            rhs,
            premise,
            table,
            &mut DeductionUnifyingConfig,
            semantic,
            session,
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

fn mapping_equals<
    T: Term,
    S: Semantic<T> + Semantic<Type> + Semantic<Lifetime> + Semantic<Constant>,
    R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>,
>(
    unification: HashMap<T, HashSet<T>>,
    substitution: &Instantiation,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    for (mut key, values) in unification {
        instantiation::instantiate(&mut key, substitution);

        for value in values {
            if !equality::equals(
                &key, &value, premise, table, semantic, session,
            )? {
                return Ok(false);
            }
        }
    }

    Ok(true)
}

#[allow(clippy::type_complexity)]
fn from_unification_to_substitution<
    T: Term,
    S: Semantic<T> + Semantic<Type> + Semantic<Lifetime> + Semantic<Constant>,
    R: Session<T> + Session<Type> + Session<Lifetime> + Session<Constant>,
>(
    unification: HashMap<T, HashSet<T>>,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<
    Option<HashMap<MemberID<ID<T::GenericParameter>, GenericID>, T>>,
    ExceedLimitError,
> {
    let mut result = HashMap::new();
    for (key, values) in unification {
        let mut values = values.into_iter();

        let sampled = values.next().expect("should at least have one element");

        for value in values {
            if !equality::equals(
                &sampled, &value, premise, table, semantic, session,
            )? {
                return Ok(None);
            }
        }

        result
            .insert(
                key.into_generic_parameter()
                    .expect("should be a generic parameter"),
                sampled,
            )
            .unwrap();
    }

    Ok(Some(result))
}

impl GenericArguments {
    /// Performs generic parameter deduction.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    #[allow(clippy::too_many_lines)]
    pub fn deduce<
        S: Semantic<Type> + Semantic<Lifetime> + Semantic<Constant>,
        R: Session<Type> + Session<Lifetime> + Session<Constant>,
    >(
        &self,
        another: &Self,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<Option<Instantiation>, ExceedLimitError> {
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
            premise,
            table,
            semantic,
            session,
            Mapping::default(),
        )?
        else {
            return Ok(None);
        };
        let Some(unification) = unify(
            &self.types,
            &another.types,
            premise,
            table,
            semantic,
            session,
            unification,
        )?
        else {
            return Ok(None);
        };
        let Some(unification) = unify(
            &self.constants,
            &another.constants,
            premise,
            table,
            semantic,
            session,
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
                premise,
                table,
                semantic,
                session,
            )?
            else {
                return Ok(None);
            };
            let Some(types) = from_unification_to_substitution(
                type_param_map,
                premise,
                table,
                semantic,
                session,
            )?
            else {
                return Ok(None);
            };
            let Some(constants) = from_unification_to_substitution(
                constant_param_map,
                premise,
                table,
                semantic,
                session,
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
            premise,
            table,
            semantic,
            session,
        )? || !mapping_equals(
            trait_constant_map,
            &base_unification,
            premise,
            table,
            semantic,
            session,
        )? {
            return Ok(None);
        }

        Ok(Some(base_unification))
    }
}
