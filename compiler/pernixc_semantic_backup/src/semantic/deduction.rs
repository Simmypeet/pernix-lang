//! Contains the generic arguments deduction logic.

use std::collections::HashMap;

use super::{
    model::Model,
    predicate::Premises,
    session::Session,
    substitution::Substitution,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Term},
    unification, Semantic,
};
use crate::table::{State, Table};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct DeductionUnifingConfig;

impl<S: Model> unification::Config<Type<S>> for DeductionUnifingConfig {
    fn unifiable(&mut self, lhs: &Type<S>, _: &Type<S>) -> bool {
        lhs.is_parameter() || lhs.is_trait_member()
    }
}

impl<S: Model> unification::Config<Lifetime<S>> for DeductionUnifingConfig {
    fn unifiable(&mut self, lhs: &Lifetime<S>, _: &Lifetime<S>) -> bool { lhs.is_parameter() }
}

impl<S: Model> unification::Config<Constant<S>> for DeductionUnifingConfig {
    fn unifiable(&mut self, lhs: &Constant<S>, _: &Constant<S>) -> bool {
        lhs.is_parameter() || lhs.is_trait_member()
    }
}

fn extract<T: Eq + std::hash::Hash>(
    map: HashMap<T, T>,
    mut predicate: impl FnMut(&T) -> bool,
) -> (HashMap<T, T>, HashMap<T, T>) {
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
    S: Semantic<T>
        + Semantic<Type<<T as Term>::Model>>
        + Semantic<Constant<<T as Term>::Model>>
        + Semantic<Lifetime<<T as Term>::Model>>,
    R: Session<T>
        + Session<Type<<T as Term>::Model>>
        + Session<Constant<<T as Term>::Model>>
        + Session<Lifetime<<T as Term>::Model>>,
>(
    mappigs: HashMap<T, T>,
    deduced_unification: &Substitution<<T as Term>::Model>,
    premises: &Premises<<T as Term>::Model>,
    table: &Table<impl State>,
    semantic: &S,
    session: &mut R,
) -> bool {
    for (mut key, value) in mappigs {
        key.apply(deduced_unification);

        if !key.equals(&value, premises, table, semantic, session) {
            return false;
        }
    }

    true
}

impl<M: Model> GenericArguments<M> {
    /// Performs type deduction.
    ///
    /// This operation performs type deduction on the generic arguments of the current generic
    /// arguments with respect to the generic arguments of the `another` generic arguments. This
    /// deduction will return a [`Substitution`] that maps the type, lifetime, and constant
    /// parameter of `self` to the type, lifetime, and constant parameter of `another`.
    ///
    ///
    /// # Examples
    ///
    /// ``` txt
    /// deduce([int32, A[uint32]], [T?, A[?U]] ) = {
    ///     ?T -> int32,
    ///     ?U -> uint32
    /// }
    /// ```
    pub fn deduce<
        S: Semantic<Type<M>> + Semantic<Lifetime<M>> + Semantic<Constant<M>>,
        R: Session<Type<M>> + Session<Lifetime<M>> + Session<Constant<M>>,
    >(
        &self,
        another: &Self,
        premises: &Premises<M>,
        table: &Table<impl State>,
        semantic: &S,
        session: &mut R,
    ) -> Option<Substitution<M>> {
        // gets the deduced generic arguments
        let Some(base_unification) = self.unify(
            another,
            premises,
            table,
            semantic,
            session,
            &mut DeductionUnifingConfig,
        ) else {
            return None;
        };

        // extract the member
        let (base_unification, trait_type_map, trait_constant_map) = {
            let (type_param_map, trait_type_map) =
                extract(base_unification.types, Type::is_trait_member);
            let (constant_param_map, trait_constant_map) =
                extract(base_unification.constants, Constant::is_trait_member);

            (
                Substitution {
                    types: type_param_map,
                    constants: constant_param_map,
                    lifetimes: base_unification.lifetimes,
                },
                trait_type_map,
                trait_constant_map,
            )
        };

        if !(mapping_equals(
            trait_type_map,
            &base_unification,
            premises,
            table,
            semantic,
            session,
        ) && mapping_equals(
            trait_constant_map,
            &base_unification,
            premises,
            table,
            semantic,
            session,
        )) {
            return None;
        }

        Some(base_unification)
    }
}
