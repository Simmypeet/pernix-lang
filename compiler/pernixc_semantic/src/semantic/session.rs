//! Contains the definition of [`Session`].

use std::collections::HashSet;

use super::{
    definite, equality,
    model::Model,
    predicate,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments, Term},
    unification,
};
use crate::{arena::ID, symbol};

/// Used to *remember* the queries that have been made.
pub trait Cache<Record> {
    /// Marks the given record as working on.
    ///
    /// Returns `true` if the record is not already in the cache.
    fn mark_as_working_on(&mut self, record: Record) -> bool;

    /// Marks the given record as done.
    fn mark_as_done(&mut self, record: Record);
}

/// Used to detect the circular reasoning in the system.
pub trait Session<T: Term>:
    for<'a> Cache<definite::Record<'a, T>>
    + for<'a> Cache<unification::Record<'a, T>>
    + for<'a> Cache<equality::Record<'a, T>>
    + for<'a> Cache<predicate::TraitRecord<'a, <T as Term>::Model>>
{
}

impl<T: Term, U> Session<T> for U where
    U: for<'a> Cache<definite::Record<'a, T>>
        + for<'a> Cache<unification::Record<'a, T>>
        + for<'a> Cache<equality::Record<'a, T>>
        + for<'a> Cache<predicate::TraitRecord<'a, <T as Term>::Model>>
{
}

/// Default implementation of [`Session`].
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Default<S: Model> {
    type_definite: HashSet<Type<S>>,
    constant_definitie: HashSet<Constant<S>>,
    lifetime_definite: HashSet<Lifetime<S>>,

    type_unifies: HashSet<(Type<S>, Type<S>)>,
    constant_unifies: HashSet<(Constant<S>, Constant<S>)>,
    lifetime_unifies: HashSet<(Lifetime<S>, Lifetime<S>)>,

    type_equals: HashSet<(Type<S>, Type<S>)>,
    constant_equals: HashSet<(Constant<S>, Constant<S>)>,
    lifetime_equals: HashSet<(Lifetime<S>, Lifetime<S>)>,

    constnat_type_satisfies: HashSet<Type<S>>,
    trait_satisfies: HashSet<(ID<symbol::Trait>, bool, GenericArguments<S>)>,
}

macro_rules! implements_cache {
    ($module:ident, $kind:path, $field_name:ident, $record:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a, S: Model> Cache<$module::Record<'a, $kind>> for Default<S> {
            fn mark_as_working_on(&mut self, $record: $module::Record<'a, $kind>) -> bool {
                self.$field_name.insert($expr_in)
            }

            fn mark_as_done(&mut self, $record: $module::Record<'a, $kind>) {
                self.$field_name.remove($expr_out);
            }
        }
    };
}

implements_cache!(
    definite,
    Type<S>,
    type_definite,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    definite,
    Constant<S>,
    constant_definitie,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    definite,
    Lifetime<S>,
    lifetime_definite,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    unification,
    Type<S>,
    type_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification,
    Constant<S>,
    constant_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification,
    Lifetime<S>,
    lifetime_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Type<S>,
    type_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Constant<S>,
    constant_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Lifetime<S>,
    lifetime_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

impl<'a, S: Model> Cache<predicate::TraitRecord<'a, S>> for Default<S> {
    fn mark_as_working_on(&mut self, record: predicate::TraitRecord<'a, S>) -> bool {
        self.trait_satisfies.insert((
            record.trait_id,
            record.const_trait,
            record.generic_arguments.clone(),
        ))
    }

    fn mark_as_done(&mut self, record: predicate::TraitRecord<'a, S>) {
        self.trait_satisfies.remove(&(
            record.trait_id,
            record.const_trait,
            record.generic_arguments.clone(),
        ));
    }
}
