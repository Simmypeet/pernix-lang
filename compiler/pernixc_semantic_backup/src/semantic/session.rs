//! Contains the definition of [`Session`].

use std::{collections::HashSet, fmt::Debug};

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
    'static
    + Send
    + Sync
    + Clone
    + Debug
    + for<'a> Cache<definite::Record<'a, T>>
    + for<'a> Cache<unification::Record<'a, T>>
    + for<'a> Cache<equality::Record<'a, T>>
    + for<'a> Cache<predicate::OutlivesRecord<'a, T>>
    + for<'a> Cache<predicate::ConstantTypeRecord<'a, T>>
    + for<'a> Cache<predicate::TraitRecord<'a, <T as Term>::Model>>
{
}

impl<T: Term, U> Session<T> for U where
    U: 'static
        + Send
        + Sync
        + Clone
        + Debug
        + for<'a> Cache<definite::Record<'a, T>>
        + for<'a> Cache<unification::Record<'a, T>>
        + for<'a> Cache<equality::Record<'a, T>>
        + for<'a> Cache<predicate::OutlivesRecord<'a, T>>
        + for<'a> Cache<predicate::ConstantTypeRecord<'a, T>>
        + for<'a> Cache<predicate::TraitRecord<'a, <T as Term>::Model>>
{
}

/// Default implementation of [`Session`].
#[derive(Debug, Clone, Default)]
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

    trait_satisfies: HashSet<(ID<symbol::Trait>, bool, GenericArguments<S>)>,

    lifetime_outlives: HashSet<(Lifetime<S>, Lifetime<S>)>,
    type_outlives: HashSet<(Type<S>, Lifetime<S>)>,
    constant_outlives: HashSet<(Constant<S>, Lifetime<S>)>,

    type_is_constant_type: HashSet<Type<S>>,
    constant_is_constant_type: HashSet<Constant<S>>,
    lifetime_is_constant_type: HashSet<Lifetime<S>>,
}

macro_rules! implements_cache {
    ($module:ident, $name:ident, $kind:path, $field_name:ident, $record:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a, S: Model> Cache<$module::$name<'a, $kind>> for Default<S> {
            fn mark_as_working_on(&mut self, $record: $module::$name<'a, $kind>) -> bool {
                self.$field_name.insert($expr_in)
            }

            fn mark_as_done(&mut self, $record: $module::$name<'a, $kind>) {
                self.$field_name.remove($expr_out);
            }
        }
    };
}

implements_cache!(
    definite,
    Record,
    Type<S>,
    type_definite,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    definite,
    Record,
    Constant<S>,
    constant_definitie,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    definite,
    Record,
    Lifetime<S>,
    lifetime_definite,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    unification,
    Record,
    Type<S>,
    type_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification,
    Record,
    Constant<S>,
    constant_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification,
    Record,
    Lifetime<S>,
    lifetime_unifies,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Record,
    Type<S>,
    type_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Record,
    Constant<S>,
    constant_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    equality,
    Record,
    Lifetime<S>,
    lifetime_equals,
    record,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    predicate,
    OutlivesRecord,
    Type<S>,
    type_outlives,
    record,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate,
    OutlivesRecord,
    Lifetime<S>,
    lifetime_outlives,
    record,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate,
    OutlivesRecord,
    Constant<S>,
    constant_outlives,
    record,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate,
    ConstantTypeRecord,
    Type<S>,
    type_is_constant_type,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate,
    ConstantTypeRecord,
    Constant<S>,
    constant_is_constant_type,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate,
    ConstantTypeRecord,
    Lifetime<S>,
    lifetime_is_constant_type,
    record,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate,
    TraitRecord,
    S,
    trait_satisfies,
    record,
    (
        record.trait_id,
        record.const_trait,
        record.generic_arguments.clone()
    ),
    &(
        record.trait_id,
        record.const_trait,
        record.generic_arguments.clone()
    )
);
