//! Contains the definition of [`Session`].

use std::{
    collections::{hash_map::Entry, HashMap},
    hash::Hash,
};

use getset::CopyGetters;

use super::{
    equality,
    model::Model,
    predicate::{self, ConstantTypeQuerySource, TraitSatisfiability},
    simplify,
    sub_term::SubTerm,
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments,
        Term,
    },
    unification::{self, Unification},
    ExceedLimitError, Satisfied,
};
use crate::{arena::ID, symbol};

pub(super) trait Get: SubTerm {
    type Session<T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>>: Session<Self>;

    fn get_session<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a T,
    ) -> &'a Self::Session<T>;

    fn get_session_mut<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a mut T,
    ) -> &'a mut Self::Session<T>;
}

impl<M: Model> Get for Lifetime<M> {
    type Session<
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    > = T;

    fn get_session<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a T,
    ) -> &'a Self::Session<T> {
        session
    }

    fn get_session_mut<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a mut T,
    ) -> &'a mut Self::Session<T> {
        session
    }
}

impl<M: Model> Get for Type<M> {
    type Session<
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    > = T;

    fn get_session<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a T,
    ) -> &'a Self::Session<T> {
        session
    }

    fn get_session_mut<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a mut T,
    ) -> &'a mut Self::Session<T> {
        session
    }
}

impl<M: Model> Get for Constant<M> {
    type Session<
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    > = T;

    fn get_session<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a T,
    ) -> &'a Self::Session<T> {
        session
    }

    fn get_session_mut<
        'a,
        T: Session<Lifetime<Self::Model>>
            + Session<Type<Self::Model>>
            + Session<Constant<Self::Model>>,
    >(
        session: &'a mut T,
    ) -> &'a mut Self::Session<T> {
        session
    }
}

/// The result of a query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Cached<I, T> {
    /// The query is in progress.
    InProgress(I),

    /// The query is done and the result is stored.
    Done(T),
}

/// Used to limit the number of queries that can be made.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, CopyGetters)]
pub(super) struct Limit<R> {
    session: R,

    /// The limit of the number of queries.
    #[get_copy = "pub"]
    limit: usize,

    /// The number of queries that have been made.
    #[get_copy = "pub"]
    count: usize,
}

impl<R> std::default::Default for Limit<R>
where
    R: std::default::Default,
{
    fn default() -> Self { Self::new(R::default()) }
}

impl<R> Limit<R> {
    /// The default limit of the number of queries.
    pub(super) const DEFAULT_LIMIT: usize = 65536;

    /// Creates a new limit.
    pub(super) fn new(session: R) -> Self {
        Self { session, limit: Self::DEFAULT_LIMIT, count: 0 }
    }

    /// Creates a new [`Limit`] with the given limit number.
    #[allow(unused)]
    pub(super) fn with_limit(session: R, limit: usize) -> Self {
        Self { session, limit, count: 0 }
    }

    /// Marks the given query as working on.
    ///
    /// # Errors
    ///
    /// Returns an error if the number of queries exceeds the limit.
    #[allow(clippy::type_complexity)]
    pub(super) fn mark_as_in_progress<T: Get, Q>(
        &mut self,
        query: Q,
        metadata: <T::Session<R> as Cache<Q>>::InProgress,
    ) -> Result<
        Option<
            Cached<
                <T::Session<R> as Cache<Q>>::InProgress,
                <T::Session<R> as Cache<Q>>::Result,
            >,
        >,
        ExceedLimitError,
    >
    where
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,

        T::Session<R>: Cache<Q>,
    {
        // limit exceeded, reset the state and return an error
        if self.count >= self.limit {
            self.count = 0;
            T::get_session_mut(&mut self.session).delete_all_work_in_progress();
            return Err(ExceedLimitError);
        }

        self.count += 1;

        Ok(T::get_session_mut(&mut self.session)
            .mark_as_in_progress(query, metadata))
    }

    /// Marks the given query as done and stores the result.
    pub(super) fn mark_as_done<T: Get, Q>(
        &mut self,
        query: Q,
        result: <T::Session<R> as Cache<Q>>::Result,
    ) where
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,

        T::Session<R>: Cache<Q>,
    {
        T::get_session_mut(&mut self.session).mark_as_done(query, result);
    }

    /// Clears the cached result of the given query.
    pub(super) fn clear_query<T: Get, Q>(
        &mut self,
        query: Q,
    ) -> Option<
        Cached<
            <T::Session<R> as Cache<Q>>::InProgress,
            <T::Session<R> as Cache<Q>>::Result,
        >,
    >
    where
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,

        T::Session<R>: Cache<Q>,
    {
        T::get_session_mut(&mut self.session).clear_query(query)
    }

    /// Returns the result of the given query.
    #[allow(unused)]
    pub(super) fn get_result<'a, T: Get, Q>(
        &'a self,
        query: Q,
    ) -> Option<
        &Cached<
            <T::Session<R> as Cache<Q>>::InProgress,
            <T::Session<R> as Cache<Q>>::Result,
        >,
    >
    where
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,

        T::Session<R>: Cache<Q> + 'a,
    {
        T::get_session(&self.session).get_result(query)
    }

    /// Clears all the queries that are in progress state.
    #[allow(unused)]
    pub fn delete_all_work_in_progress<T: Get, Q>(&mut self)
    where
        R: Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,

        T::Session<R>: Cache<Q>,
    {
        T::get_session_mut(&mut self.session).delete_all_work_in_progress();
    }
}

/// Used to *remember* the queries that have been made and their results.
pub(super) trait Cache<Query> {
    /// The result type returned by the query.
    type Result;

    /// The in-progress state metadata.
    type InProgress;

    /// Marks the given query as working on.
    fn mark_as_in_progress(
        &mut self,
        query: Query,
        metadata: Self::InProgress,
    ) -> Option<Cached<Self::InProgress, Self::Result>>;

    /// Marks the given query as done and stores the result.
    fn mark_as_done(&mut self, record: Query, result: Self::Result);

    /// Clears the cached result of the given query.
    fn clear_query(
        &mut self,
        query: Query,
    ) -> Option<Cached<Self::InProgress, Self::Result>>;

    /// Returns the result of the given query.
    fn get_result(
        &self,
        query: Query,
    ) -> Option<&Cached<Self::InProgress, Self::Result>>;

    /// Clears all the queries that are in progress state.
    fn delete_all_work_in_progress(&mut self);
}

/// The session of semantic queries.
///
/// Most of the semantic queries (i.e., equals, unification, etc) are recursive
/// in nature. This means that they can be circular. This trait is used to
/// detect the circular reasoning in the system.
///
/// Most of the time, you should use [`Default`] as the implementation of this
/// trait.
pub(super) trait Session<T: SubTerm>:
    for<'a> Cache<equality::Query<'a, T>, Result = Satisfied, InProgress = ()>
    + for<'a> Cache<
        predicate::DefiniteQuery<'a, T>,
        Result = Satisfied,
        InProgress = (),
    > + for<'a> Cache<
        unification::Query<'a, T>,
        Result = Unification<T>,
        InProgress = (),
    > + for<'a> Cache<
        predicate::TupleQuery<'a, T>,
        Result = Satisfied,
        InProgress = (),
    > + for<'a> Cache<
        predicate::OutlivesQuery<'a, T>,
        Result = Satisfied,
        InProgress = (),
    > + for<'a> Cache<
        predicate::ConstantTypeQuery<'a, T>,
        Result = Satisfied,
        InProgress = ConstantTypeQuerySource,
    > + for<'a> Cache<
        predicate::TraitQuery<'a, T::Model>,
        Result = TraitSatisfiability<T::Model>,
        InProgress = (),
    > + for<'a> Cache<simplify::Query<'a, T>, Result = T, InProgress = ()>
{
}

impl<T: Term, U> Session<T> for U where
    U: for<'a> Cache<
            equality::Query<'a, T>,
            Result = Satisfied,
            InProgress = (),
        > + for<'a> Cache<
            predicate::DefiniteQuery<'a, T>,
            Result = Satisfied,
            InProgress = (),
        > + for<'a> Cache<
            unification::Query<'a, T>,
            Result = Unification<T>,
            InProgress = (),
        > + for<'a> Cache<
            predicate::TupleQuery<'a, T>,
            Result = Satisfied,
            InProgress = (),
        > + for<'a> Cache<
            predicate::OutlivesQuery<'a, T>,
            Result = Satisfied,
            InProgress = (),
        > + for<'a> Cache<
            predicate::ConstantTypeQuery<'a, T>,
            Result = Satisfied,
            InProgress = ConstantTypeQuerySource,
        > + for<'a> Cache<
            predicate::TraitQuery<'a, T::Model>,
            Result = TraitSatisfiability<T::Model>,
            InProgress = (),
        > + for<'a> Cache<simplify::Query<'a, T>, Result = T, InProgress = ()>
{
}

/// Default and preferred implementation of [`Session`].
#[derive(Debug, Clone, Default)]
pub(super) struct Default<M: Model> {
    lifetime_equals: HashMap<(Lifetime<M>, Lifetime<M>), Cached<(), Satisfied>>,
    type_equals: HashMap<(Type<M>, Type<M>), Cached<(), Satisfied>>,
    constant_equals: HashMap<(Constant<M>, Constant<M>), Cached<(), Satisfied>>,

    lifetime_is_definite: HashMap<Lifetime<M>, Cached<(), Satisfied>>,
    type_is_definite: HashMap<Type<M>, Cached<(), Satisfied>>,
    constant_is_definite: HashMap<Constant<M>, Cached<(), Satisfied>>,

    lifetime_unifies: HashMap<
        (Lifetime<M>, Lifetime<M>),
        Cached<(), Unification<Lifetime<M>>>,
    >,
    type_unifies: HashMap<(Type<M>, Type<M>), Cached<(), Unification<Type<M>>>>,
    constant_unifies: HashMap<
        (Constant<M>, Constant<M>),
        Cached<(), Unification<Constant<M>>>,
    >,

    lifetime_is_tuple: HashMap<Lifetime<M>, Cached<(), Satisfied>>,
    type_is_tuple: HashMap<Type<M>, Cached<(), Satisfied>>,
    constant_is_tuple: HashMap<Constant<M>, Cached<(), Satisfied>>,

    lifetime_outlives:
        HashMap<(Lifetime<M>, Lifetime<M>), Cached<(), Satisfied>>,
    type_outlives: HashMap<(Type<M>, Lifetime<M>), Cached<(), Satisfied>>,
    constant_outlives:
        HashMap<(Constant<M>, Lifetime<M>), Cached<(), Satisfied>>,

    lifetime_constant_type:
        HashMap<Lifetime<M>, Cached<ConstantTypeQuerySource, Satisfied>>,
    type_constant_type:
        HashMap<Type<M>, Cached<ConstantTypeQuerySource, Satisfied>>,
    constant_constant_type:
        HashMap<Constant<M>, Cached<ConstantTypeQuerySource, Satisfied>>,

    trait_satisfiability: HashMap<
        (ID<symbol::Trait>, bool, GenericArguments<M>),
        Cached<(), TraitSatisfiability<M>>,
    >,

    lifetime_simplify: HashMap<Lifetime<M>, Cached<(), Lifetime<M>>>,
    type_simplify: HashMap<Type<M>, Cached<(), Type<M>>>,
    constant_simplify: HashMap<Constant<M>, Cached<(), Constant<M>>>,
}

macro_rules! implements_cache {
    ($query:path, $result_t:path, $in_progress_t:ty, $param:ident, $field_name:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a, M: Model> Cache<$query> for Default<M> {
            type Result = $result_t;
            type InProgress = $in_progress_t;

            fn mark_as_in_progress(
                &mut self,
                $param: $query,
                metadata: Self::InProgress,
            ) -> Option<Cached<Self::InProgress, Self::Result>> {
                match self.$field_name.entry($expr_in) {
                    Entry::Vacant(entry) => {
                        entry.insert(Cached::InProgress(metadata));
                        None
                    }
                    Entry::Occupied(entry) => Some(entry.get().clone()),
                }
            }

            fn mark_as_done(&mut self, $param: $query, result: $result_t) {
                self.$field_name
                    .get_mut($expr_out)
                    .map(|x| *x = Cached::Done(result));
            }

            fn clear_query(
                &mut self,
                $param: $query,
            ) -> Option<Cached<Self::InProgress, Self::Result>> {
                self.$field_name.remove($expr_out)
            }

            fn get_result(
                &self,
                $param: $query,
            ) -> Option<&Cached<Self::InProgress, Self::Result>> {
                self.$field_name.get($expr_out)
            }

            fn delete_all_work_in_progress(&mut self) {
                self.$field_name.retain(|_, result| {
                    if matches!(result, Cached::InProgress(_)) {
                        false
                    } else {
                        true
                    }
                });
            }
        }
    };
}

/// A simple implementation of [`Cache`] using a [`HashMap`].
#[derive(Debug, Clone, Default)]
pub struct Storage<Q, Result, InProgress> {
    field1: HashMap<Q, Cached<InProgress, Result>>,
}

impl<Q: Eq + Hash, InProgress: Clone, Result: Clone> Cache<Q>
    for Storage<Q, Result, InProgress>
{
    type Result = Result;

    type InProgress = InProgress;

    fn mark_as_in_progress(
        &mut self,
        query: Q,
        metadata: Self::InProgress,
    ) -> Option<Cached<Self::InProgress, Self::Result>> {
        match self.field1.entry(query) {
            Entry::Vacant(entry) => {
                entry.insert(Cached::InProgress(metadata));
                None
            }
            Entry::Occupied(entry) => Some(entry.get().clone()),
        }
    }

    fn mark_as_done(&mut self, record: Q, result: Self::Result) {
        self.field1.insert(record, Cached::Done(result));
    }

    fn clear_query(
        &mut self,
        query: Q,
    ) -> Option<Cached<Self::InProgress, Self::Result>> {
        self.field1.remove(&query)
    }

    fn get_result(
        &self,
        query: Q,
    ) -> Option<&Cached<Self::InProgress, Self::Result>> {
        self.field1.get(&query)
    }

    fn delete_all_work_in_progress(&mut self) {
        self.field1
            .retain(|_, result| !matches!(result, Cached::InProgress(_)));
    }
}

implements_cache!(
    equality::Query<'a, Lifetime<M>>,
    Satisfied,
    (),
    query,
    lifetime_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    equality::Query<'a, Type<M>>,
    Satisfied,
    (),
    query,
    type_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    equality::Query<'a, Constant<M>>,
    Satisfied,
    (),
    query,
    constant_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    predicate::DefiniteQuery<'a, Lifetime<M>>,
    Satisfied,
    (),
    record,
    lifetime_is_definite,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::DefiniteQuery<'a, Type<M>>,
    Satisfied,
    (),
    record,
    type_is_definite,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::DefiniteQuery<'a, Constant<M>>,
    Satisfied,
    (),
    record,
    constant_is_definite,
    record.0.clone(),
    record.0
);

implements_cache!(
    unification::Query<'a, Lifetime<M>>,
    Unification<Lifetime<M>>,
    (),
    record,
    lifetime_unifies,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification::Query<'a, Type<M>>,
    Unification<Type<M>>,
    (),
    record,
    type_unifies,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    unification::Query<'a, Constant<M>>,
    Unification<Constant<M>>,
    (),
    record,
    constant_unifies,
    (record.lhs.clone(), record.rhs.clone()),
    &(record.lhs.clone(), record.rhs.clone())
);

implements_cache!(
    predicate::TupleQuery<'a, Lifetime<M>>,
    Satisfied,
    (),
    record,
    lifetime_is_tuple,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::TupleQuery<'a, Type<M>>,
    Satisfied,
    (),
    record,
    type_is_tuple,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::TupleQuery<'a, Constant<M>>,
    Satisfied,
    (),
    record,
    constant_is_tuple,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::OutlivesQuery<'a, Lifetime<M>>,
    Satisfied,
    (),
    record,
    lifetime_outlives,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate::OutlivesQuery<'a, Type<M>>,
    Satisfied,
    (),
    record,
    type_outlives,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate::OutlivesQuery<'a, Constant<M>>,
    Satisfied,
    (),
    record,
    constant_outlives,
    (record.operand.clone(), record.bound.clone()),
    &(record.operand.clone(), record.bound.clone())
);

implements_cache!(
    predicate::ConstantTypeQuery<'a, Lifetime<M>>,
    Satisfied,
    ConstantTypeQuerySource,
    record,
    lifetime_constant_type,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::ConstantTypeQuery<'a, Type<M>>,
    Satisfied,
    ConstantTypeQuerySource,
    record,
    type_constant_type,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::ConstantTypeQuery<'a, Constant<M>>,
    Satisfied,
    ConstantTypeQuerySource,
    record,
    constant_constant_type,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::TraitQuery<'a, M>,
    TraitSatisfiability<M>,
    (),
    record,
    trait_satisfiability,
    (record.id, record.is_const, record.generic_arguments.clone()),
    &(record.id, record.is_const, record.generic_arguments.clone())
);

implements_cache!(
    simplify::Query<'a, Lifetime<M>>,
    Lifetime<M>,
    (),
    record,
    lifetime_simplify,
    record.0.clone(),
    record.0
);

implements_cache!(
    simplify::Query<'a, Type<M>>,
    Type<M>,
    (),
    record,
    type_simplify,
    record.0.clone(),
    record.0
);

implements_cache!(
    simplify::Query<'a, Constant<M>>,
    Constant<M>,
    (),
    record,
    constant_simplify,
    record.0.clone(),
    record.0
);
