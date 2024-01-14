//! Contains the definition of [`Session`].

use std::collections::{hash_map::Entry, HashMap};

use super::{
    equality, predicate,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

/// The result of a query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Cached<T> {
    /// The query is in progress.
    InProgress,

    /// The query is done and the result is stored.
    Done(T),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error)]
#[error("exceeded the limit of the number of queries")]
#[allow(missing_docs)]
pub struct ExceedLimitError;

/// Used to limit the number of queries that can be made.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Limit<'a, R> {
    session: &'a mut R,
    limit: usize,
    count: usize,
}

impl<'a, R> Limit<'a, R> {
    /// The default limit of the number of queries.
    pub const DEFAULT_LIMIT: usize = 65536;

    /// Creates a new limit.
    pub fn new(session: &'a mut R) -> Self {
        Self {
            session,
            limit: Self::DEFAULT_LIMIT,
            count: 0,
        }
    }

    /// Creates a new [`Limit`] with the given limit number.
    pub fn with_limit(session: &'a mut R, limit: usize) -> Self {
        Self {
            session,
            limit,
            count: 0,
        }
    }

    /// Marks the given query as working on.
    ///
    /// # Errors
    ///
    /// Returns an error if the number of queries exceeds the limit.
    pub fn mark_as_in_progress<Q>(
        &mut self,
        query: Q,
    ) -> Result<Option<Cached<R::Result>>, ExceedLimitError>
    where
        R: Cache<Q>,
    {
        // limit exceeded, reset the state and return an error
        if self.count >= self.limit {
            self.count = 0;
            self.session.delete_all_work_in_progress();
            return Err(ExceedLimitError);
        }

        self.count += 1;

        Ok(self.session.mark_as_in_progress(query))
    }

    /// Marks the given query as done and stores the result.
    pub fn mark_as_done<Q>(&mut self, query: Q, result: R::Result)
    where
        R: Cache<Q>,
    {
        self.session.mark_as_done(query, result);
    }

    /// Clears the cached result of the given query.
    pub fn clear_query<Q>(&mut self, query: Q) -> Option<Cached<R::Result>>
    where
        R: Cache<Q>,
    {
        self.session.clear_query(query)
    }

    /// Returns the result of the given query.
    pub fn get_result<Q>(&self, query: Q) -> Option<&Cached<R::Result>>
    where
        R: Cache<Q>,
    {
        self.session.get_result(query)
    }

    /// Clears all the queries that are in progress state.
    pub fn delete_all_work_in_progress<Q>(&mut self)
    where
        R: Cache<Q>,
    {
        self.session.delete_all_work_in_progress();
    }
}

/// Used to *remember* the queries that have been made and their results.
pub trait Cache<Query> {
    /// The result type returned by the query.
    type Result;

    /// Marks the given query as working on.
    fn mark_as_in_progress(&mut self, query: Query) -> Option<Cached<Self::Result>>;

    /// Marks the given query as done and stores the result.
    fn mark_as_done(&mut self, record: Query, result: Self::Result);

    /// Clears the cached result of the given query.
    fn clear_query(&mut self, query: Query) -> Option<Cached<Self::Result>>;

    /// Returns the result of the given query.
    fn get_result(&self, query: Query) -> Option<&Cached<Self::Result>>;

    /// Clears all the queries that are in progress state.
    fn delete_all_work_in_progress(&mut self);
}

/// The session of semantic queries.
///
/// Most of the semantic queries (i.e., equals, unification, etc) are recursive in nature. This
/// means that they can be circular. This trait is used to detect the circular reasoning in the
/// system.
///
/// Most of the time, you should use [`Default`] as the implementation of this trait.
pub trait Session<T>:
    for<'a> Cache<equality::Query<'a, T>, Result = bool>
    + for<'a> Cache<predicate::DefiniteRecord<'a, T>, Result = bool>
{
}

impl<T, U> Session<T> for U where
    U: for<'a> Cache<equality::Query<'a, T>, Result = bool>
        + for<'a> Cache<predicate::DefiniteRecord<'a, T>, Result = bool>
{
}

/// Default and preferred implementation of [`Session`].
#[derive(Debug, Clone, Default)]
pub struct Default {
    type_equals: HashMap<(Type, Type), Cached<bool>>,
    constant_equals: HashMap<(Constant, Constant), Cached<bool>>,
    lifetime_equals: HashMap<(Lifetime, Lifetime), Cached<bool>>,

    lifetime_definites: HashMap<Lifetime, Cached<bool>>,
    type_definites: HashMap<Type, Cached<bool>>,
    constant_definites: HashMap<Constant, Cached<bool>>,
}

macro_rules! implements_cache {
    ($query:path, $result_t:path, $param:ident, $field_name:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a> Cache<$query> for Default {
            type Result = $result_t;

            fn mark_as_in_progress(&mut self, $param: $query) -> Option<Cached<Self::Result>> {
                match self.$field_name.entry($expr_in) {
                    Entry::Vacant(entry) => {
                        entry.insert(Cached::InProgress);
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

            fn clear_query(&mut self, $param: $query) -> Option<Cached<Self::Result>> {
                self.$field_name.remove($expr_out)
            }

            fn get_result(&self, $param: $query) -> Option<&Cached<Self::Result>> {
                self.$field_name.get($expr_out)
            }

            fn delete_all_work_in_progress(&mut self) {
                self.$field_name.retain(|_, result| {
                    if matches!(result, Cached::InProgress) {
                        false
                    } else {
                        true
                    }
                });
            }
        }
    };
}

implements_cache!(
    equality::Query<'a, Lifetime>,
    bool,
    query,
    lifetime_equals,
    (*query.lhs, *query.rhs),
    &(*query.lhs, *query.rhs)
);

implements_cache!(
    equality::Query<'a, Type>,
    bool,
    query,
    type_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    equality::Query<'a, Constant>,
    bool,
    query,
    constant_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    predicate::DefiniteRecord<'a, Lifetime>,
    bool,
    record,
    lifetime_definites,
    *record.0,
    record.0
);

implements_cache!(
    predicate::DefiniteRecord<'a, Type>,
    bool,
    record,
    type_definites,
    record.0.clone(),
    record.0
);

implements_cache!(
    predicate::DefiniteRecord<'a, Constant>,
    bool,
    record,
    constant_definites,
    record.0.clone(),
    record.0
);
