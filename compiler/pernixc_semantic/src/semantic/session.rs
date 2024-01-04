//! Contains the definition of [`Session`].

use std::collections::{btree_map::Entry, BTreeMap};

use super::{
    equality,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

/// The result of a query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Result<T> {
    /// The query is in progress.
    InProgress,

    /// The query is done and the result is stored.
    Done(T),
}

/// Used to *remember* the queries that have been made and their results.
pub trait Cache<Query> {
    /// The result type returned by the query.
    type Result;

    /// Marks the given query as working on.
    fn mark_as_in_progress(&mut self, query: Query) -> Option<Result<Self::Result>>;

    /// Marks the given query as done and stores the result.
    fn mark_as_done(&mut self, record: Query, result: Self::Result);

    /// Clears the cached result of the given query.
    fn clear_query(&mut self, query: Query) -> Option<Result<Self::Result>>;

    /// Returns the result of the given query.
    fn get_result(&self, query: Query) -> Option<&Result<Self::Result>>;
}

/// The session of semantic queries.
///
/// Most of the semantic queries (i.e., equals, unification, etc) are recursive in nature. This
/// means that they can be circular. This trait is used to detect the circular reasoning in the
/// system.
///
/// Most of the time, you should use [`Default`] as the implementation of this trait.
pub trait Session<T>: for<'a> Cache<equality::Query<'a, T>, Result = bool> {}

impl<T, U> Session<T> for U where U: for<'a> Cache<equality::Query<'a, T>, Result = bool> {}

/// Default and preferred implementation of [`Session`].
#[derive(Debug, Clone, Default)]
pub struct Default {
    type_equals: BTreeMap<(Type, Type), Result<bool>>,
    constant_equals: BTreeMap<(Constant, Constant), Result<bool>>,
    lifetime_equals: BTreeMap<(Lifetime, Lifetime), Result<bool>>,
}

macro_rules! implements_cache {
    ($query:path, $result_t:path, $param:ident, $field_name:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a> Cache<$query> for Default {
            type Result = $result_t;

            fn mark_as_in_progress(&mut self, $param: $query) -> Option<Result<Self::Result>> {
                match self.$field_name.entry($expr_in) {
                    Entry::Vacant(entry) => {
                        entry.insert(Result::InProgress);
                        None
                    }
                    Entry::Occupied(entry) => Some(entry.get().clone()),
                }
            }

            fn mark_as_done(&mut self, $param: $query, result: $result_t) {
                self.$field_name
                    .get_mut($expr_out)
                    .map(|x| *x = Result::Done(result));
            }

            fn clear_query(&mut self, $param: $query) -> Option<Result<Self::Result>> {
                self.$field_name.remove($expr_out)
            }

            fn get_result(&self, $param: $query) -> Option<&Result<Self::Result>> {
                self.$field_name.get($expr_out)
            }
        }
    };
}

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
    equality::Query<'a, Lifetime>,
    bool,
    query,
    lifetime_equals,
    (*query.lhs, *query.rhs),
    &(*query.lhs, *query.rhs)
);
