//! Contains the definition of [`Session`].

use std::collections::HashSet;

use super::{
    equality,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type},
};

/// Used to *remember* the queries that have been made and their results.
pub trait Cache<Query> {
    /// Marks the given query as working on.
    ///
    /// Returns `true` if the query hasn't been made before. Returns `false` if the query has been
    /// made before.
    fn mark_as_working_on(&mut self, query: Query) -> bool;

    /// Marks the given query as done.
    fn mark_as_done(&mut self, record: Query);
}

/// The session of semantic queries.  
///
/// Most of the semantic queries (i.e., equals, unification, etc) are recursive in nature. This
/// means that they can be circular. This trait is used to detect the circular reasoning in the
/// system.
///
/// Most of the time, you should use [`Default`] as the implementation of this trait.
pub trait Session<T>: for<'a> Cache<equality::Query<'a, T>> {}

impl<T, U> Session<T> for U where U: for<'a> Cache<equality::Query<'a, T>> {}

/// Default and preferred implementation of [`Session`].
#[derive(Debug, Clone, Default)]
pub struct Default {
    type_equals: HashSet<(Type, Type)>,
    constant_equals: HashSet<(Constant, Constant)>,
    lifetime_equals: HashSet<(Lifetime, Lifetime)>,
}

macro_rules! implements_cache {
    ($query:path, $param:ident, $field_name:ident, $expr_in:expr, $expr_out:expr) => {
        impl<'a> Cache<$query> for Default {
            fn mark_as_working_on(&mut self, $param: $query) -> bool {
                self.$field_name.insert($expr_in)
            }

            fn mark_as_done(&mut self, $param: $query) { self.$field_name.remove(&$expr_out); }
        }
    };
}

implements_cache!(
    equality::Query<'a, Type>,
    query,
    type_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    equality::Query<'a, Constant>,
    query,
    constant_equals,
    (query.lhs.clone(), query.rhs.clone()),
    &(query.lhs.clone(), query.rhs.clone())
);

implements_cache!(
    equality::Query<'a, Lifetime>,
    query,
    lifetime_equals,
    (*query.lhs, *query.rhs),
    &(*query.lhs, *query.rhs)
);
