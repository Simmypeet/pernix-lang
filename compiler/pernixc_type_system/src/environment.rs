//! Contains the definition of [`Environment`].

use std::{
    any::Any,
    borrow::Cow,
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::Entry, BTreeSet, HashMap},
    hash::{Hash, Hasher},
    sync::Arc,
};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_table::{GlobalID, Table};
use pernixc_term::{predicate::Predicate, Model};

use crate::{
    normalizer::{self, Normalizer},
    OverflowError,
};

/// Contains the premise of the semantic logic.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Premise<M: Model> {
    /// List of predicates that will be considered as facts.
    pub predicates: BTreeSet<Predicate<M>>,

    /// An optional [`GlobalID`] specifying the site where the queries will be
    /// taking place in.
    ///
    /// This can influence the result of resoliving the trait/marker
    /// implementations.
    pub query_site: Option<GlobalID>,
}

/// A structure that contains the environment of the semantic logic.
#[derive(Getters, CopyGetters)]
pub struct Environment<'a, M: Model, N> {
    /// The premise of the semantic logic.
    #[get = "pub"]
    premise: Cow<'a, Premise<M>>,

    /// The table that contains the information of symbols.
    #[get_copy = "pub"]
    table: &'a Table,

    /// The normalizer used to normalize the inference variables.
    #[get_copy = "pub"]
    normalizer: &'a N,

    context: RefCell<Context>,
}

impl<'a, M: Model> Environment<'a, M, normalizer::NoOp> {
    /// Creates a new environment with the given table, default premise, and
    /// default normalizer.
    pub fn with_default(table: &'a Table) -> Self {
        Self {
            premise: Cow::Owned(Premise::default()),
            table,
            normalizer: normalizer::NO_OP,
            context: RefCell::new(Context::default()),
        }
    }
}

impl<'a, M: Model, N> Environment<'a, M, N> {
    #[cfg(test)]
    #[must_use]
    pub fn new(
        premise: &'a Premise<M>,
        table: &'a Table,
        normalizer: &'a N,
    ) -> Self {
        Self {
            premise: Cow::Borrowed(premise),
            table,
            normalizer,
            context: RefCell::new(Context::default()),
        }
    }

    #[cfg(test)]
    pub fn assert_call_stack_empty(&self) {
        let context = self.context.borrow();

        assert!(context.call_stack.is_empty());
        assert_eq!(context.current_count, 0);
    }
}

impl<'a, M: Model, N: std::fmt::Debug> std::fmt::Debug
    for Environment<'a, M, N>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("premise", &self.premise)
            .field("table", &self.table)
            .field("normalizer", &self.normalizer)
            .finish()
    }
}

impl<'a, M: Model, N: Clone> Clone for Environment<'a, M, N> {
    fn clone(&self) -> Self {
        Self {
            premise: self.premise.clone(),
            table: self.table,
            normalizer: self.normalizer,
            context: self.context.clone(),
        }
    }
}

/// Type-erased `Arc<dyn Any + Send + Sync>`.
pub type DynArc = Arc<dyn Any + Send + Sync>;

/// A trait for identifying type's equality dynamically.
#[allow(missing_docs)]
pub trait DynIdent: Any + Send + Sync + 'static {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn dyn_eq(&self, other: &dyn DynIdent) -> bool;
    fn dyn_hash(&self, state: &mut dyn Hasher);
    fn dyn_ord(&self, other: &dyn DynIdent) -> Ordering;
}

impl<T: Any + Send + Sync + Eq + Hash + Ord> DynIdent for T {
    fn as_any(&self) -> &dyn Any { self }

    fn as_any_mut(&mut self) -> &mut dyn Any { self }

    fn dyn_eq(&self, other: &dyn DynIdent) -> bool {
        other.as_any().downcast_ref::<T>().map_or(false, |other| self == other)
    }

    fn dyn_hash(&self, mut state: &mut dyn Hasher) { self.hash(&mut state); }

    fn dyn_ord(&self, other: &dyn DynIdent) -> Ordering {
        other
            .as_any()
            .downcast_ref::<T>()
            .map_or(Ordering::Less, |other| self.cmp(other))
    }
}

impl PartialEq for dyn DynIdent {
    fn eq(&self, other: &Self) -> bool { self.dyn_eq(other) }
}

impl Eq for dyn DynIdent {}

impl Hash for dyn DynIdent {
    fn hash<H: Hasher>(&self, state: &mut H) { self.dyn_hash(state); }
}

impl PartialOrd for dyn DynIdent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for dyn DynIdent {
    fn cmp(&self, other: &Self) -> Ordering { self.dyn_ord(other) }
}

/// The trait implemented by all the query types.
pub trait Query: Clone + Eq + Ord + Hash + DynIdent {
    /// The model in which the query is computed.
    type Model: Model;

    /// The optional parameter provided to the query
    type Parameter: Default;

    /// The additional in-progress state of the query.
    type InProgress: Clone + DynIdent + Default;

    /// The result of the query.
    type Result: Any + Send + Sync;

    /// The error type of the query.
    type Error: From<OverflowError>;

    /// The function that computes the query.
    #[allow(clippy::missing_errors_doc)]
    fn query(
        &self,
        environment: &Environment<Self::Model, impl Normalizer<Self::Model>>,
        parameter: Self::Parameter,
        in_progress: Self::InProgress,
    ) -> Result<Option<Arc<Self::Result>>, Self::Error>;

    /// The result to return of the query when the query is cyclic.
    #[allow(clippy::missing_errors_doc)]
    fn on_cyclic(
        &self,
        _: Self::Parameter,
        _: Self::InProgress,
        _: Self::InProgress,
        _: &[Call<DynArc, DynArc>],
    ) -> Result<Option<Arc<Self::Result>>, Self::Error> {
        Ok(None) /* the default implementation is to make the query fails */
    }
}

/// The result of a query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
pub enum Cached<I, T> {
    /// The query is in progress.
    InProgress(I),

    /// The query is done and the result is stored.
    Done(T),
}

/// A struct storing the call to compute a query and the in progress state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Call<Q, I> {
    pub query: Q,
    pub in_progress: I,
}

/// The context used to manage the queries.
#[derive(Clone)]
pub struct Context {
    map: HashMap<Arc<dyn DynIdent>, Cached<DynArc, DynArc>>,

    call_stack: Vec<Call<DynArc, DynArc>>,

    limit: usize,
    current_count: usize,
}

impl std::fmt::Debug for Context {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Context")
            .field("limit", &self.limit)
            .field("current_count", &self.current_count)
            .finish_non_exhaustive()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self {
            map: HashMap::new(),
            call_stack: Vec::new(),
            limit: 69_420, // hehe
            current_count: 0,
        }
    }
}

impl Context {
    /// Specifies that the given query is being computed.
    ///
    /// # Errors
    ///
    /// Returns [`OverflowError`] if the limit of the number of queries is
    /// reached. All the in-progress queries are removed from the state and
    /// the query counter is reset.
    ///
    /// # Returns
    ///
    /// Returns `None` if this query hasn't been stored before, otherwise
    /// returns the [`Cached`] value of the query.
    #[allow(clippy::type_complexity)]
    pub fn mark_as_in_progress<Q: Query>(
        &mut self,
        query: Q,
        in_progress: Q::InProgress,
    ) -> Result<Option<Cached<Arc<Q::InProgress>, Arc<Q::Result>>>, OverflowError>
    {
        if self.current_count >= self.limit {
            return Err(OverflowError);
        }

        let query_rc = Arc::new(query);
        let in_progress_rc = Arc::new(in_progress);

        match self.map.entry(query_rc.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(Cached::InProgress(in_progress_rc.clone()));

                self.call_stack.push(Call {
                    query: query_rc,
                    in_progress: in_progress_rc,
                });
                self.current_count += 1;

                Ok(None)
            }

            Entry::Occupied(entry) => Ok(Some(match entry.get().clone() {
                Cached::InProgress(in_progress_rc) => Cached::InProgress(
                    in_progress_rc.downcast::<Q::InProgress>().unwrap(),
                ),
                Cached::Done(result_rc) => {
                    Cached::Done(result_rc.downcast::<Q::Result>().unwrap())
                }
            })),
        }
    }

    /// Specifies the given query that it's done and stores the result.
    ///
    /// The query must be the last query in the call stack.
    ///
    /// # Returns
    ///
    /// Returns `false` if the `query` is not the last query in the call stack.
    #[must_use]
    pub fn mark_as_done<Q: Query>(
        &mut self,
        query: &Q,
        result: Arc<Q::Result>,
    ) -> bool {
        let Some(last) = self.call_stack.last() else {
            return false;
        };

        if last.query.downcast_ref::<Q>().map_or(true, |x| x != query) {
            return false;
        }

        if let Some(x) = self.map.get_mut(query as &dyn DynIdent) {
            *x = Cached::Done(result);
        } else {
            return false;
        }

        self.call_stack.pop();

        if self.call_stack.is_empty() {
            self.current_count = 0;
        }

        true
    }

    /// Clears the state of the query from the context
    ///
    /// The query must be the last query in the call stack.
    ///
    /// # Returns
    ///
    /// Returns `false` if the `query` is not the last query in the call stack.
    #[allow(clippy::type_complexity)]
    pub fn clear_query<Q: Query>(
        &mut self,
        query: &Q,
    ) -> Option<Cached<Arc<Q::InProgress>, Arc<Q::Result>>> {
        let last = self.call_stack.last()?;

        if last.query.downcast_ref::<Q>().map_or(true, |x| x != query) {
            return None;
        }

        let result = self.map.remove(query as &dyn DynIdent)?;

        self.call_stack.pop();

        if self.call_stack.is_empty() {
            self.current_count = 0;
        }

        Some(match result {
            Cached::InProgress(rc) => {
                Cached::InProgress(rc.downcast::<Q::InProgress>().unwrap())
            }
            Cached::Done(rc) => {
                Cached::Done(rc.downcast::<Q::Result>().unwrap())
            }
        })
    }
}

impl<M: Model, N: Normalizer<M>> Environment<'_, M, N> {
    /// Performs the query.
    ///
    /// # Errors
    ///
    /// Returns the error of the query.
    pub fn query<Q: Query<Model = M>>(
        &self,
        query: &Q,
    ) -> Result<Option<Arc<Q::Result>>, Q::Error> {
        self.query_with(
            query,
            Q::Parameter::default(),
            Q::InProgress::default(),
        )
    }

    /// Performs the query with the given parameter and in-progress state.
    ///
    /// # Errors
    ///
    /// Returns the error of the query.
    pub fn query_with<Q: Query<Model = M>>(
        &self,
        query: &Q,
        parameter: Q::Parameter,
        in_progress: Q::InProgress,
    ) -> Result<Option<Arc<Q::Result>>, Q::Error> {
        let in_progress_result = self
            .context
            .borrow_mut()
            .mark_as_in_progress(query.clone(), in_progress.clone())?;

        match in_progress_result {
            Some(Cached::Done(result)) => return Ok(Some(result)),
            Some(Cached::InProgress(new_in_progress)) => {
                let context = self.context.borrow();

                let position = context
                    .call_stack
                    .iter()
                    .position(|x| {
                        x.query
                            .downcast_ref::<Q>()
                            .map_or(false, |x| x == query)
                    })
                    .expect("should exist");

                // circular dependency
                let result = query.on_cyclic(
                    parameter,
                    in_progress,
                    (*new_in_progress).clone(),
                    &context.call_stack[position..],
                )?;

                return Ok(result);
            }
            None => { /*no circular dependency, continue...*/ }
        }

        match query.query(self, parameter, in_progress) {
            Ok(Some(result)) => {
                // remember the result
                assert!(self
                    .context
                    .borrow_mut()
                    .mark_as_done(query, result.clone()));

                Ok(Some(result))
            }

            result @ (Ok(None) | Err(_)) => {
                // reset the query
                assert!(self.context.borrow_mut().clear_query(query).is_some());

                result
            }
        }
    }
}
