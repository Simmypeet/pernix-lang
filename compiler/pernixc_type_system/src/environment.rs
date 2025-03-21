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
use pernixc_component::implied_predicates::{
    ImpliedPredicate, ImpliedPredicates,
};
use pernixc_source_file::Span;
use pernixc_table::{component::SymbolKind, GlobalID, Table};
use pernixc_term::{
    generic_arguments::GenericArguments,
    predicate::{Compatible, Outlives, Predicate},
    r#type::{TraitMember, Type},
    visitor::RecursiveIterator,
    where_clause::WhereClause,
    Kind, Model, ModelOf,
};

use crate::{
    definite::Definite,
    equality::Equality,
    normalizer::{self, Normalizer},
    unification::Unification,
    LifetimeUnifyingPredicate, OverflowError,
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

/// An extension trait for the table for interacting with the type system .
pub trait GetActivePremiseExt {
    /// Retrieves the active premise of the current site.
    ///
    /// # Parameters
    ///
    /// - `current_site`: The current site to get the active premise of.
    /// - `handler`: The handler to report the cyclic dependency error to.
    fn get_active_premise<M: Model>(
        &self,
        current_site: GlobalID,
    ) -> Premise<M>;

    /// Retrieves the active premise of the current site with the span of the
    /// predicates.
    fn get_active_premise_predicates_with_span<M: Model>(
        &self,
        current_site: GlobalID,
    ) -> HashMap<Predicate<M>, Vec<Span>>;
}

impl GetActivePremiseExt for Table {
    fn get_active_premise<M: Model>(
        &self,
        current_site: GlobalID,
    ) -> Premise<M> {
        let mut premise = Premise {
            predicates: BTreeSet::new(),
            query_site: Some(current_site),
        };

        for id in self.scope_walker(current_site) {
            let current_id = GlobalID::new(current_site.target_id, id);
            let kind = *self.get::<SymbolKind>(current_id);

            if kind.has_where_clause() {
                if let Ok(where_clause) = self.query::<WhereClause>(current_id)
                {
                    premise.predicates.extend(
                        where_clause.predicates.iter().map(|x| {
                            Predicate::from_other_model(x.predicate.clone())
                        }),
                    );
                }
            }

            if kind.has_implied_predicates() {
                if let Ok(predicates) =
                    self.query::<ImpliedPredicates>(current_id)
                {
                    premise.predicates.extend(
                        predicates.implied_predicates.iter().map(|x| match x {
                            ImpliedPredicate::LifetimeOutlives(outlives) => {
                                Predicate::LifetimeOutlives(
                                    Outlives::from_other_model(
                                        outlives.clone(),
                                    ),
                                )
                            }
                            ImpliedPredicate::TypeOutlives(outlives) => {
                                Predicate::TypeOutlives(
                                    Outlives::from_other_model(
                                        outlives.clone(),
                                    ),
                                )
                            }
                        }),
                    );
                }
            }
        }

        premise
    }

    fn get_active_premise_predicates_with_span<M: Model>(
        &self,
        current_site: GlobalID,
    ) -> HashMap<Predicate<M>, Vec<Span>> {
        let mut spans_by_predicate: HashMap<Predicate<M>, Vec<Span>> =
            HashMap::default();

        for global_id in self
            .scope_walker(current_site)
            .map(|x| GlobalID::new(current_site.target_id, x))
        {
            let symbol_kind = *self.get::<SymbolKind>(global_id);

            if symbol_kind.has_where_clause() {
                if let Ok(where_clause) = self.query::<WhereClause>(global_id) {
                    for predicate in &where_clause.predicates {
                        let Some(span) = predicate.span.clone() else {
                            continue;
                        };

                        let predicate = Predicate::from_other_model(
                            predicate.predicate.clone(),
                        );

                        spans_by_predicate
                            .entry(predicate)
                            .or_default()
                            .push(span);
                    }
                }
            }
        }

        spans_by_predicate
    }
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

impl<M: Model, N> Environment<'_, M, N> {
    /// Asserts that the call stack is empty.
    #[cfg(test)]
    pub fn assert_call_stack_empty(&self) {
        let context = self.context.borrow();

        assert!(context.call_stack.is_empty());
        assert_eq!(context.current_count, 0);
    }
}

impl<M: Model, N: std::fmt::Debug> std::fmt::Debug for Environment<'_, M, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("premise", &self.premise)
            .field("table", &self.table)
            .field("normalizer", &self.normalizer)
            .finish()
    }
}

impl<M: Model, N: Clone> Clone for Environment<'_, M, N> {
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
        other.as_any().downcast_ref::<T>() == Some(self)
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

        if last.query.downcast_ref::<Q>() != Some(query) {
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

        if last.query.downcast_ref::<Q>() != Some(query) {
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
                    .position(|x| (x.query.downcast_ref::<Q>() == Some(query)))
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

/// An enumeration of all errors encountered while creating a new environment.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error<M: Model> {
    /// The prediccates are ambiguous.
    ///
    /// The vector contains the set of predicates that are identical except for
    /// the lifetime arguments.
    AmbiguousPredicates(Vec<Predicate<M>>),

    /// The given predicate premise is definite; meaning that it's trivially
    /// known to be true/false.
    DefinintePremise(Predicate<M>),

    /// The [`Equality::lhs`] occurs in the [`Equality::rhs`].
    RecursiveTraitTypeEqualityPredicate(Compatible<TraitMember<M>, Type<M>>),

    /// Encounters the [`super::Error`] while calculating the requirements for
    /// the given [`Predicate`].
    Abrupt(Predicate<M>, super::Error),
}

fn check_definite_predicate<
    T: Clone + Into<Predicate<M>>,
    M: Model,
    N: Normalizer<M>,
>(
    environment: &mut Environment<M, N>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<(Predicate<M>, super::Error)>,
    definite_predicates: &mut Vec<Predicate<M>>,
    definite_check: impl Fn(&T, &Environment<M, N>) -> Result<bool, super::Error>,
) {
    // pick a predicate
    'outer: for predicate_i in predicates {
        // remove the predicate before checking
        if remove_on_check {
            assert!(environment
                .premise
                .to_mut()
                .predicates
                .remove(&predicate_i.clone().into()));
        }

        match definite_check(predicate_i, environment) {
            Ok(true) => {
                // add to the set
                definite_predicates.push(predicate_i.clone().into());

                if remove_on_check {
                    assert!(environment
                        .premise
                        .to_mut()
                        .predicates
                        .insert(predicate_i.clone().into()));
                }
                continue 'outer; // no more checking for this
                                 // predicate.
            }

            Ok(false) => {
                // nothing to worry about
            }

            Err(error) => {
                // add to the overflow set
                overflow_predicates.push((predicate_i.clone().into(), error));

                if remove_on_check {
                    assert!(environment
                        .premise
                        .to_mut()
                        .predicates
                        .insert(predicate_i.clone().into()));
                }
                continue 'outer; // no more checking for this
                                 // predicate.
            }
        }

        if remove_on_check {
            assert!(environment
                .premise
                .to_mut()
                .predicates
                .insert(predicate_i.clone().into()));
        }
    }
}

fn check_ambiguous_predicates<
    T: Clone + Into<Predicate<M>>,
    M: Model,
    N: Normalizer<M>,
>(
    environment: &mut Environment<M, N>,
    remove_on_check: bool,
    predicates: &[T],
    overflow_predicates: &mut Vec<(Predicate<M>, super::Error)>,
    ambiguous_predicates: &mut Vec<Vec<T>>,
    ambiguity_check: impl Fn(
        &T,
        &T,
        &Environment<M, N>,
    ) -> Result<bool, super::Error>,
) {
    // pick a predicate
    'outer: for (i, predicate_i) in predicates.iter().enumerate() {
        // remove the predicate before checking
        if remove_on_check {
            assert!(environment
                .premise
                .to_mut()
                .predicates
                .remove(&predicate_i.clone().into()));
        }

        // check in the ambiguity set
        for ambiguous_predicates_set in &mut *ambiguous_predicates {
            // pick the first element in the set
            let first = &ambiguous_predicates_set[0];

            match ambiguity_check(predicate_i, first, environment) {
                Ok(true) => {
                    // add to the set
                    ambiguous_predicates_set.push(predicate_i.clone());

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }

                Ok(false) => {
                    // nothing to worry about
                }

                Err(error) => {
                    // add to the overflow set
                    overflow_predicates
                        .push((predicate_i.clone().into(), error));

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }
            }
        }

        // create a new set
        for predicate_j in predicates.iter().skip(i + 1) {
            if remove_on_check {
                assert!(environment
                    .premise
                    .to_mut()
                    .predicates
                    .remove(&predicate_j.clone().into()));
            }

            match ambiguity_check(predicate_i, predicate_j, environment) {
                Ok(true) => {
                    // create a new set
                    ambiguous_predicates.push(vec![predicate_i.clone()]);

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_j.clone().into()));
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }

                Ok(false) => {
                    // nothing to worry about
                }

                Err(error) => {
                    // add to the overflow set
                    overflow_predicates
                        .push((predicate_i.clone().into(), error));

                    if remove_on_check {
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_j.clone().into()));
                        assert!(environment
                            .premise
                            .to_mut()
                            .predicates
                            .insert(predicate_i.clone().into()));
                    }
                    continue 'outer; // no more checking for this
                                     // predicate.
                }
            }

            if remove_on_check {
                assert!(environment
                    .premise
                    .to_mut()
                    .predicates
                    .insert(predicate_j.clone().into()));
            }
        }

        if remove_on_check {
            assert!(environment
                .premise
                .to_mut()
                .predicates
                .insert(predicate_i.clone().into()));
        }
    }
}

fn check_ambiguous_generic_arguments<M: Model>(
    lhs: &GenericArguments<M>,
    rhs: &GenericArguments<M>,
    environment: &Environment<M, impl Normalizer<M>>,
) -> Result<bool, super::Error> {
    // check if the arguments counts are the same
    if lhs.lifetimes.len() != rhs.lifetimes.len()
        || lhs.types.len() != rhs.types.len()
        || lhs.constants.len() != rhs.constants.len()
    {
        return Ok(false);
    }

    for (lhs_ty, rhs_ty) in lhs.types.iter().zip(rhs.types.iter()) {
        if environment
            .query(&Unification::new(
                lhs_ty.clone(),
                rhs_ty.clone(),
                LifetimeUnifyingPredicate,
            ))?
            .is_none()
        {
            return Ok(false);
        }
    }

    for (lhs_const, rhs_const) in lhs.constants.iter().zip(rhs.constants.iter())
    {
        if environment
            .query(&Equality::new(lhs_const.clone(), rhs_const.clone()))?
            .is_none()
        {
            return Ok(false);
        }
    }

    Ok(true)
}

impl<'a, M: Model, N: Normalizer<M>> Environment<'a, M, N> {
    /// Creates a new [`Environment`] and check for potential invalid predicates
    /// in the premise.
    #[allow(clippy::too_many_lines)]
    pub fn diagnose(
        premise: Cow<'a, Premise<M>>,
        table: &'a Table,
        normalizer: &'a N,
    ) -> (Self, Vec<Error<M>>) {
        let mut environment = Self {
            premise,
            table,
            normalizer,
            context: RefCell::new(Context::default()),
        };

        let mut ambiguous_positive_trait_predicates_set = Vec::new();
        let mut ambiguous_negative_trait_predicates_set = Vec::new();
        let mut ambiguous_positive_marker_predicates_set = Vec::new();
        let mut ambiguous_negative_marker_predicates_set = Vec::new();
        let mut ambiguous_constant_type_predicates_set = Vec::new();
        let mut ambiguous_tuple_type_predicates_set = Vec::new();
        let mut ambiguous_trait_type_equality_predicates_set = Vec::new();

        let mut recursive_trait_type_equality_predicates = Vec::new();

        let mut overflow_predicates = Vec::new();
        let mut definite_predicate = Vec::new();

        let mut all_positive_trait_predicates = Vec::new();
        let mut all_negative_trait_predicates = Vec::new();
        let mut all_positive_marker_predicates = Vec::new();
        let mut all_negative_marker_predicates = Vec::new();
        let mut all_constant_type_predicates = Vec::new();
        let mut all_tuple_type_predicates = Vec::new();
        let mut all_trait_type_equality_predicates = Vec::new();

        for predicate in &environment.premise.predicates {
            match predicate {
                Predicate::PositiveTrait(x) => {
                    all_positive_trait_predicates.push(x.clone());
                }

                Predicate::NegativeTrait(x) => {
                    all_negative_trait_predicates.push(x.clone());
                }

                Predicate::PositiveMarker(x) => {
                    all_positive_marker_predicates.push(x.clone());
                }

                Predicate::NegativeMarker(x) => {
                    all_negative_marker_predicates.push(x.clone());
                }

                Predicate::ConstantType(x) => {
                    all_constant_type_predicates.push(x.clone());
                }

                Predicate::TupleType(x) => {
                    all_tuple_type_predicates.push(x.clone());
                }

                Predicate::TraitTypeCompatible(x) => {
                    all_trait_type_equality_predicates.push(x.clone());
                }

                Predicate::LifetimeOutlives(_) | Predicate::TypeOutlives(_) => {
                }
            }
        }

        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_positive_trait_predicates,
            &mut overflow_predicates,
            &mut ambiguous_positive_trait_predicates_set,
            |lhs, rhs, environment| {
                // check if the trait is the same
                if lhs.trait_id != rhs.trait_id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_negative_trait_predicates,
            &mut overflow_predicates,
            &mut ambiguous_negative_trait_predicates_set,
            |lhs, rhs, environment| {
                // check if the trait is the same
                if lhs.trait_id != rhs.trait_id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_positive_marker_predicates,
            &mut overflow_predicates,
            &mut ambiguous_positive_marker_predicates_set,
            |lhs, rhs, environment| {
                // check if the marker is the same
                if lhs.marker_id != rhs.marker_id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_negative_marker_predicates,
            &mut overflow_predicates,
            &mut ambiguous_negative_marker_predicates_set,
            |lhs, rhs, environment| {
                // check if the marker is the same
                if lhs.marker_id != rhs.marker_id {
                    return Ok(false);
                }

                check_ambiguous_generic_arguments(
                    &lhs.generic_arguments,
                    &rhs.generic_arguments,
                    environment,
                )
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_constant_type_predicates,
            &mut overflow_predicates,
            &mut ambiguous_constant_type_predicates_set,
            |lhs, rhs, environment| {
                environment
                    .query(&Unification::new(
                        lhs.0.clone(),
                        rhs.0.clone(),
                        LifetimeUnifyingPredicate,
                    ))
                    .map(|x| x.is_some())
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            false,
            &all_tuple_type_predicates,
            &mut overflow_predicates,
            &mut ambiguous_tuple_type_predicates_set,
            |lhs, rhs, environment| {
                environment
                    .query(&Unification::new(
                        lhs.0.clone(),
                        rhs.0.clone(),
                        LifetimeUnifyingPredicate,
                    ))
                    .map(|x| x.is_some())
            },
        );
        check_ambiguous_predicates(
            &mut environment,
            true,
            &all_trait_type_equality_predicates,
            &mut overflow_predicates,
            &mut ambiguous_trait_type_equality_predicates_set,
            |lhs, rhs, environment| {
                environment
                    .query(&Unification::new(
                        Type::TraitMember(lhs.lhs.clone()),
                        Type::TraitMember(rhs.lhs.clone()),
                        LifetimeUnifyingPredicate,
                    ))
                    .map(|x| x.is_some())
            },
        );

        check_definite_predicate(
            &mut environment,
            false,
            &all_positive_trait_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .generic_arguments_definite(&predicate.generic_arguments)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_negative_trait_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .generic_arguments_definite(&predicate.generic_arguments)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_positive_marker_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .generic_arguments_definite(&predicate.generic_arguments)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_negative_marker_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .generic_arguments_definite(&predicate.generic_arguments)
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_constant_type_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .query(&Definite(predicate.0.clone()))
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            false,
            &all_tuple_type_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .query(&Definite(predicate.0.clone()))
                    .map(|x| x.is_some())
            },
        );
        check_definite_predicate(
            &mut environment,
            true,
            &all_trait_type_equality_predicates,
            &mut overflow_predicates,
            &mut definite_predicate,
            |predicate, environment| {
                environment
                    .query(&Definite(Type::TraitMember(predicate.lhs.clone())))
                    .map(|x| x.is_some())
            },
        );

        for equality in all_trait_type_equality_predicates
            .iter()
            .cloned()
            .map(Predicate::TraitTypeCompatible)
        {
            // temporarily remove the equality
            assert!(environment.premise.to_mut().predicates.remove(&equality));

            // recursively check the equality
            for (kind, _) in RecursiveIterator::new(
                &equality.as_trait_type_compatible().unwrap().rhs,
            ) {
                let Kind::Type(ty) = kind else {
                    continue;
                };

                match environment.query(&Unification::new(
                    Type::TraitMember(
                        equality
                            .as_trait_type_compatible()
                            .unwrap()
                            .lhs
                            .clone(),
                    ),
                    ty.clone(),
                    LifetimeUnifyingPredicate,
                )) {
                    Ok(Some(_)) => {
                        if !ambiguous_trait_type_equality_predicates_set
                            .iter()
                            .flatten()
                            .any(|x| {
                                x == equality
                                    .as_trait_type_compatible()
                                    .unwrap()
                            })
                        {
                            recursive_trait_type_equality_predicates.push(
                                equality
                                    .as_trait_type_compatible()
                                    .unwrap()
                                    .clone(),
                            );
                        }
                        break;
                    }

                    Err(error) => {
                        let result = (equality.clone(), error);

                        if !overflow_predicates.contains(&result) {
                            overflow_predicates.push(result);
                        }

                        break;
                    }

                    Ok(None) => {}
                }
            }

            // add the equality back
            assert!(environment.premise.to_mut().predicates.insert(equality));
        }

        overflow_predicates.sort();
        overflow_predicates.dedup();

        // remove the ambiguous and ill-formed predicates
        for predicate_to_remove in overflow_predicates
            .iter()
            .map(|x| &x.0)
            .cloned()
            .chain(
                ambiguous_positive_trait_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::PositiveTrait),
            )
            .chain(
                ambiguous_negative_trait_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::NegativeTrait),
            )
            .chain(
                ambiguous_positive_marker_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::PositiveMarker),
            )
            .chain(
                ambiguous_negative_marker_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::NegativeMarker),
            )
            .chain(
                ambiguous_constant_type_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::ConstantType),
            )
            .chain(
                ambiguous_tuple_type_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::TupleType),
            )
            .chain(
                ambiguous_trait_type_equality_predicates_set
                    .iter()
                    .flatten()
                    .cloned()
                    .map(Predicate::TraitTypeCompatible),
            )
            .chain(
                recursive_trait_type_equality_predicates
                    .iter()
                    .cloned()
                    .map(Predicate::TraitTypeCompatible),
            )
            .chain(definite_predicate.iter().cloned())
        {
            environment
                .premise
                .to_mut()
                .predicates
                .remove(&predicate_to_remove);
        }

        let ambiguous_predicates_vecs = ambiguous_positive_trait_predicates_set
            .into_iter()
            .map(|x| x.into_iter().map(Into::into).collect())
            .chain(
                ambiguous_constant_type_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            )
            .chain(
                ambiguous_tuple_type_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            )
            .chain(
                ambiguous_trait_type_equality_predicates_set
                    .into_iter()
                    .map(|x| x.into_iter().map(Into::into).collect()),
            );

        let errors = overflow_predicates
            .into_iter()
            .map(|(predicate, error)| Error::Abrupt(predicate, error))
            .chain(ambiguous_predicates_vecs.map(Error::AmbiguousPredicates))
            .chain(
                recursive_trait_type_equality_predicates
                    .into_iter()
                    .map(Error::RecursiveTraitTypeEqualityPredicate),
            )
            .chain(definite_predicate.into_iter().map(Error::DefinintePremise))
            .collect::<Vec<_>>();

        (environment, errors)
    }
}

impl<'a, M: Model, N: Normalizer<M>> Environment<'a, M, N> {
    /// Creates a new [`Environment`].
    ///
    /// The ambiguous predicates will be removed from the environment and is
    /// extracted out to the vector of [`Error`].
    pub fn new(
        premise: Cow<'a, Premise<M>>,
        table: &'a Table,
        normalizer: &'a N,
    ) -> Self {
        Self {
            premise,
            table,
            normalizer,
            context: RefCell::new(Context::default()),
        }
    }
}
