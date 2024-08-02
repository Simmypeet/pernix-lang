//! Contains the definition of [`Query`] and [`Context`].

use std::collections::{btree_map::Entry, BTreeMap};

use enum_as_inner::EnumAsInner;
use getset::Getters;

use super::{
    definite::Definite,
    equality::Equality,
    model::Model,
    predicate::{self, ConstantTypeQuerySource, TraitSatisfied},
    sub_term::SubTerm,
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    type_check::TypeCheck,
    unification::{Unification, Unifier},
    OverflowError, Satisfied, Succeeded,
};

/// The result of a query.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

impl<Q, I> Call<Q, I> {
    /// Creates a new call.
    #[must_use]
    pub fn new(query: Q, in_progress: I) -> Self { Self { query, in_progress } }
}

/// An enumeration of all kinds of queries in the type system.
#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, EnumAsInner)]
#[allow(missing_docs)]
pub enum QueryCall<M: Model> {
    LifetimeEquality(Call<Equality<Lifetime<M>>, ()>),
    TypeEquality(Call<Equality<Type<M>>, ()>),
    ConstantEquality(Call<Equality<Constant<M>>, ()>),

    LifetimeDefinite(Call<Definite<Lifetime<M>>, ()>),
    TypeDefinite(Call<Definite<Type<M>>, ()>),
    ConstantDefinite(Call<Definite<Constant<M>>, ()>),

    LifetimeUnification(Call<Unification<Lifetime<M>>, ()>),
    TypeUnification(Call<Unification<Type<M>>, ()>),
    ConstantUnification(Call<Unification<Constant<M>>, ()>),

    LifetimeTuple(Call<predicate::Tuple<Lifetime<M>>, ()>),
    TypeTuple(Call<predicate::Tuple<Type<M>>, ()>),
    ConstantTuple(Call<predicate::Tuple<Constant<M>>, ()>),

    LifetimeOutlives(Call<predicate::Outlives<Lifetime<M>>, ()>),
    TypeOutlives(Call<predicate::Outlives<Type<M>>, ()>),
    ConstantOutlives(Call<predicate::Outlives<Constant<M>>, ()>),

    ConstantType(Call<predicate::ConstantType<M>, ConstantTypeQuerySource>),

    #[from]
    TraitSatisfiability(Call<predicate::Trait<M>, ()>),

    #[from]
    TypeCheck(Call<TypeCheck<M>, ()>),
}

/// Used for storing the state of all kinds of queries in the type system.
#[derive(Debug, Clone, Getters)]
pub struct Context<M: Model> {
    lifetime_equality:
        BTreeMap<Equality<Lifetime<M>>, Cached<(), Succeeded<Satisfied, M>>>,
    type_equality:
        BTreeMap<Equality<Type<M>>, Cached<(), Succeeded<Satisfied, M>>>,
    constant_equality:
        BTreeMap<Equality<Constant<M>>, Cached<(), Succeeded<Satisfied, M>>>,

    lifetime_definite:
        BTreeMap<Definite<Lifetime<M>>, Cached<(), Succeeded<Satisfied, M>>>,
    type_definite:
        BTreeMap<Definite<Type<M>>, Cached<(), Succeeded<Satisfied, M>>>,
    constant_definite:
        BTreeMap<Definite<Constant<M>>, Cached<(), Succeeded<Satisfied, M>>>,

    lifetime_unification: BTreeMap<
        Unification<Lifetime<M>>,
        Cached<(), Succeeded<Unifier<Lifetime<M>>, M>>,
    >,
    type_unification: BTreeMap<
        Unification<Type<M>>,
        Cached<(), Succeeded<Unifier<Type<M>>, M>>,
    >,
    constant_unification: BTreeMap<
        Unification<Constant<M>>,
        Cached<(), Succeeded<Unifier<Constant<M>>, M>>,
    >,

    lifetime_tuple: BTreeMap<
        predicate::Tuple<Lifetime<M>>,
        Cached<(), Succeeded<Satisfied, M>>,
    >,
    type_tuple: BTreeMap<
        predicate::Tuple<Type<M>>,
        Cached<(), Succeeded<Satisfied, M>>,
    >,
    constant_tuple: BTreeMap<
        predicate::Tuple<Constant<M>>,
        Cached<(), Succeeded<Satisfied, M>>,
    >,

    lifetime_outlives:
        BTreeMap<predicate::Outlives<Lifetime<M>>, Cached<(), Satisfied>>,
    type_outlives:
        BTreeMap<predicate::Outlives<Type<M>>, Cached<(), Satisfied>>,
    constant_outlives:
        BTreeMap<predicate::Outlives<Constant<M>>, Cached<(), Satisfied>>,

    constant_type: BTreeMap<
        predicate::ConstantType<M>,
        Cached<ConstantTypeQuerySource, Succeeded<Satisfied, M>>,
    >,

    trait_satisfiability: BTreeMap<
        predicate::Trait<M>,
        Cached<(), Succeeded<TraitSatisfied<M>, M>>,
    >,

    type_check: BTreeMap<TypeCheck<M>, Cached<(), Succeeded<Satisfied, M>>>,

    /// The call stack of the queries.
    #[get = "pub"]
    call_stack: Vec<QueryCall<M>>,

    limit: usize,
    current_count: usize,
}

impl<M: Model> Default for Context<M> {
    fn default() -> Self {
        Self {
            lifetime_equality: BTreeMap::default(),
            type_equality: BTreeMap::default(),
            constant_equality: BTreeMap::default(),
            lifetime_definite: BTreeMap::default(),
            type_definite: BTreeMap::default(),
            constant_definite: BTreeMap::default(),
            lifetime_unification: BTreeMap::default(),
            type_unification: BTreeMap::default(),
            constant_unification: BTreeMap::default(),
            lifetime_tuple: BTreeMap::default(),
            type_tuple: BTreeMap::default(),
            constant_tuple: BTreeMap::default(),
            lifetime_outlives: BTreeMap::default(),
            type_outlives: BTreeMap::default(),
            constant_outlives: BTreeMap::default(),
            constant_type: BTreeMap::default(),
            trait_satisfiability: BTreeMap::default(),
            type_check: BTreeMap::default(),

            call_stack: Vec::new(),

            limit: 65_536,
            current_count: 0,
        }
    }
}

impl<M: Model> Context<M> {
    /// Creates a new context.
    #[must_use]
    pub fn new() -> Self { Self::default() }

    /// Specifies the given query that it's being computed.
    ///
    /// # Errors
    ///
    /// Returns [`OverflowError`] if the number of queries exceeds the limit.
    ///
    /// # Returns
    ///
    /// Returns `None` if this query hasn't been stored before, otherwise
    /// returns the [`Cached`] value of the query.
    #[allow(private_bounds, private_interfaces)]
    pub fn mark_as_in_progress<Q: Sealed<Model = M>>(
        &mut self,
        query: Q,
        in_progress: Q::InProgress,
    ) -> Result<Option<Cached<Q::InProgress, Q::Result>>, OverflowError> {
        if self.current_count >= self.limit {
            dbg!(&self.call_stack);
            return Err(OverflowError);
        }
        self.current_count += 1;

        match Q::get_map_mut(self).entry(query.clone()) {
            Entry::Vacant(entry) => {
                entry.insert(Cached::InProgress(in_progress.clone()));
                self.call_stack.push(Q::into_query_call(
                    query.clone(),
                    in_progress.clone(),
                ));
                Ok(None)
            }
            Entry::Occupied(entry) => Ok(Some(entry.get().clone())),
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
    #[allow(private_bounds, private_interfaces)]
    pub fn mark_as_done<Q: Sealed<Model = M>>(
        &mut self,
        query: &Q,
        result: Q::Result,
    ) -> bool {
        let Some(last) = self.call_stack.last() else {
            return false;
        };

        if Q::from_call(last).map(|x| &x.query) != Some(query) {
            return false;
        }

        Q::get_map_mut(self).get_mut(&query).map(|x| *x = Cached::Done(result));
        self.call_stack.pop();

        true
    }

    /// Clears the state of the query from the context
    ///  
    /// The query must be the last query in the call stack.
    ///
    /// # Returns
    ///
    /// Returns `false` if the `query` is not the last query in the call stack.
    #[allow(private_bounds, private_interfaces)]
    pub fn clear_query<Q: Sealed<Model = M>>(
        &mut self,
        query: &Q,
    ) -> Option<Cached<Q::InProgress, Q::Result>> {
        let Some(last) = self.call_stack.last() else {
            return None;
        };

        if Q::from_call(last).map(|x| &x.query) != Some(query) {
            return None;
        }

        let result = Q::get_map_mut(self).remove(query)?;
        self.call_stack.pop();

        Some(result)
    }
}

/// The trait implemented by all the query types.
///
/// It specifies the informations required to compute the query NOT the
/// implementation logic.
pub trait Query {
    /// The model in which the query is computed.
    type Model: Model;

    /// The additional in-progress state of the query.
    type InProgress: Clone + Default;

    /// The result of the query.
    type Result: Clone;
}

pub(super) trait Sealed: Clone + Ord + Query {
    /// Gets the map of the query from the context.
    #[allow(dead_code)]
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>>;

    /// Gets the mutable map of the query from the context.
    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>>;

    /// Converts the [`QueryCall`] to the [`Call`] type of this query.
    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>>;

    /// Converts this query and its in progress state to a [`QueryCall`].
    fn into_query_call(
        query: Self,
        in_progress: Self::InProgress,
    ) -> QueryCall<Self::Model>;
}

/// A trait implemented by all kinds of terms which is used for retrieving the
/// map from the [`Context`] based on the type of the term.
pub(super) trait Element: SubTerm {
    fn get_equality_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Equality<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>;
    fn get_equality_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Equality<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    >;
    fn from_call_to_equality(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Equality<Self>, ()>>;
    fn equality_into_call(equality: Equality<Self>) -> QueryCall<Self::Model>;

    fn get_definite_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Definite<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>;
    fn get_definite_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Definite<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    >;
    fn from_call_to_definite(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Definite<Self>, ()>>;
    fn definite_into_call(definite: Definite<Self>) -> QueryCall<Self::Model>;

    fn get_unification_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    >;
    fn get_unification_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    >;
    fn from_call_to_unification(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Unification<Self>, ()>>;
    fn unification_into_call(
        unification: Unification<Self>,
    ) -> QueryCall<Self::Model>;

    fn get_tuple_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    >;
    fn get_tuple_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    >;
    fn from_call_to_tuple(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Tuple<Self>, ()>>;
    fn tuple_into_call(tuple: predicate::Tuple<Self>)
        -> QueryCall<Self::Model>;

    fn get_outlives_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>>;
    fn get_outlives_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>>;
    fn from_call_to_outlives(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Outlives<Self>, ()>>;
    fn outlives_into_call(
        outlives: predicate::Outlives<Self>,
    ) -> QueryCall<Self::Model>;
}

impl<M: Model> Element for Lifetime<M> {
    fn get_equality_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Equality<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.lifetime_equality
    }

    fn get_equality_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Equality<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.lifetime_equality
    }

    fn from_call_to_equality(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Equality<Self>, ()>> {
        call.as_lifetime_equality()
    }

    fn equality_into_call(equality: Equality<Self>) -> QueryCall<Self::Model> {
        QueryCall::LifetimeEquality(Call::new(equality, ()))
    }

    fn get_definite_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Definite<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.lifetime_definite
    }

    fn get_definite_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Definite<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.lifetime_definite
    }

    fn from_call_to_definite(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Definite<Self>, ()>> {
        call.as_lifetime_definite()
    }

    fn definite_into_call(definite: Definite<Self>) -> QueryCall<Self::Model> {
        QueryCall::LifetimeDefinite(Call::new(definite, ()))
    }

    fn get_unification_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &context.lifetime_unification
    }

    fn get_unification_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &mut context.lifetime_unification
    }

    fn from_call_to_unification(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Unification<Self>, ()>> {
        call.as_lifetime_unification()
    }

    fn unification_into_call(
        unification: Unification<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::LifetimeUnification(Call::new(unification, ()))
    }

    fn get_tuple_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &context.lifetime_tuple
    }

    fn get_tuple_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.lifetime_tuple
    }

    fn from_call_to_tuple(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Tuple<Self>, ()>> {
        call.as_lifetime_tuple()
    }

    fn tuple_into_call(
        tuple: predicate::Tuple<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::LifetimeTuple(Call::new(tuple, ()))
    }

    fn get_outlives_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &context.lifetime_outlives
    }

    fn get_outlives_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &mut context.lifetime_outlives
    }

    fn from_call_to_outlives(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Outlives<Self>, ()>> {
        call.as_lifetime_outlives()
    }

    fn outlives_into_call(
        outlives: predicate::Outlives<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::LifetimeOutlives(Call::new(outlives, ()))
    }
}

impl<M: Model> Element for Type<M> {
    fn get_equality_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Equality<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.type_equality
    }

    fn get_equality_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Equality<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.type_equality
    }

    fn from_call_to_equality(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Equality<Self>, ()>> {
        call.as_type_equality()
    }

    fn equality_into_call(equality: Equality<Self>) -> QueryCall<Self::Model> {
        QueryCall::TypeEquality(Call::new(equality, ()))
    }

    fn get_definite_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Definite<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.type_definite
    }

    fn get_definite_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Definite<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.type_definite
    }

    fn from_call_to_definite(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Definite<Self>, ()>> {
        call.as_type_definite()
    }

    fn definite_into_call(definite: Definite<Self>) -> QueryCall<Self::Model> {
        QueryCall::TypeDefinite(Call::new(definite, ()))
    }

    fn get_unification_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &context.type_unification
    }

    fn get_unification_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &mut context.type_unification
    }

    fn from_call_to_unification(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Unification<Self>, ()>> {
        call.as_type_unification()
    }

    fn unification_into_call(
        unification: Unification<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::TypeUnification(Call::new(unification, ()))
    }

    fn get_tuple_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &context.type_tuple
    }

    fn get_tuple_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.type_tuple
    }

    fn from_call_to_tuple(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Tuple<Self>, ()>> {
        call.as_type_tuple()
    }

    fn tuple_into_call(
        tuple: predicate::Tuple<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::TypeTuple(Call::new(tuple, ()))
    }

    fn get_outlives_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &context.type_outlives
    }

    fn get_outlives_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &mut context.type_outlives
    }

    fn from_call_to_outlives(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Outlives<Self>, ()>> {
        call.as_type_outlives()
    }

    fn outlives_into_call(
        outlives: predicate::Outlives<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::TypeOutlives(Call::new(outlives, ()))
    }
}

impl<M: Model> Element for Constant<M> {
    fn get_equality_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Equality<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.constant_equality
    }

    fn get_equality_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Equality<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.constant_equality
    }

    fn from_call_to_equality(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Equality<Self>, ()>> {
        call.as_constant_equality()
    }

    fn equality_into_call(equality: Equality<Self>) -> QueryCall<Self::Model> {
        QueryCall::ConstantEquality(Call::new(equality, ()))
    }

    fn get_definite_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Definite<Self>, Cached<(), Succeeded<Satisfied, Self::Model>>>
    {
        &context.constant_definite
    }

    fn get_definite_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Definite<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.constant_definite
    }

    fn from_call_to_definite(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Definite<Self>, ()>> {
        call.as_constant_definite()
    }

    fn definite_into_call(definite: Definite<Self>) -> QueryCall<Self::Model> {
        QueryCall::ConstantDefinite(Call::new(definite, ()))
    }

    fn get_unification_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &context.constant_unification
    }

    fn get_unification_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        Unification<Self>,
        Cached<(), Succeeded<Unifier<Self>, Self::Model>>,
    > {
        &mut context.constant_unification
    }

    fn from_call_to_unification(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Unification<Self>, ()>> {
        call.as_constant_unification()
    }

    fn unification_into_call(
        unification: Unification<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::ConstantUnification(Call::new(unification, ()))
    }

    fn get_tuple_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &context.constant_tuple
    }

    fn get_tuple_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<
        predicate::Tuple<Self>,
        Cached<(), Succeeded<Satisfied, Self::Model>>,
    > {
        &mut context.constant_tuple
    }

    fn from_call_to_tuple(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Tuple<Self>, ()>> {
        call.as_constant_tuple()
    }

    fn tuple_into_call(
        tuple: predicate::Tuple<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::ConstantTuple(Call::new(tuple, ()))
    }

    fn get_outlives_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &context.constant_outlives
    }

    fn get_outlives_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<predicate::Outlives<Self>, Cached<(), Satisfied>> {
        &mut context.constant_outlives
    }

    fn from_call_to_outlives(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<predicate::Outlives<Self>, ()>> {
        call.as_constant_outlives()
    }

    fn outlives_into_call(
        outlives: predicate::Outlives<Self>,
    ) -> QueryCall<Self::Model> {
        QueryCall::ConstantOutlives(Call::new(outlives, ()))
    }
}

impl<T: Term> Query for Equality<T> {
    type Model = T::Model;
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
}

impl<T: Term> Sealed for Equality<T> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_equality_map(context)
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_equality_map_mut(context)
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        T::from_call_to_equality(call)
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        T::equality_into_call(query)
    }
}

impl<T: Term> Query for Definite<T> {
    type Model = T::Model;
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
}

impl<T: Term> Sealed for Definite<T> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_definite_map(context)
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_definite_map_mut(context)
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        T::from_call_to_definite(call)
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        T::definite_into_call(query)
    }
}

impl<T: Term> Query for Unification<T> {
    type Model = T::Model;
    type InProgress = ();
    type Result = Succeeded<Unifier<T>, T::Model>;
}

impl<T: Term> Sealed for Unification<T> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_unification_map(context)
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_unification_map_mut(context)
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        T::from_call_to_unification(call)
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        T::unification_into_call(query)
    }
}

impl<T: Term> Query for predicate::Tuple<T> {
    type Model = T::Model;
    type InProgress = ();
    type Result = Succeeded<Satisfied, T::Model>;
}

impl<T: Term> Sealed for predicate::Tuple<T> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_tuple_map(context)
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_tuple_map_mut(context)
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        T::from_call_to_tuple(call)
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        T::tuple_into_call(query)
    }
}

impl<T: Term> Query for predicate::Outlives<T> {
    type Model = T::Model;
    type InProgress = ();
    type Result = Satisfied;
}

impl<T: Term> Sealed for predicate::Outlives<T> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_outlives_map(context)
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        T::get_outlives_map_mut(context)
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        T::from_call_to_outlives(call)
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        T::outlives_into_call(query)
    }
}

impl<M: Model> Query for predicate::ConstantType<M> {
    type Model = M;
    type InProgress = ConstantTypeQuerySource;
    type Result = Succeeded<Satisfied, M>;
}

impl<M: Model> Sealed for predicate::ConstantType<M> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &context.constant_type
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &mut context.constant_type
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        call.as_constant_type()
    }

    fn into_query_call(
        query: Self,
        in_progress: Self::InProgress,
    ) -> QueryCall<Self::Model> {
        QueryCall::ConstantType(Call::new(query, in_progress))
    }
}

impl<M: Model> Query for predicate::Trait<M> {
    type Model = M;
    type InProgress = ();
    type Result = Succeeded<TraitSatisfied<M>, M>;
}

impl<M: Model> Sealed for predicate::Trait<M> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &context.trait_satisfiability
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &mut context.trait_satisfiability
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        call.as_trait_satisfiability()
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        QueryCall::TraitSatisfiability(Call::new(query, ()))
    }
}

impl<M: Model> Query for TypeCheck<M> {
    type Model = M;
    type InProgress = ();
    type Result = Succeeded<Satisfied, M>;
}

impl<M: Model> Sealed for TypeCheck<M> {
    fn get_map(
        context: &Context<Self::Model>,
    ) -> &BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &context.type_check
    }

    fn get_map_mut(
        context: &mut Context<Self::Model>,
    ) -> &mut BTreeMap<Self, Cached<Self::InProgress, Self::Result>> {
        &mut context.type_check
    }

    fn from_call(
        call: &QueryCall<Self::Model>,
    ) -> Option<&Call<Self, Self::InProgress>> {
        call.as_type_check()
    }

    fn into_query_call(
        query: Self,
        (): Self::InProgress,
    ) -> QueryCall<Self::Model> {
        QueryCall::TypeCheck(Call::new(query, ()))
    }
}