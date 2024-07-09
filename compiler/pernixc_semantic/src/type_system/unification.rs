//! Contains the the unification logic.

use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use by_address::ByAddress;
use enum_as_inner::EnumAsInner;

use super::{
    equality,
    equivalence::get_equivalences_with_context,
    matching,
    model::Model,
    normalizer::Normalizer,
    query::{self, Context},
    sub_term::{SubTerm, TermLocation},
    term::{
        constant::Constant, lifetime::Lifetime, r#type::Type, ModelOf, Term,
    },
    Compute, Environment, Output, OverflowError, Satisfied, Succeeded,
};
use crate::symbol::table::State;

/// A query for performing unification.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Unification<T: ModelOf> {
    pub from: T,
    pub to: T,
    pub predicate: ByAddress<Arc<dyn PredicateA<T::Model>>>,
}

impl<T: ModelOf> Unification<T> {
    /// Creates a new unification query.
    #[must_use]
    pub fn new(from: T, to: T, unifier: Arc<dyn PredicateA<T::Model>>) -> Self {
        Self { from, to, predicate: ByAddress(unifier) }
    }
}

/// A super trait which includes the [`Unifier`] trait for all kinds of terms.
pub trait PredicateA<M: Model>:
    std::fmt::Debug
    + Predicate<Lifetime<M>>
    + Predicate<Type<M>>
    + Predicate<Constant<M>>
{
}

impl<
        M: Model,
        T: std::fmt::Debug
            + Predicate<Lifetime<M>>
            + Predicate<Type<M>>
            + Predicate<Constant<M>>,
    > PredicateA<M> for T
{
}

/// An object used for determining if two terms are unifiable.
pub trait Predicate<T: Term> {
    /// Determines if the two given terms are unifiable.
    fn unifiable(
        &self,
        from: &T,
        to: &T,
        from_logs: &Vec<Log<T::Model>>,
        to_logs: &Vec<Log<T::Model>>,
    ) -> Result<Output<Satisfied, T::Model>, OverflowError>;
}

/// A trait implemented by terms that can be unified.
pub trait Element: ModelOf {
    /// Accepts a configuration and returns `true` if the term is unifiable.
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &Vec<Log<Self::Model>>,
        to_logs: &Vec<Log<Self::Model>>,
        config: &dyn PredicateA<Self::Model>,
    ) -> Result<Output<Satisfied, Self::Model>, OverflowError>;

    /// Converts the term into a [`Log`] record as a rewitten term.
    fn into_rewritten(self) -> Log<Self::Model>;
}

impl<M: Model> Element for Lifetime<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &Vec<Log<M>>,
        to_logs: &Vec<Log<M>>,
        config: &dyn PredicateA<M>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        config.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenLifetime(self) }
}

impl<M: Model> Element for Type<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &Vec<Log<M>>,
        to_logs: &Vec<Log<M>>,
        config: &dyn PredicateA<M>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        config.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenType(self) }
}

impl<M: Model> Element for Constant<M> {
    fn unifiable(
        from: &Self,
        to: &Self,
        from_logs: &Vec<Log<M>>,
        to_logs: &Vec<Log<M>>,
        config: &dyn PredicateA<M>,
    ) -> Result<Output<Satisfied, M>, OverflowError> {
        config.unifiable(from, to, from_logs, to_logs)
    }

    fn into_rewritten(self) -> Log<M> { Log::RewrittenConstant(self) }
}

/// Contains all the unification of substructural components.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct Substructural<T: SubTerm> {
    pub lifetimes: HashMap<T::SubLifetimeLocation, Unifier<Lifetime<T::Model>>>,
    pub types: HashMap<T::SubTypeLocation, Unifier<Type<T::Model>>>,
    pub constants: HashMap<T::SubConstantLocation, Unifier<Constant<T::Model>>>,
}

impl<T> Default for Substructural<T>
where
    T: Term,
{
    fn default() -> Self {
        Self {
            lifetimes: HashMap::new(),
            types: HashMap::new(),
            constants: HashMap::new(),
        }
    }
}

/// Represents the record tracking of matching/modification made to a term to
/// unify it with another term.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Log<M: Model> {
    Substructural(TermLocation),
    RewrittenLifetime(Lifetime<M>),
    RewrittenType(Type<M>),
    RewrittenConstant(Constant<M>),
}

/// Specifies how a term matches another term.
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Matching<T: SubTerm> {
    /// The two terms are unified by passing the predicate check in the
    /// [`Config`].
    Unifiable(T, T),

    /// The two terms are substructural and can be unified by unifying their
    /// substructural components.
    Substructural(Substructural<T>),

    /// The two terms are equal, no unification is needed.
    Equality,
}

/// Represents a unification between two terms.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unifier<T: SubTerm> {
    /// If the `lhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    ///
    /// The rewritten term can occur from normalizing the term or using trait
    /// member equivalences.
    pub rewritten_from: Option<T>,

    /// If the `rhs` has been rewritten into another form, this field will be
    /// `Some` of the rewritten term.
    ///
    /// The rewritten term can occur from normalizing the term or using trait
    /// member equivalences.
    pub rewritten_right: Option<T>,

    /// The unification of the `lhs` and `rhs` terms.
    pub matching: Matching<T>,
}

impl<T: Term> Compute for Unification<T> {
    type Error = OverflowError;
    type Parameter = (
        Vec<Log<T::Model>>, /* from logs */
        Vec<Log<T::Model>>, /* to logs */
    );

    #[allow(private_bounds, private_interfaces)]
    fn implementation(
        &self,
        environment: &Environment<
            Self::Model,
            impl State,
            impl Normalizer<Self::Model>,
        >,
        context: &mut query::Context<Self::Model>,
        (from_logs, to_logs): Self::Parameter,
        (): Self::InProgress,
    ) -> Result<Option<Self::Result>, Self::Error> {
        unify(
            &self.from,
            &self.to,
            from_logs,
            to_logs,
            self.predicate.0.clone(),
            environment,
            context,
        )
    }
}

fn substructural_unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: Vec<Log<T::Model>>,
    to_logs: Vec<Log<T::Model>>,
    predicate: Arc<dyn PredicateA<T::Model>>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
) -> Result<Output<Unifier<T>, T::Model>, OverflowError> {
    let Some(substructural) = from.substructural_match(to) else {
        return Ok(None);
    };

    let mut constraints = HashSet::new();
    let mut result = Substructural::default();

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.lifetimes
    {
        let mut from_logs = from_logs.clone();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.clone();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) =
            Unification::new(from.clone(), to.clone(), predicate.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (from_logs, to_logs),
                    (),
                )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints);
        assert!(result.lifetimes.insert(from_location, new.result).is_none());
    }

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.types
    {
        let mut from_logs = from_logs.clone();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.clone();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) =
            Unification::new(from.clone(), to.clone(), predicate.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (from_logs, to_logs),
                    (),
                )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints);
        assert!(result.types.insert(from_location, new.result).is_none());
    }

    for matching::Matching {
        lhs: from,
        rhs: to,
        lhs_location: from_location,
        rhs_location: to_location,
    } in substructural.constants
    {
        let mut from_logs = from_logs.clone();
        from_logs.push(Log::Substructural(from_location.into()));

        let mut to_logs = to_logs.clone();
        to_logs.push(Log::Substructural(to_location.into()));

        let Some(new) =
            Unification::new(from.clone(), to.clone(), predicate.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (from_logs, to_logs),
                    (),
                )?
        else {
            return Ok(None);
        };

        constraints.extend(new.constraints);
        assert!(result.constants.insert(from_location, new.result).is_none());
    }

    Ok(Some(Succeeded::with_constraints(
        Unifier {
            rewritten_from: None,
            rewritten_right: None,
            matching: Matching::Substructural(result),
        },
        constraints,
    )))
}

pub(super) fn unify<T: Term>(
    from: &T,
    to: &T,
    from_logs: Vec<Log<T::Model>>,
    to_logs: Vec<Log<T::Model>>,
    predicate: Arc<dyn PredicateA<T::Model>>,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    context: &mut Context<T::Model>,
) -> Result<Output<Unifier<T>, T::Model>, OverflowError> {
    if let Some(result) = equality::Equality::new(from.clone(), to.clone())
        .query_with_context(environment, context)?
    {
        return Ok(Some(Succeeded::with_constraints(
            Unifier {
                rewritten_from: None,
                rewritten_right: None,
                matching: Matching::Equality,
            },
            result.constraints,
        )));
    }

    // check if the predicate can unify the two terms
    if let Some(constraint) =
        T::unifiable(from, to, &from_logs, &to_logs, &*predicate)?
    {
        return Ok(Some(Succeeded::with_constraints(
            Unifier {
                rewritten_from: None,
                rewritten_right: None,
                matching: Matching::Unifiable(from.clone(), to.clone()),
            },
            constraint.constraints,
        )));
    }

    if let Some(unification) = substructural_unify(
        from,
        to,
        from_logs.clone(),
        to_logs.clone(),
        predicate.clone(),
        environment,
        context,
    )? {
        return Ok(Some(unification));
    }

    // try to look for equivalences
    for Succeeded { result: eq_from, constraints } in
        get_equivalences_with_context(from, environment, context)?
    {
        let mut from_logs = from_logs.clone();
        from_logs.push(eq_from.clone().into_rewritten());

        if let Some(mut unification) =
            Unification::new(eq_from.clone(), to.clone(), predicate.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (from_logs, to_logs.clone()),
                    (),
                )?
        {
            unification.result.rewritten_from =
                unification.result.rewritten_from.or(Some(eq_from));
            unification.constraints.extend(constraints);

            return Ok(Some(unification));
        }
    }

    for Succeeded { result: eq_to, constraints } in
        get_equivalences_with_context(to, environment, context)?
    {
        let mut to_logs = to_logs.clone();
        to_logs.push(eq_to.clone().into_rewritten());

        if let Some(mut unification) =
            Unification::new(from.clone(), eq_to.clone(), predicate.clone())
                .query_with_context_full(
                    environment,
                    context,
                    (from_logs.clone(), to_logs),
                    (),
                )?
        {
            unification.result.rewritten_right =
                unification.result.rewritten_right.or(Some(eq_to));
            unification.constraints.extend(constraints);

            return Ok(Some(unification));
        }
    }

    Ok(None)
}

#[cfg(test)]
mod tests;
