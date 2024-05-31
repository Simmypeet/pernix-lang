use super::Satisfiability;
use crate::{
    semantic::{
        get_equivalences_impl,
        model::Model,
        normalizer::Normalizer,
        session::{self, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor, Environment, ExceedLimitError, Satisfied,
    },
    symbol::table::State,
};

#[derive(Debug)]
struct Visitor<
    'a,
    'l,
    T: State,
    N: Normalizer<M>,
    R: Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    M: Model,
> {
    definite: Result<bool, ExceedLimitError>,
    environment: &'a Environment<'a, M, T, N>,
    limit: &'l mut Limit<R>,
}

impl<
        'a,
        'l,
        'v,
        U: Term,
        T: State,
        N: Normalizer<U::Model>,
        R: Session<U>
            + Session<Lifetime<U::Model>>
            + Session<Type<U::Model>>
            + Session<Constant<U::Model>>,
    > visitor::Visitor<'v, U> for Visitor<'a, 'l, T, N, R, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        match definite_impl(term, self.environment, self.limit) {
            result @ (Err(_) | Ok(false)) => {
                self.definite = result;
                false
            }

            Ok(true) => true,
        }
    }
}

/// A query for checking definite predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

/// Determines whether a term is definite.
///
/// A term is definite if the term doesn't contain any generic parameters.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn definite<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
) -> Result<bool, ExceedLimitError> {
    let mut limit = Limit::<session::Default<_>>::default();
    definite_impl(term, environment, &mut limit)
}

pub(in crate::semantic) fn definite_impl<T: Term>(
    term: &T,
    environment: &Environment<T::Model, impl State, impl Normalizer<T::Model>>,
    limit: &mut Limit<
        impl Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<bool, ExceedLimitError> {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        limit.mark_as_done::<T, _>(Query(term), Satisfied);
        return Ok(true);
    }

    match limit.mark_as_in_progress::<T, _>(Query(term), ())? {
        Some(session::Cached::Done(Satisfied)) => return Ok(true),
        Some(session::Cached::InProgress(())) => {
            return Ok(false);
        }
        None => {}
    }

    // satisfiable with congruence
    if satisfiability == Satisfiability::Congruent {
        let mut visitor = Visitor { definite: Ok(true), environment, limit };

        let _ = term.accept_one_level(&mut visitor);

        if visitor.definite? {
            limit.mark_as_done::<T, _>(Query(term), Satisfied);
            return Ok(true);
        }
    }

    // get the equivalences
    for eq in get_equivalences_impl(term, environment, limit)? {
        if definite_impl(&eq, environment, limit)? {
            limit.mark_as_done::<T, _>(Query(term), Satisfied);
            return Ok(true);
        }
    }

    limit.clear_query::<T, _>(Query(term));
    Ok(false)
}

#[cfg(test)]
mod tests;
