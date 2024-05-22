use super::Satisfiability;
use crate::{
    semantic::{
        get_equivalences,
        model::Model,
        normalizer::Normalizer,
        session::{self, ExceedLimitError, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor, Environment, Satisfied,
    },
    symbol::table::State,
};

#[derive(Debug)]
struct Visitor<
    'a,
    'r,
    'l,
    T: State,
    N: Normalizer<M>,
    R: Session<Lifetime<M>> + Session<Type<M>> + Session<Constant<M>>,
    M: Model,
> {
    definite: Result<bool, ExceedLimitError>,
    environment: &'a Environment<'a, M, T, N>,
    limit: &'l mut Limit<'r, R>,
}

impl<
        'a,
        'r,
        'l,
        'v,
        U: Term,
        T: State,
        N: Normalizer<U::Model>,
        R: Session<U>
            + Session<Lifetime<U::Model>>
            + Session<Type<U::Model>>
            + Session<Constant<U::Model>>,
    > visitor::Visitor<'v, U> for Visitor<'a, 'r, 'l, T, N, R, U::Model>
{
    fn visit(&mut self, term: &'v U, _: U::Location) -> bool {
        match definite(term, self.environment, self.limit) {
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
    limit: &mut Limit<
        impl Session<T>
            + Session<Lifetime<T::Model>>
            + Session<Type<T::Model>>
            + Session<Constant<T::Model>>,
    >,
) -> Result<bool, ExceedLimitError> {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        limit.mark_as_done(Query(term), Satisfied);
        return Ok(true);
    }

    match limit.mark_as_in_progress(Query(term), ())? {
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
            limit.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    // get the equivalences
    for eq in get_equivalences(term, environment, limit)? {
        if definite(&eq, environment, limit)? {
            limit.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    limit.clear_query(Query(term));
    Ok(false)
}

// TODO: Add tests
// #[cfg(test)]
// mod tests;
