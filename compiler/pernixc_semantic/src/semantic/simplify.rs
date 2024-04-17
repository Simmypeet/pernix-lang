use super::{
    equality,
    session::{Cached, ExceedLimitError, Limit, Session},
    term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
    visitor::Mutable,
    Environment,
};
use crate::table::State;

/// A query for simplifying a term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

struct Visitor<
    'a,
    'r,
    'l,
    T: State,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    result: Result<(), ExceedLimitError>,
    environment: &'a Environment<'a, T>,
    limit: &'l mut Limit<'r, R>,
}

impl<
        'a,
        'r,
        'l,
        U: Term,
        T: State,
        R: Session<Lifetime> + Session<Type> + Session<Constant> + Session<U>,
    > Mutable<U> for Visitor<'a, 'r, 'l, T, R>
{
    fn visit(
        &mut self,
        term: &mut U,
        _: <U as super::visitor::Element>::Location,
    ) -> bool {
        match self.result {
            Ok(()) => {
                match simplify_internal(term, self.environment, self.limit) {
                    Ok(simplified) => {
                        if let Some(simplified) = simplified {
                            *term = simplified;
                        }

                        true
                    }
                    Err(err) => {
                        self.result = Err(err);
                        false
                    }
                }
            }
            Err(_) => false,
        }
    }
}

fn simplify_internal<T: Term>(
    term: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<Option<T>, ExceedLimitError> {
    match limit.mark_as_in_progress(Query(term), ())? {
        Some(Cached::Done(result)) => return Ok(Some(result)),
        Some(Cached::InProgress(())) => return Ok(None),
        None => {}
    }

    let mut simplified = 'simplified: {
        let mut equivalents =
            term.normalize(environment, limit)?.into_iter().collect::<Vec<_>>();

        if term.as_trait_member().is_some() {
            for equivalent in environment
                .premise
                .predicates
                .iter()
                .filter_map(|x| T::as_trait_member_equality_predicate(x))
            {
                let trait_member = T::from(equivalent.trait_member.clone());
                if equality::equals(&trait_member, term, environment, limit)? {
                    equivalents.push(equivalent.equivalent.clone());
                }
            }
        }

        let mut simplified_equivalents = Vec::new();
        for equivalent in equivalents {
            if let Some(sim) =
                simplify_internal(&equivalent, environment, limit)?
            {
                simplified_equivalents.push(sim);
            }
        }

        let mut iter = simplified_equivalents.iter();
        let Some(first) = iter.next() else {
            break 'simplified None;
        };

        if iter.all(|x| first == x) {
            Some(first.clone())
        } else {
            None
        }
    }
    .unwrap_or_else(|| term.clone());

    // simplify the sub-terms
    let mut visitor = Visitor { result: Ok(()), environment, limit };
    let _ = simplified.accept_one_level_mut(&mut visitor);
    visitor.result?;

    limit.mark_as_done(Query(term), simplified.clone());
    Ok(Some(simplified))
}

/// Simplifies a term by normalizing and applying equivalences.
///
/// # Errors
///
/// See [`ExceedLimitError`] for more information.
pub fn simplify<T: Term>(
    term: &T,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<T, ExceedLimitError> {
    simplify_internal(term, environment, limit)
        .map(|x| x.unwrap_or_else(|| term.clone()))
}

#[cfg(test)]
mod tests;
