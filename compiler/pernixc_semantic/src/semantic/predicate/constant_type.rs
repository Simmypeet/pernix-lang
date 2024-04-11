use super::{contains_forall_lifetime, Satisfiability};
use crate::{
    semantic::{
        equality, get_equivalences,
        instantiation::{self, Instantiation},
        session::{self, ExceedLimitError, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor, Environment, Satisfied,
    },
    table::{self, DisplayObject, State, Table},
};

#[derive(Debug)]
struct Visitor<
    'a,
    'r,
    'l,
    T: State,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    constant_type: Result<bool, ExceedLimitError>,
    environment: &'a Environment<'a, T>,
    limit: &'l mut Limit<'r, R>,
}

impl<
        'a,
        'r,
        'l,
        U: Term,
        T: State,
        R: Session<U> + Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Visitor<U> for Visitor<'a, 'r, 'l, T, R>
{
    fn visit(&mut self, term: &U, _: U::Location) -> bool {
        match satisfies_internal(
            term,
            QuerySource::Normal,
            self.environment,
            self.limit,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.constant_type = result;
                false
            }

            Ok(true) => true,
        }
    }
}

/// A query for checking constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T>(pub &'a T);

/// Describes the source of the query for constant type predicate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QuerySource {
    /// Used to reason the satisfiability by querying it by an another
    /// equivalent type.
    FromEquivalence,

    /// Normal
    Normal,
}

/// Represents a type can be used as a type of a compile-time constant value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstantType(pub Type);

impl<T: State> table::Display<T> for ConstantType {
    fn fmt(
        &self,
        table: &Table<T>,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "const type {}", DisplayObject { display: &self.0, table })
    }
}

impl ConstantType {
    /// Checks if the type contains a `forall` lifetime.
    #[must_use]
    pub fn contains_forall_lifetime(&self) -> bool {
        contains_forall_lifetime(&self.0)
    }

    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}

fn satisfies_internal<T: Term>(
    term: &T,
    query_source: QuerySource,
    environment: &Environment<impl State>,
    limit: &mut Limit<
        impl Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >,
) -> Result<bool, ExceedLimitError> {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        return Ok(true);
    }

    match limit.mark_as_in_progress(Query(term), query_source)? {
        Some(session::Cached::Done(Satisfied)) => return Ok(true),
        Some(session::Cached::InProgress(source)) => {
            return Ok(match source {
                QuerySource::FromEquivalence => false,
                QuerySource::Normal => true,
            })
        }
        None => {}
    }

    // if the term is congruent, then we need to check the sub-terms
    if satisfiability == Satisfiability::Congruent {
        let mut visitor =
            Visitor { constant_type: Ok(true), environment, limit };

        let _ = term.accept_one_level(&mut visitor);

        if visitor.constant_type? {
            let mut result = false;

            // look for the fields of the term as well (if it's an ADT)
            match term.get_adt_fields(environment.table) {
                Some(fields) => 'result: {
                    for field in fields {
                        if !satisfies_internal(
                            &field,
                            QuerySource::Normal,
                            environment,
                            limit,
                        )? {
                            break 'result;
                        }
                    }

                    result = true;
                }
                None => result = true,
            }

            if result {
                limit.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }
    }

    // satisfiable with premises
    for premise_term in environment
        .premise
        .predicates
        .iter()
        .filter_map(|x| T::as_constant_type_predicate(x))
    {
        if equality::equals(term, premise_term, environment, limit)? {
            limit.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    // satisfiable with equivalence
    for eq in get_equivalences(term, environment, limit)? {
        if satisfies_internal(
            &eq,
            QuerySource::FromEquivalence,
            environment,
            limit,
        )? {
            limit.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    limit.clear_query(Query(term));
    Ok(false)
}

impl ConstantType {
    /// Checks if the given type satisfies the predicate.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfies(
        ty: &Type,
        environment: &Environment<impl State>,
        limit: &mut Limit<
            impl Session<Lifetime> + Session<Type> + Session<Constant>,
        >,
    ) -> Result<bool, ExceedLimitError> {
        satisfies_internal(ty, QuerySource::Normal, environment, limit)
    }
}
