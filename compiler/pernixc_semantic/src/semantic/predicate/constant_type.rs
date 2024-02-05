use super::Satisfiability;
use crate::{
    semantic::{
        equality,
        instantiation::{self, Instantiation},
        session::{self, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        visitor, Premise, Semantic,
    },
    table::{State, Table},
};

#[derive(Debug)]
struct Visitor<
    'a,
    's,
    'r,
    'l,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    constant_type: Result<bool, ExceedLimitError>,
    premise: &'a Premise,
    table: &'a Table<T>,
    semantic: &'s mut S,
    session: &'l mut Limit<'r, R>,
}

impl<
        'a,
        's,
        'r,
        'l,
        T: State,
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    > visitor::Visitor for Visitor<'a, 's, 'r, 'l, T, S, R>
{
    fn visit_type(&mut self, ty: &Type) -> bool {
        match satisfies_internal(
            ty,
            QuerySource::Normal,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.constant_type = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool {
        match satisfies_internal(
            lifetime,
            QuerySource::Normal,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.constant_type = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }

    fn visit_constant(&mut self, constant: &Constant) -> bool {
        match satisfies_internal(
            constant,
            QuerySource::Normal,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.constant_type = result;
                return false;
            }

            Ok(true) => {}
        }

        true
    }

    fn visit_type_mut(&mut self, _: &mut Type) -> bool { todo!() }

    fn visit_lifetime_mut(&mut self, _: &mut Lifetime) -> bool { todo!() }

    fn visit_constant_mut(&mut self, _: &mut Constant) -> bool { todo!() }
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

impl ConstantType {
    /// Applies the instantiation to the type.
    pub fn instantiate(&mut self, instantiation: &Instantiation) {
        instantiation::instantiate(&mut self.0, instantiation);
    }
}

fn satisfies_internal<
    T: Term,
    S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
>(
    term: &T,
    query_source: QuerySource,
    premise: &Premise,
    table: &Table<impl State>,
    semantic: &mut S,
    session: &mut Limit<R>,
) -> Result<bool, ExceedLimitError> {
    let satisfiability = term.definite_satisfiability();

    // trivially satisfiable
    if satisfiability == Satisfiability::Satisfied {
        return Ok(true);
    }

    match session.mark_as_in_progress(Query(term), query_source)? {
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
        let mut visitor = Visitor {
            constant_type: Ok(true),
            premise,
            table,
            semantic,
            session,
        };

        let _ = term.accept_one_level(&mut visitor);

        if visitor.constant_type? {
            let mut result = false;
            session.mark_as_done(Query(term), Satisfied);

            // look for the fields of the term as well (if it's an ADT)
            match term.get_adt_fields(table) {
                Some(fields) => 'result: {
                    for field in fields {
                        if !satisfies_internal(
                            &field,
                            QuerySource::Normal,
                            premise,
                            table,
                            semantic,
                            session,
                        )? {
                            break 'result;
                        }
                    }

                    result = true;
                }
                None => result = true,
            }

            if result {
                session.mark_as_done(Query(term), Satisfied);
                return Ok(true);
            }
        }
    }

    // satisfiable with premises
    for premise_term in T::constant_type_predicates(premise) {
        if equality::equals(
            term,
            premise_term,
            premise,
            table,
            semantic,
            session,
        )? {
            session.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    // satisfiable with normalization
    if let Some(normalized) =
        semantic.normalize(term, premise, table, session)?
    {
        if satisfies_internal(
            &normalized,
            QuerySource::FromEquivalence,
            premise,
            table,
            semantic,
            session,
        )? {
            session.mark_as_done(Query(term), Satisfied);
            return Ok(true);
        }
    }

    for (key, values) in T::get_mapping(&premise.equalities_mapping) {
        if equality::equals(term, key, premise, table, semantic, session)? {
            for value in values {
                if satisfies_internal(
                    value,
                    QuerySource::FromEquivalence,
                    premise,
                    table,
                    semantic,
                    session,
                )? {
                    session.mark_as_done(Query(term), Satisfied);
                    return Ok(true);
                }
            }
        }
    }

    session.clear_query(Query(term));
    Ok(false)
}

impl ConstantType {
    /// Checks if the given type satisfies the predicate.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfies<
        S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        ty: &Type,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        satisfies_internal(
            ty,
            QuerySource::Normal,
            premise,
            table,
            semantic,
            session,
        )
    }
}
