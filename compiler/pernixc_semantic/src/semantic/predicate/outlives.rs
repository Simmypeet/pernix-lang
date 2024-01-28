use crate::{
    semantic::{
        mapping::Mapping,
        predicate::Satisfiability,
        session::{Cached, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        unification::{self, Unification},
        visitor, Premise, Semantic,
    },
    table::{State, Table},
};

/// A query for checking [`Outlives`] predicate satisfiability.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Query<'a, T> {
    /// The term that must outlive the bound.
    pub operand: &'a T,

    /// The lifetime that the term must outlive.
    pub bound: &'a Lifetime,
}

/// A predicate that a term outlives a lifetime.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Outlives<T> {
    /// The term that must outlive the bound.
    pub operand: T,

    /// The lifetime that the term must outlive.
    pub bound: Lifetime,
}

struct Visitor<
    'a,
    's,
    'r,
    'l,
    T: State,
    S: Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
    R: Session<Lifetime> + Session<Type> + Session<Constant>,
> {
    outlives: Result<bool, ExceedLimitError>,
    bound: &'a Lifetime,
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
        match Outlives::satisfies(
            ty,
            self.bound,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.outlives = result;
                false
            }
            Ok(true) => true,
        }
    }

    fn visit_lifetime(&mut self, lifetime: &Lifetime) -> bool {
        match Outlives::satisfies(
            lifetime,
            self.bound,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.outlives = result;
                false
            }
            Ok(true) => true,
        }
    }

    fn visit_constant(&mut self, constant: &Constant) -> bool {
        match Outlives::satisfies(
            constant,
            self.bound,
            self.premise,
            self.table,
            self.semantic,
            self.session,
        ) {
            result @ (Err(_) | Ok(false)) => {
                self.outlives = result;
                false
            }
            Ok(true) => true,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct OutlivesUnifyingConfig;

impl unification::Config for OutlivesUnifyingConfig {
    fn lifetime_unifiable(&mut self, _: &Lifetime, _: &Lifetime) -> bool {
        true
    }

    fn type_unifiable(&mut self, _: &Type, _: &Type) -> bool { false }

    fn constant_unifiable(&mut self, _: &Constant, _: &Constant) -> bool {
        false
    }
}

impl<T: Term> Outlives<T> {
    fn satisfies_by_lifetime_matching<
        S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        unification: Unification<T>,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        let mapping = Mapping::from_unification(unification);

        assert!(mapping.types.is_empty());
        assert!(mapping.constants.is_empty());

        for (bound, operands) in mapping.lifetimes {
            for operand in operands {
                if semantic.outlives_satisfiability(
                    &bound, &operand, premise, table, session,
                )? != Satisfiability::Satisfied
                {
                    return Ok(false);
                }
            }
        }

        Ok(true)
    }

    /// Determines whether a predicate of the term and the bound is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfies<
        S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        operand: &T,
        bound: &Lifetime,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        let satisfiability = semantic
            .outlives_satisfiability(operand, bound, premise, table, session)?;

        if satisfiability == Satisfiability::Satisfied {
            return Ok(true);
        }

        match session.mark_as_in_progress(Query { operand, bound })? {
            Some(Cached::Done(Satisfied)) => return Ok(true),
            Some(Cached::InProgress) => return Ok(false),
            None => {}
        }

        // check if all sub-terms are satisfiable
        if satisfiability == Satisfiability::Congruent {
            let mut visitor = Visitor {
                outlives: Ok(true),
                bound,
                premise,
                table,
                semantic,
                session,
            };

            let _ = operand.accept_one_level(&mut visitor);

            if visitor.outlives? {
                session.mark_as_done(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        for Self { operand: next_operand, bound: next_bound } in
            T::outlives_predicates(premise)
        {
            let Some(unification) = unification::unify(
                operand,
                next_operand,
                premise,
                table,
                &mut OutlivesUnifyingConfig,
                semantic,
                session,
            )?
            else {
                continue;
            };

            if !Self::satisfies_by_lifetime_matching(
                unification,
                premise,
                table,
                semantic,
                session,
            )? {
                continue;
            }

            if Outlives::satisfies(
                next_bound, bound, premise, table, semantic, session,
            )? {
                session.mark_as_done(Query { operand, bound }, Satisfied);
                return Ok(true);
            }
        }

        session.clear_query(Query { operand, bound });
        Ok(false)
    }
}

#[cfg(test)]
mod tests;
