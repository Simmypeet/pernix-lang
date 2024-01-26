use crate::{
    semantic::{
        equality,
        predicate::Satisfiability,
        session::{Cached, ExceedLimitError, Limit, Satisfied, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
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
        match Outlives::satisfied(
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
        match Outlives::satisfied(
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
        match Outlives::satisfied(
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

impl<T: Term> Outlives<T> {
    /// Determines whether a predicate of the term and the bound is satisfiable.
    ///
    /// # Errors
    ///
    /// See [`ExceedLimitError`] for more information.
    pub fn satisfied<
        S: Semantic<T> + Semantic<Lifetime> + Semantic<Type> + Semantic<Constant>,
        R: Session<T> + Session<Lifetime> + Session<Type> + Session<Constant>,
    >(
        term: &T,
        bound: &Lifetime,
        premise: &Premise,
        table: &Table<impl State>,
        semantic: &mut S,
        session: &mut Limit<R>,
    ) -> Result<bool, ExceedLimitError> {
        let satisfiability = semantic
            .outlives_satisfiability(term, bound, premise, table, session)?;

        if satisfiability == Satisfiability::Satisfied {
            return Ok(true);
        }

        match session.mark_as_in_progress(Query { operand: term, bound })? {
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

            let _ = term.accept_one_level(&mut visitor);

            if visitor.outlives? {
                session.mark_as_done(Query { operand: term, bound }, Satisfied);
                return Ok(true);
            }
        }

        // check in premise
        for outlive_premise in <T as Term>::outlives_predicates(premise) {
            if equality::equals(
                term,
                &outlive_premise.operand,
                premise,
                table,
                semantic,
                session,
            )? && equality::equals(
                bound,
                &outlive_premise.bound,
                premise,
                table,
                semantic,
                session,
            )? {
                session.mark_as_done(Query { operand: term, bound }, Satisfied);
                return Ok(true);
            }
        }

        todo!()
    }
}
