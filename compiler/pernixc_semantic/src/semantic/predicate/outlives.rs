use crate::{
    semantic::{
        session::{ExceedLimitError, Limit, Session},
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, Term},
        Premise, Semantic,
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

impl<T: Term> Outlives<T> {
    /// Determines wheter a predicate of the term and the bound is satisfiable.
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
        todo!()
    }
}
