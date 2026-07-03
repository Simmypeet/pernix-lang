use pernixc_type::predicate::Predicate2;

use crate::{
    constraints::Constraints,
    solver::{OverflowError, Solver},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateError {
    Unsolvable,
}

impl Solver<'_> {
    #[allow(clippy::unused_async)]
    pub async fn solve_predicate(
        &mut self,
        _predicate: Predicate2,
    ) -> Result<Result<Constraints, PredicateError>, OverflowError> {
        todo!()
    }
}
