//! Contains the logic for compile-time constant evaluation.

use pernixc_base::handler::Handler;
use pernixc_syntax::syntax_tree::expression::Expression;

use super::{State, Table};
use crate::{
    error,
    symbol::ItemID,
    type_system::{model::Default, term::constant::Constant},
};

/// The error type for evaluating compile-time-constant expressions.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,

    #[error("The program is in a suboptimal state")]
    Suboptimal,
}

impl<S: State> Table<S> {
    /// Const-evaluates the given expression into a [`Constant`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn evaluate(
        &self,
        _: &Expression,
        _: ItemID,
        _: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Constant<Default>, Error> {
        todo!()
    }
}
