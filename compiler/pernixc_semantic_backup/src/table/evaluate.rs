//! Contains codes related to evaluating compile-time-constant expressions.

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;
use thiserror::Error;

use super::{
    resolution::{self, Config},
    State, Table,
};
use crate::{
    error,
    semantic::{model::Model, term::constant::Constant},
    symbol::GlobalID,
};

/// The error type for evaluating compile-time-constant expressions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Error)]
#[allow(missing_docs)]
pub enum Error {
    #[error("The given `referring_site` id does not exist in the table")]
    InvalidReferringSiteID,

    #[error("Encountered a fatal semantic error that aborts the process")]
    SemanticError,

    #[error("The program is in a suboptimal state")]
    Suboptimal,
}

impl From<Error> for resolution::Error {
    fn from(value: Error) -> Self {
        match value {
            Error::InvalidReferringSiteID => Self::InvalidReferringSiteID,
            Error::SemanticError | Error::Suboptimal => Self::SemanticError,
        }
    }
}

impl<T: State> Table<T> {
    /// Const-evaluates the given expression into a [`Constant`].
    ///
    /// # Errors
    ///
    /// See [`Error`] for more information.
    pub fn evaluate<S: Model>(
        &self,
        _syntax_tree: &syntax_tree::expression::Expression,
        _referring_site: GlobalID,
        _config: &mut dyn Config<S>,
        _handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Constant<S>, Error> {
        todo!()
    }
}
