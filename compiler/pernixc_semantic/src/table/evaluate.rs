//! Contains codes related to evaluating compile-time-constant expressions.

use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::Table;
use crate::{
    entity::{constant::Constant, Model},
    error,
    symbol::GlobalID,
};

impl Table {
    /// Const-evaluates the given expression into a [`Constant`].
    ///
    /// # Errors
    ///
    /// - [`super::Error::InvalidID`]: If the given `referring_site` is not a valid [`GlobalID`].
    /// - [`super::Error::SemanticError`]: If there's a fatal semantic error during the evaluation.
    pub fn evaluate<S: Model>(
        &self,
        _syntax_tree: &syntax_tree::expression::Expression,
        _referring_site: GlobalID,
        _handler: &dyn Handler<error::Error>,
    ) -> Result<Constant<S>, super::Error> {
        todo!()
    }

    /// Const-evaluates the given expression into a [`Constant`].
    ///
    /// # Errors
    ///
    /// - [`super::Error::InvalidID`]: If the given `referring_site` is not a valid [`GlobalID`].
    /// - [`super::Error::SemanticError`]: If there's a fatal semantic error during the evaluation.
    pub fn evaluate_functional<S: Model>(
        &self,
        _syntax_tree: &syntax_tree::expression::Functional,
        _referring_site: GlobalID,
        _handler: &dyn Handler<error::Error>,
    ) -> Result<Constant<S>, super::Error> {
        todo!()
    }
}
