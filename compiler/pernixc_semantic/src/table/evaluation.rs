use pernixc_base::diagnostic::Handler;
use pernixc_syntax::syntax_tree;

use super::{resolution::CheckingBehavior, Table};
use crate::{constant, error, symbol::GlobalItemRef};

impl Table {
    pub(super) fn evaluate_constant(
        &mut self,
        _expression_syntax: &syntax_tree::expression::Expression,
        _evaluation_site: GlobalItemRef,
        _checking_behavior: CheckingBehavior,
        _handler: &impl Handler<error::Error>,
    ) -> Result<constant::Constant, super::Error> {
        todo!("implements constant evaluation")
    }
}
