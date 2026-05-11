use pernixc_arena::ID;
use pernixc_ir::value::register::{Register, do_with::DoWith};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::context::Context;

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_do_with(
        &self,
        do_with: &DoWith,
        _span: &RelativeSpan,
        _register_id: ID<Register>,
    ) -> Result<(), UnrecoverableError> {
        let _ = self.nest_borrow_check_ir(do_with.do_ir_id()).await;

        Ok(())
    }
}
