use pernixc_ir::value::register::Phi;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{context::Context, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_phi(
        &self,
        phi: &Phi,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let subtype_result =
            phi.subtypes(self.values(), self.environment()).await.map_err(
                |x| x.report_as_type_check_overflow(*span, &self.handler()),
            )?;

        Ok(Changes::builder()
            .subtype_result(subtype_result)
            .instantiation_span(*span)
            .handler(self.handler())
            .build())
    }
}
