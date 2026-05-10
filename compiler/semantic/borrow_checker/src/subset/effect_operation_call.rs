use pernixc_ir::value::register::EffectOperationCall;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{context::Context, diagnostic::Diagnostic, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_effect_operation_call(
        &self,
        effect_operation_call: &EffectOperationCall,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let wf_constraints = effect_operation_call
            .wf_check::<_, Diagnostic>(
                self.environment(),
                *span,
                &self.handler(),
            )
            .await?;

        let subtype_result = effect_operation_call
            .subtypes(self.values(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(*span, &self.handler())
            })?;

        Ok(Changes::builder()
            .lifetime_constraints(wf_constraints)
            .subtype_result(subtype_result)
            .instantiation_span(*span)
            .handler(self.handler())
            .build())
    }
}
