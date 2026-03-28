use pernixc_ir::value::register::Variant;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{context::Context, diagnostic::Diagnostic, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_variant(
        &self,
        variant: &Variant,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        // Get well-formedness constraints
        let wf_constraints = variant
            .wf_check::<_, Diagnostic>(
                self.environment(),
                *span,
                &self.handler(),
            )
            .await?;

        // Get subtyping constraints and handle forall lifetime errors
        let subtype_result =
            variant.subtypes(self.values(), self.environment()).await.map_err(
                |x| x.report_as_type_check_overflow(*span, &self.handler()),
            )?;

        Ok(Changes::builder()
            .lifetime_constraints(wf_constraints)
            .subtype_result(subtype_result)
            .instantiation_span(*span)
            .handler(self.handler())
            .build())
    }
}
