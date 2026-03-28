use pernixc_ir::value::register::tuple::Tuple;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{context::Context, diagnostic::Diagnostic, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_tuple(
        &self,
        tuple: &Tuple,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let lifetime_constraints = tuple
            .wf_check::<_, Diagnostic>(
                self.environment(),
                self.values(),
                &self.handler(),
            )
            .await?;

        Ok(Changes::builder()
            .lifetime_constraints(lifetime_constraints)
            .instantiation_span(*span)
            .handler(self.handler())
            .build())
    }
}
