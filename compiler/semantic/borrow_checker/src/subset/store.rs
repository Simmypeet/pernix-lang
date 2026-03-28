use pernixc_ir::{instruction::Store, value::TypeOf};
use pernixc_type_system::{
    Succeeded, UnrecoverableError, normalizer::Normalizer,
};

use crate::{context::Context, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_store_inst(
        &self,
        store_inst: &Store,
    ) -> Result<Changes, UnrecoverableError> {
        let subtype_result = store_inst
            .subtypes(self.values(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(
                    store_inst.span,
                    &self.handler(),
                )
            })?;

        // Get address type for overwritten regions
        let Succeeded { result: address_ty, constraints } = self
            .values()
            .type_of(&store_inst.address, self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(
                    store_inst.span,
                    &self.handler(),
                )
            })?;

        Ok(Changes::builder()
            .subtype_result(subtype_result)
            .lifetime_constraints(constraints)
            .instantiation_span(store_inst.span)
            .overwritten_address_type(&address_ty)
            .handler(self.handler())
            .build())
    }
}
