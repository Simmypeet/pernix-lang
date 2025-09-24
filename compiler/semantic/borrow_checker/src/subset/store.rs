use pernixc_hash::HashSet;
use pernixc_ir::{address::Address, instruction::Store, value::TypeOf};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::{r#type::Type, visitor::RecursiveIterator};
use pernixc_type_system::{
    normalizer::Normalizer, Succeeded, UnrecoverableError,
};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_store(
        &self,
        store_address: &Address,
        mut value_type: Succeeded<Type>,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let Succeeded { result: address_ty, constraints: address_constraints } =
            self.values()
                .type_of(store_address, self.current_site(), self.environment())
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        *span,
                        &self.handler(),
                    )
                })?;

        // get the compatibility constraints between the value and the address
        self.subtypes(
            value_type.result,
            address_ty.clone(),
            Variance::Covariant,
            *span,
            &mut value_type.constraints,
        )
        .await?;

        Ok(Changes {
            subset_relations: value_type
                .constraints
                .into_iter()
                .chain(address_constraints)
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, *span))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: RecursiveIterator::new(&address_ty)
                .filter_map(|x| x.0.into_lifetime().ok())
                .filter_map(|x| Region::try_from(x.clone()).ok())
                .collect::<HashSet<_>>(),
        })
    }

    pub(super) async fn get_changes_of_store_inst(
        &self,
        store_inst: &Store,
    ) -> Result<Changes, UnrecoverableError> {
        let value_ty = self
            .values()
            .type_of(&store_inst.value, self.current_site(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_calculating_overflow(
                    store_inst.span.unwrap(),
                    &self.handler(),
                )
            })?;

        self.get_changes_of_store(
            &store_inst.address,
            value_ty,
            &store_inst.span.unwrap(),
        )
        .await
    }
}
