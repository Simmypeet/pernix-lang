use pernixc_hash::FxHashSet;
use pernixc_ir::{
    instruction::Store,
    value::{TypeOf, register::subtype::Subtype},
};
use pernixc_term::visitor::RecursiveIterator;
use pernixc_type_system::{
    Succeeded, UnrecoverableError, constraints::Constraints,
    normalizer::Normalizer,
};

use crate::{
    Region,
    context::Context,
    diagnostic::{Diagnostic, SubtypeForallLifetimeError},
    subset::Changes,
};

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

        let (constraints, address_ty) = match subtype_result {
            Subtype::Succeeded(constraints) => {
                // Get address type for overwritten regions
                let Succeeded { result: address_ty, .. } = self
                    .values()
                    .type_of(&store_inst.address, self.environment())
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            store_inst.span,
                            &self.handler(),
                        )
                    })?;
                (constraints, address_ty)
            }
            Subtype::Incompatible { found_type, expected_type } => {
                panic!(
                    "in borrow checking, all subtyping should be valid: found \
                     {found_type:?}, expected {expected_type:?}"
                );
            }
            Subtype::ForallLifetimeError { found_type, expected_type } => {
                self.handler().receive(Diagnostic::SubtypeForallLifetimeError(
                    SubtypeForallLifetimeError {
                        span: store_inst.span,
                        found_type,
                        expected_type,
                    },
                ));
                // Get address type for overwritten regions even on error
                let Succeeded { result: address_ty, .. } = self
                    .values()
                    .type_of(&store_inst.address, self.environment())
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            store_inst.span,
                            &self.handler(),
                        )
                    })?;
                (Constraints::default(), address_ty)
            }
        };

        Ok(Changes {
            subset_relations: constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, store_inst.span))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: RecursiveIterator::new(&address_ty)
                .filter_map(|x| x.0.into_lifetime().ok())
                .filter_map(|x| Region::try_from(x.clone()).ok())
                .collect::<FxHashSet<_>>(),
        })
    }
}
