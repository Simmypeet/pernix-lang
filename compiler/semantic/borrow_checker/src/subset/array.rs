use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::{register::Array, TypeOf};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_type_system::{
    normalizer::Normalizer, Succeeded, UnrecoverableError,
};

use crate::{
    subset::{Changes, Context},
    Region,
};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_array(
        &self,
        array: &Array,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let array_ty = array.element_type.clone();
        let mut lifetime_constraints = BTreeSet::new();

        for value in &array.elements {
            let value_span = *self.values().span_of_value(value).unwrap();

            let Succeeded { result: value_ty, constraints } = self
                .values()
                .type_of(value, self.current_site(), self.environment())
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        value_span.clone(),
                        &self.handler(),
                    )
                })?;

            lifetime_constraints.extend(constraints);

            let compatibility = self
                .environment()
                .subtypes(value_ty, array_ty.clone(), Variance::Covariant)
                .await
                .map_err(|x| {
                    x.report_as_type_check_overflow(
                        value_span.clone(),
                        &self.handler(),
                    )
                })?;

            // append the lifetime constraints
            if let Some(compat) = compatibility {
                assert!(compat.result.forall_lifetime_errors.is_empty());
                assert!(compat
                    .result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compat.constraints.iter().cloned());
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::default(),
        })
    }
}
