use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::{register::Phi, TypeOf};
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
    pub(super) async fn get_changes_of_phi(
        &self,
        phi: &Phi,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let mut constraints = BTreeSet::new();
        for value in phi.incoming_values.values() {
            let Succeeded {
                result: value_ty,
                constraints: value_ty_constraints,
            } = self
                .values()
                .type_of(value, self.current_site(), self.environment())
                .await
                .map_err(|x| {
                    x.report_as_type_check_overflow(
                        *self.values().span_of_value(value).unwrap(),
                        &self.handler(),
                    )
                })?;

            constraints.extend(value_ty_constraints);

            let compatibility = self
                .environment()
                .subtypes(value_ty, phi.r#type.clone(), Variance::Covariant)
                .await
                .map_err(|x| {
                    x.report_as_type_check_overflow(
                        span.clone(),
                        &self.handler(),
                    )
                })?;

            if let Some(compatibility) = compatibility {
                assert!(compatibility
                    .result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());
                assert!(compatibility.result.forall_lifetime_errors.is_empty());

                constraints.extend(compatibility.constraints.iter().cloned());
            }
        }

        Ok(Changes {
            subset_relations: constraints
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
