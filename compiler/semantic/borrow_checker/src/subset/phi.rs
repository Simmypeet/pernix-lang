use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::Phi;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_phi(
        &self,
        phi: &Phi,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let mut constraints = BTreeSet::new();
        for value in phi.incoming_values.values() {
            self.subtypes_value(
                phi.r#type.clone(),
                value,
                Variance::Covariant,
                &mut constraints,
            )
            .await?;
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
