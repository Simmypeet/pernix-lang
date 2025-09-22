use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::Array;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_array(
        &self,
        array: &Array,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let array_ty = array.element_type.clone();
        let mut lifetime_constraints = BTreeSet::new();

        for value in &array.elements {
            self.subtypes_value(
                array_ty.clone(),
                value,
                Variance::Covariant,
                &mut lifetime_constraints,
            )
            .await?;
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, *span))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::default(),
        })
    }
}
