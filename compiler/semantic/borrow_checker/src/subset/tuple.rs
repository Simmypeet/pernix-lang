use pernixc_hash::FxHashSet;
use pernixc_ir::value::register::tuple::Tuple;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{Region, context::Context, diagnostic::Diagnostic, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_tuple(
        &self,
        tuple: &Tuple,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let lifetime_constraints = tuple
            .wf_check::<_, Diagnostic>(self.environment(), self.values(), &self.handler())
            .await?;

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
            overwritten_regions: FxHashSet::default(),
        })
    }
}
