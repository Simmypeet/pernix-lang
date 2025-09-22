use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::{register::Tuple, TypeOf};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_term::predicate::{self, Predicate};
use pernixc_type_system::{
    normalizer::Normalizer, Succeeded, UnrecoverableError,
};

use crate::{context::Context, subset::Changes, Region};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_tuple(
        &self,
        tuple: &Tuple,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let mut lifetime_constraints = BTreeSet::new();
        for element in tuple.elements.iter().filter(|x| x.is_unpacked) {
            let Succeeded { result: ty, constraints } = self
                .values()
                .type_of(
                    &element.value,
                    self.current_site(),
                    self.environment(),
                )
                .await
                .map_err(|x| {
                    x.report_as_type_calculating_overflow(
                        *span,
                        &self.handler(),
                    )
                })?;

            lifetime_constraints.extend(constraints);

            let predicate = Predicate::TupleType(predicate::Tuple(ty));
            lifetime_constraints.extend(
                self.environment()
                    .predicate_satisfied(
                        predicate,
                        *self.values().span_of_value(&element.value).unwrap(),
                        None,
                        false,
                        &self.handler(),
                    )
                    .await?,
            );
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
