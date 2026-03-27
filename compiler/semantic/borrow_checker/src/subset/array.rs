use pernixc_hash::FxHashSet;
use pernixc_ir::value::register::{Array, subtype::Subtype};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{
    UnrecoverableError, constraints::Constraints, normalizer::Normalizer,
};

use crate::{
    Region,
    context::Context,
    diagnostic::{Diagnostic, SubtypeForallLifetimeError},
    subset::Changes,
};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_array(
        &self,
        array: &Array,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let subtype_result =
            array.subtypes(self.values(), self.environment()).await.map_err(
                |x| x.report_as_type_check_overflow(*span, &self.handler()),
            )?;

        let lifetime_constraints = match subtype_result {
            Subtype::Succeeded(constraints) => constraints,
            Subtype::Incompatible { found_type, expected_type } => {
                panic!(
                    "in borrow checking, all subtyping should be valid: found \
                     {found_type:?}, expected {expected_type:?}"
                );
            }
            Subtype::ForallLifetimeError { found_type, expected_type } => {
                self.handler().receive(Diagnostic::SubtypeForallLifetimeError(
                    SubtypeForallLifetimeError {
                        span: *span,
                        found_type,
                        expected_type,
                    },
                ));
                Constraints::default()
            }
        };

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
