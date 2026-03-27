use pernixc_hash::FxHashSet;
use pernixc_ir::value::register::{FunctionCall, subtype::Subtype};
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
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_function_call(
        &self,
        function_call: &FunctionCall,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let mut lifetime_constraints = Constraints::new();

        // Get well-formedness constraints
        let wf_constraints = function_call
            .wf_check::<_, Diagnostic>(
                self.environment(),
                *span,
                &self.handler(),
            )
            .await?;
        lifetime_constraints.extend(wf_constraints);

        // Get subtyping constraints and handle forall lifetime errors
        let subtype_result = function_call
            .subtypes(self.values(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(*span, &self.handler())
            })?;

        match subtype_result {
            Subtype::Succeeded(constraints) => {
                lifetime_constraints.extend(constraints);
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
                        span: *span,
                        found_type,
                        expected_type,
                    },
                ));
            }
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
            overwritten_regions: FxHashSet::default(),
        })
    }
}
