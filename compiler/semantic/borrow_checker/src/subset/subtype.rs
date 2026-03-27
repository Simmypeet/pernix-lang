use pernixc_ir::value::{TypeOf, Value};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    Succeeded, UnrecoverableError, constraints::Constraints,
    normalizer::Normalizer,
};

use crate::{
    context::Context,
    diagnostic::{Diagnostic, SubtypeForallLifetimeError},
};

impl<N: Normalizer> Context<'_, N> {
    pub async fn subtypes(
        &self,
        target: Type,
        source: Type,
        variance: Variance,
        span: RelativeSpan,
        set: &mut Constraints,
    ) -> Result<(), UnrecoverableError> {
        let result = self
            .type_environment()
            .subtypes(target.clone(), source.clone(), variance)
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(span, &self.handler())
            })?;

        let Some(succeeded) = result else {
            panic!("in borrow checking, all subtyping should be valid");
        };

        // Report forall lifetime errors to the handler
        for _ in &succeeded.result.forall_lifetime_errors {
            self.handler().receive(Diagnostic::SubtypeForallLifetimeError(
                SubtypeForallLifetimeError {
                    span,
                    found_type: source.clone(),
                    expected_type: target.clone(),
                },
            ));
        }

        assert!(
            succeeded
                .result
                .forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty()
        );

        set.extend(succeeded.constraints.iter().cloned());

        Ok(())
    }

    pub async fn subtypes_value(
        &self,
        target: Type,
        source_value: &Value,
        variance: Variance,
        set: &mut Constraints,
    ) -> Result<(), UnrecoverableError> {
        let span = *self.values().span_of_value(source_value);
        let Succeeded { result: source, constraints } = self
            .values()
            .type_of(source_value, self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(span, &self.handler())
            })?;

        set.extend(constraints);

        self.subtypes(target, source, variance, span, set).await
    }
}
