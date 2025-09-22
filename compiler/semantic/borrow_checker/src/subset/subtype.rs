use std::collections::BTreeSet;

use pernixc_ir::value::{TypeOf, Value};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::variance::Variance;
use pernixc_term::r#type::Type;
use pernixc_type_system::{
    lifetime_constraint::LifetimeConstraint, normalizer::Normalizer, Succeeded,
    UnrecoverableError,
};

use crate::context::Context;

impl<N: Normalizer> Context<'_, N> {
    pub async fn subtypes(
        &self,
        target: Type,
        source: Type,
        variance: Variance,
        span: RelativeSpan,
        set: &mut BTreeSet<LifetimeConstraint>,
    ) -> Result<(), UnrecoverableError> {
        let result = self
            .environment()
            .subtypes(target, source, variance)
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(span, &self.handler())
            })?;

        let Some(succeeded) = result else {
            panic!("in borrow checking, all subtyping should be valid");
        };

        assert!(succeeded.result.forall_lifetime_errors.is_empty());
        assert!(succeeded
            .result
            .forall_lifetime_instantiations
            .lifetimes_by_forall
            .is_empty());

        set.extend(succeeded.constraints.iter().cloned());

        Ok(())
    }

    pub async fn subtypes_value(
        &self,
        target: Type,
        source_value: &Value,
        variance: Variance,
        set: &mut BTreeSet<LifetimeConstraint>,
    ) -> Result<(), UnrecoverableError> {
        let span = self.values().span_of_value(source_value).unwrap().clone();
        let Succeeded { result: source, constraints } = self
            .values()
            .type_of(source_value, self.current_site(), self.environment())
            .await
            .map_err(|x| {
                x.report_as_type_check_overflow(span.clone(), &self.handler())
            })?;

        set.extend(constraints);

        self.subtypes(target, source, variance, span, set).await
    }
}
