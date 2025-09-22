use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::{register::Struct, TypeOf};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{fields::get_fields, variance::Variance};
use pernixc_term::instantiation::get_instantiation;
use pernixc_type_system::{
    normalizer::Normalizer, Succeeded, UnrecoverableError,
};

use crate::{
    subset::{Changes, Context},
    Region,
};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_struct(
        &self,
        struct_lit: &Struct,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let instantiation = self
            .tracked_engine()
            .get_instantiation(
                struct_lit.struct_id,
                struct_lit.generic_arguments.clone(),
            )
            .await?
            .unwrap();

        let fields =
            self.tracked_engine().get_fields(struct_lit.struct_id).await?;

        let mut lifetime_constraints = BTreeSet::new();

        // compare each values in the field to the struct's field type
        for field_id in fields.field_declaration_order.iter().copied() {
            let mut field_ty =
                fields.fields.get(field_id).unwrap().r#type.clone();

            instantiation.instantiate(&mut field_ty);

            let value_span = self
                .values()
                .span_of_value(
                    struct_lit.initializers_by_field_id.get(&field_id).unwrap(),
                )
                .unwrap();

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.values()
                    .type_of(
                        struct_lit
                            .initializers_by_field_id
                            .get(&field_id)
                            .unwrap(),
                        self.current_site(),
                        self.environment(),
                    )
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            value_span.clone(),
                            &self.handler(),
                        )
                    })?;

            lifetime_constraints.extend(value_constraints);

            let compatibility = self
                .environment()
                .subtypes(value_ty, field_ty, Variance::Covariant)
                .await
                .map_err(|x| {
                    x.report_as_type_check_overflow(
                        value_span.clone(),
                        &self.handler(),
                    )
                })?;

            // append the lifetime constraints
            if let Some(result) = compatibility {
                assert!(result.result.forall_lifetime_errors.is_empty());
                assert!(result
                    .result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(result.constraints.iter().cloned());
            }
        }

        let well_formed_lifetime_constraints = self
            .environment()
            .wf_check(
                struct_lit.struct_id,
                *span,
                &instantiation,
                false,
                &self.handler(),
            )
            .await?;

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .chain(well_formed_lifetime_constraints)
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
