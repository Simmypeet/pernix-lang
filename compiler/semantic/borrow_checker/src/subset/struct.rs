use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::Struct;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{fields::get_fields, variance::Variance};
use pernixc_term::instantiation::get_instantiation;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{Region, context::Context, subset::Changes};

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
            .await
            .map_err(|_| UnrecoverableError::Reported)?;

        let fields =
            self.tracked_engine().get_fields(struct_lit.struct_id).await;

        let mut lifetime_constraints = BTreeSet::new();

        // compare each values in the field to the struct's field type
        for field_id in fields.field_declaration_order.iter().copied() {
            let mut field_ty =
                fields.fields.get(field_id).unwrap().r#type.clone();

            instantiation.instantiate(&mut field_ty);

            self.subtypes_value(
                field_ty,
                struct_lit.initializers_by_field_id.get(&field_id).unwrap(),
                Variance::Covariant,
                &mut lifetime_constraints,
            )
            .await?;
        }

        let well_formed_lifetime_constraints = self
            .type_environment()
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

                    Some((from, to, *span))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::default(),
        })
    }
}
