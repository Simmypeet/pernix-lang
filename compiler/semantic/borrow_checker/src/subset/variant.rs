use std::collections::BTreeSet;

use pernixc_hash::HashSet;
use pernixc_ir::value::register::Variant;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    variance::Variance, variant::get_variant_associated_type,
};
use pernixc_symbol::parent::get_parent;
use pernixc_target::Global;
use pernixc_term::instantiation::get_instantiation;
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{Region, context::Context, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_variant(
        &self,
        variant: &Variant,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let enum_id = Global::new(
            variant.variant_id.target_id,
            self.tracked_engine().get_parent(variant.variant_id).await.unwrap(),
        );

        let variant_sym = self
            .tracked_engine()
            .get_variant_associated_type(variant.variant_id)
            .await?;

        let instantiation = self
            .tracked_engine()
            .get_instantiation(enum_id, variant.generic_arguments.clone())
            .await?
            .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        // compare each values in the field to the struct's field type
        if let Some(mut associated_type) = variant_sym.as_deref().cloned() {
            instantiation.instantiate(&mut associated_type);

            let associated_value = variant.associated_value.as_ref().unwrap();

            self.subtypes_value(
                associated_type,
                associated_value,
                Variance::Covariant,
                &mut lifetime_constraints,
            )
            .await?;
        }

        // handle the constraints introduced by the outlive predicates of the
        // struct
        let well_fromed_lifetime_constraints = self
            .type_environment()
            .wf_check(enum_id, *span, &instantiation, false, &self.handler())
            .await?;

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .chain(well_fromed_lifetime_constraints)
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
