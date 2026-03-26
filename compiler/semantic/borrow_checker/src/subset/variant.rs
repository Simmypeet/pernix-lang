
use pernixc_hash::HashSet;
use pernixc_type_system::constraints::Constraints;

use pernixc_ir::value::register::Variant;
use pernixc_lexical::tree::RelativeSpan;
use pernixc_semantic_element::{
    variance::Variance, variant::get_variant_associated_type,
};
use pernixc_type_system::{UnrecoverableError, normalizer::Normalizer};

use crate::{Region, context::Context, subset::Changes};

impl<N: Normalizer> Context<'_, N> {
    #[allow(clippy::too_many_lines)]
    pub(super) async fn get_changes_of_variant(
        &self,
        variant: &Variant,
        span: &RelativeSpan,
    ) -> Result<Changes, UnrecoverableError> {
        let enum_id = variant.parent_enum_id(self.tracked_engine()).await;

        let variant_sym = self
            .tracked_engine()
            .get_variant_associated_type(variant.variant_id())
            .await;

        let instantiation =
            variant.create_instantiation(self.tracked_engine()).await;

        let mut lifetime_constraints = Constraints::new();

        // compare each values in the field to the struct's field type
        if let Some(variant_sym) = variant_sym {
            let mut associated_type = (*variant_sym).clone();
            instantiation.instantiate(&mut associated_type);

            self.subtypes_value(
                associated_type,
                variant.associated_value().unwrap(),
                Variance::Covariant,
                &mut lifetime_constraints,
            )
            .await?;
        }

        // handle the constraints introduced by the outlive predicates of the
        // struct
        let well_fromed_lifetime_constraints = self
            .type_environment()
            .wf_check_instantiation(
                enum_id,
                span,
                &instantiation,
                &self.handler(),
            )
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
