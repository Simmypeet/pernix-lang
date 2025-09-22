use pernixc_arena::ID;
use pernixc_hash::HashSet;
use pernixc_ir::value::register::{Borrow, Register};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::{
    subset::{Changes, Context},
    Region,
};

impl<N: Normalizer> Context<'_, N> {
    pub(super) async fn get_changes_of_borrow(
        &self,
        borrow: &Borrow,
        span: &RelativeSpan,
        register_id: ID<Register>,
    ) -> Result<Changes, UnrecoverableError> {
        let region_in_addresss = self
            .borrowing_context
            .get_regions_in_address(&borrow.address, *span, true)
            .await?;

        let borrow_local_region = borrow.lifetime.into_inference().unwrap();

        Ok(Changes {
            subset_relations: {
                region_in_addresss
                    .into_iter()
                    .map(|x| {
                        (x, Region::Local(borrow_local_region), span.clone())
                    })
                    .collect()
            },
            borrow_created: Some((register_id, borrow_local_region)),
            overwritten_regions: HashSet::default(),
        })
    }
}
