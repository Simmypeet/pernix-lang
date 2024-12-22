use cache::RegisterTypes;
use transform::transform_to_borrow_model;

use crate::{
    error::TypeSystemOverflow,
    ir::{
        self,
        representation::{
            binding::HandlerWrapper, borrow::Model as BorrowModel,
            Representation, Values,
        },
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
    },
};

mod cache;
mod liveness;
mod local_region_generator;
mod subset;
mod transform;

impl Representation<ir::Model> {
    pub(super) fn borrow_check<S: table::State>(
        &self,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        // NOTE: we clone the whole ir here, is there a better way to do this?
        let (ir, _) =
            transform_to_borrow_model(self.clone(), environment.table());

        let register_types =
            RegisterTypes::new(&ir.values, current_site, environment)?;

        let subset = subset::subset_analysis(
            &ir,
            &register_types,
            current_site,
            environment,
        )?;

        Ok(())
    }
}

#[cfg(test)]
mod test;
