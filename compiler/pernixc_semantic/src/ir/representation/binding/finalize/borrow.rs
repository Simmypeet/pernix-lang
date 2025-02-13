use std::collections::HashSet;

use cache::{RegionVariances, RegisterInfos};
use pernixc_base::source_file::Span;
use transform::transform_to_borrow_model;

use crate::{
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::Address,
        representation::{
            binding::HandlerWrapper,
            borrow::{Model as BorrowModel, Region},
            Representation, Values,
        },
    },
    symbol::{table, ItemID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
        term::r#type::Qualifier, visitor::RecursiveIterator, Succeeded,
    },
};

mod cache;
mod check;
mod invalidate;
mod liveness;
mod local_region_generator;
mod subset;
mod transform;

impl Representation<ir::Model> {
    pub(super) fn borrow_check<S: table::State>(
        &self,
        current_site: ItemID,
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

        let register_infos =
            RegisterInfos::new(&ir, current_site, environment)?;
        let region_variances =
            RegionVariances::new(&ir, current_site, environment)?;
        let reachability = ir.control_flow_graph.reachability();

        let subset = subset::analyze(
            &ir,
            &register_infos,
            &region_variances,
            current_site,
            environment,
        )?;

        ir.borrow_check_internal(
            &subset,
            &register_infos,
            &region_variances,
            &reachability,
            current_site,
            environment,
            handler,
        )?;

        Ok(())
    }
}

#[cfg(test)]
mod test;
