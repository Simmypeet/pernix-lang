use std::collections::hash_map::Entry;

use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::{HashMap, HashSet};
use pernixc_ir::{
    control_flow_graph::Point,
    value::{register::Register, TypeOf},
    IR,
};
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::{parameter::get_parameters, variance::Variance};
use pernixc_symbol::kind::get_kind;
use pernixc_target::Global;
use pernixc_term::{r#type::Type, visitor::RecursiveIterator};
use pernixc_type_system::{
    environment::Environment, normalizer::Normalizer,
    variance::get_variance_of, UnrecoverableError,
};

use crate::{diagnostic::Diagnostic, Region, UniversalRegion};

/// The cache for `type_of` register operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegisterInfo {
    pub r#type: Type,
    pub regions: HashSet<Region>,
    pub assigned_at: Point,
}

/// A map of register id to its type and regions.
#[derive(
    Debug, Clone, PartialEq, Eq, derive_more::Deref, derive_more::DerefMut,
)]
pub struct RegisterInfos(HashMap<ID<Register>, RegisterInfo>);

impl RegisterInfos {
    /// Creates a new register info cache for all register assignments in the
    /// IR.
    pub async fn new<N: Normalizer>(
        ir: &IR,
        current_site: Global<pernixc_symbol::ID>,
        environment: &Environment<'_, N>,
        handler: &dyn Handler<Diagnostic>,
    ) -> Result<Self, UnrecoverableError> {
        let mut cache = HashMap::default();

        for (block_id, block) in ir.control_flow_graph.blocks().iter() {
            for (point, inst) in block
                .instructions()
                .iter()
                .enumerate()
                .filter_map(|(idx, inst)| {
                    inst.as_register_assignment().map(|x| (idx, x.id))
                })
                .map(move |(idx, inst)| {
                    (Point { instruction_index: idx, block_id }, inst)
                })
            {
                let register = ir.values.registers.get(inst).unwrap();
                let ty = ir
                    .values
                    .type_of(inst, current_site, environment)
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            register.span.unwrap(),
                            &handler,
                        )
                    })?;

                cache.insert(inst, RegisterInfo {
                    regions: RecursiveIterator::new(&ty.result)
                        .filter_map(|x| x.0.into_lifetime().ok())
                        .filter_map(|x| x.clone().try_into().ok())
                        .collect(),
                    r#type: ty.result,
                    assigned_at: point,
                });
            }
        }

        // make sure every registers are included in the cache
        assert_eq!(cache.len(), ir.values.registers.len(),);

        Ok(Self(cache))
    }
}

/// Caches for the variance of all regions in the IR.
#[derive(
    Debug, Clone, PartialEq, Eq, derive_more::Deref, derive_more::DerefMut,
)]
pub struct RegionVariances(HashMap<Region, Variance>);

impl RegionVariances {
    /// Create a new region variance cache for all regions appearing in the
    /// allocas and parameters in IR.
    #[allow(clippy::uninhabited_references)]
    pub async fn new(
        ir: &IR,
        current_site: Global<pernixc_symbol::ID>,
        tracked_engine: &TrackedEngine,
    ) -> Result<Self, UnrecoverableError> {
        let mut region_cache = HashMap::<_, Variance>::default();

        for alloca in ir.values.allocas.items() {
            for (region, term_locations) in
                RecursiveIterator::new(&alloca.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| Region::try_from(x.clone()).ok())
                        .map(|y| (y, x.1))
                })
            {
                let variance = tracked_engine
                    .get_variance_of(
                        &alloca.r#type,
                        Variance::Covariant,
                        term_locations.into_iter(),
                    )
                    .await?;

                // insert the variance into the cache or combine it with the
                // existing variance
                match region_cache.entry(region) {
                    Entry::Occupied(occupied_entry) => {
                        *occupied_entry.into_mut() =
                            occupied_entry.get().get_lower_bound(variance);
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(variance);
                    }
                }
            }
        }

        // add the variance of the parameters
        if !tracked_engine.get_kind(current_site).await.has_function_signature()
        {
            // return just variance from variables
            return Ok(Self(region_cache));
        }

        let function_signature =
            tracked_engine.get_parameters(current_site).await?;

        for parameter in function_signature.parameters.items() {
            for (region, term_locations) in
                RecursiveIterator::new(&parameter.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| {
                            // convert the lifetime to a region
                            UniversalRegion::try_from(x.clone())
                                .ok()
                                .map(Region::Universal)
                        })
                        .map(|y| (y, x.1))
                })
            {
                let variance = tracked_engine
                    .get_variance_of(
                        &parameter.r#type,
                        Variance::Covariant,
                        term_locations.into_iter(),
                    )
                    .await?;

                // insert the variance into the cache or combine it with the
                // existing variance
                match region_cache.entry(region) {
                    Entry::Occupied(occupied_entry) => {
                        *occupied_entry.into_mut() =
                            occupied_entry.get().get_lower_bound(variance);
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(variance);
                    }
                }
            }
        }

        Ok(Self(region_cache))
    }
}
