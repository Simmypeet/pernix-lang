use std::collections::{hash_map::Entry, HashMap, HashSet};

use pernixc_abort::Abort;
use pernixc_arena::ID;
use pernixc_component::function_signature::FunctionSignature;
use pernixc_handler::Handler;
use pernixc_ir::{
    control_flow_graph::Point, value::register::Register, Representation,
};
use pernixc_semantic::{component::SymbolKind, diagnostic::Diagnostic, GlobalID};
use pernixc_term::{
    r#type::Type, variance::Variance, visitor::RecursiveIterator, Model,
};
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};

use crate::{Model as BorrowModel, Region, UniversalRegion};

/// The cache for `type_of` register operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegisterInfo {
    pub r#type: Type<BorrowModel>,
    pub regions: HashSet<Region>,
    pub assigned_at: Point<BorrowModel>,
}

/// A map of register id to its type and regions.
#[derive(
    Debug, Clone, PartialEq, Eq, derive_more::Deref, derive_more::DerefMut,
)]
pub struct RegisterInfos(HashMap<ID<Register<BorrowModel>>, RegisterInfo>);

impl RegisterInfos {
    /// Creates a new register info cache for all register assignments in the
    /// IR.
    pub fn new(
        ir: &Representation<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Self, Abort> {
        let mut cache = HashMap::new();

        for (point, inst) in ir.control_flow_graph.blocks().iter().flat_map(
            |(block_id, block)| {
                block
                    .instructions()
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, inst)| {
                        inst.as_register_assignment().map(|x| (idx, x))
                    })
                    .map(move |(idx, inst)| {
                        (Point { instruction_index: idx, block_id }, inst)
                    })
            },
        ) {
            let register = ir.values.registers.get(inst.id).unwrap();
            let ty = ir
                .values
                .type_of_register(inst.id, current_site, environment)
                .map_err(|x| {
                    x.report_overflow(|x| {
                        x.report_as_type_calculating_overflow(
                            register.span.clone().unwrap(),
                            handler,
                        )
                    })
                })?;

            cache.insert(inst.id, RegisterInfo {
                regions: RecursiveIterator::new(&ty.result)
                    .filter_map(|x| x.0.into_lifetime().ok())
                    .filter_map(|x| (*x).try_into().ok())
                    .collect(),
                r#type: ty.result,
                assigned_at: point,
            });
        }

        // make sure every registers are included in the cache
        assert_eq!(cache.len(), ir.values.registers.len());

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
    pub fn new(
        ir: &Representation<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    ) -> Result<Self, Abort> {
        let mut region_cache = HashMap::<_, Variance>::new();

        for alloca in ir.values.allocas.items() {
            for (region, term_locations) in
                RecursiveIterator::new(&alloca.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| Region::try_from(*x).ok())
                        .map(|y| (y, x.1))
                })
            {
                let variance = environment.get_variance_of(
                    &alloca.r#type,
                    Variance::Covariant,
                    term_locations.into_iter(),
                )?;

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
        if !environment
            .table()
            .get::<SymbolKind>(current_site)
            .has_function_signature()
        {
            // return just variacne from variables
            return Ok(Self(region_cache));
        };

        let function_signature =
            environment.table().query::<FunctionSignature>(current_site)?;

        for parameter in function_signature.parameters.items() {
            for (region, term_locations) in
                RecursiveIterator::new(&parameter.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| {
                            // convert the lifetime to a region
                            UniversalRegion::try_from(*x)
                                .ok()
                                .map(Region::Universal)
                        })
                        .map(|y| (y, x.1))
                })
            {
                let variance = environment.get_variance_of(
                    &BorrowModel::from_default_type(parameter.r#type.clone()),
                    Variance::Covariant,
                    term_locations.into_iter(),
                )?;

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
