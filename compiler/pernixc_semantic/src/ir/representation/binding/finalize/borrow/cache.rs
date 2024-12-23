use std::collections::{hash_map::Entry, HashMap, HashSet};

use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        control_flow_graph::Point,
        representation::{
            borrow::{Model as BorrowModel, Region, UniversalRegion},
            Representation,
        },
        value::register::Register,
    },
    symbol::{table, CallableID, GlobalID},
    type_system::{
        environment::Environment,
        normalizer::Normalizer,
        observer::Observer,
        term::{lifetime::Lifetime, r#type::Type, Term},
        variance::Variance,
        visitor::RecursiveIterator,
    },
};

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
    pub fn new<S: table::State>(
        ir: &Representation<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Self, TypeSystemOverflow<ir::Model>> {
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
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: register.span.clone(),
                    overflow_error: overflow_error.into_overflow().unwrap(),
                })?;

            cache.insert(inst.id, RegisterInfo {
                regions: RecursiveIterator::new(&ty.result)
                    .filter_map(|x| x.0.into_lifetime().ok())
                    .filter_map(|x| x.clone().try_into().ok())
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
    pub fn new<S: table::State>(
        ir: &Representation<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Self, TypeSystemOverflow<ir::Model>> {
        let mut region_cache = HashMap::<_, Variance>::new();

        for alloca in ir.values.allocas.items() {
            for (region, term_locations) in
                RecursiveIterator::new(&alloca.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| Region::try_from(x.clone()).ok())
                        .map(|y| (y, x.1))
                })
            {
                let variance = environment
                    .get_variance_of(
                        &alloca.r#type,
                        Variance::Covariant,
                        term_locations.into_iter(),
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: alloca.span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

                // insert the variance into the cache or combine it with the
                // existing variance
                match region_cache.entry(region) {
                    Entry::Occupied(occupied_entry) => {
                        *occupied_entry.into_mut() =
                            occupied_entry.get().combine(variance);
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(variance);
                    }
                }
            }
        }

        // add the variance of the parameters
        let Ok(callable_id) = CallableID::try_from(current_site) else {
            // return just variacne from variables
            return Ok(Self(region_cache));
        };

        let callable = environment.table().get_callable(callable_id).unwrap();

        for parameter in callable.parameters().items() {
            for (region, term_locations) in
                RecursiveIterator::new(&parameter.r#type).filter_map(|x| {
                    x.0.into_lifetime()
                        .ok()
                        .and_then(|x| {
                            // convert the lifetime to a region
                            Some(Region::Universal(match x {
                                Lifetime::Static => UniversalRegion::Static,
                                Lifetime::Parameter(member_id) => {
                                    UniversalRegion::LifetimeParameter(
                                        *member_id,
                                    )
                                }

                                Lifetime::Inference(never) => match *never {},

                                Lifetime::Forall(_) | Lifetime::Error(_) => {
                                    return None
                                }
                            }))
                        })
                        .map(|y| (y, x.1))
                })
            {
                let variance = environment
                    .get_variance_of(
                        &Type::from_default_model(parameter.r#type.clone()),
                        Variance::Covariant,
                        term_locations.into_iter(),
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: parameter.span.clone().expect(
                            "should have a span, we a building the ir from \
                             the source code",
                        ),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

                // insert the variance into the cache or combine it with the
                // existing variance
                match region_cache.entry(region) {
                    Entry::Occupied(occupied_entry) => {
                        *occupied_entry.into_mut() =
                            occupied_entry.get().combine(variance);
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
