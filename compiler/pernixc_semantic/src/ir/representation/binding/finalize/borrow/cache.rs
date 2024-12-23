use std::collections::{HashMap, HashSet};

use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        control_flow_graph::Point,
        representation::{
            borrow::{Model as BorrowModel, Region},
            Representation,
        },
        value::register::Register,
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment, normalizer::Normalizer, observer::Observer,
        term::r#type::Type, visitor::RecursiveIterator,
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
    /// Create a new empty register types cache.
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
