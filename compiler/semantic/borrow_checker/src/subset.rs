use std::{collections::BTreeSet, ops::Deref};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_arena::ID;
use pernixc_hash::{HashMap, HashSet};
use pernixc_ir::{
    control_flow_graph::{Block, Point},
    instruction::{Instruction, RegisterAssignment},
    value::{
        register::{Assignment, Register},
        TypeOf, Value,
    },
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::{
    elided_lifetime::get_elided_lifetimes, return_type::get_return_type,
    variance::Variance,
};
use pernixc_symbol::{kind::get_kind, parent::scope_walker};
use pernixc_target::Global;
use pernixc_term::{
    generic_parameters::{get_generic_parameters, LifetimeParameterID},
    inference,
    lifetime::{ElidedLifetimeID, Lifetime},
    predicate::Predicate,
    visitor::RecursiveIterator,
};
use pernixc_transitive_closure::TransitiveClosure;
use pernixc_type_system::{
    environment::get_active_premise, normalizer::Normalizer, UnrecoverableError,
};

use crate::{context, NonStaticUniversalRegion, Region, UniversalRegion};

mod array;
mod borrow;
mod function_call;
mod phi;
mod store;
mod r#struct;
mod subtype;
mod tuple;
mod variant;

/// Represents a point in the control flow graph where the borrow checker
/// is considering the subset relation between regions.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum RegionPoint {
    /// Involving a particular point in the control flow graph.
    InBlock(Point),

    /// The moment before executing the first instruction in the block.
    EnteringBlock(ID<Block>),
}

impl RegionPoint {
    /// Gets the block id where the region is considered.
    pub const fn block_id(&self) -> ID<Block> {
        match self {
            Self::InBlock(point) => point.block_id,
            Self::EnteringBlock(block_id) => *block_id,
        }
    }
}

/// Represents a universal region at a particular point in the control flow
/// graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniversalRegionAt {
    /// The universal region at all points in the control flow graph.
    pub region: UniversalRegion,

    /// Specifies the point in the control flow graph where the region is
    pub point: RegionPoint,
}

/// Used for representing a region at a particular point in the control flow
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum RegionAt {
    /// The universal region at all points in the control flow graph.
    Universal(UniversalRegionAt),

    /// The local region at a particular point in the control flow graph.
    Local(LocalRegionAt),
}

/// Used for representing a local region at a particular point in the control
/// flow
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalRegionAt {
    pub local_region: inference::Variable<Lifetime>,

    /// Specifies the point in the control flow graph where the region is
    /// considered.
    ///
    /// If `None`, then the region doesn't care about the location in the
    /// control flow graph (location insensitive). The borrow checker should
    /// use the same region for all points in the control flow graph. These
    /// location insensitive regions are used for short-lived regions (mostly
    /// regions created by the register assignments unlike regions created from
    /// variables).
    pub point: Option<RegionPoint>,
}

impl RegionAt {
    pub const fn new_location_sensitive(
        local_region: Region,
        point: RegionPoint,
    ) -> Self {
        match local_region {
            Region::Universal(region) => {
                Self::Universal(UniversalRegionAt { region, point })
            }
            Region::Local(local_region) => {
                Self::Local(LocalRegionAt { local_region, point: Some(point) })
            }
        }
    }

    pub const fn new_location_insensitive(
        local_region: inference::Variable<Lifetime>,
    ) -> Self {
        Self::Local(LocalRegionAt { local_region, point: None })
    }

    pub const fn to_region(self) -> Region {
        match self {
            Self::Universal(region) => Region::Universal(region.region),
            Self::Local(local_region_at) => {
                Region::Local(local_region_at.local_region)
            }
        }
    }

    pub const fn region_point(&self) -> Option<&RegionPoint> {
        match self {
            Self::Universal(region) => Some(&region.point),
            Self::Local(local_region) => local_region.point.as_ref(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RegionChangeLog {
    pub updated_at_instruction_indices: HashMap<ID<Block>, Vec<usize>>,
}

impl RegionChangeLog {
    /// Gets the [`RegionPoint`] where the region is most updated.
    pub fn get_most_updated_point(&self, point: Point) -> RegionPoint {
        let updated_points = self
            .updated_at_instruction_indices
            .get(&point.block_id)
            .unwrap_or_else(|| {
                panic!("no changes for block {:?}", point.block_id)
            });

        // use the binary search to find the most updated point where
        // `point.index >= updated_point`

        updated_points
            .binary_search_by(|x| x.cmp(&point.instruction_index))
            .map_or_else(
                |err| {
                    if err == 0 {
                        RegionPoint::EnteringBlock(point.block_id)
                    } else {
                        RegionPoint::InBlock(Point {
                            block_id: point.block_id,
                            instruction_index: updated_points[err - 1],
                        })
                    }
                },
                |ok| {
                    RegionPoint::InBlock(Point {
                        block_id: point.block_id,
                        instruction_index: updated_points[ok],
                    })
                },
            )
    }
}

/// Contains the direct subset relations between regions. It's the result of
/// the subset analysis with no optimization.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
#[allow(clippy::type_complexity)]
pub struct Intermediate {
    /// The accumulated subset relations between regions since the beginning
    /// of the control flow graph.
    subset_relations: HashSet<(RegionAt, RegionAt, Option<RelativeSpan>)>,

    /// Represents the [`Borrow`] register assignment created so far.
    ///
    /// The key is the register ID of the borrow assignment, and the value is
    /// the region where the borrow is created.
    #[get = "pub"]
    created_borrows:
        HashMap<ID<Register>, (inference::Variable<Lifetime>, Point)>,

    /// Maps the region to the block ids that the region first appears. (mostly
    /// is the entry block)
    entry_block_ids_by_universal_regions: HashMap<UniversalRegion, ID<Block>>,
}

/// A struct used for building a subset relations between regions in the borrow
/// checker.
///
/// Each region will have each state for each point in the control flow graph.
/// The state of the region at a particular point will flow to the next point in
/// the control flow graph. However, we'll not keep the state of the region at
/// all points in the control flow graph. Instead, we'll keep the state of the
/// region at the points where the region is changed (e.g., created, borrowed,
/// added to subset relation).
pub struct Builder<'a, N: Normalizer> {
    context: &'a context::Context<'a, N>,

    /// A map between the region and the instruction index where the region
    /// was last changed.
    ///
    /// The "long-lived" regions (created by allocas or universal regions) will
    /// present in this map. The value is the point in the control flow
    /// graph where the region is most updated.
    latest_change_points_by_region: HashMap<Region, Option<usize>>,
}

impl<N: Normalizer> Clone for Builder<'_, N> {
    fn clone(&self) -> Self {
        Self {
            context: self.context,
            latest_change_points_by_region: self
                .latest_change_points_by_region
                .clone(),
        }
    }
}

/// Represents the changes made to the subset relations between regions at
/// a particular point in the control flow graph.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Changes {
    /// Creates a new subset relation between two regions.
    subset_relations: HashSet<(Region, Region, RelativeSpan)>,

    /// The borrow is created at the given region.
    borrow_created: Option<(ID<Register>, inference::Variable<Lifetime>)>,

    /// The region is abourt to be rewritten.
    ///
    /// # Example
    ///
    /// ```pnx
    /// let a: &'0 i32: ..;
    /// let b: &'1 i32 = a;
    ///
    /// a = b;
    /// ```
    ///
    /// The `a = b` will produce an `OverwrittenRegion` change, and the region
    /// that appears here is `'0`.
    overwritten_regions: HashSet<Region>,
}

impl<N: Normalizer> Builder<'_, N> {
    pub async fn get_regions_in_generic_parameters(
        &self,
        regions: &mut HashSet<Region>,
    ) -> Result<(), UnrecoverableError> {
        regions.insert(Region::Universal(UniversalRegion::Static));

        let mut scope_walker = self
            .context
            .environment()
            .tracked_engine()
            .scope_walker(self.context.current_site());

        while let Some(x) = scope_walker.next().await {
            let global_id =
                Global::new(self.context.current_site().target_id, x);

            let symbol_kind =
                self.context.tracked_engine().get_kind(global_id).await;

            if symbol_kind.has_generic_parameters() {
                regions.extend(
                    self.context
                        .tracked_engine()
                        .get_generic_parameters(global_id)
                        .await?
                        .lifetime_order()
                        .iter()
                        .copied()
                        .map(|x| {
                            Region::Universal(UniversalRegion::NonStatic(
                                NonStaticUniversalRegion::Named(
                                    LifetimeParameterID {
                                        parent_id: global_id,
                                        id: x,
                                    },
                                ),
                            ))
                        }),
                );
            }

            if symbol_kind.has_elided_lifetimes() {
                regions.extend(
                    self.context
                        .tracked_engine()
                        .get_elided_lifetimes(global_id)
                        .await?
                        .ids()
                        .map(|x| {
                            Region::Universal(UniversalRegion::NonStatic(
                                NonStaticUniversalRegion::Elided(
                                    ElidedLifetimeID {
                                        parent_id: global_id,
                                        id: x,
                                    },
                                ),
                            ))
                        }),
                );
            }
        }

        Ok(())
    }
    /// Returns a list of new region introduced by the scope push instruction.
    ///
    /// This function returns all the regions that are created by the variables
    /// declared in the scope push instruction.
    pub async fn get_new_regions(
        &self,
        instruction: &Instruction,
    ) -> Result<HashSet<Region>, UnrecoverableError> {
        match instruction {
            Instruction::ScopePush(scope_push) => {
                let mut regions = HashSet::default();

                for alloca in
                    self.context.ir().values.allocas.iter().filter_map(|x| {
                        (x.1.declared_in_scope_id == scope_push.0)
                            .then_some(x.1)
                    })
                {
                    regions.extend(
                        RecursiveIterator::new(&alloca.r#type).filter_map(
                            |x| {
                                x.0.into_lifetime()
                                    .ok()
                                    .and_then(|y| y.as_inference().copied())
                                    .map(Region::Local)
                            },
                        ),
                    );
                }

                // we'll insert a universal region for the root scope
                if scope_push.0 == self.context.ir().scope_tree.root_scope_id()
                {
                    self.get_regions_in_generic_parameters(&mut regions)
                        .await?;
                }

                Ok(regions)
            }

            Instruction::ScopePop(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::RegisterDiscard(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => Ok(HashSet::default()),
        }
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    pub async fn get_removing_regions(
        &self,
        instruction: &Instruction,
    ) -> Result<HashSet<Region>, UnrecoverableError> {
        match instruction {
            Instruction::ScopePop(scope_pop) => {
                let mut regions = HashSet::default();

                for alloca in
                    self.context.ir().values.allocas.iter().filter_map(|x| {
                        (x.1.declared_in_scope_id == scope_pop.0).then_some(x.1)
                    })
                {
                    regions.extend(
                        RecursiveIterator::new(&alloca.r#type).filter_map(
                            |x| {
                                x.0.into_lifetime()
                                    .ok()
                                    .and_then(|y| y.as_inference().copied())
                                    .map(Region::Local)
                            },
                        ),
                    );
                }

                // we'll insert a universal region for the root scope
                if scope_pop.0 == self.context.ir().scope_tree.root_scope_id() {
                    regions.insert(Region::Universal(UniversalRegion::Static));

                    self.get_regions_in_generic_parameters(&mut regions)
                        .await
                        .unwrap();
                }

                Ok(regions)
            }

            Instruction::RegisterDiscard(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::ScopePush(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => Ok(HashSet::default()),
        }
    }

    pub async fn get_changes_of_return_type(
        &self,
        register_id: ID<Register>,
    ) -> Result<
        Option<HashSet<(Region, Region, RelativeSpan)>>,
        UnrecoverableError,
    > {
        if let Some(return_inst) = self
            .context
            .control_flow_graph()
            .blocks()
            .items()
            .filter_map(|x| x.terminator().as_ref().and_then(|x| x.as_return()))
            .find(|x| x.value == Value::Register(register_id))
        {
            let return_ty = self
                .context
                .tracked_engine()
                .get_return_type(self.context.current_site())
                .await?;

            let mut constraints = BTreeSet::new();
            self.context
                .subtypes_value(
                    return_ty.deref().clone(),
                    &Value::Register(register_id),
                    Variance::Covariant,
                    &mut constraints,
                )
                .await?;

            return Ok(Some(
                constraints
                    .into_iter()
                    .filter_map(|x| {
                        let x = x.into_lifetime_outlives().ok()?;

                        let from = Region::try_from(x.operand).ok()?;
                        let to = Region::try_from(x.bound).ok()?;

                        Some((from, to, return_inst.span.unwrap()))
                    })
                    .collect::<HashSet<_>>(),
            ));
        }

        Ok(None)
    }

    pub async fn get_changes_of_register_assignment(
        &self,
        register_assignment: &RegisterAssignment,
    ) -> Result<Changes, UnrecoverableError> {
        let register = self
            .context
            .values()
            .registers
            .get(register_assignment.id)
            .unwrap();

        // if the register assignment will be used as a return value,
        // then, compute the subset relation against the type of the
        // return type.
        let extra_relations =
            self.get_changes_of_return_type(register_assignment.id).await?;

        let changes = match &register.assignment {
            Assignment::VariantNumber(_)
            | Assignment::Cast(_)
            | Assignment::Binary(_)
            | Assignment::Prefix(_)
            | Assignment::Load(_) => Ok(Changes::default()),

            Assignment::Tuple(tuple) => {
                self.context
                    .get_changes_of_tuple(
                        tuple,
                        register.span.as_ref().unwrap(),
                    )
                    .await
            }
            Assignment::Borrow(borrow) => {
                self.context
                    .get_changes_of_borrow(
                        borrow,
                        register.span.as_ref().unwrap(),
                        register_assignment.id,
                    )
                    .await
            }
            Assignment::FunctionCall(function_call) => {
                self.context
                    .get_changes_of_function_call(
                        function_call,
                        register.span.as_ref().unwrap(),
                    )
                    .await
            }
            Assignment::Struct(struct_lit) => {
                self.context
                    .get_changes_of_struct(
                        struct_lit,
                        register.span.as_ref().unwrap(),
                    )
                    .await
            }
            Assignment::Variant(variant) => {
                self.context
                    .get_changes_of_variant(
                        variant,
                        register.span.as_ref().unwrap(),
                    )
                    .await
            }
            Assignment::Array(array) => {
                self.context
                    .get_changes_of_array(
                        array,
                        register.span.as_ref().unwrap(),
                    )
                    .await
            }
            Assignment::Phi(phi) => {
                self.context
                    .get_changes_of_phi(phi, register.span.as_ref().unwrap())
                    .await
            }
        }?;

        Ok(Changes {
            subset_relations: changes
                .subset_relations
                .into_iter()
                .chain(extra_relations.into_iter().flatten())
                .collect(),
            borrow_created: changes.borrow_created,
            overwritten_regions: changes.overwritten_regions,
        })
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    #[allow(clippy::too_many_lines)]
    pub async fn get_changes(
        &self,
        instruction: &Instruction,
    ) -> Result<Changes, UnrecoverableError> {
        match instruction {
            Instruction::Store(store) => {
                self.context.get_changes_of_store_inst(store).await
            }
            Instruction::RegisterAssignment(register_assignment) => {
                self.get_changes_of_register_assignment(register_assignment)
                    .await
            }
            Instruction::TuplePack(tuple_pack) => {
                let tuple_ty = self
                    .context
                    .values()
                    .type_of(
                        &tuple_pack.tuple_address,
                        self.context.current_site(),
                        self.context.environment(),
                    )
                    .await
                    .map_err(|x| {
                        x.report_as_type_calculating_overflow(
                            tuple_pack.packed_tuple_span.unwrap(),
                            &self.context.handler(),
                        )
                    })?;

                self.context
                    .get_changes_of_store(
                        &tuple_pack.store_address,
                        tuple_ty.map(|x| {
                            x.into_tuple()
                                .unwrap()
                                .elements
                                .into_iter()
                                .find_map(|x| x.is_unpacked.then_some(x.term))
                                .unwrap()
                        }),
                        tuple_pack.packed_tuple_span.as_ref().unwrap(),
                    )
                    .await
            }
            Instruction::RegisterDiscard(_)
            | Instruction::ScopePush(_)
            | Instruction::ScopePop(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Drop(_) => Ok(Changes::default()),
        }
    }
}

impl<N: Normalizer> Builder<'_, N> {
    pub async fn walk_instruction(
        &mut self,
        instruction: &Instruction,
        instruction_point: Point,
        subset_result: &mut Intermediate,
    ) -> Result<(), UnrecoverableError> {
        for region in self.get_new_regions(instruction).await? {
            assert!(self
                .latest_change_points_by_region
                .insert(region, None)
                .is_none());

            if let Region::Universal(universal_region) = region {
                assert!(subset_result
                    .entry_block_ids_by_universal_regions
                    .insert(universal_region, instruction_point.block_id)
                    .is_none());
            }
        }

        // gets the changes made by the instruction
        let changes = self.get_changes(instruction).await?;
        self.handle_chages(changes, subset_result, instruction_point);

        for region in self.get_removing_regions(instruction).await? {
            assert!(self
                .latest_change_points_by_region
                .remove(&region)
                .is_some());
        }

        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    pub fn handle_chages(
        &mut self,
        changes: Changes,
        subset_result: &mut Intermediate,
        instruction_point: Point,
    ) {
        if let Some((borrow_register_id, local_region)) = changes.borrow_created
        {
            assert!(!self
                .latest_change_points_by_region
                .contains_key(&Region::Local(local_region)));
            assert!(subset_result
                .created_borrows
                .insert(borrow_register_id, (local_region, instruction_point))
                .is_none());
        }

        // add subset relations
        for (from, to, span) in changes.subset_relations {
            let latest_from =
                self.latest_change_points_by_region.get(&from).copied();
            let latest_to =
                self.latest_change_points_by_region.get(&to).copied();

            let from_region_at = match from {
                Region::Universal(universal_region) => {
                    RegionAt::new_location_sensitive(
                        Region::Universal(universal_region),
                        RegionPoint::InBlock(instruction_point),
                    )
                }
                Region::Local(id) => RegionAt::Local(LocalRegionAt {
                    local_region: id,
                    point: latest_from
                        .map(|_| RegionPoint::InBlock(instruction_point)),
                }),
            };
            let to_region_at = match to {
                Region::Universal(universal_region) => {
                    RegionAt::new_location_sensitive(
                        Region::Universal(universal_region),
                        RegionPoint::InBlock(instruction_point),
                    )
                }
                Region::Local(id) => RegionAt::Local(LocalRegionAt {
                    local_region: id,
                    point: latest_to
                        .map(|_| RegionPoint::InBlock(instruction_point)),
                }),
            };

            // add subset relation
            subset_result.subset_relations.insert((
                from_region_at,
                to_region_at,
                Some(span),
            ));

            // flows the previous region state to the current one
            let mut flow =
                |region_at: RegionAt,
                 lastest_region_update: Option<Option<usize>>| {
                    // if the region is overwritten, then the region state
                    // will not flow to the current point
                    if let (false, Some(latest_updated_inst_index)) = (
                        changes
                            .overwritten_regions
                            .contains(&region_at.to_region())
                            && !region_at.to_region().is_universal(),
                        lastest_region_update,
                    ) {
                        let flow_from = RegionAt::new_location_sensitive(
                            region_at.to_region(),
                            latest_updated_inst_index.map_or_else(
                                || {
                                    RegionPoint::EnteringBlock(
                                        instruction_point.block_id,
                                    )
                                },
                                |x| {
                                    RegionPoint::InBlock(Point {
                                        instruction_index: x,
                                        block_id: instruction_point.block_id,
                                    })
                                },
                            ),
                        );

                        let flow_to = region_at;

                        // flows state to current
                        match self
                            .context
                            .region_variances()
                            .get(&region_at.to_region())
                            .copied()
                            .unwrap_or(Variance::Covariant)
                        {
                            Variance::Covariant => {
                                subset_result
                                    .subset_relations
                                    .insert((flow_from, flow_to, None));
                            }
                            Variance::Contravariant => {
                                subset_result
                                    .subset_relations
                                    .insert((flow_to, flow_from, None));
                            }
                            Variance::Invariant => {
                                subset_result
                                    .subset_relations
                                    .insert((flow_from, flow_to, None));
                                subset_result
                                    .subset_relations
                                    .insert((flow_to, flow_from, None));
                            }
                            Variance::Bivariant => {}
                        }
                    }
                };

            flow(from_region_at, latest_from);
            flow(to_region_at, latest_to);

            // update the latest location
            if latest_from.is_some() {
                assert!(self
                    .latest_change_points_by_region
                    .insert(from, Some(instruction_point.instruction_index))
                    .is_some());
            }
            if latest_to.is_some() {
                assert!(self
                    .latest_change_points_by_region
                    .insert(to, Some(instruction_point.instruction_index))
                    .is_some());
            }
        }
    }

    pub async fn walk_block(
        &mut self,
        block_id: ID<Block>,
        subset_result: &mut Intermediate,
    ) -> Result<(), UnrecoverableError> {
        let block =
            self.context.control_flow_graph().blocks().get(block_id).unwrap();

        for (index, instruction) in block.instructions().iter().enumerate() {
            self.walk_instruction(
                instruction,
                Point { block_id, instruction_index: index },
                subset_result,
            )
            .await?;
        }

        Ok(())
    }
}

#[derive(Clone)]
#[allow(clippy::type_complexity, clippy::struct_field_names)]
struct Walker<'a, N: Normalizer> {
    context: &'a context::Context<'a, N>,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id: HashMap<ID<Block>, Option<Builder<'a, N>>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_regions_by_block_id:
        HashMap<ID<Block>, (ID<Block>, HashSet<Region>)>,
}

impl<'a, N: Normalizer> Walker<'a, N> {
    pub fn tracked_engine(&self) -> &'a TrackedEngine {
        self.context.environment().tracked_engine()
    }

    pub fn current_site(&self) -> Global<pernixc_symbol::ID> {
        self.context.current_site()
    }
}

impl<'a, N: Normalizer> Walker<'a, N> {
    #[allow(clippy::too_many_lines)]
    pub async fn walk_block(
        &mut self,
        block_id: ID<Block>,
        subset_result: &mut Intermediate,
    ) -> Result<Option<Builder<'a, N>>, UnrecoverableError> {
        // skip if already processed
        if let Some(walk_result) = self.walk_results_by_block_id.get(&block_id)
        {
            return Ok(walk_result.clone());
        }

        // mark as processing
        self.walk_results_by_block_id.insert(block_id, None);

        let block =
            self.context.control_flow_graph().get_block(block_id).unwrap();

        let mut builder = if block.is_entry() {
            assert!(block.predecessors().is_empty());

            let builder = Builder {
                context: self.context,
                latest_change_points_by_region: HashMap::default(),
            };

            let predicates = self
                .tracked_engine()
                .get_active_premise(self.current_site())
                .await?;

            let mut adding_edges = HashSet::default();

            for predicate in &predicates.predicates {
                match predicate {
                    Predicate::LifetimeOutlives(outlives) => {
                        let (Some(operand), Some(bound)) = (
                            outlives.operand.try_into().ok(),
                            outlives.bound.try_into().ok(),
                        ) else {
                            continue;
                        };

                        adding_edges.insert((
                            RegionAt::Universal(UniversalRegionAt {
                                region: operand,
                                point: RegionPoint::EnteringBlock(block_id),
                            }),
                            RegionAt::Universal(UniversalRegionAt {
                                region: bound,
                                point: RegionPoint::EnteringBlock(block_id),
                            }),
                        ));
                    }

                    Predicate::TypeOutlives(outlives) => {
                        let Some(bound) = outlives.bound.try_into().ok() else {
                            continue;
                        };

                        for operand in RecursiveIterator::new(&outlives.operand)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| (*x).try_into().ok())
                        {
                            adding_edges.insert((
                                RegionAt::Universal(UniversalRegionAt {
                                    region: operand,
                                    point: RegionPoint::EnteringBlock(block_id),
                                }),
                                RegionAt::Universal(UniversalRegionAt {
                                    region: bound,
                                    point: RegionPoint::EnteringBlock(block_id),
                                }),
                            ));
                        }
                    }

                    _ => {}
                }
            }

            for (from, to) in adding_edges {
                subset_result.subset_relations.insert((from, to, None));
            }

            builder
        } else {
            let predecessors =
                block.predecessors().iter().copied().collect::<Vec<_>>();

            let mut flowing_subset_builders = Vec::new();
            let mut looped_block_ids = Vec::new();

            // gets the subset builder from the predecessors. we'll flow
            // the state of the regions from the predecessors to the current
            // block.
            for predecessor_id in predecessors.iter().copied() {
                if let Some(builder) =
                    Box::pin(self.walk_block(predecessor_id, subset_result))
                        .await?
                {
                    flowing_subset_builders.push((predecessor_id, builder));
                } else {
                    looped_block_ids.push(predecessor_id);
                }
            }

            if flowing_subset_builders.is_empty() {
                // try again later
                self.walk_results_by_block_id.remove(&block_id);

                return Ok(None);
            }

            let builder = Builder {
                context: self.context,
                latest_change_points_by_region: flowing_subset_builders[0]
                    .1
                    .latest_change_points_by_region
                    .keys()
                    .map(|x| (*x, None))
                    .collect(),
            };

            // flow the state of the regions from the predecessors to the
            // current block
            for (from_block_id, merging_builder) in flowing_subset_builders {
                assert_eq!(
                    merging_builder.latest_change_points_by_region.len(),
                    builder.latest_change_points_by_region.len()
                );

                for (region, latest_point) in
                    merging_builder.latest_change_points_by_region
                {
                    assert!(builder
                        .latest_change_points_by_region
                        .contains_key(&region));

                    let from_region_at = RegionAt::new_location_sensitive(
                        region,
                        latest_point.map_or_else(
                            || RegionPoint::EnteringBlock(from_block_id),
                            |x| {
                                RegionPoint::InBlock(Point {
                                    block_id: from_block_id,
                                    instruction_index: x,
                                })
                            },
                        ),
                    );
                    let to_region_at = RegionAt::new_location_sensitive(
                        region,
                        RegionPoint::EnteringBlock(block_id),
                    );

                    // taken account the variance
                    match self
                        .context
                        .region_variances()
                        .get(&region)
                        .copied()
                        .unwrap_or(Variance::Covariant)
                    {
                        Variance::Covariant => {
                            subset_result.subset_relations.insert((
                                from_region_at,
                                to_region_at,
                                None,
                            ));
                        }
                        Variance::Contravariant => {
                            subset_result.subset_relations.insert((
                                to_region_at,
                                from_region_at,
                                None,
                            ));
                        }
                        Variance::Invariant => {
                            subset_result.subset_relations.insert((
                                from_region_at,
                                to_region_at,
                                None,
                            ));
                            subset_result.subset_relations.insert((
                                to_region_at,
                                from_region_at,
                                None,
                            ));
                        }
                        Variance::Bivariant => {}
                    }
                }
            }

            // mark the looped block
            for looped in looped_block_ids {
                self.target_regions_by_block_id.insert(
                    looped,
                    (
                        block_id,
                        builder
                            .latest_change_points_by_region
                            .keys()
                            .copied()
                            .collect(),
                    ),
                );
            }

            builder
        };

        builder.walk_block(block_id, subset_result).await?;

        // flows the state of the regions back to the predecessors
        if let Some((to_block_id, regions)) =
            self.target_regions_by_block_id.get(&block_id)
        {
            assert_eq!(
                regions.len(),
                builder.latest_change_points_by_region.len()
            );

            for (region, latest_point) in
                &builder.latest_change_points_by_region
            {
                assert!(regions.contains(region));

                let from_region_at = RegionAt::new_location_sensitive(
                    *region,
                    latest_point.map_or_else(
                        || RegionPoint::EnteringBlock(block_id),
                        |x| {
                            RegionPoint::InBlock(Point {
                                block_id,
                                instruction_index: x,
                            })
                        },
                    ),
                );
                let to_region_at = RegionAt::new_location_sensitive(
                    *region,
                    RegionPoint::EnteringBlock(*to_block_id),
                );

                // taken account the variance
                match self
                    .context
                    .region_variances()
                    .get(region)
                    .copied()
                    .unwrap_or(Variance::Covariant)
                {
                    Variance::Covariant => {
                        subset_result.subset_relations.insert((
                            from_region_at,
                            to_region_at,
                            None,
                        ));
                    }
                    Variance::Contravariant => {
                        subset_result.subset_relations.insert((
                            to_region_at,
                            from_region_at,
                            None,
                        ));
                    }
                    Variance::Invariant => {
                        subset_result.subset_relations.insert((
                            from_region_at,
                            to_region_at,
                            None,
                        ));
                        subset_result.subset_relations.insert((
                            to_region_at,
                            from_region_at,
                            None,
                        ));
                    }
                    Variance::Bivariant => {}
                }
            }
        }

        // mark as done
        assert!(self
            .walk_results_by_block_id
            .insert(block_id, Some(builder.clone()))
            .unwrap()
            .is_none());

        Ok(Some(builder))
    }
}

/// The final result of the subset analysis. It allows querying the subset
/// relation between regions at any given point in the control flow graph.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
#[allow(clippy::type_complexity)]
pub struct Subset {
    indices_by_region_at: HashMap<RegionAt, usize>,
    region_ats_by_index: Vec<RegionAt>,
    transitive_closure: TransitiveClosure,

    #[get = "pub"]
    direct_subset_relations:
        HashSet<(RegionAt, RegionAt, Option<RelativeSpan>)>,

    #[get = "pub"]
    created_borrows:
        HashMap<ID<Register>, (inference::Variable<Lifetime>, Point)>,

    /// Maps the region to the block ids that the region first appears. (mostly
    /// is the entry block)
    entry_block_ids_by_universal_regions: HashMap<UniversalRegion, ID<Block>>,

    change_logs_by_region: HashMap<Region, RegionChangeLog>,
    active_region_sets_by_block_id: HashMap<ID<Block>, HashSet<Region>>,
    location_insensitive_regions: HashSet<inference::Variable<Lifetime>>,
}

impl Subset {
    pub fn get_universal_regions_containing(
        &self,
        mut to_region_at: RegionAt,
    ) -> HashSet<UniversalRegion> {
        let to_region = to_region_at.to_region();
        let region_point = match &mut to_region_at {
            RegionAt::Universal(universal) => Some(&mut universal.point),
            RegionAt::Local(region) => region.point.as_mut(),
        };

        if let Some(region_point) = region_point {
            if let RegionPoint::InBlock(in_block) = region_point {
                // update to the correct pos
                *region_point = self.change_logs_by_region[&to_region]
                    .get_most_updated_point(*in_block);
            }
        }

        self.entry_block_ids_by_universal_regions
            .iter()
            .map(|(region, block_id)| {
                // track from the root
                RegionAt::new_location_sensitive(
                    Region::Universal(*region),
                    RegionPoint::EnteringBlock(*block_id),
                )
            })
            .filter_map(|x| {
                self.transitive_closure
                    .has_path(
                        *self.indices_by_region_at.get(&x)?,
                        self.indices_by_region_at[&to_region_at],
                    )
                    .unwrap()
                    .then_some(x.as_universal().unwrap().region)
            })
            .collect()
    }

    /// Gets a list of region that contains the given borrow at the given point.
    pub fn get_regions_containing_borrow(
        &self,
        borrow_register_id: ID<Register>,
        point: Point,
    ) -> HashSet<Region> {
        let block_id = point.block_id;
        let borrow_region = RegionAt::new_location_insensitive(
            self.created_borrows.get(&borrow_register_id).unwrap().0,
        );

        self.location_insensitive_regions
            .iter()
            .copied()
            .filter_map(|x| {
                // start with the location insensitive regions
                self.transitive_closure
                    .has_path(
                        self.indices_by_region_at[&borrow_region],
                        self.indices_by_region_at
                            [&RegionAt::new_location_insensitive(x)],
                    )
                    .unwrap()
                    .then_some(Region::Local(x))
            })
            .chain(
                // then check active regions that require location sensitivity
                self.active_region_sets_by_block_id[&block_id]
                    .iter()
                    .copied()
                    .filter_map(|x| {
                        let most_updated_point = self.change_logs_by_region[&x]
                            .get_most_updated_point(point);
                        let region_at = RegionAt::new_location_sensitive(
                            x,
                            most_updated_point,
                        );

                        self.transitive_closure
                            .has_path(
                                self.indices_by_region_at[&borrow_region],
                                *self.indices_by_region_at.get(&region_at)?,
                            )
                            .unwrap()
                            .then_some(x)
                    }),
            )
            .collect::<HashSet<_>>()
    }
}

#[allow(clippy::too_many_lines)]
pub async fn analyze<N: Normalizer>(
    context: &context::Context<'_, N>,
) -> Result<Subset, UnrecoverableError> {
    let mut context = Walker {
        context,
        walk_results_by_block_id: HashMap::default(),
        target_regions_by_block_id: HashMap::default(),
    };

    let all_block_ids =
        context.context.control_flow_graph().blocks().ids().collect::<Vec<_>>();

    let mut subset_result = Intermediate {
        subset_relations: HashSet::default(),
        created_borrows: HashMap::default(),
        entry_block_ids_by_universal_regions: HashMap::default(),
    };

    for block_id in all_block_ids.iter().copied() {
        context.walk_block(block_id, &mut subset_result).await?;
    }

    // make sure all blocks are processed
    assert!(context.walk_results_by_block_id.iter().all(|(_, x)| x.is_some()));

    // populate the region and assign the index
    let mut region_ats_by_index = Vec::new();
    let mut indices_by_region_at = HashMap::default();

    let mut all_regions = HashSet::default();
    let mut location_insensitive_regions = HashSet::default();
    let mut active_region_sets_by_block_id =
        HashMap::<_, HashSet<_>>::default();
    let mut change_logs_by_region = HashMap::<_, RegionChangeLog>::default();

    for region_at in subset_result
        .subset_relations
        .iter()
        .flat_map(|(from, to, _)| [*from, *to])
        .chain(
            subset_result
                .created_borrows
                .iter()
                .map(|x| RegionAt::new_location_insensitive(x.1 .0)),
        )
    {
        all_regions.insert(region_at);

        if let Some(region_point) = region_at.region_point() {
            active_region_sets_by_block_id
                .entry(region_point.block_id())
                .or_default()
                .insert(region_at.to_region());

            match region_point {
                RegionPoint::InBlock(point) => {
                    change_logs_by_region
                        .entry(region_at.to_region())
                        .or_default()
                        .updated_at_instruction_indices
                        .entry(point.block_id)
                        .or_default()
                        .push(point.instruction_index);
                }
                RegionPoint::EnteringBlock(id) => {
                    change_logs_by_region
                        .entry(region_at.to_region())
                        .or_default()
                        .updated_at_instruction_indices
                        .entry(*id)
                        .or_default();
                }
            }
        } else {
            location_insensitive_regions
                .insert(region_at.into_local().unwrap().local_region);
        }
    }

    // should at least have a key present
    for block_id in all_block_ids {
        active_region_sets_by_block_id.entry(block_id).or_default();
    }

    for indices in change_logs_by_region
        .values_mut()
        .flat_map(|x| x.updated_at_instruction_indices.values_mut())
    {
        indices.sort_unstable();
        indices.dedup();
    }

    for region_at in all_regions.iter().copied() {
        let index = region_ats_by_index.len();

        region_ats_by_index.push(region_at);
        assert!(indices_by_region_at.insert(region_at, index).is_none());
    }

    let transitive_closure = TransitiveClosure::new(
        subset_result.subset_relations.iter().map(|(from, to, _)| {
            (indices_by_region_at[from], indices_by_region_at[to])
        }),
        all_regions.len(),
        true,
    )
    .expect("failed to create transitive closure");

    Ok(Subset {
        indices_by_region_at,
        region_ats_by_index,
        entry_block_ids_by_universal_regions: subset_result
            .entry_block_ids_by_universal_regions,
        transitive_closure,
        direct_subset_relations: subset_result.subset_relations,
        created_borrows: subset_result.created_borrows,
        location_insensitive_regions,
        active_region_sets_by_block_id,
        change_logs_by_region,
    })
}
