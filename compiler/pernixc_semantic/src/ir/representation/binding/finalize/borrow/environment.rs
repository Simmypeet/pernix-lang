use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Debug,
    sync::Arc,
};

use getset::CopyGetters;
use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    liveness, local_region_generator::LocalRegionGenerator,
    transitive_closure::TransitiveClosure,
};
use crate::{
    arena::{Key, ID},
    error::{
        self, AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, OverflowOperation,
        TypeSystemOverflow, UnsatisfiedPredicate, Usage,
        VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::Point,
        instruction::Instruction,
        representation::{
            borrow::{
                LocalRegion, Model as BorrowModel, Region, UniversalRegion,
            },
            Values,
        },
        value::register::Register,
    },
    symbol::{
        table::{self, Table},
        GlobalID, LifetimeParameterID,
    },
    type_system::{
        environment::Environment as TyEnvironment,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{Outlives, Predicate},
        sub_term::TermLocation,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Qualifier, Type},
            Term,
        },
        visitor::{self, Recursive, RecursiveIterator},
        Compute, Satisfied,
    },
};

/// Contains the information about all the possible regions in the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegionInfo {
    generated_local_regions: usize,
    indices_by_univseral_region: HashMap<UniversalRegion, usize>,
    universal_regions_by_index: HashMap<usize, UniversalRegion>,
}

impl RegionInfo {
    /// Creates a new region info from the given generator and the current site.
    ///
    /// The current site is used to find the lifetimes that are declared in the
    /// generic parameters of the current site.
    pub fn new(
        generator: LocalRegionGenerator,
        current_site: GlobalID,
        table: &Table<impl table::State>,
    ) -> Self {
        let mut indices_by_univseral_region = HashMap::new();
        let mut universal_regions_by_index = HashMap::new();

        let mut starting_universal_regions_index = generator.generated_count();

        for global_id in table.scope_walker(current_site).unwrap() {
            let Some(generic_id) = global_id.try_into().ok() else {
                continue;
            };

            let generic_sym = table.get_generic(generic_id).unwrap();

            for lifetime in generic_sym
                .generic_declaration()
                .parameters
                .lifetime_order()
                .iter()
                .copied()
            {
                assert!(indices_by_univseral_region
                    .insert(
                        UniversalRegion::LifetimeParameter(
                            LifetimeParameterID {
                                parent: generic_id,
                                id: lifetime,
                            }
                        ),
                        starting_universal_regions_index,
                    )
                    .is_none());
                assert!(universal_regions_by_index
                    .insert(
                        starting_universal_regions_index,
                        UniversalRegion::LifetimeParameter(
                            LifetimeParameterID {
                                parent: generic_id,
                                id: lifetime,
                            }
                        ),
                    )
                    .is_none());

                starting_universal_regions_index += 1;
            }
        }

        // finally, insert the static region
        indices_by_univseral_region
            .insert(UniversalRegion::Static, starting_universal_regions_index);
        universal_regions_by_index
            .insert(starting_universal_regions_index, UniversalRegion::Static);

        Self {
            generated_local_regions: generator.generated_count(),
            indices_by_univseral_region,
            universal_regions_by_index,
        }
    }

    /// The total number of regions in the program.
    pub fn total_regions(&self) -> usize {
        self.generated_local_regions + self.indices_by_univseral_region.len()
    }

    /// Translates the given region into the index of the transitive closure
    pub fn get_transitive_closure_index(&self, region: Region) -> usize {
        match region {
            Region::Local(local_region) => local_region.into_index(),
            Region::Universal(universal_region) => *self
                .indices_by_univseral_region
                .get(&universal_region)
                .unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Contains<'a> {
    regions: &'a HashSet<Region>,
    contains: bool,
}

impl Recursive<'_, Lifetime<BorrowModel>> for Contains<'_> {
    fn visit(
        &mut self,
        term: &'_ Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Some(region) = term.clone().try_into().ok() else {
            return true;
        };

        self.contains |= self.regions.contains(&region);

        !self.contains
    }
}

impl Recursive<'_, Type<BorrowModel>> for Contains<'_> {
    fn visit(
        &mut self,
        _: &'_ Type<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        !self.contains
    }
}

impl Recursive<'_, Constant<BorrowModel>> for Contains<'_> {
    fn visit(
        &mut self,
        _: &'_ Constant<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        !self.contains
    }
}

impl<'a> Contains<'a> {
    pub const fn new(regions: &'a HashSet<Region>) -> Self {
        Self { regions, contains: false }
    }

    pub fn contains(
        ty: &Type<BorrowModel>,
        regions: &'a HashSet<Region>,
    ) -> bool {
        let mut contains = Self::new(regions);
        visitor::accept_recursive(ty, &mut contains);
        contains.contains
    }
}

/// The cache for the `type_of` register.
#[derive(Debug, Clone, PartialEq, Eq)]
struct RegisterTypeCache {
    r#type: Type<BorrowModel>,
    regions: HashSet<Region>,
}

/// Contains all the information related to borrows and accesses used for
/// borrow checking.
///
/// This is used for processing whether the borrows are valid or not.
///
/// ## Subset Relation
///
/// The key [`Environment::region_subset_relations`] represents the subset while
/// the values represent the superset.
#[derive(CopyGetters)]
#[allow(clippy::struct_field_names)]
pub struct Environment<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    region_info: Arc<RegionInfo>, // immutable data
    subset_relations: TransitiveClosure,

    attached_borrows: HashMap<ID<Register<BorrowModel>>, ID<LocalRegion>>,
    reversed_attached_borrows:
        HashMap<ID<LocalRegion>, ID<Register<BorrowModel>>>,

    register_type_cache:
        Arc<HashMap<ID<Register<BorrowModel>>, RegisterTypeCache>>,

    #[get_copy = "pub"]
    current_site: GlobalID,
    #[get_copy = "pub"]
    representation: &'a ir::Representation<BorrowModel>,
    #[get_copy = "pub"]
    ty_environment: &'a TyEnvironment<'a, BorrowModel, S, N, O>,
}

impl<
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Debug for Environment<'_, S, N, O>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Environment")
            .field("region_info", &self.region_info)
            .field("subset_relations", &self.subset_relations)
            .field("attached_borrows", &self.attached_borrows)
            .field("reversed_attached_borrows", &self.reversed_attached_borrows)
            .field("register_type_cache", &self.register_type_cache)
            .finish()
    }
}

impl<
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Clone for Environment<'_, S, N, O>
{
    fn clone(&self) -> Self {
        Self {
            region_info: self.region_info.clone(),
            subset_relations: self.subset_relations.clone(),
            attached_borrows: self.attached_borrows.clone(),
            reversed_attached_borrows: self.reversed_attached_borrows.clone(),
            register_type_cache: self.register_type_cache.clone(),
            current_site: self.current_site,
            representation: self.representation,
            ty_environment: self.ty_environment,
        }
    }
}

/// Gets all the lifetimes included in the given address.
pub fn get_lifetimes_in_address<S: table::State>(
    mut address: &Address<BorrowModel>,
    span: &Span,
    values: &Values<BorrowModel>,
    current_site: GlobalID,
    ty_environment: &TyEnvironment<
        BorrowModel,
        S,
        impl Normalizer<BorrowModel, S>,
        impl Observer<BorrowModel, S>,
    >,
) -> Result<HashSet<Lifetime<BorrowModel>>, TypeSystemOverflow<ir::Model>> {
    let mut lifetimes = HashSet::new();
    let address_ty = values
        .type_of_address(address, current_site, ty_environment)
        .map_err(|x| TypeSystemOverflow::<ir::Model> {
            operation: OverflowOperation::TypeOf,
            overflow_span: span.clone(),
            overflow_error: x.into_overflow().unwrap(),
        })?
        .result;

    lifetimes.extend(
        RecursiveIterator::new(&address_ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .cloned(),
    );

    loop {
        match address {
            Address::Memory(_) => break,

            Address::Field(field) => {
                address = &field.struct_address;
            }
            Address::Tuple(tuple) => {
                address = &tuple.tuple_address;
            }
            Address::Index(index) => {
                address = &index.array_address;
            }
            Address::Variant(variant) => {
                address = &variant.enum_address;
            }

            Address::Reference(reference) => {
                let pointee_ty = values
                    .type_of_address(
                        &reference.reference_address,
                        current_site,
                        ty_environment,
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?
                    .result;
                let pointee_reference_ty = pointee_ty.as_reference().unwrap();

                lifetimes.insert(pointee_reference_ty.lifetime.clone());

                if pointee_reference_ty.qualifier == Qualifier::Immutable {
                    break;
                }

                address = &reference.reference_address;
            }
        }
    }

    Ok(lifetimes)
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Environment<'a, S, N, O>
{
    pub fn new(
        region_info: Arc<RegionInfo>,
        representation: &'a ir::Representation<BorrowModel>,
        current_site: GlobalID,
        ty_environment: &'a TyEnvironment<'a, BorrowModel, S, N, O>,
    ) -> Result<Self, TypeSystemOverflow<ir::Model>> {
        Ok(Self {
            subset_relations: TransitiveClosure::new(
                std::iter::empty(),
                region_info.total_regions(),
            ),

            region_info,

            attached_borrows: HashMap::new(),
            reversed_attached_borrows: HashMap::new(),

            register_type_cache: Arc::new(
                representation
                    .values
                    .registers
                    .iter()
                    .map(|(id, x)| {
                        Ok((id, {
                            let ty = representation
                                .values
                                .type_of_register(
                                    id,
                                    current_site,
                                    ty_environment,
                                )
                                .map_err(|overflow_error| {
                                    TypeSystemOverflow::<ir::Model> {
                                        operation: OverflowOperation::TypeOf,
                                        overflow_span: x.span.clone(),
                                        overflow_error: overflow_error
                                            .into_overflow()
                                            .unwrap(),
                                    }
                                })?;

                            RegisterTypeCache {
                                regions: RecursiveIterator::new(&ty.result)
                                    .filter_map(|x| x.0.into_lifetime().ok())
                                    .filter_map(|x| x.clone().try_into().ok())
                                    .collect(),
                                r#type: ty.result,
                            }
                        }))
                    })
                    .collect::<Result<_, TypeSystemOverflow<ir::Model>>>()?,
            ),

            current_site,
            ty_environment,
            representation,
        })
    }

    /// Attaches a borrow to the local region
    pub fn attach_borrow(
        &mut self,
        borrow: ID<Register<BorrowModel>>,
        region: ID<LocalRegion>,
    ) {
        assert!(self.attached_borrows.insert(borrow, region).is_none());
        assert!(self
            .reversed_attached_borrows
            .insert(region, borrow)
            .is_none());
    }

    fn invalidate_borrow(
        &self,
        borrow: ID<Register<BorrowModel>>,
        access_span: &Span,
        mut exit: impl FnMut(&Instruction<BorrowModel>, Point<BorrowModel>) -> bool,
        point: Point<BorrowModel>,
        handler_fn: impl Fn(Usage),
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let borrow_region = self.attached_borrows.get(&borrow).unwrap();

        // get the invalidated local regions
        let invalidated_local_regions = self
            .subset_relations
            .reachable_from(borrow_region.into_index())
            .filter_map(|x| {
                (!self.region_info.universal_regions_by_index.contains_key(&x))
                    .then_some(Region::Local(ID::from_index(x)))
            })
            .collect::<HashSet<_>>();

        // these are the allocas that contain the invalidated local
        // regions, check if the allocas are *used*
        let checking_allocas =
            self.representation.values.allocas.iter().filter(|(_, x)| {
                Contains::contains(&x.r#type, &invalidated_local_regions)
            });

        for (alloca_id, alloca) in checking_allocas {
            let accesses = liveness::get_live_addresses(
                Memory::Alloca(alloca_id),
                &alloca.r#type,
                point,
                &mut exit,
                self.representation,
                self.ty_environment,
            )
            .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: access_span.clone(),
                overflow_error,
            })?;

            for (address, access_kind) in accesses {
                if get_lifetimes_in_address(
                    &address,
                    access_span,
                    &self.representation.values,
                    self.current_site,
                    self.ty_environment,
                )?
                .into_iter()
                .any(|x| {
                    let Some(x) = Region::try_from(x).ok() else {
                        return false;
                    };

                    invalidated_local_regions.contains(&x)
                }) {
                    handler_fn(access_kind.into_normal().map_or(
                        Usage::Drop,
                        |(_, span)| Usage::Local {
                            access_span: span,
                            in_loop: false,
                        },
                    ));
                }
            }
        }

        let checking_registers =
            self.register_type_cache.iter().filter(|(_, x)| {
                invalidated_local_regions
                    .iter()
                    .any(|region| x.regions.contains(region))
            });

        for (register, _) in checking_registers {
            let live_spans = liveness::live_register_spans(
                *register,
                point,
                self.representation,
            );

            for span in live_spans {
                handler_fn(Usage::Local { access_span: span, in_loop: false });
            }
        }

        let mut universal_lifetimes = Vec::new();

        for universal_region in
            self.region_info.indices_by_univseral_region.keys()
        {
            if self.subset_relations.has_path(
                self.attached_borrows.get(&borrow).unwrap().into_index(),
                self.region_info.get_transitive_closure_index(
                    Region::Universal(*universal_region),
                ),
            ) {
                universal_lifetimes.push(*universal_region);
            }
        }

        // if there's any universal lifetimes that could use the
        // invalidated borrow, report an error
        if !universal_lifetimes.is_empty() {
            handler_fn(Usage::ByUniversalRegions(universal_lifetimes));
        }

        Ok(())
    }

    /// Handles the move of the memory. This will invalidate the borrows that
    /// are attached to the memory.
    pub fn handle_move(
        &mut self,
        moved_address: &Address<BorrowModel>,
        move_span: &Span,
        point: Point<BorrowModel>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for borrow in self.attached_borrows.keys().copied() {
            let borrow_register =
                self.representation.values.registers.get(borrow).unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate = moved_address
                .is_child_of(&borrow_assignment.address)
                || borrow_assignment.address.is_child_of(moved_address);

            if !should_invalidate {
                continue;
            }

            let span = borrow_register.span.clone();

            self.invalidate_borrow(
                borrow,
                &span,
                |_, _| false,
                point,
                |borrow_usage| {
                    handler.receive(Box::new(MovedOutWhileBorrowed {
                        borrow_span: borrow_register.span.clone(),
                        usage: borrow_usage,
                        moved_out_span: move_span.clone(),
                    }));
                },
            )?;
        }

        Ok(())
    }

    /// Handles the drop of the memory. This will invalidate the borrows that
    /// are attached to the memory.
    pub fn handle_drop_memory(
        &mut self,
        drop: Memory<BorrowModel>,
        point: Point<BorrowModel>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for borrow in self.attached_borrows.keys() {
            let borrow_register =
                self.representation.values.registers.get(*borrow).unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate =
                borrow_assignment.address.is_child_of(&Address::Memory(drop))
                    && !borrow_assignment.address.is_behind_reference();

            if !should_invalidate {
                continue;
            }

            let dropped_scope = match drop {
                Memory::Parameter(_) => {
                    self.representation.scope_tree.root_scope_id()
                }
                Memory::Alloca(id) => {
                    self.representation
                        .values
                        .allocas
                        .get(id)
                        .unwrap()
                        .declared_in_scope_id
                }
            };

            let span = match drop {
                Memory::Parameter(id) => self
                    .ty_environment
                    .table()
                    .get_callable(self.current_site.try_into().unwrap())
                    .unwrap()
                    .parameters()
                    .get(id)
                    .unwrap()
                    .span
                    .clone()
                    .unwrap(),
                Memory::Alloca(id) => self
                    .representation
                    .values
                    .allocas
                    .get(id)
                    .unwrap()
                    .span
                    .clone(),
            };

            self.invalidate_borrow(
                *borrow,
                &span,
                |inst, _| {
                    // prevent multiple invalidations in loops
                    inst.as_scope_pop().map_or(false, |x| x.0 == dropped_scope)
                },
                point,
                |borrow_usage| match borrow_usage {
                    Usage::Local { access_span, .. } => {
                        handler.receive(Box::new(
                            VariableDoesNotLiveLongEnough::<ir::Model> {
                                variable_span: span.clone(),
                                for_lifetime: None,
                                instantiation_span: access_span,
                            },
                        ));
                    }

                    Usage::ByUniversalRegions(_) | Usage::Drop => {}
                },
            )?;
        }

        Ok(())
    }

    /// Handles an access to the given address.
    pub fn handle_access(
        &mut self,
        address: &Address<BorrowModel>,
        access_qualifier: Qualifier,
        span: &Span,
        point: Point<BorrowModel>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        /*
        1. check if the accesses use any "not live long enough" variables
        2. check if the accesses use any invalidated borrows.
        3. invalidates the borrows inccured by the access
            - if it's a mutable access, invalidate all the borrows
            - if it's an immutable access, invalidate the mutable borrows
         */

        // invalidate the borrows
        for borrow in self.attached_borrows.keys().copied() {
            let borrow_register =
                self.representation.values.registers.get(borrow).unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate = access_qualifier == Qualifier::Mutable
                || borrow_assignment.qualifier == Qualifier::Mutable;

            // if the address overlaps
            let is_subaddress = address.is_child_of(&borrow_assignment.address)
                || borrow_assignment.address.is_child_of(address);

            if should_invalidate && is_subaddress {
                self.invalidate_borrow(
                    borrow,
                    span,
                    |_, _| false,
                    point,
                    |borrow_usage| match borrow_assignment.qualifier {
                        Qualifier::Immutable => {
                            handler.receive(Box::new(
                                MutablyAccessWhileImmutablyBorrowed {
                                    mutable_access_span: span.clone(),
                                    immutable_borrow_span: Some(
                                        borrow_register.span.clone(),
                                    ),
                                    usage: borrow_usage,
                                },
                            ));
                        }
                        Qualifier::Mutable => {
                            handler.receive(Box::new(
                                AccessWhileMutablyBorrowed {
                                    access_span: span.clone(),
                                    mutable_borrow_span: Some(
                                        borrow_register.span.clone(),
                                    ),
                                    borrow_usage,
                                },
                            ));
                        }
                    },
                )?;
            }
        }

        Ok(())
    }

    pub fn merge(&mut self, other: &Self) {
        for (other_from, other_to) in other
            .subset_relations
            .direct_adjacency()
            .iter()
            .flat_map(|(from, tos)| tos.iter().map(|to| (*from, *to)))
        {
            self.subset_relations.add_edge(other_from, other_to);
        }

        for (other_borrow, other_region) in &other.attached_borrows {
            match self.attached_borrows.entry(*other_borrow) {
                Entry::Occupied(occupied_entry) => {
                    assert_eq!(occupied_entry.get(), other_region);
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(*other_region);
                }
            }
        }

        for (other_region, other_borrow) in &other.reversed_attached_borrows {
            match self.reversed_attached_borrows.entry(*other_region) {
                Entry::Occupied(occupied_entry) => {
                    assert_eq!(occupied_entry.get(), other_borrow);
                }
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(*other_borrow);
                }
            }
        }
    }

    /// Detaches the subset relation between the given lifetimes
    pub fn detach_subset_relations(
        &mut self,
        regions: impl IntoIterator<Item = Region>,
    ) {
        for region in regions {
            self.subset_relations
                .detach(self.region_info.get_transitive_closure_index(region));
        }
    }

    /// Gets the universal regions of the given lifetime
    #[allow(clippy::iter_on_single_items, clippy::iter_on_empty_collections)]
    pub fn get_universal_regions_of_lifetime(
        &self,
        lifetime: &Lifetime<BorrowModel>,
    ) -> impl Iterator<Item = UniversalRegion> + '_ {
        let region = match lifetime {
            Lifetime::Static => Region::Universal(UniversalRegion::Static),
            Lifetime::Parameter(member_id) => Region::Universal(
                UniversalRegion::LifetimeParameter(*member_id),
            ),
            Lifetime::Inference(local_region) => Region::Local(*local_region),
            Lifetime::Forall(_) => unreachable!("should be instantiated"),
            Lifetime::Error(_) => return None.into_iter().flatten(),
        };

        let region_index =
            self.region_info.get_transitive_closure_index(region);

        Some(self.region_info.indices_by_univseral_region.iter().filter_map(
            move |(universal_region, index)| {
                if self.subset_relations.has_path(*index, region_index) {
                    Some(*universal_region)
                } else {
                    None
                }
            },
        ))
        .into_iter()
        .flatten()
    }

    /// Gets the borrows occurred so far in the lifetime
    #[allow(clippy::iter_on_single_items, clippy::iter_on_empty_collections)]
    pub fn get_borrows_of_lifetime(
        &self,
        lifetime: &Lifetime<BorrowModel>,
    ) -> impl Iterator<Item = ID<Register<BorrowModel>>> + '_ {
        let region = match lifetime {
            Lifetime::Static => Region::Universal(UniversalRegion::Static),
            Lifetime::Parameter(member_id) => Region::Universal(
                UniversalRegion::LifetimeParameter(*member_id),
            ),
            Lifetime::Inference(local_region) => Region::Local(*local_region),
            Lifetime::Forall(_) => unreachable!("should be instantiated"),
            Lifetime::Error(_) => return None.into_iter().flatten(),
        };

        Some(self.attached_borrows.iter().filter_map(
            move |(borrow, borrow_local_region)| {
                if self.subset_relations.has_path(
                    borrow_local_region.into_index(),
                    self.region_info.get_transitive_closure_index(region),
                ) {
                    Some(*borrow)
                } else {
                    None
                }
            },
        ))
        .into_iter()
        .flatten()
    }

    /// Inserts a subset relation between the given regions from the outlives
    /// predicates that are explicitly written in the source code. (e.g. `where
    /// 'a: 'b`).
    pub fn insert_known_subset_relation(
        &mut self,
        regions: impl IntoIterator<Item = (Region, Region)>,
    ) {
        for region in regions {
            let from_index =
                self.region_info.get_transitive_closure_index(region.0);
            let to_index =
                self.region_info.get_transitive_closure_index(region.1);

            self.subset_relations.add_edge(from_index, to_index);
        }
    }

    /// Handles the outlives constraints from subtyping and lifetime bounds.
    /// The envrionment will build subset relations between the regions and
    /// check the outlives for the universal regions.
    #[allow(clippy::too_many_lines)]
    pub fn handle_outlives_constraints<'o>(
        &mut self,
        outlives_constraints: impl IntoIterator<
            Item = &'o Outlives<Lifetime<BorrowModel>>,
        >,
        checking_span: &Span,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let mut adding_edges: Vec<(Region, Region)> = Vec::new();
        let mut local_to_universal_checks = Vec::new();

        for outlives in outlives_constraints {
            match (&outlives.operand, &outlives.bound) {
                (operand, Lifetime::Inference(bound_origin)) => {
                    let Some(operand_region) = operand.clone().try_into().ok()
                    else {
                        continue;
                    };

                    adding_edges
                        .push((operand_region, Region::Local(*bound_origin)));
                }

                (Lifetime::Inference(operand_region), bound) => {
                    local_to_universal_checks.push((*operand_region, bound));
                }

                _ => {
                    match outlives.query(self.ty_environment).map_err(
                        |overflow_error| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::Predicate(
                                Predicate::from_other_model(
                                    Predicate::LifetimeOutlives(
                                        outlives.clone(),
                                    ),
                                ),
                            ),
                            overflow_span: checking_span.clone(),
                            overflow_error,
                        },
                    )? {
                        Some(Satisfied) => {}
                        None => {
                            handler.receive(Box::new(UnsatisfiedPredicate::<
                                ir::Model,
                            > {
                                predicate: Predicate::from_other_model(
                                    Predicate::LifetimeOutlives(
                                        outlives.clone(),
                                    ),
                                ),
                                instantiation_span: checking_span.clone(),
                                predicate_declaration_span: None,
                            }));
                        }
                    }
                }
            }
        }

        // adding the edges
        for (from, to) in adding_edges {
            let from_index =
                self.region_info.get_transitive_closure_index(from);
            let to_index = self.region_info.get_transitive_closure_index(to);

            self.subset_relations.add_edge(from_index, to_index);
        }

        for (operand_region, universal_region_bound) in
            local_to_universal_checks
        {
            // add the edges
            'out: {
                let from_index = self.region_info.get_transitive_closure_index(
                    Region::Local(operand_region),
                );
                let to_index = self.region_info.get_transitive_closure_index(
                    Region::Universal(match universal_region_bound {
                        Lifetime::Static => UniversalRegion::Static,
                        Lifetime::Parameter(member_id) => {
                            UniversalRegion::LifetimeParameter(*member_id)
                        }
                        _ => break 'out,
                    }),
                );

                self.subset_relations.add_edge(from_index, to_index);
            }

            let mut universal_regions_to_check = HashSet::new();

            // checks for the possible use of local borrows
            let borrows = self
                .get_borrows_of_lifetime(&Lifetime::Inference(operand_region));

            for borrow in borrows {
                let mut universal_regions = Vec::new();
                let borrow_local_region =
                    *self.attached_borrows.get(&borrow).unwrap();

                // find the universal regions
                for universal_region in
                    &self.region_info.indices_by_univseral_region
                {
                    if self.subset_relations.has_path(
                        *universal_region.1,
                        borrow_local_region.into_index(),
                    ) {
                        universal_regions.push(*universal_region.0);
                    }
                }

                // has universal regions backed to it
                if !universal_regions.is_empty() {
                    universal_regions_to_check.extend(universal_regions);
                    continue;
                }

                // local variabless
                let borrowed_address = &self
                    .representation
                    .values
                    .registers
                    .get(borrow)
                    .unwrap()
                    .assignment
                    .as_borrow()
                    .unwrap()
                    .address;

                let root_memory = borrowed_address.get_root_memory();
                let variable_span = match root_memory {
                    Memory::Parameter(id) => self
                        .ty_environment
                        .table()
                        .get_callable(self.current_site.try_into().unwrap())
                        .unwrap()
                        .parameters()
                        .get(*id)
                        .unwrap()
                        .span
                        .clone()
                        .unwrap(),
                    Memory::Alloca(id) => self
                        .representation
                        .values
                        .allocas
                        .get(*id)
                        .unwrap()
                        .span
                        .clone(),
                };

                handler.receive(Box::new(VariableDoesNotLiveLongEnough::<
                    ir::Model,
                > {
                    variable_span,
                    for_lifetime: Some(Lifetime::from_other_model(
                        universal_region_bound.clone(),
                    )),
                    instantiation_span: checking_span.clone(),
                }));
            }

            universal_regions_to_check.extend(
                self.get_universal_regions_of_lifetime(&Lifetime::Inference(
                    operand_region,
                )),
            );

            for universal_region in universal_regions_to_check {
                // create a regular outlives predicate
                let outlives = Outlives::new(
                    match universal_region {
                        UniversalRegion::Static => Lifetime::Static,
                        UniversalRegion::LifetimeParameter(member_id) => {
                            Lifetime::Parameter(member_id)
                        }
                    },
                    universal_region_bound.clone(),
                );

                if outlives
                    .query(self.ty_environment)
                    .map_err(
                        |overflow_error| TypeSystemOverflow::<ir::Model> {
                            operation: OverflowOperation::Predicate(
                                Predicate::from_other_model(
                                    Predicate::LifetimeOutlives(
                                        outlives.clone(),
                                    ),
                                ),
                            ),
                            overflow_span: checking_span.clone(),
                            overflow_error,
                        },
                    )?
                    .is_none()
                {
                    handler.receive(Box::new(UnsatisfiedPredicate::<
                        ir::Model,
                    > {
                        predicate: Predicate::from_other_model(
                            Predicate::LifetimeOutlives(outlives),
                        ),
                        instantiation_span: checking_span.clone(),
                        predicate_declaration_span: None,
                    }));
                }
            }
        }

        Ok(())
    }
}
