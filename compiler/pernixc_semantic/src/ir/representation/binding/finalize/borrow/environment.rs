use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
    sync::Arc,
};

use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    local_region_generator::LocalRegionGenerator, state::Stack,
    transitive_closure::TransitiveClosure,
};
use crate::{
    arena::{Arena, Key, ID},
    error::{
        self, AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, OverflowOperation,
        TypeSystemOverflow, UnsatisfiedPredicate,
        VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::{Address, Memory},
        representation::{
            binding::finalize::borrow::state::Summary,
            borrow::{
                Access, LocalRegion, Model as BorrowModel, Region,
                UniversalRegion,
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
        term::{lifetime::Lifetime, r#type::Qualifier, Term},
        visitor::RecursiveIterator,
        Compute, Satisfied,
    },
};

/// Contains the information about all the possible regions in the program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RegionInfo {
    generated_local_regions: usize,
    indices_by_univseral_region: HashMap<UniversalRegion, usize>,
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

                starting_universal_regions_index += 1;
            }
        }

        // finally, insert the static region
        indices_by_univseral_region
            .insert(UniversalRegion::Static, starting_universal_regions_index);

        Self {
            generated_local_regions: generator.generated_count(),
            indices_by_univseral_region,
        }
    }

    /// The total number of regions in the program.
    pub fn total_regions(&self) -> usize {
        self.generated_local_regions + self.indices_by_univseral_region.len()
    }

    /// Translates the given region into the index of the transitive closure
    pub fn into_transitive_closure_index(&self, region: Region) -> usize {
        match region {
            Region::Local(local_region) => local_region.into_index(),
            Region::Universal(universal_region) => *self
                .indices_by_univseral_region
                .get(&universal_region)
                .unwrap(),
        }
    }
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
///
/// ## TODO
///
/// Use a more efficient data structure for the subset relation (probably
/// transitive closures).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    region_info: Arc<RegionInfo>, // immutable data
    subset_relations: TransitiveClosure,

    attached_borrows: HashMap<ID<Register<BorrowModel>>, ID<LocalRegion>>,
    reversed_attached_borrows:
        HashMap<ID<LocalRegion>, ID<Register<BorrowModel>>>,

    invalidated_borrows:
        HashMap<ID<Register<BorrowModel>>, HashMap<ID<Access>, bool>>,

    occurred_accesses: HashSet<ID<Access>>,
    active_borrows: HashSet<ID<Register<BorrowModel>>>,
}

fn merge_relation<K: Clone + Eq + Hash, V: Clone + Eq + Hash>(
    this: &mut HashMap<K, HashSet<V>>,
    other: &HashMap<K, HashSet<V>>,
) {
    for (other_key, other_values) in other {
        this.entry(other_key.clone())
            .or_default()
            .extend(other_values.iter().cloned());
    }
}

/// Gets all the lifetimes included in the given address.
pub fn get_lifetimes_in_address<S: table::State>(
    mut address: &Address<BorrowModel>,
    span: Span,
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

impl Environment {
    pub fn new(region_info: Arc<RegionInfo>) -> Self {
        Self {
            subset_relations: TransitiveClosure::new(
                std::iter::empty(),
                region_info.total_regions(),
            ),

            region_info,

            attached_borrows: HashMap::new(),
            reversed_attached_borrows: HashMap::new(),

            invalidated_borrows: HashMap::new(),

            occurred_accesses: HashSet::new(),
            active_borrows: HashSet::new(),
        }
    }

    pub fn check_lifetime_usages<'a, S: table::State>(
        &mut self,
        lifetimes: impl Iterator<Item = &'a Lifetime<BorrowModel>>,
        values: &Values<BorrowModel>,
        stack: &Stack,
        checking_span: Span,
        accesses: &Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        let borrows = lifetimes
            .flat_map(|x| self.get_borrows_of_lifetime(x))
            .collect::<HashSet<_>>();

        self.check_borrow_usages(
            borrows.iter().copied(),
            values,
            stack,
            checking_span.clone(),
            accesses,
            current_site,
            ty_environment,
            handler,
        );
    }

    /// Checks the usage of borrows
    pub fn check_borrow_usages<S: table::State>(
        &mut self,
        borrows: impl Iterator<Item = ID<Register<BorrowModel>>> + Clone,
        values: &Values<BorrowModel>,
        stack: &Stack,
        checking_span: Span,
        accesses: &Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        // check for the live long enough, not moved out variables
        for borrow in borrows.clone() {
            let borrow_register = values.registers.get(borrow).unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            // get the state of the borrow
            let Some(state) = stack.get_state(&borrow_assignment.address)
            else {
                // not found means it has been popped out of scope
                handler.receive(Box::new(VariableDoesNotLiveLongEnough::<
                    ir::Model,
                > {
                    variable_span: match *borrow_assignment
                        .address
                        .get_root_memory()
                    {
                        Memory::Parameter(id) => ty_environment
                            .table()
                            .get_callable(current_site.try_into().unwrap())
                            .unwrap()
                            .parameters()
                            .get(id)
                            .unwrap()
                            .span
                            .clone()
                            .unwrap(),
                        Memory::Alloca(id) => {
                            values.allocas.get(id).unwrap().span.clone()
                        }
                    },
                    for_lifetime: None,
                    instantiation_span: checking_span.clone(),
                }));
                continue;
            };

            // check the state of the variable
            match state.get_state_summary() {
                Summary::Initialized => {}
                Summary::Uninitialized => {}
                Summary::Moved(moved_out_span) => {
                    // TODO: find a way to prevent multiple reporting
                    handler.receive(Box::new(MovedOutWhileBorrowed {
                        borrow_usage_span: checking_span.clone(),
                        moved_out_span,
                    }));
                }
            }
        }

        // check for the invalidated borrows
        for borrow in borrows {
            // if the borrow is already invalidated, skip
            let Some(invalidated_accesses) =
                self.invalidated_borrows.get_mut(&borrow)
            else {
                continue;
            };

            let borrow_qualifier = values
                .registers
                .get(borrow)
                .unwrap()
                .assignment
                .as_borrow()
                .unwrap()
                .qualifier;

            for (invalidated_access_id, reported) in
                invalidated_accesses.iter_mut()
            {
                if *reported {
                    continue;
                }

                *reported = true;

                // let invalidated_access =
                //     accesses.get(invalidated_access_id).unwrap();

                match borrow_qualifier {
                    Qualifier::Immutable => {
                        handler.receive(Box::new(
                            MutablyAccessWhileImmutablyBorrowed {
                                mutable_access_span: accesses
                                    .get(*invalidated_access_id)
                                    .unwrap()
                                    .span
                                    .clone(),
                                immutable_borrow_span: Some(
                                    values
                                        .registers
                                        .get(borrow)
                                        .unwrap()
                                        .span
                                        .clone(),
                                ),
                                borrow_usage_span: checking_span.clone(),
                            },
                        ));
                    }
                    Qualifier::Mutable => {
                        handler.receive(Box::new(AccessWhileMutablyBorrowed {
                            access_span: accesses
                                .get(*invalidated_access_id)
                                .unwrap()
                                .span
                                .clone(),
                            mutable_borrow_span: Some(
                                values
                                    .registers
                                    .get(borrow)
                                    .unwrap()
                                    .span
                                    .clone(),
                            ),
                            borrow_usage_span: checking_span.clone(),
                        }));
                    }
                }
            }
        }
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

        assert!(self.active_borrows.insert(borrow));
    }

    /// Handles an access to the given address.
    pub fn handle_access<S: table::State>(
        &mut self,
        address: &Address<BorrowModel>,
        access_qualifier: Qualifier,
        span: Span,
        stack: &Stack,
        values: &Values<BorrowModel>,
        accesses: &mut Arena<Access>,
        current_site: GlobalID,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        /*
        1. check if the accesses use any "not live long enough" variables
        2. check if the accesses use any invalidated borrows.
        3. invalidates the borrows inccured by the access
            - if it's a mutable access, invalidate all the borrows
            - if it's an immutable access, invalidate the mutable borrows
         */

        let by_access_id = accesses.insert(Access {
            address: address.clone(),
            qualifier: access_qualifier,
            span: span.clone(),
        });
        assert!(
            self.occurred_accesses.insert(by_access_id),
            "access already occurred"
        );

        let address_lifetimes = get_lifetimes_in_address(
            address,
            span.clone(),
            values,
            current_site,
            ty_environment,
        )?;
        let borrows_in_address = address_lifetimes
            .iter()
            .flat_map(|x| self.get_borrows_of_lifetime(&x))
            .collect::<HashSet<_>>();

        self.check_borrow_usages(
            borrows_in_address.iter().copied(),
            values,
            stack,
            span.clone(),
            accesses,
            current_site,
            ty_environment,
            handler,
        );

        // invalidate the borrows
        for borrow in self.attached_borrows.keys().cloned() {
            let borrow_register = values.registers.get(borrow).unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate = access_qualifier == Qualifier::Mutable
                || borrow_assignment.qualifier == Qualifier::Mutable;

            // if the address overlaps
            let is_subaddress = address.is_child_of(&borrow_assignment.address)
                || borrow_assignment.address.is_child_of(&address);

            if should_invalidate
                && is_subaddress
                && self.active_borrows.contains(&borrow)
            {
                // invalidate the borrow
                self.invalidated_borrows.insert(
                    borrow,
                    std::iter::once((by_access_id, false)).collect(),
                );

                // TODO: check if the invalidated borrow is used by universal
                // regions
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

        for (other_brrow, other_invalidated_access) in
            &other.invalidated_borrows
        {
            let this_invalidated_access =
                self.invalidated_borrows.entry(*other_brrow).or_default();

            for (other_access, other_reported) in other_invalidated_access {
                match this_invalidated_access.entry(*other_access) {
                    Entry::Occupied(occupied_entry) => {
                        *occupied_entry.into_mut() |= *other_reported;
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(*other_reported);
                    }
                }
            }
        }

        self.occurred_accesses.extend(other.occurred_accesses.iter().cloned());
    }

    /// Detaches the subset relation between the given lifetimes
    pub fn detach_subset_relations(
        &mut self,
        regions: impl IntoIterator<Item = Region>,
    ) {
        let detached_regions = regions
            .into_iter()
            .flat_map(|x| {
                self.subset_relations
                    .detach(self.region_info.into_transitive_closure_index(x))
            })
            .collect::<HashSet<_>>();

        for detached in detached_regions {
            let Some(borrow) =
                self.reversed_attached_borrows.get(&ID::from_index(detached))
            else {
                continue;
            };

            // remove the borrow if the region is not used by any other
            if self.subset_relations.does_not_go_to_any_except_itself(detached)
            {
                self.active_borrows.remove(borrow);
                self.invalidated_borrows.remove(borrow);
            }
        }
    }

    /// Gets the borrows occurred so far in the lifetime
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
                    self.region_info.into_transitive_closure_index(region),
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
                self.region_info.into_transitive_closure_index(region.0);
            let to_index =
                self.region_info.into_transitive_closure_index(region.1);

            self.subset_relations.add_edge(from_index, to_index);
        }
    }

    pub fn handle_outlives_constraints<'a, S: table::State>(
        &mut self,
        outlives_constraints: impl IntoIterator<
            Item = &'a Outlives<Lifetime<BorrowModel>>,
        >,
        checking_span: Span,
        current_site: GlobalID,
        values: &Values<BorrowModel>,
        ty_environment: &TyEnvironment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
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
                    match outlives.query(ty_environment).map_err(
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
                self.region_info.into_transitive_closure_index(from);
            let to_index = self.region_info.into_transitive_closure_index(to);

            self.subset_relations.add_edge(from_index, to_index);
        }

        for (operand_region, universal_region_bound) in
            local_to_universal_checks
        {
            let borrows = self
                .get_borrows_of_lifetime(&Lifetime::Inference(operand_region));

            let mut universal_regions_to_check = HashSet::new();
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
                let borrowed_address = &values
                    .registers
                    .get(borrow)
                    .unwrap()
                    .assignment
                    .as_borrow()
                    .unwrap()
                    .address;

                let root_memory = borrowed_address.get_root_memory();
                let variable_span = match root_memory {
                    Memory::Parameter(id) => ty_environment
                        .table()
                        .get_callable(current_site.try_into().unwrap())
                        .unwrap()
                        .parameters()
                        .get(*id)
                        .unwrap()
                        .span
                        .clone()
                        .unwrap(),
                    Memory::Alloca(id) => {
                        values.allocas.get(*id).unwrap().span.clone()
                    }
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
                    .query(ty_environment)
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
