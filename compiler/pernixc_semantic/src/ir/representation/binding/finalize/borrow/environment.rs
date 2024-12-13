use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
};

use pernixc_base::{handler::Handler, source_file::Span};

use super::state::Stack;
use crate::{
    arena::{Arena, ID},
    error::{
        self, AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, OverflowOperation,
        TypeSystemOverflow, UnsatisifedPredicate,
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
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment as TyEnvironment,
        normalizer::Normalizer,
        observer::Observer,
        predicate::{Outlives, Predicate},
        term::{lifetime::Lifetime, r#type::Qualifier},
        visitor::RecursiveIterator,
        Compute, Satisfied,
    },
};

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
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Environment {
    region_subset_relations: HashMap<Region, HashSet<Region>>,
    reversed_region_subset_relations: HashMap<Region, HashSet<Region>>,

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
            .flat_map(|x| self.get_borrows_of_lifetime(x).into_iter())
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
        merge_relation(
            &mut self.region_subset_relations,
            &other.region_subset_relations,
        );
        merge_relation(
            &mut self.reversed_region_subset_relations,
            &other.reversed_region_subset_relations,
        );

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

    fn traverse_reversed_region_internal(
        &self,
        region: Region,
        visited: &mut HashSet<Region>,
        function: &mut impl FnMut(Region),
    ) {
        if !visited.insert(region) {
            return;
        }

        function(region);

        for region in self
            .reversed_region_subset_relations
            .get(&region)
            .into_iter()
            .flat_map(|x| x.iter().copied())
        {
            self.traverse_reversed_region_internal(region, visited, function);
        }
    }

    /// Traverses the region **down**  the subset relation
    pub fn traverse_reversed_region(
        &self,
        region: Region,
        mut function: impl FnMut(Region),
    ) {
        self.traverse_reversed_region_internal(
            region,
            &mut HashSet::new(),
            &mut function,
        );
    }

    /// Detaches the subset relation between the given lifetimes
    pub fn deteach_subset_relation(
        &mut self,
        lifetime: &Lifetime<BorrowModel>,
    ) {
        let detaching_region = match lifetime {
            Lifetime::Static => Region::Universal(UniversalRegion::Static),
            Lifetime::Parameter(member_id) => Region::Universal(
                UniversalRegion::LifetimeParameter(*member_id),
            ),
            Lifetime::Inference(local_region_id) => {
                Region::Local(*local_region_id)
            }
            Lifetime::Forall(_) => unreachable!(),
            Lifetime::Error(_) => return,
        };

        let Some(removing_regions) =
            self.reversed_region_subset_relations.remove(&detaching_region)
        else {
            return;
        };

        for removing_region in removing_regions {
            let relations =
                self.region_subset_relations.get_mut(&removing_region).unwrap();

            relations.remove(&detaching_region);

            if relations.is_empty() {
                self.region_subset_relations.remove(&removing_region);
            }

            let Some(local_region) = removing_region.into_local().ok() else {
                continue;
            };

            let Some(borrow) =
                self.reversed_attached_borrows.get(&local_region)
            else {
                continue;
            };

            // remove the borrow if the region is not used by any other
            if self
                .region_subset_relations
                .get(&removing_region)
                .map_or(true, |x| x.is_empty())
            {
                self.active_borrows.remove(borrow);
            }
        }
    }

    /// Gets the borrows occurred so far in the lifetime
    pub fn get_borrows_of_lifetime(
        &self,
        lifetime: &Lifetime<BorrowModel>,
    ) -> HashSet<ID<Register<BorrowModel>>> {
        let region = match lifetime {
            Lifetime::Static => Region::Universal(UniversalRegion::Static),
            Lifetime::Parameter(member_id) => Region::Universal(
                UniversalRegion::LifetimeParameter(*member_id),
            ),
            Lifetime::Inference(local_region) => Region::Local(*local_region),
            Lifetime::Forall(_) => unreachable!("should be instantiated"),
            Lifetime::Error(_) => return HashSet::new(),
        };

        let mut borrows = HashSet::new();

        self.traverse_reversed_region(region, |region| {
            borrows.extend(
                region.into_local().ok().into_iter().flat_map(|x| {
                    self.reversed_attached_borrows.get(&x).cloned()
                }),
            );
        });

        borrows
    }

    /// Inserts a subset relation between the given regions from the outlives
    /// predicates that are explicitly written in the source code. (e.g. `where
    /// 'a: 'b`).
    pub fn insert_known_subset_relation(
        &mut self,
        subset: Region,
        superset: Region,
    ) {
        self.region_subset_relations
            .entry(subset)
            .or_default()
            .insert(superset);

        self.reversed_region_subset_relations
            .entry(superset)
            .or_default()
            .insert(subset);
    }

    pub fn handle_outlives<S: table::State>(
        &mut self,
        outlives: &Outlives<Lifetime<BorrowModel>>,
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
        match (&outlives.operand, &outlives.bound) {
            (operand, Lifetime::Inference(bound_origin)) => {
                let operand_region = match operand {
                    Lifetime::Static => {
                        Region::Universal(UniversalRegion::Static)
                    }
                    Lifetime::Parameter(member_id) => Region::Universal(
                        UniversalRegion::LifetimeParameter(*member_id),
                    ),
                    Lifetime::Inference(local_region) => {
                        Region::Local(*local_region)
                    }
                    Lifetime::Forall(_) => {
                        unreachable!("should've been instantiated")
                    }
                    Lifetime::Error(_) => return Ok(()),
                };

                self.region_subset_relations
                    .entry(operand_region)
                    .or_default()
                    .insert(Region::Local(*bound_origin));

                self.reversed_region_subset_relations
                    .entry(Region::Local(*bound_origin))
                    .or_default()
                    .insert(operand_region);
            }

            (
                Lifetime::Inference(origin_id),
                bound, /* should be univseral region */
            ) => {
                let borrows = self
                    .get_borrows_of_lifetime(&Lifetime::Inference(*origin_id));

                let mut universal_regions_to_check = HashSet::new();
                for borrow in borrows {
                    let mut universal_regions = Vec::new();

                    // find the universal regions
                    self.traverse_reversed_region(
                        Region::Local(
                            *self.attached_borrows.get(&borrow).unwrap(),
                        ),
                        |region| {
                            universal_regions
                                .extend(region.into_universal().ok());
                        },
                    );

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

                    handler.receive(Box::new(VariableDoesNotLiveLongEnough {
                        variable_span,
                        for_lifetime: Some(bound.clone()),
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
                        bound.clone(),
                    );

                    if outlives
                        .query(ty_environment)
                        .map_err(|overflow_error| TypeSystemOverflow::<
                            ir::Model,
                        > {
                            operation: OverflowOperation::Predicate(
                                Predicate::from_other_model(
                                    Predicate::LifetimeOutlives(
                                        outlives.clone(),
                                    ),
                                ),
                            ),
                            overflow_span: checking_span.clone(),
                            overflow_error,
                        })?
                        .is_none()
                    {
                        handler.receive(Box::new(UnsatisifedPredicate {
                            predicate: Predicate::LifetimeOutlives(outlives),
                            instantiation_span: checking_span.clone(),
                            predicate_declaration_span: None,
                        }));
                    }
                }
            }

            _ => {
                match outlives.query(ty_environment).map_err(
                    |overflow_error| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::Predicate(
                            Predicate::from_other_model(
                                Predicate::LifetimeOutlives(outlives.clone()),
                            ),
                        ),
                        overflow_span: checking_span.clone(),
                        overflow_error,
                    },
                )? {
                    Some(Satisfied) => {}
                    None => {
                        handler.receive(Box::new(UnsatisifedPredicate {
                            predicate: Predicate::LifetimeOutlives(
                                outlives.clone(),
                            ),
                            instantiation_span: checking_span,
                            predicate_declaration_span: None,
                        }));
                    }
                }
            }
        }

        Ok(())
    }
}
