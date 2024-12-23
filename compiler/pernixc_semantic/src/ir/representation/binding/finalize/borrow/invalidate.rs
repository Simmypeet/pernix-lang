use std::collections::HashSet;

use getset::{CopyGetters, Getters};
use pernixc_base::{handler::Handler, source_file::Span};

use super::{
    cache::{RegionVariances, RegisterInfos},
    liveness,
    subset::Subset,
};
use crate::{
    arena::ID,
    error::{
        AccessWhileMutablyBorrowed, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, TypeSystemOverflow, Usage,
        VariableDoesNotLiveLongEnough,
    },
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::{Point, Reachability},
        instruction::{AccessMode, Instruction},
        representation::{
            binding::HandlerWrapper,
            borrow::{Model as BorrowModel, Region},
            Representation,
        },
        value::register::Register,
    },
    symbol::{table, GlobalID},
    type_system::{
        environment::Environment,
        normalizer::Normalizer,
        observer::Observer,
        sub_term::TermLocation,
        term::{
            constant::Constant,
            lifetime::Lifetime,
            r#type::{Qualifier, Type},
            Term,
        },
        visitor::{self, Recursive},
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Contains<'a> {
    regions: &'a HashSet<Region>,
    contains: bool,
}

impl Recursive<'_, Lifetime<BorrowModel>> for Contains<'_> {
    fn visit(
        &mut self,
        term: &'_ Lifetime<BorrowModel>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Some(Region::Local(region)) = term.clone().try_into().ok() else {
            return true;
        };

        self.contains |= self.regions.contains(&Region::Local(region));

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

/// The ckecker used for producing borrow errors.
#[derive(Getters, CopyGetters)]
pub struct Checker<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    #[get_copy = "pub"]
    representation: &'a Representation<BorrowModel>,
    #[get_copy = "pub"]
    environment: &'a Environment<'a, BorrowModel, S, N, O>,
    #[get_copy = "pub"]
    reachability: &'a Reachability<BorrowModel>,
    #[allow(unused)]
    #[get_copy = "pub"]
    region_variances: &'a RegionVariances,
    #[get_copy = "pub"]
    register_infos: &'a RegisterInfos,
    #[get_copy = "pub"]
    current_site: GlobalID,
    #[get_copy = "pub"]
    subset: &'a Subset,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Checker<'a, S, N, O>
{
    pub fn new(
        representation: &'a Representation<BorrowModel>,
        environment: &'a Environment<'a, BorrowModel, S, N, O>,
        reachability: &'a Reachability<BorrowModel>,
        register_infos: &'a RegisterInfos,
        region_variances: &'a RegionVariances,
        current_site: GlobalID,
        subset: &'a Subset,
    ) -> Self {
        Self {
            representation,
            environment,
            reachability,
            register_infos,
            current_site,
            subset,
            region_variances,
        }
    }

    fn invalidate_borrow(
        &self,
        borrow: ID<Register<BorrowModel>>,
        borrowed_at: Point<BorrowModel>,
        mut exit: impl FnMut(&Instruction<BorrowModel>, Point<BorrowModel>) -> bool,
        invalidate_at: Point<BorrowModel>,
        handler_fn: impl Fn(Usage),
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let borrow_register =
            self.representation.values.registers.get(borrow).unwrap();
        let borrow_assignment = borrow_register.assignment.as_borrow().unwrap();

        // make sure that the referant of the borrow is not reassigned
        let root_borrowed_address_type =
            match *borrow_assignment.address.get_root_memory() {
                Memory::Parameter(id) => Type::from_default_model(
                    self.environment
                        .table()
                        .get_callable(self.current_site.try_into().unwrap())
                        .unwrap()
                        .parameters()
                        .get(id)
                        .unwrap()
                        .r#type
                        .clone(),
                ),
                Memory::Alloca(id) => self
                    .representation
                    .values
                    .allocas
                    .get(id)
                    .unwrap()
                    .r#type
                    .clone(),
            };

        if !liveness::is_live(
            &borrow_assignment.address,
            &root_borrowed_address_type,
            borrowed_at,
            invalidate_at,
            self.representation,
            self.environment,
        )? {
            // not an active borrow
            return Ok(());
        }

        // get the invalidated local regions
        let mut invalidated_local_regions = HashSet::new();
        let mut invalidated_universal_regions = Vec::new();

        for x in self.subset.get_regions_containing(borrow, invalidate_at) {
            match x {
                Region::Universal(universal_region) => {
                    invalidated_universal_regions.push(universal_region);
                }
                Region::Local(id) => {
                    invalidated_local_regions.insert(Region::Local(id));
                }
            }
        }

        // these are the allocas that contain the invalidated local
        // regions, check if the allocas are *used*
        let checking_allocas =
            self.representation.values.allocas.iter().filter(|(_, x)| {
                Contains::contains(&x.r#type, &invalidated_local_regions)
            });
        let checking_registers = self.register_infos.iter().filter(|(_, x)| {
            invalidated_local_regions
                .iter()
                .any(|region| x.regions.contains(region))
                && self
                    .reachability
                    .point_reachable(x.assigned_at, invalidate_at)
                    .unwrap()
                && x.assigned_at != invalidate_at // the mutable borrow will be
                                                  // invalidated right away
                                                  // without this cond
        });

        let live_usages = liveness::get_live_usages(
            checking_allocas.map(|x| Memory::Alloca(x.0)),
            checking_registers.map(|x| *x.0).collect(),
            &invalidated_local_regions,
            borrow,
            invalidate_at,
            &mut exit,
            self.representation,
            self.current_site,
            self.environment,
        )?;

        for usage in live_usages {
            handler_fn(usage);
        }

        if !invalidated_universal_regions.is_empty() {
            handler_fn(Usage::ByUniversalRegions(
                invalidated_universal_regions,
            ));
        }

        Ok(())
    }

    /// Handles the drop of the memory. This will invalidate the borrows that
    /// are attached to the memory.
    pub fn handle_moved_memory(
        &self,
        moved_address: &Address<BorrowModel>,
        moved_span: &Span,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self.reachability.point_reachable(*borrow_point, point).unwrap()
            {
                continue;
            }

            let borrow_register = self
                .representation
                .values
                .registers
                .get(*borrow_register_id)
                .unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate =
                borrow_assignment.address.is_child_of(moved_address)
                    || moved_address.is_child_of(&borrow_assignment.address);

            if !should_invalidate {
                continue;
            }

            self.invalidate_borrow(
                *borrow_register_id,
                *borrow_point,
                |_, _| false,
                point,
                |borrow_usage| {
                    handler.receive(Box::new(MovedOutWhileBorrowed {
                        borrow_span: borrow_register.span.clone(),
                        usage: borrow_usage,
                        moved_out_span: moved_span.clone(),
                    }))
                },
            )?;
        }

        Ok(())
    }

    /// Handles the drop of the memory. This will invalidate the borrows that
    /// are attached to the memory.
    pub fn handle_drop_memory(
        &self,
        drop: Memory<BorrowModel>,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self.reachability.point_reachable(*borrow_point, point).unwrap()
            {
                continue;
            }

            let borrow_register = self
                .representation
                .values
                .registers
                .get(*borrow_register_id)
                .unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate =
                borrow_assignment.address.is_child_of(&Address::Memory(drop))
                    && !borrow_assignment.address.is_behind_reference();

            if !should_invalidate {
                continue;
            }

            let span = match drop {
                Memory::Parameter(id) => self
                    .environment
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
                *borrow_register_id,
                *borrow_point,
                |_, _| false,
                point,
                |borrow_usage| {
                    handler.receive(Box::new(VariableDoesNotLiveLongEnough {
                        variable_span: span.clone(),
                        borrow_span: borrow_register.span.clone(),
                        usage: borrow_usage,
                    }))
                },
            )?;
        }

        Ok(())
    }

    /// Handle the accesses to a particular address and check if it
    /// invalidates any borrows.
    pub fn handle_access(
        &self,
        address: &Address<BorrowModel>,
        access_mode: AccessMode,
        access_point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self
                .reachability
                .point_reachable(*borrow_point, access_point)
                .unwrap()
            {
                continue;
            }

            let borrow_register = self
                .representation
                .values
                .registers
                .get(*borrow_register_id)
                .unwrap();
            let borrow_assignment =
                borrow_register.assignment.as_borrow().unwrap();

            let should_invalidate: bool = match &access_mode {
                AccessMode::Read(read) => {
                    let qualifier_invalidate = read.qualifier
                        == Qualifier::Mutable
                        || borrow_assignment.qualifier == Qualifier::Mutable;

                    let address_overlap = address
                        .is_child_of(&borrow_assignment.address)
                        || borrow_assignment.address.is_child_of(address);

                    qualifier_invalidate && address_overlap
                }
                AccessMode::Write(_) => {
                    /*
                    ```
                    let mutable x = 0;
                    let mutable y = 0;

                    let mutable ref = &mutable x;
                    let anotherRef = &mutable *ref;

                    // this doesn't invalidate the borrow because the borrow
                    // is not used in the access
                    ref = &mutable y;

                    *anotherRef = 2;
                    ```
                     */

                    let address_overlap = address
                        .is_child_of(&borrow_assignment.address)
                        || borrow_assignment.address.is_child_of(address);

                    let borrow_address_dereference_count =
                        borrow_assignment.address.get_dereference_count();
                    let write_address_dereference_count =
                        address.get_dereference_count();

                    address_overlap
                        && write_address_dereference_count
                            >= borrow_address_dereference_count
                }
            };

            if !should_invalidate {
                // irrelevant borrow
                continue;
            }

            self.invalidate_borrow(
                *borrow_register_id,
                *borrow_point,
                |_, _| false,
                access_point,
                |borrow_usage| match borrow_assignment.qualifier {
                    Qualifier::Immutable => {
                        handler.receive(Box::new(
                            MutablyAccessWhileImmutablyBorrowed {
                                mutable_access_span: access_mode.span().clone(),
                                immutable_borrow_span: Some(
                                    borrow_register.span.clone(),
                                ),
                                usage: borrow_usage,
                            },
                        ));
                    }
                    Qualifier::Mutable => {
                        handler.receive(Box::new(AccessWhileMutablyBorrowed {
                            access_span: access_mode.span().clone(),
                            mutable_borrow_span: Some(
                                borrow_register.span.clone(),
                            ),
                            borrow_usage,
                        }));
                    }
                },
            )?;
        }

        Ok(())
    }
}
