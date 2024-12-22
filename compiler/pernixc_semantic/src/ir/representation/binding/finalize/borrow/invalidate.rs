use std::collections::HashSet;

use getset::{CopyGetters, Getters};
use pernixc_base::handler::Handler;

use super::{cache::RegisterTypes, liveness, subset::Subset};
use crate::{
    arena::ID,
    error::{
        AccessWhileMutablyBorrowed, MutablyAccessWhileImmutablyBorrowed,
        TypeSystemOverflow, Usage,
    },
    ir::{
        self,
        address::{Address, Memory},
        control_flow_graph::Point,
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
    register_types: &'a RegisterTypes,
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
        register_types: &'a RegisterTypes,
        current_site: GlobalID,
        subset: &'a Subset,
    ) -> Self {
        Self {
            representation,
            environment,
            register_types,
            current_site,
            subset,
        }
    }

    pub fn invalidate_borrow(
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

        if liveness::has_been_reassigned(
            &borrow_assignment.address,
            &root_borrowed_address_type,
            borrowed_at,
            invalidate_at,
            &self.representation.control_flow_graph,
            self.environment,
        )? {
            // not an active borrow
            return Ok(());
        }

        // get the invalidated local regions
        let invalidated_regions =
            dbg!(self.subset.get_regions_containing(borrow, invalidate_at));
        dbg!(self.subset);

        // these are the allocas that contain the invalidated local
        // regions, check if the allocas are *used*
        let checking_allocas =
            self.representation.values.allocas.iter().filter(|(_, x)| {
                Contains::contains(&x.r#type, &invalidated_regions)
            });
        let checking_registers = self.register_types.iter().filter(|(_, x)| {
            invalidated_regions.iter().any(|region| x.regions.contains(region))
        });

        let live_usages = liveness::get_live_usages(
            checking_allocas.map(|x| Memory::Alloca(x.0)),
            checking_registers.map(|x| *x.0).collect(),
            &invalidated_regions,
            invalidate_at,
            &mut exit,
            self.representation,
            self.current_site,
            self.environment,
        )?;

        for usage in live_usages {
            handler_fn(usage);
        }

        // if there's any universal lifetimes that could use the
        // invalidated borrow, report an error
        let universal_lifetimes = invalidated_regions
            .into_iter()
            .filter_map(|x| x.into_universal().ok())
            .collect::<Vec<_>>();

        if !universal_lifetimes.is_empty() {
            handler_fn(Usage::ByUniversalRegions(universal_lifetimes));
        }

        Ok(())
    }

    /// Handle the accesses to a particular address and check if it
    /// invalidates any borrows.
    pub fn handle_access(
        &self,
        address: &Address<BorrowModel>,
        access_mode: AccessMode,
        point: Point<BorrowModel>,
        handler: &HandlerWrapper,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
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
            println!(
                "invalidating borrow {} by {}",
                borrow_register.span.str(),
                access_mode.span().str()
            );

            self.invalidate_borrow(
                *borrow_register_id,
                *borrow_point,
                |_, _| false,
                point,
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
