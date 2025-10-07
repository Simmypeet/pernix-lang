use getset::{CopyGetters, Getters};
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_hash::HashSet;
use pernixc_ir::{
    address::{Address, Memory},
    control_flow_graph::{ControlFlowGraph, Point},
    instruction::{AccessMode, Instruction},
    value::{register::Register, Environment},
};
use pernixc_lexical::tree::RelativeSpan;
use pernixc_query::TrackedEngine;
use pernixc_semantic_element::parameter::get_parameters;
use pernixc_target::Global;
use pernixc_term::{
    constant::Constant,
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
    sub_term::TermLocation,
    visitor::{self, Recursive},
};
use pernixc_type_system::{normalizer::Normalizer, UnrecoverableError};

use crate::{
    context::Context,
    diagnostic::{
        AccessWhileMutablyBorrowed, Diagnostic, MovedOutWhileBorrowed,
        MutablyAccessWhileImmutablyBorrowed, Usage,
        VariableDoesNotLiveLongEnough,
    },
    subset::Subset,
    Region,
};

#[derive(Debug, Clone, PartialEq, Eq)]
struct Contains<'a> {
    regions: &'a HashSet<Region>,
    contains: bool,
}

impl Recursive<'_, Lifetime> for Contains<'_> {
    fn visit(
        &mut self,
        term: &'_ Lifetime,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        let Some(Region::Local(region)) = term.clone().try_into().ok() else {
            return true;
        };

        self.contains |= self.regions.contains(&Region::Local(region));

        !self.contains
    }
}

impl Recursive<'_, Type> for Contains<'_> {
    fn visit(
        &mut self,
        _: &'_ Type,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        !self.contains
    }
}

impl Recursive<'_, Constant> for Contains<'_> {
    fn visit(
        &mut self,
        _: &'_ Constant,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        !self.contains
    }
}

impl<'a> Contains<'a> {
    pub const fn new(regions: &'a HashSet<Region>) -> Self {
        Self { regions, contains: false }
    }

    pub fn contains(ty: &Type, regions: &'a HashSet<Region>) -> bool {
        let mut contains = Self::new(regions);
        visitor::accept_recursive(ty, &mut contains);
        contains.contains
    }
}

/// The checker used for producing borrow errors.
#[derive(Getters, CopyGetters)]
pub struct Checker<'a, N: Normalizer> {
    #[get_copy = "pub"]
    context: &'a Context<'a, N>,

    #[get_copy = "pub"]
    subset: &'a Subset,
}

impl<'a, N: Normalizer> Checker<'a, N> {
    #[allow(clippy::too_many_arguments)]
    pub const fn new(context: &'a Context<'a, N>, subset: &'a Subset) -> Self {
        Self { context, subset }
    }

    pub const fn values(&self) -> &'a pernixc_ir::Values {
        self.context.values()
    }

    pub const fn current_site(&self) -> Global<pernixc_symbol::ID> {
        self.context.current_site()
    }

    pub const fn control_flow_graph(&self) -> &'a ControlFlowGraph {
        self.context.control_flow_graph()
    }

    pub fn environment(&self) -> &'a Environment<'a, N> {
        self.context.environment()
    }

    pub fn type_environment(
        &self,
    ) -> &'a pernixc_type_system::environment::Environment<'a, N> {
        self.context.environment().type_environment
    }

    pub fn handler(&self) -> &'a dyn Handler<Diagnostic> {
        self.context.handler()
    }

    pub fn tracked_engine(&self) -> &'a TrackedEngine {
        self.context.tracked_engine()
    }

    async fn invalidate_borrow(
        &self,
        borrow: ID<Register>,
        borrowed_at: Point,
        mut exit: impl FnMut(&Instruction, Point) -> bool,
        invalidate_at: Point,
        handler_fn: impl Fn(Usage),
    ) -> Result<(), UnrecoverableError> {
        let borrow_register =
            self.context.values().registers.get(borrow).unwrap();
        let borrow_assignment = borrow_register.assignment.as_borrow().unwrap();

        // make sure that the referant of the borrow is not reassigned
        let root_borrowed_address_type = match *borrow_assignment
            .address
            .get_root_memory()
        {
            Memory::Parameter(id) => self
                .context
                .tracked_engine()
                .get_parameters(self.context.current_site())
                .await?
                .parameters
                .get(id)
                .unwrap()
                .r#type
                .clone(),

            Memory::Alloca(id) => {
                self.context.values().allocas.get(id).unwrap().r#type.clone()
            }

            Memory::Capture(_) => todo!(),
        };

        if !self
            .context
            .is_live(
                &borrow_assignment.address,
                &root_borrowed_address_type,
                borrowed_at,
                invalidate_at,
            )
            .await?
        {
            // not an active borrow
            return Ok(());
        }

        // get the invalidated local regions
        let mut invalidated_local_regions = HashSet::default();
        let mut invalidated_universal_regions = Vec::new();

        for x in
            self.subset.get_regions_containing_borrow(borrow, invalidate_at)
        {
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
            self.context.values().allocas.iter().filter(|(_, x)| {
                Contains::contains(&x.r#type, &invalidated_local_regions)
            });
        let checking_registers =
            self.context.register_infos().iter().filter(|(_, x)| {
                invalidated_local_regions
                    .iter()
                    .any(|region| x.regions.contains(region))
                    && self
                        .context
                        .reachability()
                        .point_reachable(x.assigned_at, invalidate_at)
                        .unwrap()
                    && x.assigned_at != invalidate_at // the mutable borrow will
                                                      // be
                                                      // invalidated right away
                                                      // without this cond
            });

        let live_usages = self
            .context
            .get_live_usages(
                checking_allocas.map(|x| Memory::Alloca(x.0)),
                checking_registers.map(|x| *x.0).collect(),
                &invalidated_local_regions,
                borrow,
                invalidate_at,
                &mut exit,
            )
            .await?;

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
    pub async fn handle_moved_memory(
        &self,
        moved_address: &Address,
        moved_span: &RelativeSpan,
        point: Point,
    ) -> Result<(), UnrecoverableError> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self
                .context
                .reachability()
                .point_reachable(*borrow_point, point)
                .unwrap()
            {
                continue;
            }

            let borrow_register = self
                .context
                .values()
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
                    self.context.handler().receive(
                        Diagnostic::MovedOutWhileBorrowed(
                            MovedOutWhileBorrowed {
                                borrow_span: borrow_register.span.unwrap(),
                                usage: borrow_usage,
                                moved_out_span: *moved_span,
                            },
                        ),
                    );
                },
            )
            .await?;
        }

        Ok(())
    }

    /// Handles the drop of the memory. This will invalidate the borrows that
    /// are attached to the memory.
    pub async fn handle_drop_memory(
        &self,
        drop: Memory,
        point: Point,
    ) -> Result<(), UnrecoverableError> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self
                .context
                .reachability()
                .point_reachable(*borrow_point, point)
                .unwrap()
            {
                continue;
            }

            let borrow_register = self
                .context
                .values()
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
                    .context
                    .tracked_engine()
                    .get_parameters(self.context.current_site())
                    .await?
                    .parameters
                    .get(id)
                    .unwrap()
                    .span
                    .unwrap(),

                Memory::Alloca(id) => {
                    self.context.values().allocas.get(id).unwrap().span.unwrap()
                }

                Memory::Capture(_) => todo!(),
            };

            self.invalidate_borrow(
                *borrow_register_id,
                *borrow_point,
                |_, _| false,
                point,
                |borrow_usage| {
                    self.context.handler().receive(
                        Diagnostic::VariableDoesNotLiveLongEnough(
                            VariableDoesNotLiveLongEnough {
                                variable_span: span,
                                borrow_span: borrow_register.span.unwrap(),
                                usage: borrow_usage,
                            },
                        ),
                    );
                },
            )
            .await?;
        }

        Ok(())
    }

    /// Handle the accesses to a particular address and check if it
    /// invalidates any borrows.
    pub async fn handle_access(
        &self,
        address: &Address,
        access_mode: &AccessMode,
        access_point: Point,
    ) -> Result<(), UnrecoverableError> {
        for (borrow_register_id, (_, borrow_point)) in
            self.subset.created_borrows()
        {
            // basic reachability check, this can filter out a lot of irrelevant
            // borrows without doing more expensive check
            if !self
                .context
                .reachability()
                .point_reachable(*borrow_point, access_point)
                .unwrap()
            {
                continue;
            }

            let borrow_register = self
                .context
                .values()
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
                        self.context.handler().receive(
                            Diagnostic::MutablyAccessWhileImmutablyBorrowed(
                                MutablyAccessWhileImmutablyBorrowed {
                                    mutable_access_span: access_mode
                                        .span()
                                        .copied()
                                        .unwrap(),
                                    immutable_borrow_span: Some(
                                        borrow_register.span.unwrap(),
                                    ),
                                    usage: borrow_usage,
                                },
                            ),
                        );
                    }
                    Qualifier::Mutable => {
                        self.context.handler().receive(
                            Diagnostic::AccessWhileMutablyBorrowed(
                                AccessWhileMutablyBorrowed {
                                    access_span: access_mode
                                        .span()
                                        .copied()
                                        .unwrap(),
                                    mutable_borrow_span: Some(
                                        borrow_register.span.unwrap(),
                                    ),
                                    borrow_usage,
                                },
                            ),
                        );
                    }
                },
            )
            .await?;
        }

        Ok(())
    }
}
