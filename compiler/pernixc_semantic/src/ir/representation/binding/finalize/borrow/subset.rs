use std::collections::{BTreeSet, HashMap, HashSet};

use getset::Getters;
use pernixc_base::source_file::Span;

use super::cache::RegisterTypes;
use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        control_flow_graph::{Block, Point},
        instruction::{Instruction, Store},
        representation::{
            borrow::{
                LocalRegion, Model as BorrowModel, Region, UniversalRegion,
            },
            Representation, Values,
        },
        value::{
            register::{Assignment, Borrow, FunctionCall, Register, Struct},
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index},
        GenericID, GlobalID, LifetimeParameterID,
    },
    transitive_closure::TransitiveClosure,
    type_system::{
        compatible::{Compatibility, Compatible},
        environment::Environment,
        instantiation::{self, Instantiation},
        normalizer::Normalizer,
        observer::Observer,
        predicate::{Outlives, Predicate},
        term::{r#type::Type, Term},
        variance::Variance,
        visitor::RecursiveIterator,
        LifetimeConstraint, Succeeded,
    },
};

/// Represents a point in the control flow graph where the borrow checker
/// is considering the subset relation between regions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegionPoint {
    /// Involving a particular point in the control flow graph.
    InBlock(Point<BorrowModel>),

    /// The moment before executing the first instruction in the block.
    EnteringBlock(ID<Block<BorrowModel>>),
}

impl RegionPoint {
    /// Gets the block id where the region is considered.
    pub fn block_id(&self) -> ID<Block<BorrowModel>> {
        match self {
            Self::InBlock(point) => point.block_id,
            Self::EnteringBlock(block_id) => *block_id,
        }
    }
}

/// Used for representing a region at a particular point in the control flow
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegionAt {
    pub region: Region,

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
    /// Creates a new [`RegionAt`] where the region is location sensitive.
    pub fn new(region: Region, point: RegionPoint) -> Self {
        Self { region, point: Some(point) }
    }

    /// Creates a new [`RegionAt`] where the region is location insensitive.
    pub fn new_location_insensitive(region: Region) -> Self {
        Self { region, point: None }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct RegionChangeLog {
    pub updated_at_instruction_indices:
        HashMap<ID<Block<BorrowModel>>, Vec<usize>>,
}

impl RegionChangeLog {
    /// Gets the [`RegionPoint`] where the region is most updated.
    pub fn get_most_updated_point(
        &self,
        point: Point<BorrowModel>,
    ) -> RegionPoint {
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
            .map(|x| {
                RegionPoint::InBlock(Point {
                    block_id: point.block_id,
                    instruction_index: updated_points[x],
                })
            })
            .unwrap_or_else(|x| {
                if x == 0 {
                    RegionPoint::EnteringBlock(point.block_id)
                } else {
                    RegionPoint::InBlock(Point {
                        block_id: point.block_id,
                        instruction_index: updated_points[x - 1],
                    })
                }
            })
    }
}

/// Contains the direct subset relations between regions. It's the result of
/// the subset analysis with no optimization.
#[derive(Debug, Clone, PartialEq, Eq, Getters)]
pub struct Naive {
    /// The accumulated subset relations between regions since the beginning
    /// of the control flow graph.
    subset_relations: HashSet<(RegionAt, RegionAt, Option<Span>)>,

    /// Represents the [`Borrow`] register assignment created so far.
    ///
    /// The key is the register ID of the borrow assignment, and the value is
    /// the region where the borrow is created.
    #[get = "pub"]
    created_borrows: HashMap<
        ID<Register<BorrowModel>>,
        (ID<LocalRegion>, Point<BorrowModel>),
    >,
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
#[derive(Debug)]
pub struct Builder<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    representation: &'a Representation<BorrowModel>,
    register_types: &'a RegisterTypes,
    current_site: GlobalID,
    environment: &'a Environment<'a, BorrowModel, S, N, O>,

    /// A map between the region and the instruction index where the region
    /// was last changed.
    ///
    /// The "long-lived" regions (created by allocas and universal regions)
    /// will present in this map. The value is the point in the control flow
    /// graph where the region is most updated.
    latest_change_points_by_region: HashMap<Region, Option<usize>>,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Clone for Builder<'a, S, N, O>
{
    fn clone(&self) -> Self {
        Self {
            environment: self.environment,
            representation: self.representation,
            register_types: self.register_types,

            current_site: self.current_site.clone(),
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
    subset_relations: HashSet<(Region, Region, Span)>,

    /// The borrow is created at the given region.
    borrow_created: Option<(ID<Register<BorrowModel>>, ID<LocalRegion>)>,

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

impl Values<BorrowModel> {
    pub(super) fn get_changes_of_struct<S: table::State>(
        &self,
        struct_lit: &Struct<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let instantiation = Instantiation::from_generic_arguments(
            struct_lit.generic_arguments.clone(),
            struct_lit.struct_id.into(),
            &environment
                .table()
                .get(struct_lit.struct_id)
                .unwrap()
                .generic_declaration
                .parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        let struct_sym = environment.table().get(struct_lit.struct_id).unwrap();

        // compare each values in the field to the struct's field type
        for field_id in struct_sym.field_declaration_order().iter().copied() {
            let mut field_ty = Type::from_other_model(
                struct_sym.fields().get(field_id).unwrap().r#type.clone(),
            );
            instantiation::instantiate(&mut field_ty, &instantiation);

            let value_span =
                match &struct_lit.initializers_by_field_id.get(&field_id) {
                    Some(Value::Register(id)) => {
                        self.registers.get(*id).unwrap().span.clone()
                    }
                    Some(Value::Literal(literal)) => literal.span().clone(),
                    None => unreachable!(),
                };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.type_of_value(
                    struct_lit.initializers_by_field_id.get(&field_id).unwrap(),
                    current_site,
                    environment,
                )
                .map_err(|x| TypeSystemOverflow::<
                    ir::Model,
                > {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(&field_ty, Variance::Covariant, environment)
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: value_span.clone(),
                    overflow_error,
                })?;

            // append the lifetime constraints
            if let Some(Succeeded {
                result,
                constraints: compatibility_constraints,
            }) = copmatibility
            {
                assert!(result.forall_lifetime_errors.is_empty());
                assert!(result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compatibility_constraints);
            } else {
                panic!("{value_ty:#?} => {field_ty:#?}")
            }
        }

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in environment
            .table()
            .get_active_premise(struct_lit.struct_id.into())
            .unwrap()
            .predicates
            .into_iter()
            .map(|x| {
                let mut x = Predicate::from_default_model(x);
                x.instantiate(&instantiation);

                x
            })
        {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    lifetime_constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(outlives.clone()),
                    );
                }
                Predicate::TypeOutlives(outlives) => {
                    for lt in RecursiveIterator::new(&outlives.operand)
                        .filter_map(|x| x.0.into_lifetime().ok())
                    {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: lt.clone(),
                                bound: outlives.bound.clone(),
                            }),
                        );
                    }
                }

                _ => {}
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound.clone()).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    pub(super) fn get_changes_of_function_call<S: table::State>(
        &self,
        function_call: &FunctionCall<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let callable = environment
            .table()
            .get_callable(function_call.callable_id)
            .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        for (parameter, argument) in callable
            .parameter_order()
            .iter()
            .copied()
            .map(|x| callable.parameters().get(x).unwrap())
            .zip(&function_call.arguments)
        {
            assert_eq!(
                callable.parameter_order().len(),
                function_call.arguments.len()
            );

            let mut parameter_ty =
                Type::from_other_model(parameter.r#type.clone());
            instantiation::instantiate(
                &mut parameter_ty,
                &function_call.instantiation,
            );

            // obtains the type of argument ty
            let argument_span = match argument {
                Value::Register(id) => {
                    self.registers.get(*id).unwrap().span.clone()
                }
                Value::Literal(literal) => literal.span().clone(),
            };
            let Succeeded {
                result: argument_ty,
                constraints: argument_ty_constraints,
            } = self
                .type_of_value(argument, current_site, environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: argument_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(argument_ty_constraints);

            let copmatibility = argument_ty
                .compatible(&parameter_ty, Variance::Covariant, environment)
                .map_err(|overflow_error| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: argument_span,
                    overflow_error,
                })?;

            // append the lifetime constraints
            if let Some(Succeeded {
                result,
                constraints: compatibility_constraints,
            }) = copmatibility
            {
                assert!(result.forall_lifetime_errors.is_empty());
                assert!(result
                    .forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());

                lifetime_constraints.extend(compatibility_constraints);
            } else {
                panic!("{argument_ty:#?} => {parameter_ty:#?}")
            }
        }

        for predicate in environment
            .table()
            .get_active_premise(function_call.callable_id.into())
            .unwrap()
            .predicates
            .into_iter()
            .map(|x| {
                let mut x = Predicate::from_default_model(x);
                x.instantiate(&function_call.instantiation);

                x
            })
        {
            match predicate {
                Predicate::LifetimeOutlives(outlives) => {
                    lifetime_constraints.insert(
                        LifetimeConstraint::LifetimeOutlives(outlives.clone()),
                    );
                }
                Predicate::TypeOutlives(outlives) => {
                    for lt in RecursiveIterator::new(&outlives.operand)
                        .filter_map(|x| x.0.into_lifetime().ok())
                    {
                        lifetime_constraints.insert(
                            LifetimeConstraint::LifetimeOutlives(Outlives {
                                operand: lt.clone(),
                                bound: outlives.bound.clone(),
                            }),
                        );
                    }
                }

                _ => {}
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound.clone()).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    pub(super) fn get_changes_of_borrow<S: table::State>(
        &self,
        borrow: &Borrow<BorrowModel>,
        span: &Span,
        register_id: ID<Register<BorrowModel>>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let regions_in_address = self.get_regions_in_address(
            &borrow.address,
            span,
            true,
            current_site,
            environment,
        )?;

        let borrow_local_region =
            borrow.lifetime.clone().into_inference().unwrap();

        Ok(Changes {
            subset_relations: {
                regions_in_address
                    .into_iter()
                    .map(|x| {
                        (x, Region::Local(borrow_local_region), span.clone())
                    })
                    .collect()
            },
            borrow_created: Some((register_id, borrow_local_region)),
            overwritten_regions: HashSet::new(),
        })
    }

    pub(super) fn get_changes_of_store<S: table::State>(
        &self,
        store_inst: &Store<BorrowModel>,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let Succeeded { result: address_ty, constraints: address_constraints } =
            self.type_of_address(&store_inst.address, current_site, environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: store_inst.span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;
        let Succeeded { result: value_ty, constraints: value_constraints } =
            self.type_of_value(&store_inst.value, current_site, environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: store_inst.span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

        // get the compatibility constraints between the value and the address
        let compatibility_constraints = match value_ty.compatible(
            &address_ty,
            Variance::Covariant,
            environment,
        ) {
            Ok(Some(Succeeded {
                result:
                    Compatibility {
                        forall_lifetime_instantiations,
                        forall_lifetime_errors,
                    },
                constraints: compatibility_constraints,
            })) => {
                assert!(forall_lifetime_instantiations
                    .lifetimes_by_forall
                    .is_empty());
                assert!(forall_lifetime_errors.is_empty());

                compatibility_constraints
            }
            Ok(None) => {
                panic!("incompatible types {value_ty:#?} => {address_ty:#?}");
            }
            Err(err) => {
                return Err(TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: store_inst.span.clone(),
                    overflow_error: err,
                });
            }
        };

        Ok(Changes {
            subset_relations: value_constraints
                .into_iter()
                .chain(address_constraints.into_iter())
                .chain(compatibility_constraints.into_iter())
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound.clone()).ok()?;

                    Some((from, to, store_inst.span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: RecursiveIterator::new(&address_ty)
                .filter_map(|x| x.0.into_lifetime().ok())
                .filter_map(|x| Region::try_from(x.clone()).ok())
                .collect::<HashSet<_>>(),
        })
    }
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Builder<'a, S, N, O>
{
    /// Returns a list of new region introduced by the scope push instruction.
    ///
    /// This function returns all the regions that are created by the variables
    /// declared in the scope push instruction.
    pub fn get_new_regions(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> HashSet<Region> {
        match instruction {
            Instruction::ScopePush(scope_push) => {
                let mut regions = HashSet::<Region>::new();

                for alloca in
                    self.representation.values.allocas.iter().filter_map(|x| {
                        (x.1.declared_in_scope_id == scope_push.0)
                            .then_some(x.1)
                    })
                {
                    regions.extend(
                        RecursiveIterator::new(&alloca.r#type)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| Region::try_from(x.clone()).ok()),
                    );
                }

                // we'll insert a universal region for the root scope
                if scope_push.0
                    == self.representation.scope_tree.root_scope_id()
                {
                    regions.insert(Region::Universal(UniversalRegion::Static));

                    for generic_id in self
                        .environment
                        .table()
                        .scope_walker(self.current_site)
                        .unwrap()
                        .filter_map(|x| GenericID::try_from(x).ok())
                    {
                        regions.extend(
                            self.environment
                                .table()
                                .get_generic(generic_id)
                                .unwrap()
                                .generic_declaration()
                                .parameters
                                .lifetime_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    Region::Universal(
                                        UniversalRegion::LifetimeParameter(
                                            LifetimeParameterID {
                                                parent: generic_id,
                                                id: x,
                                            },
                                        ),
                                    )
                                }),
                        );
                    }
                }

                regions
            }

            Instruction::ScopePop(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::RegisterDiscard(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => HashSet::new(),
        }
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    pub fn get_removing_regions(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> HashSet<Region> {
        match instruction {
            Instruction::ScopePop(scope_pop) => {
                let mut regions = HashSet::<Region>::new();

                for alloca in
                    self.representation.values.allocas.iter().filter_map(|x| {
                        (x.1.declared_in_scope_id == scope_pop.0).then_some(x.1)
                    })
                {
                    regions.extend(
                        RecursiveIterator::new(&alloca.r#type)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| Region::try_from(x.clone()).ok()),
                    );
                }

                // remove universal regions at root
                if scope_pop.0 == self.representation.scope_tree.root_scope_id()
                {
                    regions.insert(Region::Universal(UniversalRegion::Static));

                    for generic_id in self
                        .environment
                        .table()
                        .scope_walker(self.current_site)
                        .unwrap()
                        .filter_map(|x| GenericID::try_from(x).ok())
                    {
                        regions.extend(
                            self.environment
                                .table()
                                .get_generic(generic_id)
                                .unwrap()
                                .generic_declaration()
                                .parameters
                                .lifetime_order()
                                .iter()
                                .copied()
                                .map(|x| {
                                    Region::Universal(
                                        UniversalRegion::LifetimeParameter(
                                            LifetimeParameterID {
                                                parent: generic_id,
                                                id: x,
                                            },
                                        ),
                                    )
                                }),
                        );
                    }
                }

                regions
            }

            Instruction::RegisterDiscard(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::ScopePush(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => HashSet::new(),
        }
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    pub fn get_changes(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        match instruction {
            Instruction::Store(store) => {
                self.representation.values.get_changes_of_store(
                    store,
                    self.current_site,
                    self.environment,
                )
            }
            Instruction::RegisterAssignment(register_assignment) => {
                let register = self
                    .representation
                    .values
                    .registers
                    .get(register_assignment.id)
                    .unwrap();

                match &register.assignment {
                    Assignment::VariantNumber(_)
                    | Assignment::Cast(_)
                    | Assignment::Binary(_)
                    | Assignment::Prefix(_)
                    | Assignment::Tuple(_)
                    | Assignment::Load(_) => Ok(Changes::default()),
                    Assignment::Borrow(borrow) => {
                        self.representation.values.get_changes_of_borrow(
                            borrow,
                            &register.span,
                            register_assignment.id,
                            self.current_site,
                            self.environment,
                        )
                    }
                    Assignment::FunctionCall(function_call) => {
                        self.representation.values.get_changes_of_function_call(
                            function_call,
                            &register.span,
                            self.current_site,
                            self.environment,
                        )
                    }
                    Assignment::Struct(struct_lit) => {
                        self.representation.values.get_changes_of_struct(
                            struct_lit,
                            &register.span,
                            self.current_site,
                            self.environment,
                        )
                    }
                    Assignment::Variant(_) => Ok(Changes::default()),
                    Assignment::Array(_) => Ok(Changes::default()),
                    Assignment::Phi(_) => Ok(Changes::default()),
                }
            }
            Instruction::RegisterDiscard(_) => Ok(Changes::default()),
            Instruction::TuplePack(_) => Ok(Changes::default()),
            Instruction::ScopePush(_) => Ok(Changes::default()),
            Instruction::ScopePop(_) => Ok(Changes::default()),
            Instruction::DropUnpackTuple(_) => Ok(Changes::default()),
            Instruction::Drop(_) => Ok(Changes::default()),
        }
    }
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Builder<'a, S, N, O>
{
    pub fn walk_instruction(
        &mut self,
        instruction: &Instruction<BorrowModel>,
        instruction_point: Point<BorrowModel>,
        subset_result: &mut Naive,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for region in self.get_new_regions(instruction) {
            assert!(self
                .latest_change_points_by_region
                .insert(region, None)
                .is_none());
        }

        // gets the changes made by the instruction
        let changes = self.get_changes(instruction)?;

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
            dbg!(from, to, &span);
            let latest_from =
                self.latest_change_points_by_region.get(&from).cloned();
            let latest_to =
                self.latest_change_points_by_region.get(&to).cloned();

            let from_region_at = RegionAt {
                region: from,
                point: latest_from
                    .map(|_| RegionPoint::InBlock(instruction_point)),
            };
            let to_region_at = RegionAt {
                region: to,
                point: latest_to
                    .map(|_| RegionPoint::InBlock(instruction_point)),
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
                        changes.overwritten_regions.contains(&region_at.region),
                        lastest_region_update,
                    ) {
                        assert!(region_at.point.is_some());

                        // flows state to current
                        subset_result.subset_relations.insert((
                            RegionAt {
                                region: region_at.region,
                                point: Some(
                                    latest_updated_inst_index.map_or_else(
                                        || {
                                            RegionPoint::EnteringBlock(
                                                instruction_point.block_id,
                                            )
                                        },
                                        |x| {
                                            RegionPoint::InBlock(Point {
                                                instruction_index: x,
                                                block_id: instruction_point
                                                    .block_id,
                                            })
                                        },
                                    ),
                                ),
                            },
                            region_at,
                            None,
                        ));
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

        for region in self.get_removing_regions(instruction) {
            assert!(self
                .latest_change_points_by_region
                .remove(&region)
                .is_some());
        }

        Ok(())
    }

    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Naive,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        let block = self
            .representation
            .control_flow_graph()
            .blocks()
            .get(block_id)
            .unwrap();

        for (index, instruction) in block.instructions().iter().enumerate() {
            self.walk_instruction(
                instruction,
                Point { block_id, instruction_index: index },
                subset_result,
            )?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
#[allow(clippy::type_complexity)]
struct Context<
    'a,
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
> {
    representation: &'a ir::Representation<BorrowModel>,
    current_site: GlobalID,
    environment: &'a Environment<'a, BorrowModel, S, N, O>,
    register_types: &'a RegisterTypes,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id:
        HashMap<ID<Block<BorrowModel>>, Option<Builder<'a, S, N, O>>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_regions_by_block_id: HashMap<
        ID<Block<BorrowModel>>,
        (ID<Block<BorrowModel>>, HashSet<Region>),
    >,
}

impl<
        'a,
        S: table::State,
        N: Normalizer<BorrowModel, S>,
        O: Observer<BorrowModel, S>,
    > Context<'a, S, N, O>
{
    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Naive,
    ) -> Result<Option<Builder<'a, S, N, O>>, TypeSystemOverflow<ir::Model>>
    {
        // skip if already processed
        if let Some(walk_result) = self.walk_results_by_block_id.get(&block_id)
        {
            return Ok(walk_result.clone());
        }

        // mark as processing
        self.walk_results_by_block_id.insert(block_id, None);

        let block =
            self.representation.control_flow_graph.get_block(block_id).unwrap();

        let mut builder = if block.is_entry() {
            assert!(block.predecessors().is_empty());

            let builder = Builder {
                representation: self.representation,
                register_types: self.register_types,
                current_site: self.current_site,
                environment: self.environment,
                latest_change_points_by_region: HashMap::new(),
            };

            let predicates = self
                .environment
                .table()
                .get_active_premise::<BorrowModel>(self.current_site)
                .unwrap()
                .predicates;

            let mut adding_edges = HashSet::new();

            for predicate in predicates {
                match predicate {
                    Predicate::LifetimeOutlives(outlives) => {
                        let (Some(operand), Some(bound)) = (
                            outlives.operand.try_into().ok(),
                            outlives.bound.try_into().ok(),
                        ) else {
                            continue;
                        };

                        adding_edges.insert((operand, bound));
                    }

                    Predicate::TypeOutlives(outlives) => {
                        let Some(bound) = outlives.bound.try_into().ok() else {
                            continue;
                        };

                        for operand in RecursiveIterator::new(&outlives.operand)
                            .filter_map(|x| x.0.into_lifetime().ok())
                            .filter_map(|x| x.clone().try_into().ok())
                        {
                            adding_edges.insert((operand, bound));
                        }
                    }

                    _ => {}
                }
            }

            for (from, to) in adding_edges {
                let from_region_at = RegionAt {
                    region: from,
                    point: Some(RegionPoint::EnteringBlock(block_id)),
                };
                let to_region_at = RegionAt {
                    region: to,
                    point: Some(RegionPoint::EnteringBlock(block_id)),
                };

                subset_result.subset_relations.insert((
                    from_region_at,
                    to_region_at,
                    None,
                ));
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
                    self.walk_block(predecessor_id, subset_result)?
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
                representation: self.representation,
                register_types: self.register_types,
                current_site: self.current_site,
                environment: self.environment,

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

                    let from_region_at = RegionAt {
                        region,
                        point: Some(latest_point.map_or_else(
                            || RegionPoint::EnteringBlock(from_block_id),
                            |x| {
                                RegionPoint::InBlock(Point {
                                    block_id: from_block_id,
                                    instruction_index: x,
                                })
                            },
                        )),
                    };

                    let to_region_at = RegionAt {
                        region,
                        point: Some(RegionPoint::EnteringBlock(block_id)),
                    };

                    subset_result.subset_relations.insert((
                        from_region_at,
                        to_region_at,
                        None,
                    ));
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

        builder.walk_block(block_id, subset_result)?;

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

                let from_region_at = RegionAt {
                    region: *region,
                    point: Some(latest_point.map_or_else(
                        || RegionPoint::EnteringBlock(block_id),
                        |x| {
                            RegionPoint::InBlock(Point {
                                block_id,
                                instruction_index: x,
                            })
                        },
                    )),
                };

                let to_region_at = RegionAt {
                    region: *region,
                    point: Some(RegionPoint::EnteringBlock(*to_block_id)),
                };

                subset_result.subset_relations.insert((
                    from_region_at,
                    to_region_at,
                    None,
                ));
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
pub struct Subset {
    indices_by_region_at: HashMap<RegionAt, usize>,
    region_ats_by_index: Vec<RegionAt>,
    transitive_closure: TransitiveClosure,

    #[get = "pub"]
    direct_subset_relations: HashSet<(RegionAt, RegionAt, Option<Span>)>,

    #[get = "pub"]
    created_borrows: HashMap<
        ID<Register<BorrowModel>>,
        (ID<LocalRegion>, Point<BorrowModel>),
    >,

    change_logs_by_region: HashMap<Region, RegionChangeLog>,
    active_region_sets_by_block_id:
        HashMap<ID<Block<BorrowModel>>, HashSet<Region>>,
    location_insensitive_regions: HashSet<ID<LocalRegion>>,
}

impl Subset {
    /// Gets a list of region that contains the given borrow at the given point.
    pub fn get_regions_containing(
        &self,
        borrow_register_id: ID<Register<BorrowModel>>,
        point: Point<BorrowModel>,
    ) -> HashSet<Region> {
        let block_id = point.block_id;
        let borrow_region = RegionAt::new_location_insensitive(Region::Local(
            self.created_borrows.get(&borrow_register_id).unwrap().0,
        ));

        self.location_insensitive_regions
            .iter()
            .copied()
            .filter_map(|x| {
                // start with the location insensitive regions
                self.transitive_closure
                    .has_path(
                        self.indices_by_region_at[&borrow_region],
                        self.indices_by_region_at
                            [&RegionAt::new_location_insensitive(
                                Region::Local(x),
                            )],
                    )
                    .unwrap()
                    .then_some(Region::Local(x))
            })
            .chain(
                // then check active regions that require location sensitivity
                self.active_region_sets_by_block_id
                    .get(&block_id)
                    .unwrap_or_else(|| panic!("{:?} {self:?}", block_id))
                    .iter()
                    .copied()
                    .filter_map(|x| {
                        let most_updated_point = self.change_logs_by_region[&x]
                            .get_most_updated_point(point);
                        let region_at = RegionAt::new(x, most_updated_point);

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

pub fn analyze<
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
>(
    ir: &ir::Representation<BorrowModel>,
    register_types: &RegisterTypes,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, S, N, O>,
) -> Result<Subset, TypeSystemOverflow<ir::Model>> {
    let mut context = Context {
        representation: ir,
        current_site,
        environment,
        register_types,
        walk_results_by_block_id: HashMap::new(),
        target_regions_by_block_id: HashMap::new(),
    };

    let all_block_ids =
        ir.control_flow_graph().blocks().ids().collect::<Vec<_>>();
    let mut subset_result = Naive {
        subset_relations: HashSet::new(),
        created_borrows: HashMap::new(),
    };

    for block_id in all_block_ids.iter().copied() {
        context.walk_block(block_id, &mut subset_result)?;
    }

    // make sure all blocks are processed
    assert!(context.walk_results_by_block_id.iter().all(|(_, x)| x.is_some()));

    // populate the region and assign the index
    let mut region_ats_by_index = Vec::new();
    let mut indices_by_region_at = HashMap::new();

    let mut all_regions = HashSet::new();
    let mut location_insensitive_regions = HashSet::new();
    let mut active_region_sets_by_block_id = HashMap::<_, HashSet<_>>::new();
    let mut change_logs_by_region = HashMap::<_, RegionChangeLog>::new();

    for region_at in
        subset_result
            .subset_relations
            .iter()
            .flat_map(|(from, to, _)| [*from, *to])
            .chain(subset_result.created_borrows.iter().map(|x| {
                RegionAt::new_location_insensitive(Region::Local(x.1 .0))
            }))
    {
        assert!(region_at.point.is_some() || region_at.region.is_local());

        all_regions.insert(region_at);

        if let Some(region_point) = region_at.point {
            active_region_sets_by_block_id
                .entry(region_point.block_id())
                .or_default()
                .insert(region_at.region);

            match region_point {
                RegionPoint::InBlock(point) => {
                    change_logs_by_region
                        .entry(region_at.region)
                        .or_default()
                        .updated_at_instruction_indices
                        .entry(point.block_id)
                        .or_default()
                        .push(point.instruction_index);
                }
                RegionPoint::EnteringBlock(id) => {
                    change_logs_by_region
                        .entry(region_at.region)
                        .or_default()
                        .updated_at_instruction_indices
                        .entry(id)
                        .or_default();
                }
            }
        } else {
            location_insensitive_regions
                .insert(region_at.region.into_local().unwrap());
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

    Ok(dbg!(Subset {
        indices_by_region_at,
        region_ats_by_index,
        transitive_closure,
        direct_subset_relations: subset_result.subset_relations,
        created_borrows: subset_result.created_borrows,
        location_insensitive_regions,
        active_region_sets_by_block_id,
        change_logs_by_region,
    }))
}
