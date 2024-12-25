use std::collections::{BTreeSet, HashMap, HashSet};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_base::source_file::Span;

use super::cache::{RegionVariances, RegisterInfos};
use crate::{
    arena::ID,
    error::{OverflowOperation, TypeSystemOverflow},
    ir::{
        self,
        address::Address,
        control_flow_graph::{Block, Point},
        instruction::{Instruction, Store},
        representation::{
            borrow::{
                LocalRegion, Model as BorrowModel, Region, UniversalRegion,
            },
            Representation, Values,
        },
        value::{
            register::{
                Array, Assignment, Borrow, FunctionCall, Phi, Register, Struct,
                Variant,
            },
            Value,
        },
    },
    symbol::{
        table::{self, representation::Index},
        CallableID, GenericID, GlobalID, LifetimeParameterID,
    },
    transitive_closure::TransitiveClosure,
    type_system::{
        compatible::{Compatibility, Compatible},
        environment::Environment,
        instantiation::{self, Instantiation},
        normalizer::Normalizer,
        observer::Observer,
        predicate::{self, Outlives, PositiveTrait, Predicate},
        term::{r#type::Type, Term},
        variance::Variance,
        visitor::RecursiveIterator,
        well_formedness, LifetimeConstraint, Succeeded,
    },
};

/// Represents a point in the control flow graph where the borrow checker
/// is considering the subset relation between regions.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum RegionPoint {
    /// Involving a particular point in the control flow graph.
    InBlock(Point<BorrowModel>),

    /// The moment before executing the first instruction in the block.
    EnteringBlock(ID<Block<BorrowModel>>),
}

impl RegionPoint {
    /// Gets the block id where the region is considered.
    pub const fn block_id(&self) -> ID<Block<BorrowModel>> {
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
    pub local_region: ID<LocalRegion>,

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
        local_region: ID<LocalRegion>,
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

    /// Maps the region to the block ids that the region first appears. (mostly
    /// is the entry block)
    entry_block_ids_by_universal_regions:
        HashMap<UniversalRegion, ID<Block<BorrowModel>>>,
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
    register_infos: &'a RegisterInfos,
    current_site: GlobalID,
    environment: &'a Environment<'a, BorrowModel, S, N, O>,
    region_variances: &'a RegionVariances,

    /// A map between the region and the instruction index where the region
    /// was last changed.
    ///
    /// The "long-lived" regions (created by allocas or universal regions) will
    /// present in this map. The value is the point in the control flow
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
            register_infos: self.register_infos,
            region_variances: self.region_variances,

            current_site: self.current_site,
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
    #[allow(clippy::too_many_lines)]
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
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    pub(super) fn get_changes_of_phi<S: table::State>(
        &self,
        phi: &Phi<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let mut constraints = BTreeSet::new();
        for value in phi.incoming_values.values() {
            let Succeeded {
                result: value_ty,
                constraints: value_ty_constraints,
            } = self.type_of_value(value, current_site, environment).map_err(
                |x| TypeSystemOverflow {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: match value {
                        Value::Register(id) => {
                            self.registers.get(*id).unwrap().span.clone()
                        }
                        Value::Literal(literal) => literal.span().clone(),
                    },
                    overflow_error: x.into_overflow().unwrap(),
                },
            )?;

            constraints.extend(value_ty_constraints);

            match value_ty.compatible(
                &phi.r#type,
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

                    constraints.extend(compatibility_constraints);
                }
                Ok(None) => {
                    panic!("{value_ty:#?} => {:#?}", phi.r#type);
                }
                Err(err) => {
                    return Err(TypeSystemOverflow {
                        operation: OverflowOperation::TypeCheck,
                        overflow_span: span.clone(),
                        overflow_error: err,
                    });
                }
            }
        }

        Ok(Changes {
            subset_relations: constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    pub(super) fn get_changes_of_array<S: table::State>(
        &self,
        array: &Array<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let array_ty = array.element_type.clone();
        let mut lifetime_constraints = BTreeSet::new();

        for value in &array.elements {
            let value_span = match value {
                Value::Register(id) => {
                    self.registers.get(*id).unwrap().span.clone()
                }
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints } = self
                .type_of_value(value, current_site, environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: value_span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

            lifetime_constraints.extend(constraints);

            let copmatibility = value_ty
                .compatible(&array_ty, Variance::Covariant, environment)
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
                panic!("{value_ty:#?} => {array_ty:#?}")
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    #[allow(clippy::too_many_lines)]
    pub(super) fn get_changes_of_variant<S: table::State>(
        &self,
        variant: &Variant<BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let variant_sym = environment.table().get(variant.variant_id).unwrap();
        let enum_id = environment
            .table()
            .get(variant.variant_id)
            .unwrap()
            .parent_enum_id();
        let enum_sym = environment.table().get(enum_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            variant.generic_arguments.clone(),
            enum_id.into(),
            &enum_sym.generic_declaration.parameters,
        )
        .unwrap();

        let mut lifetime_constraints = BTreeSet::new();

        // compare each values in the field to the struct's field type
        if let Some(mut associated_type) = variant_sym
            .associated_type
            .as_ref()
            .map(|x| Type::from_default_model(x.clone()))
        {
            instantiation::instantiate(&mut associated_type, &instantiation);
            let associated_value = variant.associated_value.as_ref().unwrap();
            let value_span = match associated_value {
                Value::Register(id) => {
                    self.registers.get(*id).unwrap().span.clone()
                }
                Value::Literal(literal) => literal.span().clone(),
            };

            let Succeeded { result: value_ty, constraints: value_constraints } =
                self.type_of_value(associated_value, current_site, environment)
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: value_span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

            lifetime_constraints.extend(value_constraints);

            let copmatibility = value_ty
                .compatible(&associated_type, Variance::Covariant, environment)
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
                panic!("{value_ty:#?} => {associated_type:#?}")
            }
        }

        // handle the constraints introduced by the outlive predicates of the
        // struct

        for predicate in environment
            .table()
            .get_active_premise(variant.variant_id.into())
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
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: HashSet::new(),
        })
    }

    #[allow(clippy::too_many_lines)]
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

        let mut well_formedness_constraints = well_formedness::check(
            function_call.callable_id.into(),
            &function_call.instantiation,
            false,
            environment,
        )
        .0;

        match function_call.callable_id {
            CallableID::Function(_) => {}
            CallableID::TraitFunction(id) => {
                // parent trait requirement
                let parent_trait_id =
                    environment.table().get(id).unwrap().parent_id();

                let trait_arguments = function_call
                    .instantiation
                    .create_generic_arguments(
                        parent_trait_id.into(),
                        &environment
                            .table()
                            .get(parent_trait_id)
                            .unwrap()
                            .generic_declaration
                            .parameters,
                    )
                    .unwrap();

                // check extra trait satisfiability
                well_formedness_constraints.extend(
                    well_formedness::predicate_satisfied(
                        predicate::Predicate::PositiveTrait(PositiveTrait {
                            id: parent_trait_id,
                            is_const: false, /* TODO: reflect the
                                              * actual value */
                            generic_arguments: trait_arguments,
                        }),
                        None,
                        false,
                        environment,
                    )
                    .0,
                );
            }
            CallableID::TraitImplementationFunction(_)
            | CallableID::AdtImplementationFunction(_) => {
                let parent_implementation_id: GenericID = match function_call
                    .callable_id
                {
                    CallableID::TraitImplementationFunction(id) => {
                        environment.table().get(id).unwrap().parent_id().into()
                    }
                    CallableID::AdtImplementationFunction(id) => {
                        environment.table().get(id).unwrap().parent_id().into()
                    }

                    CallableID::Function(_) | CallableID::TraitFunction(_) => {
                        unreachable!()
                    }
                };

                well_formedness_constraints.extend(
                    well_formedness::check(
                        parent_implementation_id,
                        &function_call.instantiation,
                        false,
                        environment,
                    )
                    .0,
                );
            }
        }

        Ok(Changes {
            subset_relations: lifetime_constraints
                .into_iter()
                .chain(well_formedness_constraints)
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

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

    pub(super) fn get_changes_of_store_internal<S: table::State>(
        &self,
        store_address: &Address<BorrowModel>,
        value_type: Succeeded<Type<BorrowModel>, BorrowModel>,
        span: &Span,
        current_site: GlobalID,
        environment: &Environment<
            BorrowModel,
            S,
            impl Normalizer<BorrowModel, S>,
            impl Observer<BorrowModel, S>,
        >,
    ) -> Result<Changes, TypeSystemOverflow<ir::Model>> {
        let Succeeded { result: address_ty, constraints: address_constraints } =
            self.type_of_address(store_address, current_site, environment)
                .map_err(|x| TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeOf,
                    overflow_span: span.clone(),
                    overflow_error: x.into_overflow().unwrap(),
                })?;

        // get the compatibility constraints between the value and the address
        let compatibility_constraints = match value_type.result.compatible(
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
                panic!(
                    "incompatible types {:#?} => {address_ty:#?}",
                    value_type.result
                );
            }
            Err(err) => {
                return Err(TypeSystemOverflow::<ir::Model> {
                    operation: OverflowOperation::TypeCheck,
                    overflow_span: span.clone(),
                    overflow_error: err,
                });
            }
        };

        Ok(Changes {
            subset_relations: value_type
                .constraints
                .into_iter()
                .chain(address_constraints)
                .chain(compatibility_constraints)
                .filter_map(|x| {
                    let x = x.into_lifetime_outlives().ok()?;

                    let from = Region::try_from(x.operand.clone()).ok()?;
                    let to = Region::try_from(x.bound).ok()?;

                    Some((from, to, span.clone()))
                })
                .collect(),
            borrow_created: None,
            overwritten_regions: RecursiveIterator::new(&address_ty)
                .filter_map(|x| x.0.into_lifetime().ok())
                .filter_map(|x| Region::try_from(x.clone()).ok())
                .collect::<HashSet<_>>(),
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
        let value_ty = self
            .type_of_value(&store_inst.value, current_site, environment)
            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                operation: OverflowOperation::TypeOf,
                overflow_span: store_inst.span.clone(),
                overflow_error: x.into_overflow().unwrap(),
            })?;

        self.get_changes_of_store_internal(
            &store_inst.address,
            value_ty,
            &store_inst.span,
            current_site,
            environment,
        )
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
                let mut regions = HashSet::new();

                for alloca in
                    self.representation.values.allocas.iter().filter_map(|x| {
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
                let mut regions = HashSet::new();

                for alloca in
                    self.representation.values.allocas.iter().filter_map(|x| {
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
    #[allow(clippy::too_many_lines)]
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
                // if the register assignment will be used as a return value,
                // then, compute the subset relation against the type of the
                // return type.
                let extra_relations = self
                    .representation
                    .control_flow_graph
                    .blocks()
                    .items()
                    .filter_map(|x| {
                        x.terminator().as_ref().and_then(|x| x.as_return())
                    })
                    .find(|x| {
                        x.value == Value::Register(register_assignment.id)
                    })
                    .map(|return_inst| {
                        let callable = self
                            .environment
                            .table()
                            .get_callable(self.current_site.try_into().unwrap())
                            .unwrap();

                        let return_ty = Type::from_other_model(
                            callable.return_type().clone(),
                        );

                        let Succeeded {
                            result: register_ty,
                            constraints: register_constraints,
                        } = self
                            .representation
                            .values
                            .type_of_register(
                                register_assignment.id,
                                self.current_site,
                                self.environment,
                            )
                            .map_err(|x| TypeSystemOverflow::<ir::Model> {
                                operation: OverflowOperation::TypeOf,
                                overflow_span: self
                                    .representation
                                    .values
                                    .registers
                                    .get(register_assignment.id)
                                    .unwrap()
                                    .span
                                    .clone(),
                                overflow_error: x.into_overflow().unwrap(),
                            })?;

                        let compatibility_constraints = match register_ty
                            .compatible(
                                &return_ty,
                                Variance::Covariant,
                                self.environment,
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
                                panic!(
                                    "incompatible types {register_ty:#?} => \
                                     {return_ty:#?}"
                                );
                            }
                            Err(err) => {
                                return Err(TypeSystemOverflow::<ir::Model> {
                                    operation: OverflowOperation::TypeCheck,
                                    overflow_span: self
                                        .representation
                                        .values
                                        .registers
                                        .get(register_assignment.id)
                                        .unwrap()
                                        .span
                                        .clone(),
                                    overflow_error: err,
                                });
                            }
                        };

                        Ok(register_constraints
                            .into_iter()
                            .chain(compatibility_constraints.into_iter())
                            .filter_map(|x| {
                                let x = x.into_lifetime_outlives().ok()?;

                                let from =
                                    Region::try_from(x.operand.clone()).ok()?;
                                let to = Region::try_from(x.bound).ok()?;

                                Some((from, to, return_inst.span.clone()))
                            })
                            .collect::<HashSet<_>>())
                    })
                    .transpose()?;

                let register = self
                    .representation
                    .values
                    .registers
                    .get(register_assignment.id)
                    .unwrap();

                let changes = match &register.assignment {
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
                    Assignment::Variant(variant) => {
                        self.representation.values.get_changes_of_variant(
                            variant,
                            &register.span,
                            self.current_site,
                            self.environment,
                        )
                    }
                    Assignment::Array(array) => {
                        self.representation.values.get_changes_of_array(
                            array,
                            &register.span,
                            self.current_site,
                            self.environment,
                        )
                    }
                    Assignment::Phi(phi) => {
                        self.representation.values.get_changes_of_phi(
                            phi,
                            &register.span,
                            self.current_site,
                            self.environment,
                        )
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
            Instruction::TuplePack(tuple_pack) => {
                let tuple_ty = self
                    .representation
                    .values
                    .type_of_address(
                        &tuple_pack.tuple_address,
                        self.current_site,
                        self.environment,
                    )
                    .map_err(|x| TypeSystemOverflow::<ir::Model> {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: tuple_pack.packed_tuple_span.clone(),
                        overflow_error: x.into_overflow().unwrap(),
                    })?;

                self.representation.values.get_changes_of_store_internal(
                    &tuple_pack.store_address,
                    tuple_ty.map(|x| {
                        x.into_tuple()
                            .unwrap()
                            .elements
                            .into_iter()
                            .find_map(|x| x.is_unpacked.then_some(x.term))
                            .unwrap()
                    }),
                    &tuple_pack.packed_tuple_span,
                    self.current_site,
                    self.environment,
                )
            }
            Instruction::RegisterDiscard(_)
            | Instruction::ScopePush(_)
            | Instruction::ScopePop(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Drop(_) => Ok(Changes::default()),
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
        subset_result: &mut Intermediate,
    ) -> Result<(), TypeSystemOverflow<ir::Model>> {
        for region in self.get_new_regions(instruction) {
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
        let changes = self.get_changes(instruction)?;
        self.handle_chages(changes, subset_result, instruction_point);

        for region in self.get_removing_regions(instruction) {
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
        instruction_point: Point<BorrowModel>,
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
                            .contains(&region_at.to_region()),
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
                            .region_variances
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

    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Intermediate,
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
    register_infos: &'a RegisterInfos,
    region_variances: &'a RegionVariances,

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
    #[allow(clippy::too_many_lines)]
    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Intermediate,
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
                register_infos: self.register_infos,
                current_site: self.current_site,
                environment: self.environment,
                region_variances: self.region_variances,
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
                            .filter_map(|x| x.clone().try_into().ok())
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
                register_infos: self.register_infos,
                current_site: self.current_site,
                environment: self.environment,
                region_variances: self.region_variances,

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
                        .region_variances
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
                    .region_variances
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
    direct_subset_relations: HashSet<(RegionAt, RegionAt, Option<Span>)>,

    #[get = "pub"]
    created_borrows: HashMap<
        ID<Register<BorrowModel>>,
        (ID<LocalRegion>, Point<BorrowModel>),
    >,

    /// Maps the region to the block ids that the region first appears. (mostly
    /// is the entry block)
    entry_block_ids_by_universal_regions:
        HashMap<UniversalRegion, ID<Block<BorrowModel>>>,

    change_logs_by_region: HashMap<Region, RegionChangeLog>,
    active_region_sets_by_block_id:
        HashMap<ID<Block<BorrowModel>>, HashSet<Region>>,
    location_insensitive_regions: HashSet<ID<LocalRegion>>,
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
        borrow_register_id: ID<Register<BorrowModel>>,
        point: Point<BorrowModel>,
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
pub fn analyze<
    S: table::State,
    N: Normalizer<BorrowModel, S>,
    O: Observer<BorrowModel, S>,
>(
    ir: &ir::Representation<BorrowModel>,
    register_infos: &RegisterInfos,
    region_variances: &RegionVariances,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, S, N, O>,
) -> Result<Subset, TypeSystemOverflow<ir::Model>> {
    let mut context = Context {
        representation: ir,
        current_site,
        environment,
        register_infos,
        region_variances,
        walk_results_by_block_id: HashMap::new(),
        target_regions_by_block_id: HashMap::new(),
    };

    let all_block_ids =
        ir.control_flow_graph().blocks().ids().collect::<Vec<_>>();
    let mut subset_result = Intermediate {
        subset_relations: HashSet::new(),
        created_borrows: HashMap::new(),
        entry_block_ids_by_universal_regions: HashMap::new(),
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
