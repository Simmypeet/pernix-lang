use std::collections::{BTreeSet, HashMap, HashSet};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use pernixc_abort::Abort;
use pernixc_arena::ID;
use pernixc_handler::Handler;
use pernixc_semantic::{
    component::{
        derived::{
            elided_lifetimes::{ElidedLifetimeID, ElidedLifetimes},
            fields::Fields,
            function_signature::FunctionSignature,
            generic_parameters::{GenericParameters, LifetimeParameterID},
            ir::{
                address::Address,
                control_flow_graph::{Block, Point},
                instruction::{Instruction, Store},
                value::{
                    register::{
                        Array, Assignment, Borrow, FunctionCall, Phi, Register,
                        Struct, Tuple, Variant,
                    },
                    Value,
                },
                Representation, Values,
            },
            variances::Variance,
            variant,
        },
        input::{Parent, SymbolKind},
    },
    diagnostic::Diagnostic,
    table::GlobalID,
    term::{
        instantiation::{self, Instantiation},
        predicate::{self, PositiveTrait, Predicate},
        r#type::Type,
        visitor::RecursiveIterator,
        Model, ModelOf,
    },
};
use pernixc_source_file::Span;
use pernixc_transitive_closure::TransitiveClosure;
use pernixc_type_of::TypeOf;
use pernixc_type_system::{
    compatible::Compatibility,
    environment::{Environment, GetActivePremiseExt},
    normalizer::Normalizer,
    well_formedness, Succeeded,
};

use super::cache::{RegionVariances, RegisterInfos};
use crate::{
    get_regions_in_address, LocalRegionID, Model as BorrowModel,
    NonStaticUniversalRegion, Region, UniversalRegion,
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
    pub local_region: LocalRegionID,

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

    pub const fn new_location_insensitive(local_region: LocalRegionID) -> Self {
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
    created_borrows:
        HashMap<ID<Register<BorrowModel>>, (LocalRegionID, Point<BorrowModel>)>,

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
pub struct Builder<'a, N: Normalizer<BorrowModel>> {
    representation: &'a Representation<BorrowModel>,
    register_infos: &'a RegisterInfos,
    current_site: GlobalID,
    environment: &'a Environment<'a, BorrowModel, N>,
    region_variances: &'a RegionVariances,

    /// A map between the region and the instruction index where the region
    /// was last changed.
    ///
    /// The "long-lived" regions (created by allocas or universal regions) will
    /// present in this map. The value is the point in the control flow
    /// graph where the region is most updated.
    latest_change_points_by_region: HashMap<Region, Option<usize>>,

    handler: &'a dyn Handler<Box<dyn Diagnostic>>,
}

impl<N: Normalizer<BorrowModel>> Clone for Builder<'_, N> {
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
            handler: self.handler,
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
    borrow_created: Option<(ID<Register<BorrowModel>>, LocalRegionID)>,

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

#[allow(clippy::too_many_lines)]
pub(super) fn get_changes_of_struct(
    values: &Values<BorrowModel>,
    struct_lit: &Struct<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let struct_generic_params =
        environment.table().query::<GenericParameters>(struct_lit.struct_id)?;

    let instantiation = Instantiation::from_generic_arguments(
        struct_lit.generic_arguments.clone(),
        struct_lit.struct_id,
        &struct_generic_params,
    )
    .unwrap();

    let mut lifetime_constraints = BTreeSet::new();

    let fields = environment.table().query::<Fields>(struct_lit.struct_id)?;

    // compare each values in the field to the struct's field type
    for field_id in fields.field_declaration_order.iter().copied() {
        let mut field_ty = Type::from_other_model(
            fields.fields.get(field_id).unwrap().r#type.clone(),
        );
        instantiation::instantiate(&mut field_ty, &instantiation);

        let value_span = match &struct_lit
            .initializers_by_field_id
            .get(&field_id)
        {
            Some(Value::Register(id)) => {
                values.registers.get(*id).unwrap().span.clone().unwrap()
            }
            Some(Value::Literal(literal)) => literal.span().cloned().unwrap(),
            None => unreachable!(),
        };

        let Succeeded { result: value_ty, constraints: value_constraints } =
            values
                .type_of(
                    struct_lit.initializers_by_field_id.get(&field_id).unwrap(),
                    current_site,
                    environment,
                )
                .map_err(|x| {
                    x.report_overflow(|x| {
                        x.report_as_type_calculating_overflow(
                            value_span.clone(),
                            handler,
                        )
                    })
                })?;

        lifetime_constraints.extend(value_constraints);

        let compatibility = environment
            .compatible(&value_ty, &field_ty, Variance::Covariant)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(value_span.clone(), handler)
                })
            })?;

        // append the lifetime constraints
        if let Some(Succeeded {
            result,
            constraints: compatibility_constraints,
        }) = compatibility
        {
            assert!(result.forall_lifetime_errors.is_empty());
            assert!(result
                .forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty());

            lifetime_constraints.extend(compatibility_constraints);
        }
    }

    let well_fromed_lifetime_constraints = well_formedness::check(
        struct_lit.struct_id,
        &instantiation,
        false,
        environment,
    )?
    .0;

    Ok(Changes {
        subset_relations: lifetime_constraints
            .into_iter()
            .chain(well_fromed_lifetime_constraints)
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

pub(super) fn get_changes_of_phi(
    values: &Values<BorrowModel>,
    phi: &Phi<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let mut constraints = BTreeSet::new();
    for value in phi.incoming_values.values() {
        let Succeeded { result: value_ty, constraints: value_ty_constraints } =
            values.type_of(value, current_site, environment).map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(
                        match value {
                            Value::Register(id) => values
                                .registers
                                .get(*id)
                                .unwrap()
                                .span
                                .clone()
                                .unwrap(),
                            Value::Literal(literal) => {
                                literal.span().cloned().unwrap()
                            }
                        },
                        handler,
                    )
                })
            })?;

        constraints.extend(value_ty_constraints);

        let compatibility = environment
            .compatible(&value_ty, &phi.r#type, Variance::Covariant)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(span.clone(), handler)
                })
            })?;

        if let Some(Succeeded {
            result:
                Compatibility {
                    forall_lifetime_instantiations,
                    forall_lifetime_errors,
                },
            constraints: compatibility_constraints,
        }) = compatibility
        {
            assert!(forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty());
            assert!(forall_lifetime_errors.is_empty());

            constraints.extend(compatibility_constraints);
        }
    }

    Ok(Changes {
        subset_relations: constraints
            .into_iter()
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

pub(super) fn get_changes_of_array(
    values: &Values<BorrowModel>,
    array: &Array<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let array_ty = array.element_type.clone();
    let mut lifetime_constraints = BTreeSet::new();

    for value in &array.elements {
        let value_span = match value {
            Value::Register(id) => {
                values.registers.get(*id).unwrap().span.clone().unwrap()
            }
            Value::Literal(literal) => literal.span().cloned().unwrap(),
        };

        let Succeeded { result: value_ty, constraints } =
            values.type_of(value, current_site, environment).map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        value_span.clone(),
                        handler,
                    )
                })
            })?;

        lifetime_constraints.extend(constraints);

        let compatibility = environment
            .compatible(&value_ty, &array_ty, Variance::Covariant)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(value_span.clone(), handler)
                })
            })?;

        // append the lifetime constraints
        if let Some(Succeeded {
            result,
            constraints: compatibility_constraints,
        }) = compatibility
        {
            assert!(result.forall_lifetime_errors.is_empty());
            assert!(result
                .forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty());

            lifetime_constraints.extend(compatibility_constraints);
        }
    }

    Ok(Changes {
        subset_relations: lifetime_constraints
            .into_iter()
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

#[allow(clippy::too_many_lines)]
pub(super) fn get_changes_of_variant(
    values: &Values<BorrowModel>,
    variant: &Variant<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let enum_id = GlobalID::new(
        variant.variant_id.target_id,
        environment.table().get::<Parent>(variant.variant_id).unwrap(),
    );

    let enum_generic_parameters =
        environment.table().query::<GenericParameters>(enum_id)?;

    let variant_sym =
        environment.table().query::<variant::Variant>(variant.variant_id)?;

    let instantiation = Instantiation::from_generic_arguments(
        variant.generic_arguments.clone(),
        enum_id,
        &enum_generic_parameters,
    )
    .unwrap();

    let mut lifetime_constraints = BTreeSet::new();

    // compare each values in the field to the struct's field type
    if let Some(mut associated_type) = variant_sym
        .associated_type
        .as_ref()
        .map(|x| BorrowModel::from_default_type(x.clone()))
    {
        instantiation::instantiate(&mut associated_type, &instantiation);
        let associated_value = variant.associated_value.as_ref().unwrap();
        let value_span = match associated_value {
            Value::Register(id) => {
                values.registers.get(*id).unwrap().span.clone().unwrap()
            }
            Value::Literal(literal) => literal.span().cloned().unwrap(),
        };

        let Succeeded { result: value_ty, constraints: value_constraints } =
            values
                .type_of(associated_value, current_site, environment)
                .map_err(|x| {
                    x.report_overflow(|x| {
                        x.report_as_type_calculating_overflow(
                            value_span.clone(),
                            handler,
                        )
                    })
                })?;

        lifetime_constraints.extend(value_constraints);

        let compatibility = environment
            .compatible(&value_ty, &associated_type, Variance::Covariant)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(value_span.clone(), handler)
                })
            })?;

        // append the lifetime constraints
        if let Some(Succeeded {
            result,
            constraints: compatibility_constraints,
        }) = compatibility
        {
            assert!(result.forall_lifetime_errors.is_empty());
            assert!(result
                .forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty());

            lifetime_constraints.extend(compatibility_constraints);
        }
    }

    // handle the constraints introduced by the outlive predicates of the
    // struct
    let well_fromed_lifetime_constraints =
        well_formedness::check(enum_id, &instantiation, false, environment)?.0;

    Ok(Changes {
        subset_relations: lifetime_constraints
            .into_iter()
            .chain(well_fromed_lifetime_constraints)
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

#[allow(clippy::too_many_lines)]
pub(super) fn get_changes_of_tuple(
    values: &Values<BorrowModel>,
    tuple: &Tuple<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
) -> Result<Changes, Abort> {
    let mut lifetime_constraints = BTreeSet::new();
    for element in tuple.elements.iter().filter(|x| x.is_unpacked) {
        let ty = values
            .type_of(&element.value, current_site, environment)
            .unwrap()
            .result;

        let predicate = Predicate::TupleType(predicate::Tuple(ty));
        lifetime_constraints.extend(
            well_formedness::predicate_satisfied(
                predicate,
                None,
                false,
                environment,
            )?
            .0,
        );
    }

    Ok(Changes {
        subset_relations: lifetime_constraints
            .into_iter()
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

#[allow(clippy::too_many_lines)]
pub(super) fn get_changes_of_function_call(
    values: &Values<BorrowModel>,
    function_call: &FunctionCall<BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let function_signature = environment
        .table()
        .query::<FunctionSignature>(function_call.callable_id)?;

    let mut lifetime_constraints = BTreeSet::new();

    for (parameter, argument) in function_signature
        .parameter_order
        .iter()
        .copied()
        .map(|x| function_signature.parameters.get(x).unwrap())
        .zip(&function_call.arguments)
    {
        /*
        The c-varargs will break this assertion
        assert_eq!(
            function_signature.parameter_order.len(),
            function_call.arguments.len()
        );
        */

        let mut parameter_ty = Type::from_other_model(parameter.r#type.clone());
        instantiation::instantiate(
            &mut parameter_ty,
            &function_call.instantiation,
        );

        // obtains the type of argument ty
        let argument_span = match argument {
            Value::Register(id) => {
                values.registers.get(*id).unwrap().span.clone().unwrap()
            }
            Value::Literal(literal) => literal.span().cloned().unwrap(),
        };

        let Succeeded {
            result: argument_ty,
            constraints: argument_ty_constraints,
        } = values.type_of(argument, current_site, environment).map_err(
            |x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(
                        argument_span.clone(),
                        handler,
                    )
                })
            },
        )?;

        lifetime_constraints.extend(argument_ty_constraints);

        let compatibility = environment
            .compatible(&argument_ty, &parameter_ty, Variance::Covariant)
            .map_err(|x| {
                x.report_overflow(|x| {
                    x.report_as_type_check_overflow(argument_span, handler)
                })
            })?;

        // append the lifetime constraints
        if let Some(Succeeded {
            result,
            constraints: compatibility_constraints,
        }) = compatibility
        {
            assert!(result.forall_lifetime_errors.is_empty());
            assert!(result
                .forall_lifetime_instantiations
                .lifetimes_by_forall
                .is_empty());

            lifetime_constraints.extend(compatibility_constraints);
        }
    }

    let mut well_formedness_constraints = well_formedness::check(
        function_call.callable_id,
        &function_call.instantiation,
        false,
        environment,
    )?
    .0;

    let symbol_kind =
        *environment.table().get::<SymbolKind>(function_call.callable_id);

    match symbol_kind {
        SymbolKind::Function | SymbolKind::ExternFunction => {}

        SymbolKind::TraitFunction => {
            // parent trait requirement
            let parent_trait_id = GlobalID::new(
                function_call.callable_id.target_id,
                environment
                    .table()
                    .get::<Parent>(function_call.callable_id)
                    .unwrap(),
            );

            let trait_generic_params = environment
                .table()
                .query::<GenericParameters>(parent_trait_id)?;

            let trait_arguments = function_call
                .instantiation
                .create_generic_arguments(
                    parent_trait_id,
                    &trait_generic_params,
                )
                .unwrap();

            // check extra trait satisfiability
            well_formedness_constraints.extend(
                well_formedness::predicate_satisfied(
                    predicate::Predicate::PositiveTrait(PositiveTrait {
                        trait_id: parent_trait_id,
                        is_const: false, /* TODO: reflect the
                                          * actual value */
                        generic_arguments: trait_arguments,
                    }),
                    None,
                    false,
                    environment,
                )?
                .0,
            );
        }

        SymbolKind::TraitImplementationFunction
        | SymbolKind::AdtImplementationFunction => {
            let parent_implementation_id = GlobalID::new(
                function_call.callable_id.target_id,
                environment
                    .table()
                    .get::<Parent>(function_call.callable_id)
                    .unwrap(),
            );

            well_formedness_constraints.extend(
                well_formedness::check(
                    parent_implementation_id,
                    &function_call.instantiation,
                    false,
                    environment,
                )?
                .0,
            );
        }

        SymbolKind::Module
        | SymbolKind::Struct
        | SymbolKind::Trait
        | SymbolKind::Enum
        | SymbolKind::Type
        | SymbolKind::Constant
        | SymbolKind::Variant
        | SymbolKind::TraitType
        | SymbolKind::TraitConstant
        | SymbolKind::PositiveTraitImplementation
        | SymbolKind::NegativeTraitImplementation
        | SymbolKind::TraitImplementationType
        | SymbolKind::TraitImplementationConstant
        | SymbolKind::AdtImplementation
        | SymbolKind::Marker
        | SymbolKind::PositiveMarkerImplementation
        | SymbolKind::NegativeMarkerImplementation => {
            panic!("Unexpected symbol kind encountered")
        }
    }

    Ok(Changes {
        subset_relations: lifetime_constraints
            .into_iter()
            .chain(well_formedness_constraints)
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: HashSet::new(),
    })
}

pub(super) fn get_changes_of_borrow(
    values: &Values<BorrowModel>,
    borrow: &Borrow<BorrowModel>,
    span: &Span,
    register_id: ID<Register<BorrowModel>>,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let regions_in_address = get_regions_in_address(
        values,
        &borrow.address,
        span,
        true,
        current_site,
        environment,
        handler,
    )?;

    let borrow_local_region = borrow.lifetime.into_inference().unwrap();

    Ok(Changes {
        subset_relations: {
            regions_in_address
                .into_iter()
                .map(|x| (x, Region::Local(borrow_local_region), span.clone()))
                .collect()
        },
        borrow_created: Some((register_id, borrow_local_region)),
        overwritten_regions: HashSet::new(),
    })
}

pub(super) fn get_changes_of_store_internal(
    values: &Values<BorrowModel>,
    store_address: &Address<BorrowModel>,
    value_type: Succeeded<Type<BorrowModel>, BorrowModel>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let Succeeded { result: address_ty, constraints: address_constraints } =
        values.type_of(store_address, current_site, environment).map_err(
            |x| {
                x.report_overflow(|x| {
                    x.report_as_type_calculating_overflow(span.clone(), handler)
                })
            },
        )?;

    // get the compatibility constraints between the value and the address
    let compatibility = environment
        .compatible(&value_type.result, &address_ty, Variance::Covariant)
        .map_err(|x| {
            x.report_overflow(|x| {
                x.report_as_type_check_overflow(span.clone(), handler)
            })
        })?;

    let compatibility_constraints = if let Some(Succeeded {
        result:
            Compatibility { forall_lifetime_instantiations, forall_lifetime_errors },
        constraints: compatibility_constraints,
    }) = compatibility
    {
        assert!(forall_lifetime_instantiations.lifetimes_by_forall.is_empty());
        assert!(forall_lifetime_errors.is_empty());

        compatibility_constraints
    } else {
        BTreeSet::new()
    };

    Ok(Changes {
        subset_relations: value_type
            .constraints
            .into_iter()
            .chain(address_constraints)
            .chain(compatibility_constraints)
            .filter_map(|x| {
                let x = x.into_lifetime_outlives().ok()?;

                let from = Region::try_from(x.operand).ok()?;
                let to = Region::try_from(x.bound).ok()?;

                Some((from, to, span.clone()))
            })
            .collect(),
        borrow_created: None,
        overwritten_regions: RecursiveIterator::new(&address_ty)
            .filter_map(|x| x.0.into_lifetime().ok())
            .filter_map(|x| Region::try_from(*x).ok())
            .collect::<HashSet<_>>(),
    })
}

pub(super) fn get_changes_of_store(
    values: &Values<BorrowModel>,
    store_inst: &Store<BorrowModel>,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, impl Normalizer<BorrowModel>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Changes, Abort> {
    let value_ty = values
        .type_of(&store_inst.value, current_site, environment)
        .map_err(|x| {
            x.report_overflow(|x| {
                x.report_as_type_calculating_overflow(
                    store_inst.span.clone().unwrap(),
                    handler,
                )
            })
        })?;

    get_changes_of_store_internal(
        values,
        &store_inst.address,
        value_ty,
        &store_inst.span.clone().unwrap(),
        current_site,
        environment,
        handler,
    )
}

impl<N: Normalizer<BorrowModel>> Builder<'_, N> {
    /// Returns a list of new region introduced by the scope push instruction.
    ///
    /// This function returns all the regions that are created by the variables
    /// declared in the scope push instruction.
    pub fn get_new_regions(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<HashSet<Region>, Abort> {
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

                    for global_id in self
                        .environment
                        .table()
                        .scope_walker(self.current_site)
                        .map(|x| GlobalID::new(self.current_site.target_id, x))
                    {
                        let symbol_kind = self
                            .environment
                            .table()
                            .get::<SymbolKind>(global_id);

                        if symbol_kind.has_generic_parameters() {
                            regions.extend(
                                self.environment
                                    .table()
                                    .query::<GenericParameters>(global_id)?
                                    .lifetime_order()
                                    .iter()
                                    .copied()
                                    .map(|x| {
                                        Region::Universal(
                                            UniversalRegion::NonStatic(
                                                NonStaticUniversalRegion::Named(
                                                    LifetimeParameterID {
                                                        parent: global_id,
                                                        id: x,
                                                    },
                                                ),
                                            ),
                                        )
                                    }),
                            );
                        }

                        if symbol_kind.has_elided_lifetimes() {
                            regions.extend(
                                self.environment
                                    .table()
                                    .query::<ElidedLifetimes>(global_id)?
                                    .elided_lifetimes
                                    .ids()
                                    .map(|x| {
                                        Region::Universal(
                                            UniversalRegion::NonStatic(
                                                NonStaticUniversalRegion::Elided(
                                                    ElidedLifetimeID {
                                                        parent: global_id,
                                                        id: x,
                                                    },
                                                ),
                                            ),
                                        )
                                    }),
                            );
                        }
                    }
                }

                Ok(regions)
            }

            Instruction::ScopePop(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::RegisterDiscard(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => Ok(HashSet::new()),
        }
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    pub fn get_removing_regions(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<HashSet<Region>, Abort> {
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

                    for global_id in self
                        .environment
                        .table()
                        .scope_walker(self.current_site)
                        .map(|x| GlobalID::new(self.current_site.target_id, x))
                    {
                        let symbol_kind = self
                            .environment
                            .table()
                            .get::<SymbolKind>(global_id);

                        if symbol_kind.has_generic_parameters() {
                            regions.extend(
                                self.environment
                                    .table()
                                    .query::<GenericParameters>(global_id)?
                                    .lifetime_order()
                                    .iter()
                                    .copied()
                                    .map(|x| {
                                        Region::Universal(
                                            UniversalRegion::NonStatic(
                                                NonStaticUniversalRegion::Named(
                                                    LifetimeParameterID {
                                                        parent: global_id,
                                                        id: x,
                                                    },
                                                ),
                                            ),
                                        )
                                    }),
                            );
                        }

                        if symbol_kind.has_elided_lifetimes() {
                            regions.extend(
                                self.environment
                                    .table()
                                    .query::<ElidedLifetimes>(global_id)?
                                    .elided_lifetimes
                                    .ids()
                                    .map(|x| {
                                        Region::Universal(
                                            UniversalRegion::NonStatic(
                                                NonStaticUniversalRegion::Elided(
                                                    ElidedLifetimeID {
                                                        parent: global_id,
                                                        id: x,
                                                    },
                                                ),
                                            ),
                                        )
                                    }),
                            );
                        }
                    }
                }

                Ok(regions)
            }

            Instruction::RegisterDiscard(_)
            | Instruction::RegisterAssignment(_)
            | Instruction::ScopePush(_)
            | Instruction::DropUnpackTuple(_)
            | Instruction::Store(_)
            | Instruction::TuplePack(_)
            | Instruction::Drop(_) => Ok(HashSet::new()),
        }
    }

    /// Returns a list of regions that are removed by the scope pop instruction.
    #[allow(clippy::too_many_lines)]
    pub fn get_changes(
        &self,
        instruction: &Instruction<BorrowModel>,
    ) -> Result<Changes, Abort> {
        match instruction {
            Instruction::Store(store) => get_changes_of_store(
                &self.representation.values,
                store,
                self.current_site,
                self.environment,
                self.handler,
            ),
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
                        let function_signature = self
                            .environment
                            .table()
                            .query::<FunctionSignature>(
                            self.current_site,
                        )?;

                        let return_ty = BorrowModel::from_default_type(
                            function_signature.return_type.clone(),
                        );

                        let register_span = self
                            .representation
                            .values
                            .registers
                            .get(register_assignment.id)
                            .unwrap()
                            .span
                            .clone()
                            .unwrap();
                        let Succeeded {
                            result: register_ty,
                            constraints: register_constraints,
                        } = self
                            .representation
                            .values
                            .type_of(
                                register_assignment.id,
                                self.current_site,
                                self.environment,
                            )
                            .map_err(|x| {
                                x.report_overflow(|x| {
                                    x.report_as_type_calculating_overflow(
                                        register_span.clone(),
                                        self.handler,
                                    )
                                })
                            })?;

                        let compatibility = self
                            .environment
                            .compatible(
                                &register_ty,
                                &return_ty,
                                Variance::Covariant,
                            )
                            .map_err(|x| {
                                x.report_overflow(|x| {
                                    x.report_as_type_check_overflow(
                                        register_span,
                                        self.handler,
                                    )
                                })
                            })?;

                        let constraitns = if let Some(Succeeded {
                            result:
                                Compatibility {
                                    forall_lifetime_instantiations,
                                    forall_lifetime_errors,
                                },
                            constraints: compatibility_constraints,
                        }) = compatibility
                        {
                            assert!(forall_lifetime_instantiations
                                .lifetimes_by_forall
                                .is_empty());
                            assert!(forall_lifetime_errors.is_empty());

                            compatibility_constraints
                        } else {
                            BTreeSet::new()
                        };

                        Ok(register_constraints
                            .into_iter()
                            .chain(constraitns.into_iter())
                            .filter_map(|x| {
                                let x = x.into_lifetime_outlives().ok()?;

                                let from = Region::try_from(x.operand).ok()?;
                                let to = Region::try_from(x.bound).ok()?;

                                Some((
                                    from,
                                    to,
                                    return_inst.span.clone().unwrap(),
                                ))
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
                    | Assignment::Load(_) => Ok(Changes::default()),

                    Assignment::Tuple(tuple) => get_changes_of_tuple(
                        &self.representation.values,
                        tuple,
                        register.span.as_ref().unwrap(),
                        self.current_site,
                        self.environment,
                    ),
                    Assignment::Borrow(borrow) => get_changes_of_borrow(
                        &self.representation.values,
                        borrow,
                        register.span.as_ref().unwrap(),
                        register_assignment.id,
                        self.current_site,
                        self.environment,
                        self.handler,
                    ),
                    Assignment::FunctionCall(function_call) => {
                        get_changes_of_function_call(
                            &self.representation.values,
                            function_call,
                            register.span.as_ref().unwrap(),
                            self.current_site,
                            self.environment,
                            self.handler,
                        )
                    }
                    Assignment::Struct(struct_lit) => get_changes_of_struct(
                        &self.representation.values,
                        struct_lit,
                        register.span.as_ref().unwrap(),
                        self.current_site,
                        self.environment,
                        self.handler,
                    ),
                    Assignment::Variant(variant) => get_changes_of_variant(
                        &self.representation.values,
                        variant,
                        register.span.as_ref().unwrap(),
                        self.current_site,
                        self.environment,
                        self.handler,
                    ),
                    Assignment::Array(array) => get_changes_of_array(
                        &self.representation.values,
                        array,
                        register.span.as_ref().unwrap(),
                        self.current_site,
                        self.environment,
                        self.handler,
                    ),
                    Assignment::Phi(phi) => get_changes_of_phi(
                        &self.representation.values,
                        phi,
                        register.span.as_ref().unwrap(),
                        self.current_site,
                        self.environment,
                        self.handler,
                    ),
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
                    .type_of(
                        &tuple_pack.tuple_address,
                        self.current_site,
                        self.environment,
                    )
                    .map_err(|x| {
                        x.report_overflow(|x| {
                            x.report_as_type_calculating_overflow(
                                tuple_pack.packed_tuple_span.clone().unwrap(),
                                self.handler,
                            )
                        })
                    })?;

                get_changes_of_store_internal(
                    &self.representation.values,
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
                    self.current_site,
                    self.environment,
                    self.handler,
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

impl<N: Normalizer<BorrowModel>> Builder<'_, N> {
    pub fn walk_instruction(
        &mut self,
        instruction: &Instruction<BorrowModel>,
        instruction_point: Point<BorrowModel>,
        subset_result: &mut Intermediate,
    ) -> Result<(), Abort> {
        for region in self.get_new_regions(instruction)? {
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

        for region in self.get_removing_regions(instruction)? {
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

    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Intermediate,
    ) -> Result<(), Abort> {
        let block = self
            .representation
            .control_flow_graph
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

#[derive(Clone)]
#[allow(clippy::type_complexity)]
struct Context<'a, N: Normalizer<BorrowModel>> {
    representation: &'a Representation<BorrowModel>,
    current_site: GlobalID,
    environment: &'a Environment<'a, BorrowModel, N>,
    register_infos: &'a RegisterInfos,
    region_variances: &'a RegionVariances,

    /// The key represents the block ID that needs to be checked/explored.
    ///
    /// - `None` value means the block is being processed.
    /// - `Some` value means the block has been processed
    /// - No value means the block has not been explored
    walk_results_by_block_id:
        HashMap<ID<Block<BorrowModel>>, Option<Builder<'a, N>>>,

    /// If the block id appears in this map, it means the block is a looped
    /// block and the value is the starting environment of the looped block.
    target_regions_by_block_id: HashMap<
        ID<Block<BorrowModel>>,
        (ID<Block<BorrowModel>>, HashSet<Region>),
    >,
}

impl<'a, N: Normalizer<BorrowModel>> Context<'a, N> {
    #[allow(clippy::too_many_lines)]
    pub fn walk_block(
        &mut self,
        block_id: ID<Block<BorrowModel>>,
        subset_result: &mut Intermediate,
        handler: &'a dyn Handler<Box<dyn Diagnostic>>,
    ) -> Result<Option<Builder<'a, N>>, Abort> {
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
                handler,
            };

            let predicates = self
                .environment
                .table()
                .get_active_premise::<BorrowModel>(self.current_site)
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
                    self.walk_block(predecessor_id, subset_result, handler)?
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

                handler,
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
    direct_subset_relations: HashSet<(RegionAt, RegionAt, Option<Span>)>,

    #[get = "pub"]
    created_borrows:
        HashMap<ID<Register<BorrowModel>>, (LocalRegionID, Point<BorrowModel>)>,

    /// Maps the region to the block ids that the region first appears. (mostly
    /// is the entry block)
    entry_block_ids_by_universal_regions:
        HashMap<UniversalRegion, ID<Block<BorrowModel>>>,

    change_logs_by_region: HashMap<Region, RegionChangeLog>,
    active_region_sets_by_block_id:
        HashMap<ID<Block<BorrowModel>>, HashSet<Region>>,
    location_insensitive_regions: HashSet<LocalRegionID>,
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
pub fn analyze<N: Normalizer<BorrowModel>>(
    ir: &Representation<BorrowModel>,
    register_infos: &RegisterInfos,
    region_variances: &RegionVariances,
    current_site: GlobalID,
    environment: &Environment<BorrowModel, N>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<Subset, Abort> {
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
        ir.control_flow_graph.blocks().ids().collect::<Vec<_>>();
    let mut subset_result = Intermediate {
        subset_relations: HashSet::new(),
        created_borrows: HashMap::new(),
        entry_block_ids_by_universal_regions: HashMap::new(),
    };

    for block_id in all_block_ids.iter().copied() {
        context.walk_block(block_id, &mut subset_result, handler)?;
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
