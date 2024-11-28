use pernixc_base::{handler::Handler, source_file::Span};

use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::{
                    state::BuildSymbolError,
                    symbol::{
                        adt_implementation, adt_implementation_function,
                        constant, function, marker,
                        negative_marker_implementation,
                        negative_trait_implementation,
                        positive_marker_implementation,
                        positive_trait_implementation, r#enum, r#struct,
                        r#trait, r#type, trait_constant, trait_function,
                        trait_implementation_constant,
                        trait_implementation_function,
                        trait_implementation_type, trait_type,
                    },
                    Finalizer,
                },
                Index, RwLockContainer,
            },
            resolution, Building, Table,
        },
        AdtID, GlobalID, MarkerImplementationID, ResolvableImplementedID,
        TraitImplementationID, TraitImplementationType,
    },
    type_system::{
        self,
        environment::Environment,
        model::Model,
        normalizer::Normalizer,
        query::{Context, Record},
        term::{self, r#type::Type, Symbol},
        OverflowError,
    },
};

// assume the symbols are built with `Basic` builder
#[derive(Clone, Copy)]
pub struct TypeSystem<'a> {
    site: GlobalID,
    handler: &'a dyn Handler<Box<dyn error::Error>>,
}

impl<'a> TypeSystem<'a> {
    pub fn new(
        site: GlobalID,
        handler: &'a dyn Handler<Box<dyn error::Error>>,
    ) -> Self {
        Self { site, handler }
    }
}

impl<'a, M: Model>
    type_system::observer::Observer<M, Building<RwLockContainer, Finalizer>>
    for TypeSystem<'a>
{
    fn on_query(
        record: &Record<M>,
        environment: &Environment<
            M,
            Building<RwLockContainer, Finalizer>,
            impl Normalizer<M, Building<RwLockContainer, Finalizer>>,
            Self,
        >,
        _: &mut Context<M>,
    ) -> Result<(), OverflowError> {
        match record {
            Record::LifetimeEquality(_)
            | Record::TypeEquality(_)
            | Record::ConstantEquality(_)
            | Record::LifetimeDefinite(_)
            | Record::TypeDefinite(_)
            | Record::ConstantDefinite(_)
            | Record::LifetimeUnification(_)
            | Record::TypeUnification(_)
            | Record::ConstantUnification(_)
            | Record::LifetimeTuple(_)
            | Record::TypeTuple(_)
            | Record::ConstantTuple(_)
            | Record::LifetimeOutlives(_)
            | Record::TypeOutlives(_)
            | Record::ConstantOutlives(_)
            | Record::TypeCheck(_)
            | Record::PositiveMarkerSatisfiability(_)
            | Record::NegativeMarkerSatisfiability(_)
            | Record::PositiveTraitSatisfiability(_)
            | Record::NegativeTraitSatisfiability(_) => {}

            Record::ConstantType(q) => {
                let Type::Symbol(Symbol {
                    id: term::r#type::SymbolID::Adt(id),
                    ..
                }) = &q.query.0
                else {
                    return Ok(());
                };

                if match id {
                    AdtID::Struct(id) => environment.table().build_to(
                        *id,
                        Some(environment.observer().site),
                        r#struct::DEFINITION_STATE,
                        environment.observer().handler,
                    ),
                    AdtID::Enum(id) => environment.table().build_to(
                        *id,
                        Some(environment.observer().site),
                        r#enum::DEFINITION_STATE,
                        environment.observer().handler,
                    ),
                } == Err(BuildSymbolError::CyclicDependency)
                {
                    return Err(OverflowError);
                }
            }
        }

        Ok(())
    }

    fn on_retrieving_variance(
        adt_id: crate::symbol::AdtID,
        environment: &Environment<
            M,
            Building<RwLockContainer, Finalizer>,
            impl Normalizer<M, Building<RwLockContainer, Finalizer>>,
            Self,
        >,
    ) -> Result<(), OverflowError> {
        let result = match adt_id {
            crate::symbol::AdtID::Struct(id) => environment.table().build_to(
                id,
                Some(environment.observer().site),
                r#struct::DEFINITION_STATE,
                environment.observer().handler,
            ),
            crate::symbol::AdtID::Enum(id) => environment.table().build_to(
                id,
                Some(environment.observer().site),
                r#enum::DEFINITION_STATE,
                environment.observer().handler,
            ),
        };

        match result {
            Ok(()) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
            Err(BuildSymbolError::CyclicDependency) => Err(OverflowError),
            Err(BuildSymbolError::InvalidStateFlag { .. }) => {
                panic!("invalid state flag!")
            }
        }
    }

    fn on_resolving_implementation(
        id: ResolvableImplementedID,
        _: &term::GenericArguments<M>,
        environment: &Environment<
            M,
            Building<RwLockContainer, Finalizer>,
            impl Normalizer<M, Building<RwLockContainer, Finalizer>>,
            Self,
        >,
    ) -> Result<(), OverflowError> {
        match id {
            ResolvableImplementedID::Trait(id) => {
                let implementations = environment
                    .table()
                    .get(id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();

                let mut error = false;

                for implementation_id in implementations {
                    if match implementation_id {
                        TraitImplementationID::Positive(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                positive_trait_implementation::ARGUMENT_STATE,
                                environment.observer().handler,
                            )
                        }
                        TraitImplementationID::Negative(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                negative_trait_implementation::ARGUMENT_STATE,
                                environment.observer().handler,
                            )
                        }
                    } == Err(BuildSymbolError::CyclicDependency)
                    {
                        error = true;
                    }
                }

                if error {
                    Err(OverflowError)
                } else {
                    Ok(())
                }
            }
            ResolvableImplementedID::Marker(id) => {
                let implementations = environment
                    .table()
                    .get(id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();

                let mut error = false;

                for implementation_id in implementations {
                    if match implementation_id {
                        MarkerImplementationID::Positive(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                positive_marker_implementation::ARGUMENT_STATE,
                                environment.observer().handler,
                            )
                        }
                        MarkerImplementationID::Negative(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                negative_marker_implementation::ARGUMENT_STATE,
                                environment.observer().handler,
                            )
                        }
                    } == Err(BuildSymbolError::CyclicDependency)
                    {
                        error = true;
                    }
                }

                if error {
                    Err(OverflowError)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn on_trait_implementation_type_resolved(
        trait_implementation_type: ID<TraitImplementationType>,
        environment: &Environment<
            M,
            Building<RwLockContainer, Finalizer>,
            impl Normalizer<M, Building<RwLockContainer, Finalizer>>,
            Self,
        >,
        _: &mut Context<M>,
    ) -> Result<(), OverflowError> {
        let result = environment.table().build_to(
            trait_implementation_type,
            Some(environment.observer().site),
            trait_implementation_type::DEFINITION_STATE,
            environment.observer().handler,
        );

        match result {
            Ok(()) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
            Err(BuildSymbolError::CyclicDependency) => Err(OverflowError),
            Err(BuildSymbolError::InvalidStateFlag { .. }) => {
                panic!("invalid state flag!")
            }
        }
    }
}

/// A builder that implements [`resolution::Observer`] trait that builds the
/// symbols while resolving the symbols.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Resolution {
    #[allow(clippy::type_complexity)]
    builder_function: for<'a, 'h> fn(
        &'a Table<Building<RwLockContainer, Finalizer>>,
        GlobalID,
        Option<GlobalID>,
        &'h dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), BuildSymbolError>,
}

impl Resolution {
    /// Creates a resolution builder that builds the symbols for generic
    /// parameters and implementation's generic arguments
    pub fn basic() -> Self {
        Self { builder_function: build_for_basic_resolution }
    }

    /// Creates a resolution builder that builds the symbols for where clause
    /// predicates.
    pub fn definition() -> Self {
        Self { builder_function: build_for_definition }
    }
}

impl<M: Model> resolution::Observer<Building<RwLockContainer, Finalizer>, M>
    for Resolution
{
    fn on_global_id_resolved(
        &mut self,
        table: &Table<Building<RwLockContainer, Finalizer>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        _: &Span,
    ) -> bool {
        (self.builder_function)(table, global_id, Some(referring_site), handler)
            .is_ok()
    }

    fn on_resolution_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &resolution::Resolution<M>,
        _: &Span,
    ) -> bool {
        true
    }

    fn on_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        ty: term::r#type::Type<M>,
        _: &pernixc_syntax::syntax_tree::r#type::Type,
    ) -> Option<term::r#type::Type<M>> {
        Some(ty)
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        lt: term::lifetime::Lifetime<M>,
        _: &pernixc_syntax::syntax_tree::Lifetime,
    ) -> Option<term::lifetime::Lifetime<M>> {
        Some(lt)
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &term::constant::Constant<M>,
        _: &pernixc_syntax::syntax_tree::Constant,
    ) {
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &term::r#type::Type<M>,
        _: &pernixc_syntax::syntax_tree::r#type::Type,
    ) {
    }

    fn on_unpacked_constant_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &term::constant::Constant<M>,
        _: &pernixc_syntax::syntax_tree::expression::Expression,
    ) {
    }
}

/// Build the symbol so that it has where clause predicates.
#[allow(clippy::too_many_lines)]
pub fn build_for_where_clause(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    let first_result = match global_id {
        GlobalID::Struct(id) => table.build_to(
            id,
            required_from,
            r#struct::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Enum(id) => table.build_to(
            id,
            required_from,
            r#enum::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Trait(id) => table.build_to(
            id,
            required_from,
            r#trait::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Type(id) => table.build_to(
            id,
            required_from,
            r#type::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Function(id) => table.build_to(
            id,
            required_from,
            function::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Constant(id) => table.build_to(
            id,
            required_from,
            constant::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitFunction(id) => table.build_to(
            id,
            required_from,
            trait_function::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitConstant(id) => table.build_to(
            id,
            required_from,
            trait_constant::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitType(id) => table.build_to(
            id,
            required_from,
            trait_type::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitImplementationFunction(id) => table.build_to(
            id,
            required_from,
            trait_implementation_function::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitImplementationConstant(id) => table.build_to(
            id,
            required_from,
            trait_implementation_constant::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::TraitImplementationType(id) => table.build_to(
            id,
            required_from,
            trait_implementation_type::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::AdtImplementationFunction(id) => table.build_to(
            id,
            required_from,
            adt_implementation_function::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::PositiveTraitImplementation(id) => table.build_to(
            id,
            required_from,
            positive_trait_implementation::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::NegativeTraitImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::AdtImplementation(id) => table.build_to(
            id,
            required_from,
            adt_implementation::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Marker(id) => table.build_to(
            id,
            required_from,
            marker::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::PositiveMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            positive_marker_implementation::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::NegativeMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::WHERE_CLAUSE_STATE,
            handler,
        ),

        GlobalID::Variant(_) | GlobalID::Module(_) => Ok(()),
    };

    match first_result {
        Ok(()) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
        err @ Err(_) => err,
    }
}

/// Buld the symbol so that it has basic information required for resolution.
///
/// This includes:
///
/// - Generic parameters are built.
/// - Generic arguments are built for the implementation symbols.
#[allow(clippy::too_many_lines)]
pub fn build_for_basic_resolution(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    let first_result = match global_id {
        GlobalID::Struct(id) => table.build_to(
            id,
            required_from,
            r#struct::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Enum(id) => table.build_to(
            id,
            required_from,
            r#enum::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Trait(id) => table.build_to(
            id,
            required_from,
            r#trait::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Type(id) => {
            table.build_to(id, required_from, r#type::DEFINITION_STATE, handler)
        }

        GlobalID::Function(id) => table.build_to(
            id,
            required_from,
            function::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Constant(id) => table.build_to(
            id,
            required_from,
            constant::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitFunction(id) => table.build_to(
            id,
            required_from,
            trait_function::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitConstant(id) => table.build_to(
            id,
            required_from,
            trait_constant::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitType(id) => table.build_to(
            id,
            required_from,
            trait_type::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitImplementationFunction(id) => table.build_to(
            id,
            required_from,
            trait_implementation_function::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitImplementationConstant(id) => table.build_to(
            id,
            required_from,
            trait_implementation_constant::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitImplementationType(id) => table.build_to(
            id,
            required_from,
            trait_implementation_type::DEFINITION_STATE,
            handler,
        ),

        GlobalID::AdtImplementationFunction(id) => table.build_to(
            id,
            required_from,
            adt_implementation_function::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::PositiveTraitImplementation(id) => table.build_to(
            id,
            required_from,
            positive_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::NegativeTraitImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::AdtImplementation(id) => table.build_to(
            id,
            required_from,
            adt_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::Marker(id) => table.build_to(
            id,
            required_from,
            marker::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::PositiveMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            positive_marker_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::NegativeMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::Variant(_) | GlobalID::Module(_) => Ok(()),
    };

    match first_result {
        Ok(()) | Err(BuildSymbolError::EntryNotFound(_)) => {}
        err @ Err(_) => return err,
    }

    // this variable will either be AdtImplementation or
    // PositiveTraitImplementation
    let implementaion_member_parent_id = match global_id {
        id @ (GlobalID::TraitImplementationFunction(_)
        | GlobalID::TraitImplementationType(_)
        | GlobalID::TraitImplementationConstant(_)
        | GlobalID::AdtImplementationFunction(_)) => {
            Some(table.get_global(id).unwrap().parent_global_id().unwrap())
        }

        _ => None,
    };

    match implementaion_member_parent_id {
        Some(GlobalID::PositiveTraitImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::ARGUMENT_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ Err(
                    BuildSymbolError::CyclicDependency
                    | BuildSymbolError::InvalidStateFlag { .. },
                ) => err,
            }
        }
        Some(GlobalID::AdtImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::ARGUMENT_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ Err(
                    BuildSymbolError::CyclicDependency
                    | BuildSymbolError::InvalidStateFlag { .. },
                ) => err,
            }
        }

        _ => Ok(()),
    }
}

/// Build the symbol so that it has the definition information.
pub fn build_for_definition(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    build_for_definition_internal(
        table,
        global_id,
        required_from,
        false,
        handler,
    )
}

#[allow(clippy::too_many_lines)]
fn build_for_definition_internal(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    pre_definition: bool,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    let first_result = match global_id {
        GlobalID::Struct(id) => table.build_to(
            id,
            required_from,
            if pre_definition {
                r#struct::PRE_DEFINITION_STATE
            } else {
                r#struct::DEFINITION_STATE
            },
            handler,
        ),

        GlobalID::Enum(id) => table.build_to(
            id,
            required_from,
            if pre_definition {
                r#enum::PRE_DEFINITION_STATE
            } else {
                r#enum::DEFINITION_STATE
            },
            handler,
        ),

        GlobalID::Trait(id) => table.build_to(
            id,
            required_from,
            r#trait::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Type(id) => {
            table.build_to(id, required_from, r#type::DEFINITION_STATE, handler)
        }

        GlobalID::Function(id) => table.build_to(
            id,
            required_from,
            function::SIGNATURE_STATE,
            handler,
        ),

        GlobalID::Constant(id) => table.build_to(
            id,
            required_from,
            constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitFunction(id) => table.build_to(
            id,
            required_from,
            trait_function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitConstant(id) => table.build_to(
            id,
            required_from,
            trait_constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitType(id) => table.build_to(
            id,
            required_from,
            trait_type::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitImplementationFunction(id) => table.build_to(
            id,
            required_from,
            trait_implementation_function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitImplementationConstant(id) => table.build_to(
            id,
            required_from,
            trait_implementation_constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitImplementationType(id) => table.build_to(
            id,
            required_from,
            trait_implementation_type::DEFINITION_STATE,
            handler,
        ),

        GlobalID::AdtImplementationFunction(id) => table.build_to(
            id,
            required_from,
            adt_implementation_function::SIGNATURE_STATE,
            handler,
        ),

        GlobalID::PositiveTraitImplementation(id) => table.build_to(
            id,
            required_from,
            positive_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::NegativeTraitImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::Marker(id) => table.build_to(
            id,
            required_from,
            marker::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::PositiveMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            positive_marker_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::NegativeMarkerImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::ARGUMENT_STATE,
            handler,
        ),

        GlobalID::AdtImplementation(id) => {
            let first = table.build_to(
                id,
                required_from,
                adt_implementation::ARGUMENT_STATE,
                handler,
            );
            let implemented_id = table.get(id).unwrap().implemented_id;
            let second = match implemented_id {
                AdtID::Struct(id) => build_for_definition_internal(
                    table,
                    id.into(),
                    required_from,
                    pre_definition,
                    handler,
                ),
                AdtID::Enum(id) => build_for_definition_internal(
                    table,
                    id.into(),
                    required_from,
                    pre_definition,
                    handler,
                ),
            };

            first?;
            second
        }

        GlobalID::Variant(_) | GlobalID::Module(_) => Ok(()),
    };

    match first_result {
        Ok(()) | Err(BuildSymbolError::EntryNotFound(_)) => {}
        err @ Err(_) => return err,
    }

    // this variable will either be AdtImplementation or
    // PositiveTraitImplementation
    let implementaion_member_parent_id = match global_id {
        id @ (GlobalID::TraitImplementationFunction(_)
        | GlobalID::TraitImplementationType(_)
        | GlobalID::TraitImplementationConstant(_)
        | GlobalID::AdtImplementationFunction(_)) => {
            Some(table.get_global(id).unwrap().parent_global_id().unwrap())
        }

        _ => None,
    };

    match implementaion_member_parent_id {
        Some(GlobalID::PositiveTraitImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::ARGUMENT_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ Err(
                    BuildSymbolError::CyclicDependency
                    | BuildSymbolError::InvalidStateFlag { .. },
                ) => err,
            }
        }
        Some(GlobalID::AdtImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::ARGUMENT_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ Err(
                    BuildSymbolError::CyclicDependency
                    | BuildSymbolError::InvalidStateFlag { .. },
                ) => err,
            }
        }

        _ => Ok(()),
    }
}
