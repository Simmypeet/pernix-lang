use pernixc_base::handler::Handler;

use super::{BuildSymbolError, Finalizer};
use crate::{
    arena::ID,
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::finalize::{
                    adt_implementation, adt_implementation_function, constant,
                    function, negative_trait_implementation,
                    positive_trait_implementation, r#enum, r#struct, r#trait,
                    r#type, trait_constant, trait_function,
                    trait_implementation_constant,
                    trait_implementation_function, trait_implementation_type,
                    trait_type,
                },
                Index, RwLockContainer,
            },
            resolution::{Observer, Resolution},
            Building, Table,
        },
        GlobalID, TraitImplementationID, TraitImplementationType,
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
pub struct Definition<'a> {
    site: GlobalID,
    handler: &'a dyn Handler<Box<dyn error::Error>>,
}

impl<'a> Definition<'a> {
    pub fn new(
        site: GlobalID,
        handler: &'a dyn Handler<Box<dyn error::Error>>,
    ) -> Self {
        Self { site, handler }
    }
}

impl<'a, M: Model>
    type_system::observer::Observer<M, Building<RwLockContainer, Finalizer>>
    for Definition<'a>
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
        let build_result = match record {
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
            | Record::TypeCheck(_) => Ok(()),

            Record::ConstantType(q) => {
                let Type::Symbol(Symbol { id, .. }) = &q.query.0 else {
                    return Ok(());
                };

                match id {
                    term::r#type::SymbolID::Struct(id) => {
                        environment.table().build_to(
                            *id,
                            Some(environment.observer().site),
                            r#struct::DEFINITION_STATE,
                            environment.observer().handler,
                        )
                    }
                    term::r#type::SymbolID::Enum(id) => {
                        environment.table().build_to(
                            *id,
                            Some(environment.observer().site),
                            r#enum::DEFINITION_STATE,
                            environment.observer().handler,
                        )
                    }
                }
            }

            Record::TraitSatisfiability(q) => {
                let final_implementations = environment
                    .table()
                    .get(q.query.id)
                    .unwrap()
                    .implementations
                    .iter()
                    .copied()
                    .collect::<Vec<_>>();

                for implementation_id in final_implementations {
                    let _ = match implementation_id {
                        TraitImplementationID::Positive(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                positive_trait_implementation::ARGUMENTS_STATE,
                                environment.observer().handler,
                            )
                        }
                        TraitImplementationID::Negative(id) => {
                            environment.table().build_to(
                                id,
                                Some(environment.observer().site),
                                negative_trait_implementation::ARGUMENTS_STATE,
                                environment.observer().handler,
                            )
                        }
                    };
                }

                Ok(())
            }
        };

        match build_result {
            Ok(_) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
            Err(BuildSymbolError::CyclicDependency) => Err(OverflowError),
            Err(BuildSymbolError::InvalidStateFlag { .. }) => {
                panic!("invalid state flag!")
            }
        }
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
            Ok(_) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
            Err(BuildSymbolError::CyclicDependency) => Err(OverflowError),
            Err(BuildSymbolError::InvalidStateFlag { .. }) => {
                panic!("invalid state flag!")
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
            Some(environment.observer().site.into()),
            trait_implementation_type::DEFINITION_STATE,
            environment.observer().handler,
        );

        match result {
            Ok(_) | Err(BuildSymbolError::EntryNotFound(_)) => Ok(()),
            Err(BuildSymbolError::CyclicDependency) => Err(OverflowError),
            Err(BuildSymbolError::InvalidStateFlag { .. }) => {
                panic!("invalid state flag!")
            }
        }
    }
}

/// A struct implementing [`Observer`] trait that builds the symbols
/// so that it contains basic information required for resolution.
///
/// This directly invokes the [`build_for_basic_resolution`] function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Basic;

impl<M: Model> Observer<Building<RwLockContainer, Finalizer>, M> for Basic {
    fn on_global_id_resolved(
        &mut self,
        table: &Table<Building<RwLockContainer, Finalizer>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        _: &pernixc_lexical::token::Identifier,
    ) -> bool {
        build_for_basic_resolution(
            table,
            global_id,
            Some(referring_site),
            handler,
        )
        .is_ok()
    }

    fn on_resolution_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &Resolution<M>,
        _: &pernixc_syntax::syntax_tree::GenericIdentifier,
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
        lt: crate::type_system::term::lifetime::Lifetime<M>,
        _: &pernixc_syntax::syntax_tree::Lifetime,
    ) -> Option<crate::type_system::term::lifetime::Lifetime<M>> {
        Some(lt)
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::constant::Constant<M>,
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
        _: &crate::type_system::term::constant::Constant<M>,
        _: &pernixc_syntax::syntax_tree::expression::Expression,
    ) {
    }
}
/// Build the symbol so that it has where clause predicates.
pub fn build_for_where_clause(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    match global_id {
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

        GlobalID::Variant(_) => Ok(()),

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

        GlobalID::Module(_) => Ok(()),
    }
}

/// Buld the symbol so that it has basic information required for resolution.
///
/// This includes:
///
/// - Generic parameters are built.
/// - Generic arguments are built for the implementation symbols.
pub fn build_for_basic_resolution(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: Option<GlobalID>,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    match global_id {
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
            positive_trait_implementation::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::NegativeTraitImplementation(id) => table.build_to(
            id,
            required_from,
            negative_trait_implementation::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::AdtImplementation(id) => table.build_to(
            id,
            required_from,
            adt_implementation::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Variant(_) | GlobalID::Module(_) => Ok(()),
    }?;

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
                positive_trait_implementation::ARGUMENTS_STATE,
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
                positive_trait_implementation::ARGUMENTS_STATE,
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

#[allow(unused)]
pub fn build_for_definition_internal(
    table: &Table<Building<RwLockContainer, Finalizer>>,
    global_id: GlobalID,
    required_from: GlobalID,
    pre_definition: bool,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    match global_id {
        GlobalID::Struct(id) => table.build_to(
            id,
            Some(required_from),
            if pre_definition {
                r#struct::PRE_DEFINITION_STATE
            } else {
                r#struct::DEFINITION_STATE
            },
            handler,
        ),

        GlobalID::Enum(id) => table.build_to(
            id,
            Some(required_from),
            if pre_definition {
                r#enum::PRE_DEFINITION_STATE
            } else {
                r#enum::DEFINITION_STATE
            },
            handler,
        ),

        GlobalID::Trait(id) => table.build_to(
            id,
            Some(required_from),
            r#trait::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::Type(id) => table.build_to(
            id,
            Some(required_from),
            r#type::DEFINITION_STATE,
            handler,
        ),

        GlobalID::Function(id) => table.build_to(
            id,
            Some(required_from),
            function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::Constant(id) => table.build_to(
            id,
            Some(required_from),
            constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitFunction(id) => table.build_to(
            id,
            Some(required_from),
            trait_function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitConstant(id) => table.build_to(
            id,
            Some(required_from),
            trait_constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitType(id) => table.build_to(
            id,
            Some(required_from),
            trait_type::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::TraitImplementationFunction(id) => table.build_to(
            id,
            Some(required_from),
            trait_implementation_function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitImplementationConstant(id) => table.build_to(
            id,
            Some(required_from),
            trait_implementation_constant::DEFINITION_STATE,
            handler,
        ),

        GlobalID::TraitImplementationType(id) => table.build_to(
            id,
            Some(required_from),
            trait_implementation_type::DEFINITION_STATE,
            handler,
        ),

        GlobalID::AdtImplementationFunction(id) => table.build_to(
            id,
            Some(required_from),
            adt_implementation_function::DEFINITION_STATE,
            handler,
        ),

        GlobalID::PositiveTraitImplementation(id) => table.build_to(
            id,
            Some(required_from),
            positive_trait_implementation::ARGUMENTS_STATE,
            handler,
        ),

        GlobalID::NegativeTraitImplementation(id) => table.build_to(
            id,
            Some(required_from),
            negative_trait_implementation::ARGUMENTS_STATE,
            handler,
        ),

        GlobalID::AdtImplementation(id) => table.build_to(
            id,
            Some(required_from),
            adt_implementation::DEFINITION_STATE,
            handler,
        ),

        GlobalID::Variant(_) | GlobalID::Module(_) => Ok(()),
    }?;

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
                Some(required_from),
                positive_trait_implementation::ARGUMENTS_STATE,
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
                Some(required_from),
                positive_trait_implementation::ARGUMENTS_STATE,
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
