use pernixc_base::diagnostic::Handler;

use super::{BuildSymbolError, Finalizer};
use crate::{
    error,
    symbol::{
        table::{
            representation::{
                building::finalizing::finalize::{
                    adt_implementation, adt_implementation_constant,
                    adt_implementation_function, adt_implementation_type,
                    constant, function, negative_trait_implementation,
                    positive_trait_implementation, r#enum, r#struct, r#trait,
                    r#type, trait_constant, trait_function,
                    trait_implementation_constant,
                    trait_implementation_function, trait_implementation_type,
                    trait_type, variant,
                },
                Index, RwLockContainer,
            },
            resolution::{self, Observer, Resolution},
            Building, Table,
        },
        GlobalID, TraitImplementationMemberID,
    },
    type_system::{
        environment::Environment, model::Model, normalizer::Normalizer,
        predicate,
    },
};

/// A struct implementing [`Observer`] trait that builds the symbols
/// so that it contains generic parameters information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParameter;

impl<M: Model> Observer<Building<RwLockContainer, Finalizer>, M>
    for GenericParameter
{
    fn on_global_id_resolved(
        &mut self,
        table: &Table<Building<RwLockContainer, Finalizer>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        _: &pernixc_lexical::token::Identifier,
    ) -> bool {
        build_for_generic_parameter(
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
        _: &crate::type_system::term::r#type::Type<M>,
        _: &pernixc_syntax::syntax_tree::r#type::Type,
    ) {
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::lifetime::Lifetime<M>,
        _: &pernixc_syntax::syntax_tree::Lifetime,
    ) {
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::constant::Constant<M>,
        _: &pernixc_syntax::syntax_tree::expression::Expression,
    ) {
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::r#type::Type<M>,
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

#[derive(Clone)]
pub struct Definition<'a, M: Model, N: Normalizer<M>> {
    environment:
        &'a Environment<'a, M, Building<RwLockContainer, Finalizer>, N>,
    pre_definition: bool,
}

impl<'a, M: Model, N: Normalizer<M>> Definition<'a, M, N> {
    pub fn new(
        environment: &'a Environment<
            'a,
            M,
            Building<RwLockContainer, Finalizer>,
            N,
        >,
    ) -> Self {
        Self { environment, pre_definition: false }
    }

    pub fn for_pre_definition(
        environment: &'a Environment<
            'a,
            M,
            Building<RwLockContainer, Finalizer>,
            N,
        >,
    ) -> Self {
        Self { environment, pre_definition: true }
    }
}

impl<'a, M: Model, N: Normalizer<M>> std::fmt::Debug for Definition<'a, M, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Definition").finish()
    }
}

impl<'a, M: Model, N: Normalizer<M>>
    Observer<Building<RwLockContainer, Finalizer>, M> for Definition<'a, M, N>
{
    fn on_global_id_resolved(
        &mut self,
        table: &Table<Building<RwLockContainer, Finalizer>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        global_id: GlobalID,
        _: &pernixc_lexical::token::Identifier,
    ) -> bool {
        if !std::ptr::eq(table, self.environment.table()) {
            return false;
        }

        build_for_generic_parameter(
            table,
            global_id,
            Some(referring_site),
            handler,
        )
        .is_ok()
    }

    fn on_resolution_resolved(
        &mut self,
        table: &Table<Building<RwLockContainer, Finalizer>>,
        referring_site: GlobalID,
        handler: &dyn Handler<Box<dyn error::Error>>,
        resolution: &Resolution<M>,
        _: &pernixc_syntax::syntax_tree::GenericIdentifier,
    ) -> bool {
        if !std::ptr::eq(table, self.environment.table()) {
            return false;
        }

        build_for_definition_internal(
            &self.environment,
            resolution,
            referring_site,
            self.pre_definition,
            handler,
        )
        .is_ok()
    }

    fn on_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::r#type::Type<M>,
        _: &pernixc_syntax::syntax_tree::r#type::Type,
    ) {
    }

    fn on_lifetime_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::lifetime::Lifetime<M>,
        _: &pernixc_syntax::syntax_tree::Lifetime,
    ) {
    }

    fn on_constant_arguments_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::constant::Constant<M>,
        _: &pernixc_syntax::syntax_tree::expression::Expression,
    ) {
    }

    fn on_unpacked_type_resolved(
        &mut self,
        _: &Table<Building<RwLockContainer, Finalizer>>,
        _: GlobalID,
        _: &dyn Handler<Box<dyn error::Error>>,
        _: &crate::type_system::term::r#type::Type<M>,
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

/// Build the given `global_id` so that it has generic parameters
/// information.
pub fn build_for_generic_parameter(
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

        GlobalID::Type(id) => table.build_to(
            id,
            required_from,
            r#type::GENERIC_PARAMETER_STATE,
            handler,
        ),

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

        GlobalID::Variant(id) => table.build_to(
            id,
            required_from,
            variant::GENERIC_PARAMETER_STATE,
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
            trait_implementation_type::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::AdtImplementationFunction(id) => table.build_to(
            id,
            required_from,
            adt_implementation_function::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::AdtImplementationConstant(id) => table.build_to(
            id,
            required_from,
            adt_implementation_constant::GENERIC_PARAMETER_STATE,
            handler,
        ),

        GlobalID::AdtImplementationType(id) => table.build_to(
            id,
            required_from,
            adt_implementation_type::GENERIC_PARAMETER_STATE,
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

        GlobalID::Module(_) => Ok(()),
    }?;

    // this variable will either be AdtImplementation or
    // PositiveTraitImplementation
    let implementaion_member_parent_id = match global_id {
        id @ (GlobalID::TraitImplementationFunction(_)
        | GlobalID::TraitImplementationType(_)
        | GlobalID::TraitImplementationConstant(_)
        | GlobalID::AdtImplementationFunction(_)
        | GlobalID::AdtImplementationType(_)
        | GlobalID::AdtImplementationConstant(_)) => {
            Some(table.get_global(id).unwrap().parent_global_id().unwrap())
        }

        _ => None,
    };

    match implementaion_member_parent_id {
        Some(GlobalID::PositiveTraitImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::DEFINITION_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ (Err(BuildSymbolError::CyclicDependency)
                | Err(BuildSymbolError::InvalidStateFlag { .. })) => err,
            }
        }
        Some(GlobalID::AdtImplementation(id)) => {
            match table.build_to(
                id,
                required_from,
                positive_trait_implementation::DEFINITION_STATE,
                handler,
            ) {
                Err(BuildSymbolError::EntryNotFound(_)) | Ok(()) => Ok(()),

                err @ (Err(BuildSymbolError::CyclicDependency)
                | Err(BuildSymbolError::InvalidStateFlag { .. })) => err,
            }
        }

        _ => Ok(()),
    }
}

pub fn build_for_definition_internal<M: Model>(
    environment: &Environment<
        M,
        Building<RwLockContainer, Finalizer>,
        impl Normalizer<M>,
    >,
    resolution: &Resolution<M>,
    required_from: GlobalID,
    pre_definition: bool,
    handler: &dyn Handler<Box<dyn error::Error>>,
) -> Result<(), BuildSymbolError> {
    match resolution {
        Resolution::Module(_) => Ok(()),
        Resolution::Variant(res) => environment.table().build_to(
            res.variant,
            Some(required_from),
            variant::DEFINITION_STATE,
            handler,
        ),
        Resolution::Generic(res) => match res.id {
            resolution::GenericID::Struct(id) => environment.table().build_to(
                id,
                Some(required_from),
                if pre_definition {
                    r#struct::PRE_DEFINITION_STATE
                } else {
                    r#struct::DEFINITION_STATE
                },
                handler,
            ),
            resolution::GenericID::Enum(id) => environment.table().build_to(
                id,
                Some(required_from),
                if pre_definition {
                    r#enum::PRE_DEFINITION_STATE
                } else {
                    r#enum::DEFINITION_STATE
                },
                handler,
            ),
            resolution::GenericID::Function(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    function::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::GenericID::Trait(id) => {
                let state = if let Ok(Some(_)) =
                    res.generic_arguments.definite(environment)
                {
                    r#trait::NON_FINAL_IMPLEMENTATION_STATE
                } else {
                    r#trait::FINAL_IMPLEMENTATION_STATE
                };

                environment.table().build_to(
                    id,
                    Some(required_from),
                    state,
                    handler,
                )
            }
            resolution::GenericID::Constant(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    constant::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::GenericID::Type(id) => environment.table().build_to(
                id,
                Some(required_from),
                r#type::DEFINITION_STATE,
                handler,
            ),
            resolution::GenericID::TraitImplementationFunction(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    trait_implementation_function::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::GenericID::TraitImplementationType(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    trait_implementation_type::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::GenericID::TraitImplementationConstant(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    trait_implementation_constant::DEFINITION_STATE,
                    handler,
                )
            }
        },
        Resolution::MemberGeneric(res) => match res.id {
            trait_member_id @ (resolution::MemberGenericID::TraitFunction(
                _,
            )
            | resolution::MemberGenericID::TraitType(_)
            | resolution::MemberGenericID::TraitConstant(
                _,
            )) => {
                // finalize the trait member
                match trait_member_id {
                    resolution::MemberGenericID::TraitFunction(id) => {
                        environment.table().build_to(
                            id,
                            Some(required_from),
                            trait_function::DEFINITION_STATE,
                            handler,
                        )?;
                    }
                    resolution::MemberGenericID::TraitType(id) => {
                        environment.table().build_to(
                            id,
                            Some(required_from),
                            trait_type::WHERE_CLAUSE_STATE,
                            handler,
                        )?;
                    }
                    resolution::MemberGenericID::TraitConstant(id) => {
                        environment.table().build_to(
                            id,
                            Some(required_from),
                            trait_constant::DEFINITION_STATE,
                            handler,
                        )?;
                    }
                    _ => unreachable!(),
                }

                let parent_trait_id = environment
                    .table()
                    .get_global(trait_member_id.into())
                    .unwrap()
                    .parent_global_id()
                    .unwrap()
                    .into_trait()
                    .unwrap();

                let implementation = predicate::resolve_implementation(
                    parent_trait_id,
                    &res.parent_generic_arguments,
                    environment,
                );

                // build the trait implementation member counter part
                if let Ok(implementation) = implementation {
                    let implementation_sym = environment
                        .table()
                        .get(implementation.result.id)
                        .unwrap();

                    let Some(id) = implementation_sym
                        .member_ids_by_name
                        .get(
                            environment
                                .table()
                                .get_global(trait_member_id.into())
                                .unwrap()
                                .name(),
                        )
                        .copied()
                    else {
                        return Ok(());
                    };

                    return match id {
                        TraitImplementationMemberID::Type(id) => {
                            environment.table().build_to(
                                id,
                                Some(required_from),
                                trait_implementation_type::DEFINITION_STATE,
                                handler,
                            )
                        }
                        TraitImplementationMemberID::Function(id) => {
                            environment.table().build_to(
                                id,
                                Some(required_from),
                                trait_implementation_function::DEFINITION_STATE,
                                handler,
                            )
                        }
                        TraitImplementationMemberID::Constant(id) => {
                            environment.table().build_to(
                                id,
                                Some(required_from),
                                trait_implementation_constant::DEFINITION_STATE,
                                handler,
                            )
                        }
                    };
                }

                Ok(())
            }

            resolution::MemberGenericID::AdtImplementationFunction(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    adt_implementation_function::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::MemberGenericID::AdtImplementationType(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    adt_implementation_type::DEFINITION_STATE,
                    handler,
                )
            }
            resolution::MemberGenericID::AdtImplementationConstant(id) => {
                environment.table().build_to(
                    id,
                    Some(required_from),
                    adt_implementation_constant::DEFINITION_STATE,
                    handler,
                )
            }
        },
    }
}
