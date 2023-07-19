use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::SourceElement;
use pernixc_syntax::syntax_tree::{
    self,
    item::{self, TypeBoundConstraint},
    GenericIdentifier, QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::States, Table};
use crate::{
    error::{
        self, LifetimeArgumentRequired, NoMemberOnCompoundType, SymbolIsNotAccessible,
        SymbolNotFound, TargetNotFound, TraitExpected, TraitResolutionNotAllowed, TypeExpected,
    },
    table,
    ty::{self, Primitive, Reference},
    Enum, EnumVariant, Function, GenericParameters, GenericableID, GlobalID, Implements,
    ImplementsFunction, LifetimeArgument, Module, ScopedID, Struct, Substitution, Trait,
    TraitFunction, TypeParameter, WhereClause, ID,
};

/// Specifies the configuration of the resolution.
#[derive(Debug, Clone, Copy)]
enum TraitOrImplementsID {
    Trait(arena::ID<Trait>),
    Implements(arena::ID<Implements>),
}

/// Specifies the configuration of the resolution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ResolutionConfig {
    /// Specifies where the resolution is occurring.
    referring_site: ID,

    /// Specifies whether if the resolution is occurring in the definition part or not.
    check_where_clause: bool,

    /// Specifies whether if the explicit lifetime is required or not.
    explicit_lifetime_required: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct GenericsResolution<Symbol> {
    symbol: arena::ID<Symbol>,
    substitution: Substitution,
}

#[derive(Debug, Clone, PartialEq)]
struct TraitResolution {
    trait_id: arena::ID<Trait>,
    substitution: Substitution,
}

#[derive(Debug, Clone, PartialEq)]
struct TraitFunctionResolution {
    trait_resolution: TraitResolution,
    trait_function_id: arena::ID<TraitFunction>,
    substitution: Substitution,
}

#[derive(Debug, Clone, PartialEq)]
struct TraitTypeResolution {
    trait_resolution: TraitResolution,
    trait_type_id: arena::ID<crate::TraitType>,
    substitution: Substitution,
}

#[derive(Debug, Clone, EnumAsInner)]
enum Resolution {
    Primitive(Primitive),
    Reference(Reference),
    Module(arena::ID<Module>),
    Struct(GenericsResolution<Struct>),
    Enum(arena::ID<Enum>),
    EnumVariant(arena::ID<EnumVariant>),
    Function(GenericsResolution<Function>),
    Trait(TraitResolution),
    TraitFunction(TraitFunctionResolution),
    TraitType(TraitTypeResolution),
    ImplementsFunction(GenericsResolution<ImplementsFunction>),
}

impl Resolution {
    #[allow(clippy::too_many_lines)]
    fn resolve(
        &mut self,
        generic_identifier: &GenericIdentifier,
        resolution_config: &ResolutionConfig,
        table: &mut Table,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        let global_id: GlobalID = match self {
            Self::Primitive(..) | Self::Reference(..) => {
                handler.receive(error::Error::NoMemberOnCompoundType(
                    NoMemberOnCompoundType {
                        span: generic_identifier.span()?,
                    },
                ));
                return Err(table::Error::FatalSemantic);
            }
            Self::Module(id) => (*id).into(),
            Self::Struct(struct_resolution) => struct_resolution.symbol.into(),
            Self::Enum(id) => (*id).into(),
            Self::EnumVariant(id) => (*id).into(),
            Self::Function(function_resolution) => function_resolution.symbol.into(),
            Self::Trait(trait_resolution) => trait_resolution.trait_id.into(),
            Self::TraitFunction(trait_function_resolution) => {
                trait_function_resolution.trait_function_id.into()
            }
            Self::TraitType(trait_type_resolution) => trait_type_resolution.trait_type_id.into(),
            Self::ImplementsFunction(..) => todo!(),
        };

        let Ok(scoped_id) = ScopedID::try_from(global_id) else {
            handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                searched_global_id: global_id,
                span: generic_identifier.span()?,
            }));
            return Err(table::Error::FatalSemantic);
        };

        let scoped_symbol = table.get_scoped(scoped_id)?;
        let Some(global_id) =
            scoped_symbol.get_child_id_by_name(generic_identifier.identifier.span.str())
        else {
            handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                searched_global_id: global_id,
                span: generic_identifier.span()?,
            }));
            return Err(table::Error::FatalSemantic);
        };

        // make sure the symbol is finalized
        table.finalize_symbol_if_is_drafted(global_id.into(), states, handler)?;
        let substitution = table.check_generic_arguments(
            global_id,
            generic_identifier.generic_arguments.as_ref(),
            resolution_config.check_where_clause,
            handler,
        )?;

        if !table.symbol_accessible(global_id, resolution_config.referring_site)? {
            handler.receive(error::Error::SymbolIsNotAccessible(SymbolIsNotAccessible {
                span: generic_identifier.span()?,
                referring_site: resolution_config.referring_site,
                referred: global_id,
            }));
        }

        match global_id {
            GlobalID::Module(id) => *self = Self::Module(id),
            GlobalID::Struct(id) => {
                *self = Self::Struct(GenericsResolution {
                    symbol: id,
                    substitution,
                });
            }
            GlobalID::Enum(id) => *self = Self::Enum(id),
            GlobalID::EnumVariant(id) => *self = Self::EnumVariant(id),
            GlobalID::Function(id) => {
                *self = Self::Function(GenericsResolution {
                    symbol: id,
                    substitution,
                });
            }
            GlobalID::Type(id) => {
                *self = table.alias_type(
                    &table.types[id].alias,
                    substitution,
                    resolution_config.check_where_clause,
                    handler,
                )?;
            }
            GlobalID::Trait(trait_id) => {
                *self = Self::Trait(TraitResolution {
                    trait_id,
                    substitution,
                });
            }
            GlobalID::TraitFunction(trait_function_id) => {
                let Self::Trait(trait_resolution) = self.clone() else {
                    unreachable!("trait function should derive from trait")
                };

                if trait_resolution.substitution.is_concrete_substitution() {
                    if !resolution_config.check_where_clause {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed {
                                span: generic_identifier.span()?,
                            },
                        ));
                        return Err(table::Error::FatalSemantic);
                    }

                    let active_implements = table.resolve_trait_implements(
                        trait_resolution.trait_id,
                        &trait_resolution.substitution,
                        handler,
                    )?;

                    *self = Self::ImplementsFunction(GenericsResolution {
                        symbol: table.implements[active_implements]
                            .implements_functions_by_trait_function[&trait_function_id],
                        substitution,
                    });
                } else {
                    *self = Self::TraitFunction(TraitFunctionResolution {
                        trait_resolution,
                        trait_function_id,
                        substitution,
                    });
                }
            }
            GlobalID::TraitType(trait_type_id) => {
                let Self::Trait(trait_resolution) = self.clone() else {
                    unreachable!("trait type should derive from trait")
                };

                if trait_resolution.substitution.is_concrete_substitution() {
                    if !resolution_config.check_where_clause {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed {
                                span: generic_identifier.span()?,
                            },
                        ));
                        return Err(table::Error::FatalSemantic);
                    }

                    let active_implements = table.resolve_trait_implements(
                        trait_resolution.trait_id,
                        &trait_resolution.substitution,
                        handler,
                    )?;

                    *self = table.alias_type(
                        &table.implements_types[table.implements[active_implements]
                            .implements_types_by_trait_type[&trait_type_id]]
                            .alias,
                        substitution,
                        resolution_config.check_where_clause,
                        handler,
                    )?;
                } else {
                    *self = Self::TraitType(TraitTypeResolution {
                        trait_resolution,
                        trait_type_id,
                        substitution,
                    });
                }
            }
        }

        Ok(())
    }
}

impl Table {
    pub(super) fn finalize_symbol(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        assert!(
            states.get_current_state(id).is_some(),
            "symbol is not drafted or being constructed"
        );

        match id {
            ID::Module(..) | ID::Enum(..) | ID::EnumVariant(_) => {
                // currently, there is nothing to finalize for this symbol
            }
            ID::Struct(struct_id) => self.finalize_struct(struct_id, states, handler)?,
            ID::Function(_) => todo!(),
            ID::Type(_) => todo!(),
            ID::Field(_) => todo!(),
            ID::FunctionParameter(_) => todo!(),
            ID::TraitFunctionParameter(_) => todo!(),
            ID::ImplementsFunctionParameter(_) => todo!(),
            ID::Trait(_) => todo!(),
            ID::TraitType(_) => todo!(),
            ID::TypeParameter(_) => todo!(),
            ID::LifetimeParameter(_) => todo!(),
            ID::TraitFunction(_) => todo!(),
            ID::Implements(_) => todo!(),
            ID::ImplementsFunction(_) => todo!(),
            ID::ImplementsType(_) => todo!(),
        }

        Ok(())
    }

    fn resolve_type_parameter(
        &self,
        referring_site: ID,
        identifier: &Identifier,
    ) -> Option<arena::ID<TypeParameter>> {
        todo!("Performs type parameter resolution down the scope tree");
    }

    fn finalize_symbol_if_is_drafted(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        if states.get_current_state(id).is_some() {
            self.finalize_symbol(id, states, handler)?;
        }

        Ok(())
    }

    fn resolve_trait_implements(
        &self,
        trait_id: arena::ID<Trait>,
        substitution: &Substitution,
        handler: &impl Handler<error::Error>,
    ) -> Result<arena::ID<Implements>, table::Error> {
        todo!()
    }

    fn check_generic_arguments(
        &self,
        global_id: GlobalID,
        generic_arguments: Option<&syntax_tree::GenericArguments>,
        check_where_clause: bool,
        handler: &impl Handler<error::Error>,
    ) -> Result<Substitution, table::Error> {
        todo!("check if the generic arguments match the generic parameter of the symbol or not")
    }

    // returns false if found any error, true otherwise
    fn check_where_clause(
        &self,
        global_id: GenericableID,
        generic_arguments: Option<&Substitution>,
        substitution: &Substitution,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        todo!()
    }

    #[allow(clippy::needless_pass_by_value)]
    fn alias_type(
        &self,
        alias: &ty::Type,
        substitution: Substitution,
        allow_trait_resolution: bool,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, table::Error> {
        todo!("")
    }

    fn get_current_trait_or_implements_id(
        &self,
        current_id: ID,
    ) -> Result<Option<TraitOrImplementsID>, table::Error> {
        for id in self.scope_walker(current_id)? {
            match id {
                ID::Trait(trait_id) => return Ok(Some(TraitOrImplementsID::Trait(trait_id))),
                ID::Implements(implements_id) => {
                    return Ok(Some(TraitOrImplementsID::Implements(implements_id)))
                }
                _ => {}
            }
        }

        Ok(None)
    }

    fn create_identical_substitution(generic_parameters: &GenericParameters) -> Substitution {
        let mut substitution = Substitution::default();

        for lifetime_parameter in generic_parameters
            .lifetime_parameter_ids_by_name
            .values()
            .copied()
        {
            substitution.lifetime_arguments_by_parameter.insert(
                lifetime_parameter,
                LifetimeArgument::Parameter(lifetime_parameter),
            );
        }

        for type_parameter in generic_parameters
            .type_parameter_ids_by_name
            .values()
            .copied()
        {
            substitution
                .type_arguments_by_parameter
                .insert(type_parameter, ty::Type::Parameter(type_parameter));
        }

        substitution
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_resolution_root(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        resolution_config: &ResolutionConfig,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, table::Error> {
        let current_trait_or_implements_id =
            self.get_current_trait_or_implements_id(resolution_config.referring_site)?;

        let global_id = if qualified_identifier.leading_scope_separator.is_some() {
            // search from the root
            if let Some(id) = self
                .target_root_module_ids_by_name
                .get(qualified_identifier.first.identifier.span.str())
                .copied()
            {
                id.into()
            } else {
                handler.receive(error::Error::TargetNotFound(TargetNotFound {
                    unknown_target_span: qualified_identifier.first.identifier.span.clone(),
                }));
                return Err(table::Error::FatalSemantic);
            }
        } else {
            // perform a local search down the module tree
            self.resolve_root(
                &qualified_identifier.first.identifier,
                resolution_config.referring_site,
                handler,
            )?
        };

        // make sure the symbol is finalized
        self.finalize_symbol_if_is_drafted(global_id.into(), states, handler)?;
        let substitution = self.check_generic_arguments(
            global_id,
            qualified_identifier.first.generic_arguments.as_ref(),
            resolution_config.check_where_clause,
            handler,
        )?;

        Ok(match global_id {
            GlobalID::Module(id) => Resolution::Module(id),
            GlobalID::Struct(id) => Resolution::Struct(GenericsResolution {
                symbol: id,
                substitution,
            }),
            GlobalID::Enum(id) => Resolution::Enum(id),
            GlobalID::EnumVariant(id) => Resolution::EnumVariant(id),
            GlobalID::Function(id) => Resolution::Function(GenericsResolution {
                symbol: id,
                substitution,
            }),
            GlobalID::Type(id) => self.alias_type(
                &self.types[id].alias,
                substitution,
                resolution_config.check_where_clause,
                handler,
            )?,
            GlobalID::Trait(id) => Resolution::Trait(TraitResolution {
                trait_id: id,
                substitution,
            }),
            GlobalID::TraitFunction(trait_function_id) => {
                assert!(!resolution_config.check_where_clause);

                let active_trait_or_implements_id = current_trait_or_implements_id.unwrap();

                let (active_implements, trait_resolution) = match active_trait_or_implements_id {
                    TraitOrImplementsID::Trait(trait_id) => (None, TraitResolution {
                        trait_id,
                        substitution: Self::create_identical_substitution(
                            &self.traits[trait_id].generics.parameters,
                        ),
                    }),
                    TraitOrImplementsID::Implements(implements_id) => {
                        (Some(implements_id), TraitResolution {
                            trait_id: self.implements[implements_id].trait_id,
                            substitution: self.implements[implements_id].substitution.clone(),
                        })
                    }
                };

                if let Some(active_implements) = active_implements {
                    Resolution::ImplementsFunction(GenericsResolution {
                        symbol: self.implements[active_implements]
                            .implements_functions_by_trait_function[&trait_function_id],
                        substitution,
                    })
                } else {
                    Resolution::TraitFunction(TraitFunctionResolution {
                        trait_resolution,
                        trait_function_id,
                        substitution,
                    })
                }
            }

            GlobalID::TraitType(trait_type_id) => {
                assert!(!resolution_config.check_where_clause);

                let active_trait_or_implements_id = current_trait_or_implements_id.unwrap();

                let (active_implements, trait_resolution) = match active_trait_or_implements_id {
                    TraitOrImplementsID::Trait(trait_id) => (None, TraitResolution {
                        trait_id,
                        substitution: Self::create_identical_substitution(
                            &self.traits[trait_id].generics.parameters,
                        ),
                    }),
                    TraitOrImplementsID::Implements(implements_id) => {
                        (Some(implements_id), TraitResolution {
                            trait_id: self.implements[implements_id].trait_id,
                            substitution: self.implements[implements_id].substitution.clone(),
                        })
                    }
                };

                if let Some(active_implements) = active_implements {
                    self.alias_type(
                        &self.implements_types[self.implements[active_implements]
                            .implements_types_by_trait_type[&trait_type_id]]
                            .alias,
                        substitution,
                        resolution_config.check_where_clause,
                        handler,
                    )?
                } else {
                    Resolution::TraitType(TraitTypeResolution {
                        trait_resolution,
                        trait_type_id,
                        substitution,
                    })
                }
            }
        })
    }

    fn resolve_symbol_with_finalization(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        resolution_config: &ResolutionConfig,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, table::Error> {
        let mut current_resolution =
            self.resolve_resolution_root(qualified_identifier, resolution_config, states, handler)?;

        for (_, generic_identifier) in &qualified_identifier.rest {
            current_resolution.resolve(
                generic_identifier,
                resolution_config,
                self,
                states,
                handler,
            )?;
        }

        Ok(current_resolution)
    }

    fn resolve_qualified_identifier_type_with_finalization(
        &mut self,
        qualified_identifier: &QualifiedIdentifier,
        resolution_config: &ResolutionConfig,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, table::Error> {
        // If the qualified identifier is a simple identifier, then try to resolve it as a
        // type parameter.
        if qualified_identifier.leading_scope_separator.is_none()
            && qualified_identifier.rest.is_empty()
            && qualified_identifier.first.generic_arguments.is_none()
        {
            if let Some(ty) = self.resolve_type_parameter(
                resolution_config.referring_site,
                &qualified_identifier.first.identifier,
            ) {
                return Ok(ty::Type::Parameter(ty));
            }
        }

        let resolution = self.resolve_symbol_with_finalization(
            qualified_identifier,
            resolution_config,
            states,
            handler,
        )?;

        Ok(match resolution {
            Resolution::Primitive(primitive) => ty::Type::Primitive(primitive),
            Resolution::Reference(reference) => ty::Type::Reference(reference),
            Resolution::Struct(struct_resolution) => ty::Type::Struct(ty::Struct {
                struct_id: struct_resolution.symbol,
                substitution: struct_resolution.substitution,
            }),
            Resolution::Enum(enum_id) => ty::Type::Enum(enum_id),
            Resolution::TraitType(trait_type) => ty::Type::TraitType(ty::TraitType {
                trait_type_id: trait_type.trait_type_id,
                trait_substitution: trait_type.trait_resolution.substitution,
                trait_type_substitution: trait_type.substitution,
            }),

            Resolution::Module(..)
            | Resolution::EnumVariant(..)
            | Resolution::Function(..)
            | Resolution::TraitFunction(..)
            | Resolution::ImplementsFunction(..)
            | Resolution::Trait(..) => {
                handler.receive(error::Error::TypeExpected(TypeExpected {
                    span: qualified_identifier.span()?,
                }));
                return Err(table::Error::FatalSemantic);
            }
        })
    }

    fn resolve_type_with_finalization(
        &mut self,
        type_specifier: &TypeSpecifier,
        resolution_config: &ResolutionConfig,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, table::Error> {
        match type_specifier {
            TypeSpecifier::Primitive(primitive_type) => {
                Ok(ty::Type::Primitive(match primitive_type {
                    syntax_tree::PrimitiveTypeSpecifier::Bool(_) => Primitive::Bool,
                    syntax_tree::PrimitiveTypeSpecifier::Void(_) => Primitive::Void,
                    syntax_tree::PrimitiveTypeSpecifier::Float32(_) => Primitive::Float32,
                    syntax_tree::PrimitiveTypeSpecifier::Float64(_) => Primitive::Float64,
                    syntax_tree::PrimitiveTypeSpecifier::Int8(_) => Primitive::Int8,
                    syntax_tree::PrimitiveTypeSpecifier::Int16(_) => Primitive::Int16,
                    syntax_tree::PrimitiveTypeSpecifier::Int32(_) => Primitive::Int32,
                    syntax_tree::PrimitiveTypeSpecifier::Int64(_) => Primitive::Int64,
                    syntax_tree::PrimitiveTypeSpecifier::Uint8(_) => Primitive::Uint8,
                    syntax_tree::PrimitiveTypeSpecifier::Uint16(_) => Primitive::Uint16,
                    syntax_tree::PrimitiveTypeSpecifier::Uint32(_) => Primitive::Uint32,
                    syntax_tree::PrimitiveTypeSpecifier::Uint64(_) => Primitive::Uint64,
                }))
            }
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => self
                .resolve_qualified_identifier_type_with_finalization(
                    qualified_identifier,
                    resolution_config,
                    states,
                    handler,
                ),
            TypeSpecifier::Reference(reference_type) => Ok(ty::Type::Reference(Reference {
                operand: Box::new(self.resolve_type_with_finalization(
                    &reference_type.operand_type,
                    resolution_config,
                    states,
                    handler,
                )?),
                qualifier: reference_type.qualifier.as_ref().map(|x| match x {
                    syntax_tree::ReferenceQualifier::Mutable(_) => ty::ReferenceQualifier::Mutable,
                    syntax_tree::ReferenceQualifier::Restrict(_) => {
                        ty::ReferenceQualifier::Restrict
                    }
                }),
                lifetime_argument: match &reference_type.lifetime_argument {
                    Some(x) => Some(self.resolve_lifetime_argument(
                        resolution_config.referring_site,
                        x,
                        handler,
                    )?),
                    None => {
                        if resolution_config.explicit_lifetime_required {
                            handler.receive(error::Error::LifetimeArgumentRequired(
                                LifetimeArgumentRequired {
                                    span: reference_type.span()?,
                                },
                            ));
                            return Err(table::Error::FatalSemantic);
                        }

                        None
                    }
                },
            })),
        }
    }

    fn handle_where_clause_trait_bound(
        &mut self,
        trait_bound: &syntax_tree::item::TraitBound,
        parent_id: ID,
        extra_generic_parameters: &GenericParameters,
        where_clause: &mut WhereClause,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        // expect the trait
        let Ok(resolution) = self.resolve_symbol_with_finalization(
            &trait_bound.qualified_identifier,
            &ResolutionConfig {
                referring_site: parent_id,
                explicit_lifetime_required: true,
                check_where_clause: false,
            },
            states,
            handler,
        ) else {
            return Ok(());
        };

        // expect the trait
        let Resolution::Trait(trait_resolution) = resolution else {
            handler.receive(error::Error::TraitExpected(TraitExpected {
                span: trait_bound.span()?,
            }));
            return Ok(());
        };

        if !where_clause.trait_bounds.insert(crate::TraitBound {
            trait_id: trait_resolution.trait_id,
            substitution: trait_resolution.substitution,
        }) {
            // TODO: report some warnings
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn handle_where_clause_type_bound(
        &mut self,
        type_bound: &syntax_tree::item::TypeBound,
        parent_id: ID,
        extra_generic_parameters: &GenericParameters,
        where_clause: &mut WhereClause,
        parent_where_clause: &WhereClause,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        let Ok(ty) = self.resolve_type_with_finalization(
            &type_bound.type_specifier,
            &ResolutionConfig {
                referring_site: parent_id,
                explicit_lifetime_required: true,
                check_where_clause: false,
            },
            states,
            handler,
        ) else {
            return;
        };

        for type_constraint in type_bound.type_bound_constraints.elements() {
            match type_constraint {
                TypeBoundConstraint::TypeSpecifier(type_bound_specifier) => {
                    let type_bound = self.resolve_type_with_finalization(
                        type_bound_specifier,
                        &ResolutionConfig {
                            referring_site: parent_id,
                            explicit_lifetime_required: true,
                            check_where_clause: false,
                        },
                        states,
                        handler,
                    );

                    // The type must not be fully resolved
                    let ty::Type::TraitType(trait_type) = &ty else {
                        todo!("Report some errors");
                    };

                    if parent_where_clause
                        .types_by_trait_type
                        .contains_key(trait_type)
                        || where_clause.types_by_trait_type.contains_key(trait_type)
                    {
                        todo!("Report some errors");
                    }
                }
                TypeBoundConstraint::LifetimeArgument(lt_syntax_tree) => {
                    let Ok(lifetime_argument) =
                        self.resolve_lifetime_argument(parent_id, lt_syntax_tree, handler)
                    else {
                        continue;
                    };

                    let lifetime_argument_set = where_clause
                        .lifetime_argument_sets_by_type
                        .entry(ty.clone())
                        .or_default();

                    if !lifetime_argument_set.insert(lifetime_argument) {
                        // TODO: Some warning report
                    }
                }
            }
        }
    }

    fn handle_where_clause_lifetime_bound(
        &mut self,
        lifetime_bound_syntax_tree: &syntax_tree::item::LifetimeBound,
        genericable_id: GenericableID,
        where_clause: &mut WhereClause,
        handler: &impl Handler<error::Error>,
    ) {
        let Ok(operand_lifetime_parameter) = self.resolve_lifetime_parameter(
            genericable_id.into(),
            &lifetime_bound_syntax_tree.operand.identifier,
            handler,
        ) else {
            return;
        };

        let lifetime_argument_set = where_clause
            .lifetime_argument_sets_by_lifetime_parameter
            .entry(operand_lifetime_parameter)
            .or_default();

        for lifetime_argument_syntax_tree in lifetime_bound_syntax_tree.arguments.elements() {
            let Ok(lifetime_argument) = self.resolve_lifetime_argument(
                genericable_id.into(),
                lifetime_argument_syntax_tree,
                handler,
            ) else {
                continue;
            };

            if !lifetime_argument_set.insert(lifetime_argument) {
                // TODO: Some warning report
            }
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn construct_where_clause(
        &mut self,
        where_clause_syntax_tree: &item::WhereClause,
        genericable_id: GenericableID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<WhereClause, table::Error> {
        let mut where_clause = WhereClause::default();
        let parent_id = self
            .get_genericable(genericable_id)?
            .parent_symbol()
            .expect("Genericable should have a parent symbol");
        let parent_active_where_clause = self.get_active_where_clause(parent_id)?;
        let extra_generic_parameters = self
            .get_genericable(genericable_id)?
            .generic_parameters()
            .clone();

        for constraint in where_clause_syntax_tree.constraint_list.elements() {
            match constraint {
                item::Constraint::TraitBound(trait_bound) => self.handle_where_clause_trait_bound(
                    trait_bound,
                    parent_id,
                    &extra_generic_parameters,
                    &mut where_clause,
                    states,
                    handler,
                )?,
                item::Constraint::LifetimeBound(lifetime_bound_syntax_tree) => self
                    .handle_where_clause_lifetime_bound(
                        lifetime_bound_syntax_tree,
                        genericable_id,
                        &mut where_clause,
                        handler,
                    ),
                item::Constraint::TypeBound(type_bound) => {}
            }
        }

        Ok(where_clause)
    }

    pub(super) fn finalize_struct(
        &mut self,
        struct_id: arena::ID<Struct>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        assert!(
            states.get_current_state(struct_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(struct_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return Err(table::Error::FatalSemantic);
        };

        if let Some(where_clause) = self.structs[struct_id]
            .syntax_tree
            .as_ref()
            .map(|x| x.where_clause.as_ref().cloned())
            .expect("syntax tree should exist")
        {
            let where_clause =
                self.construct_where_clause(&where_clause, struct_id.into(), states, handler)?;
            self.structs[struct_id].generics.where_clause = where_clause;
        }

        states.remove_constructing_symbol(struct_id.into());

        let struct_symbol = &self.structs[struct_id];

        // span out the fields
        for field_id in struct_symbol.field_order.iter().copied() {
            states.add_drafted_symbol(field_id.into());
            todo!("finalize the field symbol here");
        }

        Ok(())
    }
}
