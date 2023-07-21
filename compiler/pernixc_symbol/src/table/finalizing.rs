use enum_as_inner::EnumAsInner;
use pernixc_lexical::token::Identifier;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self,
    item::{self, TypeBoundConstraint},
    GenericArgument, GenericIdentifier, QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{drafting::States, Table};
use crate::{
    error::{
        self, LifetimeArgumentCountMismatch, LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
        LifetimeArgumentsRequired, NoGenericArgumentsRequired, NoMemberOnCompoundType,
        PrivateSymbolLeakage, SymbolIsNotAccessible, SymbolNotFound, TargetNotFound, TraitExpected,
        TraitResolutionNotAllowed, TypeArgumentCountMismatch, TypeExpected,
    },
    table,
    ty::{self, Primitive, Reference},
    Accessibility, Enum, EnumVariant, Field, Function, GenericParameters, GenericableID, GlobalID,
    Implements, ImplementsFunction, LifetimeArgument, Module, ScopedID, Struct, Substitution,
    Trait, TraitFunction, TypeParameter, WhereClause, ID,
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
    TypeParameter(arena::ID<TypeParameter>),
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
            Self::ImplementsFunction(..) | Self::TypeParameter(..) => todo!(),
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
            &generic_identifier.span()?,
            generic_identifier.generic_arguments.as_ref(),
            resolution_config,
            states,
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
                *self = table.alias_type_as_resolution(
                    &table.types[id].alias,
                    substitution,
                    resolution_config.check_where_clause,
                    &generic_identifier.span()?,
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

                    *self = table.alias_type_as_resolution(
                        &table.implements_types[table.implements[active_implements]
                            .implements_types_by_trait_type[&trait_type_id]]
                            .alias,
                        substitution,
                        resolution_config.check_where_clause,
                        &generic_identifier.span()?,
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
            ID::Module(..)
            | ID::Enum(..)
            | ID::EnumVariant(_)
            | ID::TypeParameter(_)
            | ID::LifetimeParameter(_) => {
                // currently, there is nothing to finalize for this symbol
            }
            ID::Struct(struct_id) => self.finalize_struct(struct_id, states, handler)?,
            ID::Function(_) => todo!(),
            ID::Type(_) => todo!(),
            ID::Field(field_id) => self.finalize_field(field_id, states, handler)?,
            ID::FunctionParameter(_) => todo!(),
            ID::TraitFunctionParameter(_) => todo!(),
            ID::ImplementsFunctionParameter(_) => todo!(),
            ID::Trait(_) => todo!(),
            ID::TraitType(_) => todo!(),
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
        for id in self.scope_walker(referring_site).unwrap() {
            let Ok(genericable_id) = GenericableID::try_from(id) else {
                continue;
            };

            let genericable = self.get_genericable(genericable_id).unwrap();
            if let Some(ty_parameter) = genericable
                .generic_parameters()
                .type_parameter_ids_by_name
                .get(identifier.span.str())
            {
                return Some(*ty_parameter);
            }
        }

        None
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

    // checks if the generic arguments match the generic parameter of the symbol or not
    #[allow(clippy::too_many_lines)]
    fn check_generic_arguments(
        &mut self,
        global_id: GlobalID,
        identifier_span: &Span,
        generic_arguments: Option<&syntax_tree::GenericArguments>,
        resolution_config: &ResolutionConfig,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<Substitution, table::Error> {
        let mut substitution = Substitution::default();

        let Ok(genericable_id) = GenericableID::try_from(global_id) else {
            if let Some(generic_arguments) = generic_arguments {
                handler.receive(error::Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        span: generic_arguments.span()?,
                    },
                ));
                return Err(table::Error::FatalSemantic);
            }

            return Ok(substitution);
        };

        let mut type_argument_syntax_trees = Vec::new();
        let mut lifetime_argument_syntax_trees = Vec::new();

        for generic_argument in generic_arguments
            .iter()
            .flat_map(|x| x.argument_list.elements())
        {
            match generic_argument {
                GenericArgument::TypeSpecifier(ty_specifier) => {
                    type_argument_syntax_trees.push(ty_specifier.as_ref());
                }
                GenericArgument::Lifetime(lifetime_argument) => {
                    // lifetime arguments must be supplied first
                    if !type_argument_syntax_trees.is_empty() {
                        handler.receive(
                            error::Error::LifetimeArgumentMustBeSuppliedPriorToTypeArgument(
                                LifetimeArgumentMustBeSuppliedPriorToTypeArgument {
                                    lifetime_argument_span: lifetime_argument.span()?,
                                },
                            ),
                        );
                        return Err(table::Error::FatalSemantic);
                    }

                    lifetime_argument_syntax_trees.push(lifetime_argument);
                }
            }
        }

        match (
            self.get_genericable(genericable_id)?
                .generic_parameters()
                .lifetime_parameter_order
                .len(),
            lifetime_argument_syntax_trees.len(),
            resolution_config.explicit_lifetime_required,
        ) {
            // lifetime arguments must be supplied
            (1.., 0, true) => {
                handler.receive(error::Error::LifetimeArgumentsRequired(
                    LifetimeArgumentsRequired {
                        span: identifier_span.clone(),
                    },
                ));

                return Err(table::Error::FatalSemantic);
            }

            // either supplied or not at all
            (1.., 0, false) => {
                // Do nothing
            }

            // well formed lifetime arguments
            (required, supplied, _) if required == supplied => {
                assert!(lifetime_argument_syntax_trees.len() == required);

                for (lifetime_parameter, lifetime_argument_syntax_tree) in self
                    .get_genericable(genericable_id)?
                    .generic_parameters()
                    .lifetime_parameter_order
                    .iter()
                    .copied()
                    .zip(lifetime_argument_syntax_trees.iter().copied())
                {
                    let lifetime_argument = self.resolve_lifetime_argument(
                        resolution_config.referring_site,
                        lifetime_argument_syntax_tree,
                        handler,
                    )?;

                    assert!(substitution
                        .lifetime_arguments_by_parameter
                        .insert(lifetime_parameter, lifetime_argument)
                        .is_none());
                }
            }

            // lifetime arguments count mismatch
            (required, supplied, _) => {
                handler.receive(error::Error::LifetimeArgumentCountMismatch(
                    LifetimeArgumentCountMismatch {
                        supplied,
                        expected: required,
                        generic_arguments_span: match &generic_arguments {
                            Some(generic_arguments) => generic_arguments.span()?,
                            None => identifier_span.clone(),
                        },
                    },
                ));

                return Err(table::Error::FatalSemantic);
            }
        }

        if type_argument_syntax_trees.len()
            == self
                .get_genericable(genericable_id)?
                .generic_parameters()
                .type_parameter_order
                .len()
        {
            let type_parameters = self
                .get_genericable(genericable_id)?
                .generic_parameters()
                .type_parameter_order
                .clone();

            for (type_parameter, type_specifier) in type_parameters
                .into_iter()
                .zip(type_argument_syntax_trees.iter().copied())
            {
                let ty = self.resolve_type_with_finalization(
                    type_specifier,
                    resolution_config,
                    states,
                    handler,
                )?;

                assert!(substitution
                    .type_arguments_by_parameter
                    .insert(type_parameter, ty)
                    .is_none());
            }
        } else {
            handler.receive(error::Error::TypeArgumentCountMismatch(
                TypeArgumentCountMismatch {
                    supplied: type_argument_syntax_trees.len(),
                    expected: self
                        .get_genericable(genericable_id)?
                        .generic_parameters()
                        .type_parameter_order
                        .len(),
                    generic_arguments_span: match &generic_arguments {
                        Some(generic_arguments) => generic_arguments.span()?,
                        None => identifier_span.clone(),
                    },
                },
            ));
            return Err(table::Error::FatalSemantic);
        }

        if resolution_config.check_where_clause {
            self.check_where_clause(genericable_id, &substitution, handler)?;
        }

        Ok(substitution)
    }

    // returns false if found any error, true otherwise
    fn check_where_clause(
        &self,
        global_id: GenericableID,
        substitution: &Substitution,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        todo!()
    }

    #[allow(clippy::needless_pass_by_value)]
    fn alias_type_as_resolution(
        &self,
        alias: &ty::Type,
        substitution: Substitution,
        allow_trait_resolution: bool,
        span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, table::Error> {
        Ok(
            match self.alias_type(alias, &substitution, allow_trait_resolution, span, handler)? {
                ty::Type::Enum(enum_id) => Resolution::Enum(enum_id),
                ty::Type::Struct(struct_ty) => Resolution::Struct(GenericsResolution {
                    symbol: struct_ty.struct_id,
                    substitution: struct_ty.substitution,
                }),
                ty::Type::Primitive(primtive) => Resolution::Primitive(primtive),
                ty::Type::Reference(reference) => Resolution::Reference(reference),
                ty::Type::Parameter(ty_parameter) => Resolution::TypeParameter(ty_parameter),
                ty::Type::TraitType(trait_ty) => Resolution::TraitType(TraitTypeResolution {
                    trait_resolution: TraitResolution {
                        trait_id: self.trait_types[trait_ty.trait_type_id].parent_trait_id,
                        substitution: trait_ty.trait_substitution,
                    },
                    trait_type_id: trait_ty.trait_type_id,
                    substitution: trait_ty.trait_type_substitution,
                }),
            },
        )
    }

    fn apply_substitution_on_arguments(
        &self,
        substitution_operand: &mut Substitution,
        other: &Substitution,
        allow_trait_resolution: bool,
        span: &Span,
        handler: &impl Handler<error::Error>,
    ) {
        for argument in substitution_operand
            .lifetime_arguments_by_parameter
            .values_mut()
        {
            if let LifetimeArgument::Parameter(id) = argument {
                if let Some(substitution) = other.lifetime_arguments_by_parameter.get(id) {
                    *argument = *substitution;
                }
            }
        }

        for argument in substitution_operand
            .type_arguments_by_parameter
            .values_mut()
        {
            let _ = self.substitute_type(argument, other, allow_trait_resolution, span, handler);
        }
    }

    fn substitute_type(
        &self,
        ty: &mut ty::Type,
        substitution: &Substitution,
        allow_trait_resolution: bool,
        span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        match ty {
            ty::Type::Struct(struct_ty) => {
                self.apply_substitution_on_arguments(
                    &mut struct_ty.substitution,
                    substitution,
                    allow_trait_resolution,
                    span,
                    handler,
                );
                Ok(())
            }
            ty::Type::Reference(reference_ty) => self.substitute_type(
                &mut reference_ty.operand,
                substitution,
                allow_trait_resolution,
                span,
                handler,
            ),
            ty::Type::Parameter(type_parameter) => {
                if let Some(substitution) =
                    substitution.type_arguments_by_parameter.get(type_parameter)
                {
                    *ty = substitution.clone();
                }

                Ok(())
            }
            ty::Type::TraitType(trait_type) => {
                self.apply_substitution_on_arguments(
                    &mut trait_type.trait_substitution,
                    substitution,
                    allow_trait_resolution,
                    span,
                    handler,
                );

                if trait_type.trait_substitution.is_concrete_substitution() {
                    if !allow_trait_resolution {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed { span: span.clone() },
                        ));
                        return Err(table::Error::FatalSemantic);
                    }

                    let active_implements = self.resolve_trait_implements(
                        self.trait_types[trait_type.trait_type_id].parent_trait_id,
                        &trait_type.trait_substitution,
                        handler,
                    )?;

                    *ty = self.alias_type(
                        &self.implements_types[self.implements[active_implements]
                            .implements_types_by_trait_type[&trait_type.trait_type_id]]
                            .alias,
                        substitution,
                        allow_trait_resolution,
                        span,
                        handler,
                    )?;
                } else {
                    self.apply_substitution_on_arguments(
                        &mut trait_type.trait_type_substitution,
                        substitution,
                        allow_trait_resolution,
                        span,
                        handler,
                    );
                }

                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn alias_type(
        &self,
        alias: &ty::Type,
        substitution: &Substitution,
        allow_trait_resolution: bool,
        span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, table::Error> {
        let mut cloned_ty = alias.clone();
        self.substitute_type(
            &mut cloned_ty,
            substitution,
            allow_trait_resolution,
            span,
            handler,
        )?;
        Ok(cloned_ty)
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
            &qualified_identifier.first.identifier.span()?,
            qualified_identifier.first.generic_arguments.as_ref(),
            resolution_config,
            states,
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
            GlobalID::Type(id) => self.alias_type_as_resolution(
                &self.types[id].alias,
                substitution,
                resolution_config.check_where_clause,
                &qualified_identifier.span()?,
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
                    self.alias_type_as_resolution(
                        &self.implements_types[self.implements[active_implements]
                            .implements_types_by_trait_type[&trait_type_id]]
                            .alias,
                        substitution,
                        resolution_config.check_where_clause,
                        &qualified_identifier.first.span()?,
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
            Resolution::TypeParameter(type_parameter) => ty::Type::Parameter(type_parameter),

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
                            handler.receive(error::Error::LifetimeArgumentsRequired(
                                LifetimeArgumentsRequired {
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
        genericable_id: GenericableID,
        where_clause: &mut WhereClause,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        // expect the trait
        let Ok(resolution) = self.resolve_symbol_with_finalization(
            &trait_bound.qualified_identifier,
            &ResolutionConfig {
                referring_site: genericable_id.into(),
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
        genericable_id: GenericableID,
        where_clause: &mut WhereClause,
        parent_where_clause: &WhereClause,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        let Ok(ty) = self.resolve_type_with_finalization(
            &type_bound.type_specifier,
            &ResolutionConfig {
                referring_site: genericable_id.into(),
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
                    let Ok(type_bound) = self.resolve_type_with_finalization(
                        type_bound_specifier,
                        &ResolutionConfig {
                            referring_site: genericable_id.into(),
                            explicit_lifetime_required: true,
                            check_where_clause: false,
                        },
                        states,
                        handler,
                    ) else {
                        continue;
                    };

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

                    where_clause
                        .types_by_trait_type
                        .insert(trait_type.clone(), type_bound);
                }
                TypeBoundConstraint::LifetimeArgument(lt_syntax_tree) => {
                    let Ok(lifetime_argument) = self.resolve_lifetime_argument(
                        genericable_id.into(),
                        lt_syntax_tree,
                        handler,
                    ) else {
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
        let parent_active_where_clause = self.get_active_where_clause(
            self.get_genericable(genericable_id)?
                .parent_symbol()
                .expect("Genericable should have a parent symbol"),
        )?;
        for constraint in where_clause_syntax_tree.constraint_list.elements() {
            match constraint {
                item::Constraint::TraitBound(trait_bound) => self.handle_where_clause_trait_bound(
                    trait_bound,
                    genericable_id,
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
                item::Constraint::TypeBound(type_bound) => {
                    self.handle_where_clause_type_bound(
                        type_bound,
                        genericable_id,
                        &mut where_clause,
                        &parent_active_where_clause,
                        states,
                        handler,
                    );
                }
            }
        }

        Ok(where_clause)
    }

    pub(super) fn get_least_accessibility(
        &self,
        ty: &ty::Type,
    ) -> Result<Accessibility, table::Error> {
        match ty {
            ty::Type::Enum(enum_id) => Ok(self.enums[*enum_id].accessibility),
            ty::Type::Struct(struct_ty) => {
                let mut current_accessibility = self.structs[struct_ty.struct_id].accessibility;

                for ty in struct_ty.substitution.type_arguments_by_parameter.values() {
                    let new_accessibility = self.get_least_accessibility(ty)?;
                    if new_accessibility < current_accessibility {
                        current_accessibility = new_accessibility;
                    }
                }

                Ok(current_accessibility)
            }
            ty::Type::Parameter(..) | ty::Type::Primitive(..) => Ok(Accessibility::Public),
            ty::Type::Reference(reference) => self.get_least_accessibility(&reference.operand),
            ty::Type::TraitType(trait_ty) => {
                let mut current_accessibility = self.traits
                    [self.trait_types[trait_ty.trait_type_id].parent_trait_id]
                    .accessibility;

                for ty in trait_ty
                    .trait_substitution
                    .type_arguments_by_parameter
                    .values()
                {
                    let new_accessibility = self.get_least_accessibility(ty)?;
                    if new_accessibility < current_accessibility {
                        current_accessibility = new_accessibility;
                    }
                }

                for ty in trait_ty
                    .trait_type_substitution
                    .type_arguments_by_parameter
                    .values()
                {
                    let new_accessibility = self.get_least_accessibility(ty)?;
                    if new_accessibility < current_accessibility {
                        current_accessibility = new_accessibility;
                    }
                }

                Ok(current_accessibility)
            }
        }
    }

    pub(super) fn finalize_field(
        &mut self,
        field_id: arena::ID<Field>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<(), table::Error> {
        assert!(
            states.get_current_state(field_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(field_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return Err(table::Error::FatalSemantic);
        };

        let Ok(ty) = self.resolve_type_with_finalization(
            &self.fields[field_id]
                .syntax_tree
                .as_ref()
                .unwrap()
                .clone()
                .type_annotation
                .type_specifier,
            &ResolutionConfig {
                referring_site: field_id.into(),
                check_where_clause: true,
                explicit_lifetime_required: true,
            },
            states,
            handler,
        ) else {
            states.remove_constructing_symbol(field_id.into());
            return Err(table::Error::FatalSemantic);
        };
        let ty_least_accessibility = self.get_least_accessibility(&ty)?;

        if ty_least_accessibility < self.fields[field_id].accessibility {
            handler.receive(error::Error::PrivateSymbolLeakage(PrivateSymbolLeakage {
                span: self.fields[field_id]
                    .syntax_tree
                    .as_ref()
                    .unwrap()
                    .type_annotation
                    .span()?,
            }));
        }

        // assign the resolved type
        self.fields[field_id].ty = ty;

        states.remove_constructing_symbol(field_id.into());
        Ok(())
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

        for field_id in self.structs[struct_id].field_order.iter().copied() {
            states.add_drafted_symbol(field_id.into());
        }

        states.remove_constructing_symbol(struct_id.into());

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

        for field_id in self.structs[struct_id].field_order.clone() {
            self.finalize_field(field_id, states, handler)?;
        }

        Ok(())
    }
}
