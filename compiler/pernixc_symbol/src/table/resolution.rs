//! Contains the resolution logic for the symbol table.

use std::collections::HashSet;

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::Identifier;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, GenericArgument, GenericIdentifier, QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::Table;
use crate::{
    error::{
        self, LifetimeArgumentCountMismatch, LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
        LifetimeArgumentsRequired, LifetimeNotFound, ModuleExpected, NoGenericArgumentsRequired,
        NoMemberOnThisImplementsFunction, NoMemberOnThisType, ResolutionAmbiguity, SymbolNotFound,
        SymbolWasNotAccessible, TargetNotFound, TraitBoundNotSatisfied, TraitExpected,
        TypeArgumentCountMismatch, TypeExpected,
    },
    table,
    ty::{self, Primitive, Reference},
    Enum, EnumVariant, GenericParameters, GenericableID, GlobalID, LifetimeArgument,
    LifetimeParameter, Module, ScopedID, Substitution, TraitBound, TypeParameter, ID,
};

pub(super) mod constraint;
pub(super) mod implements;
pub(super) mod transform;

/// Specifies which bound should be checked during the resolution process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum BoundChecking {
    /// Both trait and lifetime bounds are checked.
    #[default]
    Default,

    /// When lifetime bounds check occurs, the elided lifetime is ignored.
    IgnoreLifetimeChecksIfElided,
}

/// Contains the neccessary information and configuration for the resolution process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Info {
    /// Specifies where the resolution is occurring.
    pub referring_site: ID,

    /// Specifies the bound checking configuration.
    pub bound_checking: Option<BoundChecking>,

    /// Specifies whether if the lifetimes should be explicitly specified or not.
    pub explicit_lifetime_required: bool,
}

/// Specifies the configuration of the resolution.
#[derive(Debug, Clone, Copy)]
enum TraitOrImplementsID {
    Trait(arena::ID<crate::Trait>),
    Implements(arena::ID<crate::Implements>),
}

/// Represents the resolved implements of a trait.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Implements {
    /// The resolved implements [`ID`].
    pub(super) implements_id: arena::ID<crate::Implements>,

    /// The deduced substitution for the resolved implements.
    pub(super) deduced_substitution: Substitution,
}

/// Represents a resolution result of a symbol with additional generic arguments.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Generics<Symbol> {
    /// The resolved symbol [`ID`].
    #[get_copy = "pub"]
    pub(super) symbol: arena::ID<Symbol>,

    /// The generic arguments substituted for the resolved symbol.
    #[get = "pub"]
    pub(super) substitution: Substitution,
}

/// Represents a resolution to the member of an unresolved-implements trait.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct UnresolvedTraitMember<MemberSymbol> {
    /// The resolved trait [`ID`].
    #[get = "pub"]
    pub(super) trait_resolution: Generics<crate::Trait>,

    /// The resolved member of the [`Self::trait_resolution`].
    #[get = "pub"]
    pub(super) member_resolution: Generics<MemberSymbol>,
}

/// Represents a resolution to the unresolved-implements trait function.
pub type UnresolvedTraitFunction = UnresolvedTraitMember<crate::TraitFunction>;

/// Represents a resolution to the unresolved-implements trait type.
pub type UnresolvedTraitType = UnresolvedTraitMember<crate::TraitType>;

/// Represents a resolution to the trait symbol.
#[derive(Debug, Clone, Getters, CopyGetters)]
pub struct Trait {
    /// The resolved trait [`ID`]
    #[get_copy = "pub"]
    pub(super) trait_id: arena::ID<crate::Trait>,

    /// The resolved substitution of the trait.
    #[get = "pub"]
    pub(super) substitution: Substitution,

    /// The resolved implements of the trait.
    ///
    /// This is `None` if the trait is not an unresolved-implements trait.
    pub(super) resolved_implements: Option<Implements>,
}

/// Represents a resolution to the struct symbol.
pub type Struct = Generics<crate::Struct>;

/// Represents a resolution to the function symbol.
pub type Function = Generics<crate::Function>;

/// Represents a resolution to the implements function symbol.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplementsFunction {
    /// The resolved implements function [`ID`].
    pub implements_function_id: arena::ID<crate::ImplementsFunction>,

    /// The deduced substitution for the resolved implements.
    pub deduced_substitution: Substitution,

    /// The substitution for the resolved function implements.
    pub implements_function_substitution: Substitution,
}

/// Represents a result of a symbol resolution process.
#[derive(Debug, Clone, EnumAsInner)]
#[allow(missing_docs)]
pub enum Resolution {
    Primitive(Primitive),
    Reference(Reference),
    TypeParameter(arena::ID<TypeParameter>),
    Module(arena::ID<Module>),
    Struct(Struct),
    Enum(arena::ID<Enum>),
    EnumVariant(arena::ID<EnumVariant>),
    Function(Function),
    Trait(Trait),
    TraitFunction(UnresolvedTraitFunction),
    TraitType(UnresolvedTraitType),
    ImplementsFunction(ImplementsFunction),
}

#[derive(Debug, Clone)]
pub(super) struct Root {
    pub(super) referring_site: ID,
    pub(super) latest_resolution: Option<Resolution>,
}

pub(super) enum Kind {
    GlobalID(GlobalID),
    Type(ty::Type),
    ImplementsFunction(arena::ID<crate::ImplementsFunction>),
}

impl Table {
    /// Resolves the symbol from the current scope and using modules.
    #[allow(clippy::option_option)]
    fn first_resolution(
        &self,
        identifier: &Identifier,
        referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Option<Option<GlobalID>> {
        let parent_module = self.get_closet_module_id(referring_site).unwrap();

        let search_locations = self.modules[parent_module]
            .usings
            .iter()
            .copied()
            .map(ScopedID::from)
            .chain(std::iter::once(
                self.get_closet_scoped_id(referring_site).unwrap(),
            ));

        let mut candidates = HashSet::new();
        for location in search_locations {
            if let Some(id) = self
                .get_scoped(location)
                .unwrap()
                .get_child_id_by_name(identifier.span.str())
            {
                if self.symbol_accessible(id, referring_site).unwrap() {
                    candidates.insert(id);
                }
            }
        }

        match candidates.len() {
            1 => Some(Some(candidates.into_iter().next().unwrap())),
            0 => Some(None),
            _ => {
                handler.receive(error::Error::ResolutionAmbiguity(ResolutionAmbiguity {
                    span: identifier.span.clone(),
                    candidates: candidates.into_iter().collect(),
                }));
                None
            }
        }
    }

    /// Iterates down through the scope hierarchies and tries to find a symbol with the given name.
    fn second_resolution(
        &self,
        identifier: &Identifier,
        mut referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Option<GlobalID> {
        // NOTE: Accessibility is not checked here because the symbol searching is done within the
        // same module ancestor tree.

        loop {
            // try to find the symbol in the current scope
            if let Ok(scoped_id) = referring_site.try_into() {
                if let Some(id) = self
                    .get_scoped(scoped_id)
                    .unwrap()
                    .get_child_id_by_name(identifier.span.str())
                {
                    return Some(id);
                }
            }

            if let Some(parent_id) = self.get_symbol(referring_site).unwrap().parent_symbol() {
                referring_site = parent_id;
            } else {
                // try to search the symbol in the root scope
                if let Some(id) = self
                    .target_root_module_ids_by_name
                    .get(identifier.span.str())
                    .copied()
                {
                    return Some(id.into());
                }

                handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                    searched_global_id: None,
                    symbol_reference_span: identifier.span.clone(),
                }));
                return None;
            }
        }
    }

    /// Resolves the root for the first resolution if the qualified identifier is not prefixed with
    /// leading `::`.
    fn resolve_root_relative(
        &self,
        identifier: &Identifier,
        referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Option<GlobalID> {
        let found_symbol_id =
            if let Some(id) = self.first_resolution(identifier, referring_site, handler)? {
                id
            } else {
                self.second_resolution(identifier, referring_site, handler)?
            };

        Some(found_symbol_id)
    }

    /// Resolves for the [`LifetimeArgument`] based on the given lifetime arugment syntax tree.
    ///
    /// # Arguments
    /// - `referring_site`: where the resolution is requested.
    /// - `lifetime_argument`: the lifetime argument syntax tree.
    /// - `handler`: the error handler.
    ///
    /// # Errors
    /// - [`table::error::Error::InvalidID`]: if the `referring_site` is invalid.
    /// - [`table::error::Error::FatalSemantic`]: for any encountered semantic errors.
    pub fn resolve_lifetime_argument(
        &self,
        referring_site: ID,
        lifetime_argument: &syntax_tree::LifetimeArgument,
        handler: &impl Handler<error::Error>,
    ) -> Result<LifetimeArgument, table::Error> {
        match lifetime_argument.identifier() {
            syntax_tree::LifetimeArgumentIdentifier::Identifier(lifetime_parameter) => {
                Ok(LifetimeArgument::Parameter(
                    self.resolve_lifetime_parameter(referring_site, lifetime_parameter, handler)?,
                ))
            }
            syntax_tree::LifetimeArgumentIdentifier::Static(..) => Ok(LifetimeArgument::Static),
        }
    }

    /// Resolves for the [`LifetimeParameter`] based on the given lifetime parameter syntax tree.
    ///
    /// # Arugments
    /// - `referring_site`: where the resolution is requested.
    /// - `lifetime_parameter`: the lifetime parameter syntax tree.
    /// - `handler`: the error handler.
    ///
    /// # Errors
    /// - [`table::error::Error::InvalidID`]: if the `referring_site` is invalid.
    /// - [`table::error::Error::FatalSemantic`]: for any encountered semantic errors.
    pub fn resolve_lifetime_parameter(
        &self,
        referring_site: ID,
        lifetime_parameter: &Identifier,
        handler: &impl Handler<error::Error>,
    ) -> Result<arena::ID<LifetimeParameter>, table::Error> {
        let scope_walker = self
            .scope_walker(referring_site)
            .ok_or(table::Error::InvalidID)?;

        for id in scope_walker {
            let Ok(genericable_id) = GenericableID::try_from(id) else {
                continue;
            };

            let genericable = self.get_genericable(genericable_id).unwrap();
            if let Some(lifetime_parameter_id) = genericable
                .generic_parameters()
                .lifetime_parameter_ids_by_name
                .get(lifetime_parameter.span.str())
                .copied()
            {
                return Ok(lifetime_parameter_id);
            }
        }

        handler.receive(error::Error::LifetimeNotFound(LifetimeNotFound {
            unknown_lifetime_span: lifetime_parameter.span.clone(),
        }));
        Err(table::Error::FatalSemantic)
    }

    fn get_current_trait_or_implements_id(&self, current_id: ID) -> Option<TraitOrImplementsID> {
        for id in self.scope_walker(current_id).unwrap() {
            match id {
                ID::Trait(trait_id) => return Some(TraitOrImplementsID::Trait(trait_id)),
                ID::Implements(implements_id) => {
                    return Some(TraitOrImplementsID::Implements(implements_id))
                }
                _ => {}
            }
        }

        None
    }

    fn get_active_trait(&self, used_root: &Root) -> Trait {
        used_root.latest_resolution.as_ref().map_or_else(
            || match self
                .get_current_trait_or_implements_id(used_root.referring_site)
                .unwrap()
            {
                TraitOrImplementsID::Trait(trait_id) => Trait {
                    trait_id,
                    substitution: Self::create_identical_substitution(
                        &self.traits[trait_id].generics.parameters,
                    ),
                    resolved_implements: None,
                },
                TraitOrImplementsID::Implements(implements_id) => Trait {
                    trait_id: self.implements[implements_id].trait_id,
                    substitution: self.implements[implements_id].substitution.clone(),
                    resolved_implements: Some(Implements {
                        implements_id,
                        deduced_substitution: Self::create_identical_substitution(
                            &self.implements[implements_id].generics.parameters,
                        ),
                    }),
                },
            },
            |resolution| resolution.as_trait().cloned().unwrap(),
        )
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_substitution(
        &self,
        generic_identifier: &GenericIdentifier,
        found_global_id: GlobalID,
        info: &Info,
        handler: &impl Handler<error::Error>,
    ) -> Option<Substitution> {
        let mut substitution = Substitution::default();

        let Ok(genericable_id) = GenericableID::try_from(found_global_id) else {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                handler.receive(error::Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        span: generic_arguments.span(),
                    },
                ));
                return None;
            }

            return Some(substitution);
        };

        let mut type_argument_syntax_trees = Vec::new();
        let mut lifetime_argument_syntax_trees = Vec::new();

        for generic_argument in generic_identifier
            .generic_arguments()
            .iter()
            .flat_map(|x| x.argument_list().elements())
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
                                    lifetime_argument_span: lifetime_argument.span(),
                                },
                            ),
                        );
                        return None;
                    }

                    lifetime_argument_syntax_trees.push(lifetime_argument);
                }
            }
        }

        match (
            self.get_genericable(genericable_id)
                .unwrap()
                .generic_parameters()
                .lifetime_parameter_order
                .len(),
            lifetime_argument_syntax_trees.len(),
            info.explicit_lifetime_required,
        ) {
            // lifetime arguments must be supplied
            (1.., 0, true) => {
                handler.receive(error::Error::LifetimeArgumentsRequired(
                    LifetimeArgumentsRequired {
                        span: generic_identifier.span(),
                    },
                ));

                return None;
            }

            // either supplied or not at all
            (1.., 0, false) => {}

            // well formed lifetime arguments
            (required, supplied, _) if required == supplied => {
                assert!(lifetime_argument_syntax_trees.len() == required);

                for (lifetime_parameter, lifetime_argument_syntax_tree) in self
                    .get_genericable(genericable_id)
                    .unwrap()
                    .generic_parameters()
                    .lifetime_parameter_order
                    .iter()
                    .copied()
                    .zip(lifetime_argument_syntax_trees.iter().copied())
                {
                    let lifetime_argument = match self.resolve_lifetime_argument(
                        info.referring_site,
                        lifetime_argument_syntax_tree,
                        handler,
                    ) {
                        Ok(x) => x,
                        Err(table::Error::FatalSemantic) => return None,
                        Err(table::Error::InvalidID) => unreachable!(),
                    };

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
                        generic_arguments_span: generic_identifier
                            .generic_arguments()
                            .as_ref()
                            .map_or_else(|| generic_identifier.span(), SourceElement::span),
                    },
                ));

                return None;
            }
        }

        if type_argument_syntax_trees.len()
            == self
                .get_genericable(genericable_id)
                .unwrap()
                .generic_parameters()
                .type_parameter_order
                .len()
        {
            let type_parameters = self
                .get_genericable(genericable_id)
                .unwrap()
                .generic_parameters()
                .type_parameter_order
                .clone();

            for (type_parameter, type_specifier) in type_parameters
                .into_iter()
                .zip(type_argument_syntax_trees.iter().copied())
            {
                let ty = match self.resolve_type(info, type_specifier, handler) {
                    Ok(x) => x,
                    Err(table::Error::FatalSemantic) => return None,
                    Err(table::Error::InvalidID) => unreachable!(),
                };

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
                        .get_genericable(genericable_id)
                        .unwrap()
                        .generic_parameters()
                        .type_parameter_order
                        .len(),
                    generic_arguments_span: generic_identifier
                        .generic_arguments()
                        .as_ref()
                        .map_or_else(|| generic_identifier.span(), SourceElement::span),
                },
            ));
            return None;
        }

        Some(substitution)
    }

    /// second pass resolution involves in checking the where clause and resolving the generic
    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_single_second_pass(
        &self,
        generic_identifier_span: &Span,
        used_root: &Root,
        found_global_id: GlobalID,
        resolved_substitution: Substitution,
        bound_checking: Option<BoundChecking>,
        handler: &impl Handler<error::Error>,
    ) -> Option<Resolution> {
        let parent_substitution =
            used_root
                .latest_resolution
                .as_ref()
                .and_then(|latest_resolution| match latest_resolution {
                    Resolution::Primitive(..)
                    | Resolution::Reference(..)
                    | Resolution::Function(..)
                    | Resolution::EnumVariant(..)
                    | Resolution::TraitFunction(..)
                    | Resolution::TraitType(..)
                    | Resolution::ImplementsFunction(..)
                    | Resolution::TypeParameter(..) => unreachable!(),

                    Resolution::Module(..) | Resolution::Struct(..) | Resolution::Enum(..) => None,

                    Resolution::Trait(trait_resolution) => Some(&trait_resolution.substitution),
                });

        let active_where_clause = self
            .get_active_where_clause(used_root.referring_site)
            .unwrap();

        if let Ok(genericable_id) = GenericableID::try_from(found_global_id) {
            if let Some(bound_checking) = bound_checking {
                let substitution = parent_substitution.map(|parent_substitution| {
                    Substitution::coombine(&resolved_substitution, parent_substitution)
                });

                dbg!(&active_where_clause);
                if !self.check_where_clause(
                    genericable_id,
                    substitution
                        .as_ref()
                        .map_or(&resolved_substitution, |substitution| substitution),
                    bound_checking,
                    generic_identifier_span,
                    &active_where_clause,
                    handler,
                ) {
                    return None;
                }
            }
        } else {
            assert!(resolved_substitution.is_empty());
        }

        Some(match found_global_id {
            GlobalID::Module(id) => Resolution::Module(id),
            GlobalID::Struct(id) => Resolution::Struct(Generics {
                symbol: id,
                substitution: resolved_substitution,
            }),
            GlobalID::Enum(id) => Resolution::Enum(id),
            GlobalID::EnumVariant(id) => Resolution::EnumVariant(id),
            GlobalID::Function(id) => Resolution::Function(Generics {
                symbol: id,
                substitution: resolved_substitution,
            }),
            GlobalID::Type(id) => self.substitute_type_as_resolution(
                &self.types[id].alias,
                &resolved_substitution,
                bound_checking,
                &active_where_clause,
                generic_identifier_span,
                handler,
            )?,
            GlobalID::Trait(id) => {
                if resolved_substitution.is_concrete_substitution() {
                    let implements = self.resolve_trait_implements(
                        id,
                        &resolved_substitution,
                        generic_identifier_span,
                        bound_checking,
                        &active_where_clause,
                        handler,
                    )?;

                    Resolution::Trait(Trait {
                        trait_id: id,
                        substitution: resolved_substitution,
                        resolved_implements: Some(implements),
                    })
                } else {
                    let required_trait_bound = TraitBound {
                        trait_id: id,
                        substitution: resolved_substitution,
                    };

                    if bound_checking.is_some()
                        && !active_where_clause
                            .trait_bounds
                            .contains(&required_trait_bound)
                    {
                        handler.receive(error::Error::TraitBoundNotSatisfied(
                            TraitBoundNotSatisfied {
                                required_trait_bound_string: self
                                    .get_qualified_name_with_substitution(
                                        required_trait_bound.trait_id.into(),
                                        &required_trait_bound.substitution,
                                    )
                                    .unwrap(),
                                generic_identifier_span: generic_identifier_span.clone(),
                            },
                        ));
                        return None;
                    }

                    Resolution::Trait(Trait {
                        trait_id: id,
                        substitution: required_trait_bound.substitution,
                        resolved_implements: None,
                    })
                }
            }
            GlobalID::TraitFunction(id) => {
                let active_trait = self.get_active_trait(used_root);

                match active_trait.resolved_implements {
                    Some(implements) => Resolution::ImplementsFunction(ImplementsFunction {
                        implements_function_id: self.implements[implements.implements_id]
                            .implements_functions_by_trait_function[&id],
                        deduced_substitution: implements.deduced_substitution,
                        implements_function_substitution:
                            Self::transform_trait_member_substitution_to_implements_substitution(
                                resolved_substitution,
                                &self.trait_functions[id].generics.parameters,
                                &self.implements_functions[self.implements
                                    [implements.implements_id]
                                    .implements_functions_by_trait_function[&id]]
                                    .generics
                                    .parameters,
                            ),
                    }),
                    None => Resolution::TraitFunction(UnresolvedTraitMember {
                        trait_resolution: Generics {
                            symbol: active_trait.trait_id,
                            substitution: active_trait.substitution,
                        },
                        member_resolution: Generics {
                            symbol: id,
                            substitution: resolved_substitution,
                        },
                    }),
                }
            }
            GlobalID::TraitType(id) => {
                let active_trait = self.get_active_trait(used_root);

                match active_trait.resolved_implements {
                    Some(implements) => {
                        let combined_substitution = Substitution::coombine(
                            &Self::transform_trait_member_substitution_to_implements_substitution(
                                resolved_substitution,
                                &self.trait_types[id].generic_parameters,
                                &self.implements_types[self.implements[implements.implements_id]
                                    .implements_types_by_trait_type[&id]]
                                    .generic_parameters,
                            ),
                            &implements.deduced_substitution,
                        );
                        self.substitute_type_as_resolution(
                            &self.implements_types[self.implements[implements.implements_id]
                                .implements_types_by_trait_type[&id]]
                                .alias,
                            &combined_substitution,
                            bound_checking,
                            &active_where_clause,
                            generic_identifier_span,
                            handler,
                        )?
                    }
                    None => Resolution::TraitType(UnresolvedTraitMember {
                        trait_resolution: Generics {
                            symbol: active_trait.trait_id,
                            substitution: active_trait.substitution,
                        },
                        member_resolution: Generics {
                            symbol: id,
                            substitution: resolved_substitution,
                        },
                    }),
                }
            }
        })
    }

    #[must_use]
    fn transform_trait_member_substitution_to_implements_substitution(
        trait_member_substituteion: Substitution,
        trait_member_generic_parameters: &GenericParameters,
        implements_member_generic_parameters: &GenericParameters,
    ) -> Substitution {
        let mut result_substitution = Substitution::default();

        for (ty_parameter, ty_argument) in trait_member_substituteion.type_arguments_by_parameter {
            let ty_parameter_index = trait_member_generic_parameters
                .type_parameter_order
                .iter()
                .copied()
                .position(|x| x == ty_parameter)
                .unwrap();
            result_substitution.type_arguments_by_parameter.insert(
                implements_member_generic_parameters.type_parameter_order[ty_parameter_index],
                ty_argument,
            );
        }

        for (lt_parameter, lt_argument) in
            trait_member_substituteion.lifetime_arguments_by_parameter
        {
            let lt_parameter_index = trait_member_generic_parameters
                .lifetime_parameter_order
                .iter()
                .copied()
                .position(|x| x == lt_parameter)
                .unwrap();
            result_substitution.lifetime_arguments_by_parameter.insert(
                implements_member_generic_parameters.lifetime_parameter_order[lt_parameter_index],
                lt_argument,
            );
        }

        result_substitution
    }

    // first pass resolution only involves in searching for the global id living in the root
    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_single_first_pass(
        &self,
        identifier: &Identifier,
        root: &Root,
        search_from_target: bool,
        handler: &impl Handler<error::Error>,
    ) -> Option<GlobalID> {
        let global_id = match &root.latest_resolution {
            None => {
                if search_from_target {
                    self.target_root_module_ids_by_name
                        .get(identifier.span.str())
                        .copied()
                        .map_or_else(
                            || {
                                handler.receive(error::Error::TargetNotFound(TargetNotFound {
                                    unknown_target_span: identifier.span.clone(),
                                }));
                                None
                            },
                            |id| Some(id.into()),
                        )?
                } else {
                    // perform a local search down the module tree
                    self.resolve_root_relative(identifier, root.referring_site, handler)?
                }
            }
            Some(resolution) => {
                let resolution_kind = match resolution {
                    Resolution::Primitive(primitive) => Kind::Type(ty::Type::Primitive(*primitive)),
                    Resolution::Reference(refernce) => {
                        Kind::Type(ty::Type::Reference(refernce.clone()))
                    }
                    Resolution::TypeParameter(ty_parameter) => {
                        Kind::Type(ty::Type::Parameter(*ty_parameter))
                    }
                    Resolution::Module(id) => Kind::GlobalID((*id).into()),
                    Resolution::Struct(struct_resolution) => {
                        Kind::GlobalID(struct_resolution.symbol.into())
                    }
                    Resolution::Enum(id) => Kind::GlobalID((*id).into()),
                    Resolution::EnumVariant(id) => Kind::GlobalID((*id).into()),
                    Resolution::Function(function_resolution) => {
                        Kind::GlobalID(function_resolution.symbol.into())
                    }
                    Resolution::Trait(trait_resolution) => {
                        Kind::GlobalID(trait_resolution.trait_id.into())
                    }
                    Resolution::TraitFunction(trait_function_resolution) => {
                        Kind::GlobalID(trait_function_resolution.member_resolution.symbol.into())
                    }
                    Resolution::TraitType(trait_type_resolution) => {
                        Kind::GlobalID(trait_type_resolution.member_resolution.symbol.into())
                    }
                    Resolution::ImplementsFunction(implemenst_function_resolution) => {
                        Kind::ImplementsFunction(
                            implemenst_function_resolution.implements_function_id,
                        )
                    }
                };

                let global_id = match resolution_kind {
                    Kind::GlobalID(global_id) => global_id,
                    Kind::Type(ty) => {
                        handler.receive(error::Error::NoMemberOnThisType(NoMemberOnThisType {
                            symbol_reference_span: identifier.span.clone(),
                            ty,
                        }));
                        return None;
                    }
                    Kind::ImplementsFunction(implements_function_id) => {
                        handler.receive(error::Error::NoMemberOnThisImplementsFunction(
                            NoMemberOnThisImplementsFunction {
                                symbol_reference_span: identifier.span.clone(),
                                implements_function_id,
                            },
                        ));
                        return None;
                    }
                };

                let Ok(scoped_id) = ScopedID::try_from(global_id) else {
                    handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                        searched_global_id: Some(global_id),
                        symbol_reference_span: identifier.span.clone(),
                    }));
                    return None;
                };

                let Some(global_id) = self
                    .get_scoped(scoped_id)
                    .unwrap()
                    .get_child_id_by_name(identifier.span.str())
                else {
                    handler.receive(error::Error::SymbolNotFound(SymbolNotFound {
                        searched_global_id: Some(global_id),
                        symbol_reference_span: identifier.span.clone(),
                    }));
                    return None;
                };

                global_id
            }
        };

        if !self
            .symbol_accessible(global_id, root.referring_site)
            .unwrap()
        {
            handler.receive(error::Error::SymbolWasNotAccessible(
                SymbolWasNotAccessible {
                    span: identifier.span.clone(),
                    referring_module_site: self.get_closet_module_id(root.referring_site).unwrap(),
                    referred: global_id,
                },
            ));

            return None;
        }

        Some(global_id)
    }

    pub(super) fn resolve_type_parameter(
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

    fn resolve_qualified_identifier_type(
        &self,
        info: &Info,
        qualified_identifier: &QualifiedIdentifier,
        handler: &impl Handler<error::Error>,
    ) -> Option<ty::Type> {
        // If the qualified identifier is a simple identifier, then try to resolve it as a
        // type parameter.
        if qualified_identifier.leading_scope_separator().is_none()
            && qualified_identifier.rest().is_empty()
            && qualified_identifier.first().generic_arguments().is_none()
        {
            if let Some(ty) = self.resolve_type_parameter(
                info.referring_site,
                qualified_identifier.first().identifier(),
            ) {
                return Some(ty::Type::Parameter(ty));
            }
        }

        let resolution = match self.resolve(info, qualified_identifier, handler) {
            Ok(x) => x,
            Err(table::Error::FatalSemantic) => return None,
            Err(table::Error::InvalidID) => unreachable!(),
        };

        Some(match resolution {
            Resolution::Primitive(primitive) => ty::Type::Primitive(primitive),
            Resolution::Reference(reference) => ty::Type::Reference(reference),
            Resolution::Struct(struct_resolution) => ty::Type::Struct(ty::Struct {
                struct_id: struct_resolution.symbol,
                substitution: struct_resolution.substitution,
            }),
            Resolution::Enum(enum_id) => ty::Type::Enum(enum_id),
            Resolution::TraitType(trait_type) => ty::Type::TraitType(ty::TraitType {
                trait_type_id: trait_type.member_resolution.symbol,
                trait_substitution: trait_type.trait_resolution.substitution,
                trait_type_substitution: trait_type.member_resolution.substitution,
            }),
            Resolution::TypeParameter(type_parameter) => ty::Type::Parameter(type_parameter),

            Resolution::Module(..)
            | Resolution::EnumVariant(..)
            | Resolution::Function(..)
            | Resolution::TraitFunction(..)
            | Resolution::ImplementsFunction(..)
            | Resolution::Trait(..) => {
                handler.receive(error::Error::TypeExpected(TypeExpected {
                    non_type_symbol_span: qualified_identifier.span(),
                }));
                return None;
            }
        })
    }

    /// Resolves the [`TypeSpecifier`] syntax into a [`ty::Type`].
    ///
    /// # Errors
    /// - [`table::Error::FatalSemantic`]: If encountered a fatal semantic error during the lookup.
    /// - [`table:Error::InvalidID`]: If the given [`Info::referring_site`] is not from this table.
    pub fn resolve_type(
        &self,
        info: &Info,
        type_specifier: &TypeSpecifier,
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
                .resolve_qualified_identifier_type(info, qualified_identifier, handler)
                .ok_or(table::Error::FatalSemantic),
            TypeSpecifier::Reference(reference_type) => Ok(ty::Type::Reference(Reference {
                operand: Box::new(self.resolve_type(
                    info,
                    reference_type.operand_type(),
                    handler,
                )?),
                qualifier: reference_type.qualifier().as_ref().map(|x| match x {
                    syntax_tree::ReferenceQualifier::Mutable(_) => ty::ReferenceQualifier::Mutable,
                    syntax_tree::ReferenceQualifier::Restrict(_) => {
                        ty::ReferenceQualifier::Restrict
                    }
                }),
                lifetime_argument: match reference_type.lifetime_argument() {
                    Some(x) => {
                        Some(self.resolve_lifetime_argument(info.referring_site, x, handler)?)
                    }
                    None => {
                        if info.explicit_lifetime_required {
                            handler.receive(error::Error::LifetimeArgumentsRequired(
                                LifetimeArgumentsRequired {
                                    span: reference_type.span(),
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

    /// Performs a full symbol lookup.
    ///
    /// # Errors
    /// - [`table::Error::FatalSemantic`]: If encountered a fatal semantic error during the lookup.
    /// - [`table:Error::InvalidID`]: If the given [`Info::referring_site`] is not from this table.
    pub fn resolve(
        &self,
        info: &Info,
        qualified_identifier: &QualifiedIdentifier,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, table::Error> {
        if self.get_symbol(info.referring_site).is_none() {
            return Err(table::Error::InvalidID);
        }

        let mut current_root = Root {
            referring_site: info.referring_site,
            latest_resolution: None,
        };

        for generics_identifier in std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
        {
            let global_id = self
                .resolve_single_first_pass(
                    generics_identifier.identifier(),
                    &current_root,
                    qualified_identifier.leading_scope_separator().is_some(),
                    handler,
                )
                .ok_or(table::Error::FatalSemantic)?;

            let substitution = self
                .resolve_substitution(generics_identifier, global_id, info, handler)
                .ok_or(table::Error::FatalSemantic)?;

            current_root.latest_resolution = Some(
                self.resolve_single_second_pass(
                    &generics_identifier.span(),
                    &current_root,
                    global_id,
                    substitution,
                    info.bound_checking,
                    handler,
                )
                .ok_or(table::Error::FatalSemantic)?,
            );
        }

        Ok(current_root.latest_resolution.unwrap())
    }
}

impl Table {
    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_trait_path(
        &self,
        qualified_identifier: &QualifiedIdentifier,
        referring_site: ID,
        handler: &impl Handler<error::Error>,
    ) -> Option<arena::ID<crate::Trait>> {
        let mut current_root = Root {
            referring_site,
            latest_resolution: None,
        };

        let mut iter = std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
            .peekable();

        while let Some(generic_identifier) = iter.next() {
            let is_last = iter.peek().is_none();

            let Some(global_id) = self.resolve_single_first_pass(
                generic_identifier.identifier(),
                &current_root,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            ) else {
                return None;
            };

            if !is_last {
                let GlobalID::Module(module_id) = global_id else {
                    handler.receive(error::Error::ModuleExpected(ModuleExpected {
                        symbol_reference_span: generic_identifier.span(),
                        found: global_id,
                    }));
                    return None;
                };

                if generic_identifier.generic_arguments().is_some() {
                    handler.receive(error::Error::NoGenericArgumentsRequired(
                        NoGenericArgumentsRequired {
                            span: generic_identifier.span(),
                        },
                    ));
                    return None;
                }

                current_root.latest_resolution = Some(Resolution::Module(module_id));
            } else if let GlobalID::Trait(trait_id) = global_id {
                return Some(trait_id);
            } else {
                handler.receive(error::Error::TraitExpected(TraitExpected {
                    symbol_reference_span: generic_identifier.span(),
                }));
                return None;
            }
        }

        unreachable!()
    }
}
