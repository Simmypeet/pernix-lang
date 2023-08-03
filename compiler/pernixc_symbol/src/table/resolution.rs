//! Contains the resolution logic for the symbol table.

use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
};

use enum_as_inner::EnumAsInner;
use getset::{CopyGetters, Getters};
use pernixc_lexical::token::Identifier;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, GenericArgument, GenericIdentifier, QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{
    arena,
    diagnostic::{Dummy, Handler},
};

use super::Table;
use crate::{
    error::{
        self, LifetimeArgumentCountMismatch, LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
        LifetimeArgumentsRequired, LifetimeDoesNotOutlive, LifetimeNotFound, ModuleExpected,
        NoGenericArgumentsRequired, NoMemberOnThisImplementsFunction, NoMemberOnThisType,
        ResolutionAmbiguity, SymbolNotFound, SymbolWasNotAccessible, TargetNotFound,
        TraitBoundNotSatisfied, TraitExpected, TraitResolutionNotAllowed,
        TraitTypeBoundNotSatisfied, TypeArgumentCountMismatch, TypeDoesNotOutliveLifetimeArgument,
        TypeExpected,
    },
    table,
    ty::{self, Primitive, Reference},
    Enum, EnumVariant, GenericParameters, GenericableID, GlobalID, LifetimeArgument,
    LifetimeParameter, Module, ScopedID, Substitution, TraitBound, TypeParameter, WhereClause, ID,
};

/// Describing how the resolution should deal with the generics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum GenericConfiguration {
    /// Where clauses are checked and trait resolutions are allowed.
    #[default]
    Default,

    /// Where clauses will be ignored and trait resolutions cause an error.
    TraitResolutionNotAllowed,

    /// Where clauses and trait resolutions will be ignored.
    TraitResolutionIgnored,
}

/// Contains the neccessary information and configuration for the resolution process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Info {
    /// Specifies where the resolution is occurring.
    pub referring_site: ID,

    /// Describes how the resolution should deal with the generics.
    pub generic_configuration: GenericConfiguration,

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

    pub(super) fn resolve_lifetime_argument(
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

    /// Resolves the lifetime parameter.
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

    #[allow(clippy::too_many_lines)]
    fn substitute_where_clause(
        &self,
        where_clause: &WhereClause,
        substitution: &Substitution,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<WhereClause> {
        let mut result_where_clause = WhereClause::default();

        for (lifetime_parameter, lifetime_argument_set) in
            &where_clause.lifetime_argument_sets_by_lifetime_parameter
        {
            let lifetime_parameter_to_insert = match substitution
                .lifetime_arguments_by_parameter
                .get(lifetime_parameter)
            {
                Some(LifetimeArgument::Parameter(parameter)) => *parameter,
                Some(LifetimeArgument::Static) => continue,
                None => *lifetime_parameter,
            };

            let result_lifetime_argument_set = result_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .entry(lifetime_parameter_to_insert)
                .or_insert_with(HashSet::new);

            for lifetime_argument in lifetime_argument_set {
                match lifetime_argument {
                    LifetimeArgument::Static => {
                        result_lifetime_argument_set.insert(LifetimeArgument::Static);
                    }
                    LifetimeArgument::Parameter(parameter) => {
                        result_lifetime_argument_set.insert(
                            substitution
                                .lifetime_arguments_by_parameter
                                .get(parameter)
                                .copied()
                                .unwrap_or(LifetimeArgument::Parameter(*parameter)),
                        );
                    }
                }
            }
        }

        for (ty, lifetime_argument_set) in &where_clause.lifetime_argument_sets_by_type {
            let aliased = self.alias_type(
                ty,
                substitution,
                GenericConfiguration::Default,
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;
            let result_lifetime_argument_set = result_where_clause
                .lifetime_argument_sets_by_type
                .entry(aliased)
                .or_insert_with(HashSet::default);

            for lifetime_argument in lifetime_argument_set {
                match lifetime_argument {
                    LifetimeArgument::Static => {
                        result_lifetime_argument_set.insert(LifetimeArgument::Static);
                    }
                    LifetimeArgument::Parameter(parameter) => {
                        result_lifetime_argument_set.insert(
                            substitution
                                .lifetime_arguments_by_parameter
                                .get(parameter)
                                .copied()
                                .unwrap_or(LifetimeArgument::Parameter(*parameter)),
                        );
                    }
                }
            }
        }

        for trait_bound in &where_clause.trait_bounds {
            let mut new_substitution = trait_bound.substitution.clone();
            if !self.apply_substitution_on_arguments(
                &mut new_substitution,
                substitution,
                GenericConfiguration::Default,
                active_where_clause,
                generic_identifier_span,
                handler,
            ) {
                return None;
            }

            result_where_clause.trait_bounds.insert(TraitBound {
                trait_id: trait_bound.trait_id,
                substitution: new_substitution,
            });
        }

        for (trait_type, ty) in &where_clause.types_by_trait_type {
            let aliased_ty_bound = self.alias_type(
                ty,
                substitution,
                GenericConfiguration::Default,
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;
            let aliased_trait_ty = self.alias_type(
                &ty::Type::TraitType(trait_type.clone()),
                substitution,
                GenericConfiguration::Default,
                active_where_clause,
                generic_identifier_span,
                handler,
            )?;

            match aliased_trait_ty {
                ty::Type::TraitType(trait_ty) => {
                    result_where_clause
                        .types_by_trait_type
                        .insert(trait_ty, aliased_ty_bound);
                }
                ty => {
                    if ty != aliased_ty_bound {
                        handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                            TraitTypeBoundNotSatisfied {
                                required_type: ty,
                                generics_identifier_span: generic_identifier_span.clone(),
                            },
                        ));
                        return None;
                    }
                }
            }
        }

        Some(result_where_clause)
    }

    #[must_use]
    fn apply_substitution_on_arguments(
        &self,
        substitution_operand: &mut Substitution,
        other: &Substitution,
        generic_configuration: GenericConfiguration,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
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
            if !self.substitute_type(
                argument,
                other,
                generic_configuration,
                active_where_clause,
                generic_identifier_span,
                handler,
            ) {
                return false;
            }
        }

        true
    }

    fn deduce(
        implements_ty: &ty::Type,
        trait_ty: &ty::Type,
        (mut previous_substitution, mut trait_type_substitution): (
            BTreeMap<arena::ID<TypeParameter>, ty::Type>,
            HashMap<ty::TraitType, ty::Type>,
        ),
    ) -> Option<(
        BTreeMap<arena::ID<TypeParameter>, ty::Type>,
        HashMap<ty::TraitType, ty::Type>,
    )> {
        match (implements_ty, trait_ty) {
            (ty::Type::Parameter(implements_ty), trait_ty) => {
                if let Some(previous_ty) =
                    previous_substitution.insert(*implements_ty, trait_ty.clone())
                {
                    if previous_ty != *trait_ty {
                        return None;
                    }
                }

                Some((previous_substitution, trait_type_substitution))
            }
            (ty::Type::TraitType(implements_ty), trait_ty) => {
                if let Some(previous_ty) =
                    trait_type_substitution.insert(implements_ty.clone(), trait_ty.clone())
                {
                    if previous_ty != *trait_ty {
                        return None;
                    }
                }

                Some((previous_substitution, trait_type_substitution))
            }
            (ty::Type::Reference(implements_ty), ty::Type::Reference(trait_ty)) => Self::deduce(
                &implements_ty.operand,
                &trait_ty.operand,
                (previous_substitution, trait_type_substitution),
            ),
            (ty::Type::Struct(implements_ty), ty::Type::Struct(trait_ty)) => {
                if implements_ty.struct_id != trait_ty.struct_id {
                    return None;
                }

                let mut result = (previous_substitution, trait_type_substitution);

                for (implements_ty, trait_ty) in implements_ty
                    .substitution
                    .type_arguments_by_parameter
                    .values()
                    .zip(trait_ty.substitution.type_arguments_by_parameter.values())
                {
                    let Some(new_result) = Self::deduce(implements_ty, trait_ty, result) else {
                        return None;
                    };
                    result = new_result;
                }

                Some(result)
            }
            (implements_ty, trait_ty) => {
                if implements_ty != trait_ty {
                    return None;
                }

                Some((previous_substitution, trait_type_substitution))
            }
        }
    }

    fn get_deduced_substitution(
        &self,
        generic_identifier_span: &Span,
        implement_substitution: &Substitution,
        trait_substitution: &Substitution,
        active_where_clause: &WhereClause,
    ) -> Option<Substitution> {
        let mut type_substitution = BTreeMap::new();
        let mut trait_type_substitution = HashMap::new();

        for (implements_ty, trait_ty) in implement_substitution
            .type_arguments_by_parameter
            .values()
            .zip(trait_substitution.type_arguments_by_parameter.values())
        {
            if let Some((previous, trait_type)) = Self::deduce(
                implements_ty,
                trait_ty,
                (type_substitution, trait_type_substitution),
            ) {
                type_substitution = previous;
                trait_type_substitution = trait_type;
            } else {
                return None;
            }
        }

        let deduced_substitution = Substitution {
            type_arguments_by_parameter: type_substitution,
            lifetime_arguments_by_parameter: implement_substitution
                .lifetime_arguments_by_parameter
                .clone(),
        };

        for (trait_ty, required_ty) in trait_type_substitution {
            let trait_ty = ty::Type::TraitType(trait_ty);
            let Some(aliased_ty) = self.alias_type(
                &trait_ty,
                &deduced_substitution,
                GenericConfiguration::Default,
                active_where_clause,
                generic_identifier_span,
                &Dummy,
            ) else {
                return None;
            };

            if aliased_ty != required_ty {
                return None;
            }
        }

        Some(deduced_substitution)
    }

    fn resolve_trait_implements(
        &self,
        trait_id: arena::ID<crate::Trait>,
        substitution: &Substitution,
        generic_identifier_span: &Span,
        active_where_clause: &WhereClause,
        handler: &impl Handler<error::Error>,
    ) -> Option<Implements> {
        let mut implements_ids_by_deduced_substitution = HashMap::new();

        for implements_id in self.traits[trait_id].implements.iter().copied() {
            let Some(deduced_substitution) = self.get_deduced_substitution(
                generic_identifier_span,
                &self.implements[implements_id].substitution,
                substitution,
                active_where_clause,
            ) else {
                continue;
            };

            if !self.check_where_clause(
                implements_id.into(),
                &deduced_substitution,
                generic_identifier_span,
                active_where_clause,
                &Dummy,
            ) {
                continue;
            }

            assert!(implements_ids_by_deduced_substitution
                .insert(implements_id, deduced_substitution)
                .is_none());
        }

        let implements = implements_ids_by_deduced_substitution
            .keys()
            .copied()
            .reduce(|lhs, rhs| {
                match compare_implements(
                    &self.implements[lhs].substitution,
                    &self.implements[rhs].substitution,
                ) {
                    ImplementsComparison::MoreSpecialized => lhs,
                    ImplementsComparison::LessSpecialized => rhs,
                    ImplementsComparison::Incompatible | ImplementsComparison::Ambiguous => {
                        unreachable!()
                    }
                }
            });

        implements.map_or_else(
            || {
                handler.receive(error::Error::NoImplementsFound(error::NoImplementsFound {
                    span: generic_identifier_span.clone(),
                }));
                None
            },
            |implements| {
                Some(Implements {
                    implements_id: implements,
                    deduced_substitution: implements_ids_by_deduced_substitution
                        .remove(&implements)
                        .unwrap(),
                })
            },
        )
    }

    #[must_use]
    #[allow(clippy::too_many_lines)]
    fn substitute_type(
        &self,
        ty: &mut ty::Type,
        substitution: &Substitution,
        generic_configuration: GenericConfiguration,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        match ty {
            ty::Type::Struct(struct_ty) => {
                if !self.apply_substitution_on_arguments(
                    &mut struct_ty.substitution,
                    substitution,
                    generic_configuration,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    return false;
                }

                if !self.check_where_clause(
                    struct_ty.struct_id.into(),
                    &struct_ty.substitution,
                    generic_identifier_span,
                    active_where_clause,
                    handler,
                ) {
                    return false;
                }

                true
            }
            ty::Type::Reference(reference_ty) => {
                if let Some(LifetimeArgument::Parameter(lt)) = reference_ty.lifetime_argument {
                    if let Some(substitution) = substitution
                        .lifetime_arguments_by_parameter
                        .get(&lt)
                        .copied()
                    {
                        reference_ty.lifetime_argument = Some(substitution);
                    }
                }

                self.substitute_type(
                    &mut reference_ty.operand,
                    substitution,
                    generic_configuration,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                )
            }
            ty::Type::Parameter(type_parameter) => {
                if let Some(substitution) =
                    substitution.type_arguments_by_parameter.get(type_parameter)
                {
                    *ty = substitution.clone();
                }

                true
            }
            ty::Type::TraitType(trait_type) => {
                if !self.apply_substitution_on_arguments(
                    &mut trait_type.trait_substitution,
                    substitution,
                    generic_configuration,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) || !self.apply_substitution_on_arguments(
                    &mut trait_type.trait_type_substitution,
                    substitution,
                    generic_configuration,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    return false;
                }

                if !self.check_where_clause(
                    self.trait_types[trait_type.trait_type_id]
                        .parent_trait_id
                        .into(),
                    &trait_type.trait_substitution,
                    generic_identifier_span,
                    active_where_clause,
                    handler,
                ) {
                    return false;
                }
                match (
                    trait_type.trait_substitution.is_concrete_substitution(),
                    generic_configuration,
                ) {
                    (true, GenericConfiguration::Default) => {
                        let Some(active_implements) = self.resolve_trait_implements(
                            self.trait_types[trait_type.trait_type_id].parent_trait_id,
                            &trait_type.trait_substitution,
                            generic_identifier_span,
                            active_where_clause,
                            handler,
                        ) else {
                            return false;
                        };

                        let Some(new_ty) = self.alias_type(
                            &self.implements_types[self.implements
                                [active_implements.implements_id]
                                .implements_types_by_trait_type[&trait_type.trait_type_id]]
                                .alias,
                            &Self::combine_substitution(
                                &trait_type.trait_substitution,
                                &Self::transform_trait_member_substitution_to_implements_substitution(
                                    trait_type.trait_type_substitution.clone(),
                                    &self.trait_types[trait_type.trait_type_id].generic_parameters,
                                    &self.implements_types[self.implements[active_implements.implements_id]
                                        .implements_types_by_trait_type[&trait_type.trait_type_id]]
                                        .generic_parameters,
                                )
                            ),
                            generic_configuration,
                            active_where_clause,
                            generic_identifier_span,
                            handler,
                        ) else {
                            return false;
                        };

                        *ty = new_ty;
                    }
                    (true, GenericConfiguration::TraitResolutionNotAllowed) => {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed {
                                trait_resolution_span: generic_identifier_span.clone(),
                            },
                        ));
                        return false;
                    }
                    (_, _) => {}
                };

                true
            }
            _ => true,
        }
    }

    fn alias_type(
        &self,
        alias: &ty::Type,
        substitution: &Substitution,
        generic_configuration: GenericConfiguration,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<ty::Type> {
        let mut cloned_ty = alias.clone();
        if !self.substitute_type(
            &mut cloned_ty,
            substitution,
            generic_configuration,
            active_where_clause,
            generic_identifier_span,
            handler,
        ) {
            return None;
        }
        Some(cloned_ty)
    }

    #[must_use]
    fn substitution_outlives(
        required_lifetime_argument: LifetimeArgument,
        substitution: &Substitution,
        active_where_clause: &WhereClause,
        ty: &ty::Type,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        for lifetime_argument in substitution.lifetime_arguments_by_parameter.values() {
            let LifetimeArgument::Parameter(lifetime_parameter) = lifetime_argument else {
                continue;
            };

            if let Some(lifetime_argument_set) = active_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .get(lifetime_parameter)
            {
                if lifetime_argument_set.contains(lifetime_argument) {
                    return true;
                }
            }

            handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                TypeDoesNotOutliveLifetimeArgument {
                    required_lifetime_argument,
                    ty: ty.clone(),
                    generics_identifier_span: generic_identifier_span.clone(),
                },
            ));
            return false;
        }

        true
    }

    fn ty_outlives(
        required_lifetime_argument: LifetimeArgument,
        ty: &ty::Type,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        if let Some(lifetime_argument_set) =
            active_where_clause.lifetime_argument_sets_by_type.get(ty)
        {
            if lifetime_argument_set.contains(&required_lifetime_argument) {
                return true;
            }
        }

        match ty {
            ty::Type::Primitive(_) | ty::Type::Enum(_) => true,

            ty::Type::Struct(struct_ty) => Self::substitution_outlives(
                required_lifetime_argument,
                &struct_ty.substitution,
                active_where_clause,
                ty,
                generic_identifier_span,
                handler,
            ),

            ty::Type::Reference(reference_ty) => {
                'error: {
                    if let Some(lifetime_argument) = reference_ty.lifetime_argument {
                        if let LifetimeArgument::Parameter(parameter) = lifetime_argument {
                            if let Some(lifetime_argument_set) = active_where_clause
                                .lifetime_argument_sets_by_lifetime_parameter
                                .get(&parameter)
                            {
                                if !lifetime_argument_set.contains(&lifetime_argument) {
                                    break 'error;
                                }
                            } else {
                                break 'error;
                            }
                        }
                    } else {
                        break 'error;
                    }

                    return Self::ty_outlives(
                        required_lifetime_argument,
                        &reference_ty.operand,
                        active_where_clause,
                        generic_identifier_span,
                        handler,
                    );
                }

                handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                    TypeDoesNotOutliveLifetimeArgument {
                        required_lifetime_argument,
                        ty: ty.clone(),
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                false
            }
            ty::Type::TraitType(_) | ty::Type::Parameter(_) => {
                handler.receive(error::Error::TypeDoesNotOutliveLifetimeArgument(
                    TypeDoesNotOutliveLifetimeArgument {
                        required_lifetime_argument,
                        ty: ty.clone(),
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                false
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub(super) fn check_where_clause(
        &self,
        genericable_id: GenericableID,
        substitution: &Substitution,
        generic_identifier_span: &Span,
        active_where_clause: &WhereClause,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        let Some(genericable_where_clause) =
            self.get_genericable(genericable_id).unwrap().where_clause()
        else {
            return true;
        };

        let Some(genericable_where_clause) = self.substitute_where_clause(
            genericable_where_clause,
            substitution,
            active_where_clause,
            generic_identifier_span,
            handler,
        ) else {
            return false;
        };

        let mut found_error = false;

        // perform lifetime bound checking
        for (required_lifetime_parameter, required_lifetime_argument_set) in
            genericable_where_clause.lifetime_argument_sets_by_lifetime_parameter
        {
            let Some(active_lifetime_argument_set) = active_where_clause
                .lifetime_argument_sets_by_lifetime_parameter
                .get(&required_lifetime_parameter)
            else {
                found_error = !required_lifetime_argument_set.is_empty();
                for bound_lifetime_argument in required_lifetime_argument_set {
                    handler.receive(error::Error::LifetimeDoesNotOutlive(
                        LifetimeDoesNotOutlive {
                            passed_lifetime_parameter: required_lifetime_parameter,
                            required_lifetime_argument: bound_lifetime_argument,
                            type_span: generic_identifier_span.clone(),
                        },
                    ));
                }
                continue;
            };

            for bound_lifetime_argument in required_lifetime_argument_set {
                if !active_lifetime_argument_set.contains(&bound_lifetime_argument) {
                    found_error = true;
                    handler.receive(error::Error::LifetimeDoesNotOutlive(
                        LifetimeDoesNotOutlive {
                            passed_lifetime_parameter: required_lifetime_parameter,
                            required_lifetime_argument: bound_lifetime_argument,
                            type_span: generic_identifier_span.clone(),
                        },
                    ));
                }
            }
        }

        // perform trait bound checking
        for required_trait_bound in genericable_where_clause.trait_bounds {
            if required_trait_bound.substitution.is_concrete_substitution() {
                continue;
            }

            if !active_where_clause
                .trait_bounds
                .contains(&required_trait_bound)
            {
                found_error = true;

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
            }
        }

        // perform trait type bound checking
        for (required_trait_type, required_ty) in genericable_where_clause.types_by_trait_type {
            let Some(active_ty) = active_where_clause
                .types_by_trait_type
                .get(&required_trait_type)
            else {
                found_error = true;
                handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                    TraitTypeBoundNotSatisfied {
                        required_type: required_ty,
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
                continue;
            };

            if active_ty != &required_ty {
                found_error = true;
                handler.receive(error::Error::TraitTypeBoundNotSatisfied(
                    TraitTypeBoundNotSatisfied {
                        required_type: required_ty,
                        generics_identifier_span: generic_identifier_span.clone(),
                    },
                ));
            }
        }

        // performs type lifetime bound checking
        for (ty, lifetime_argument_set) in genericable_where_clause.lifetime_argument_sets_by_type {
            for lifetime_argument in lifetime_argument_set {
                if !Self::ty_outlives(
                    lifetime_argument,
                    &ty,
                    active_where_clause,
                    generic_identifier_span,
                    handler,
                ) {
                    found_error = true;
                }
            }
        }

        !found_error
    }

    fn combine_substitution(lhs: &Substitution, rhs: &Substitution) -> Substitution {
        let mut substitution = Substitution::default();

        for (lifetime_parameter, lifetime_argument) in &lhs.lifetime_arguments_by_parameter {
            assert!(substitution
                .lifetime_arguments_by_parameter
                .insert(*lifetime_parameter, *lifetime_argument)
                .is_none());
        }

        for (lifetime_parameter, lifetime_argument) in &rhs.lifetime_arguments_by_parameter {
            assert!(substitution
                .lifetime_arguments_by_parameter
                .insert(*lifetime_parameter, *lifetime_argument)
                .is_none());
        }

        for (type_parameter, ty) in &lhs.type_arguments_by_parameter {
            assert!(substitution
                .type_arguments_by_parameter
                .insert(*type_parameter, ty.clone())
                .is_none());
        }

        for (type_parameter, ty) in &rhs.type_arguments_by_parameter {
            assert!(substitution
                .type_arguments_by_parameter
                .insert(*type_parameter, ty.clone())
                .is_none());
        }

        substitution
    }

    fn alias_type_as_resolution(
        &self,
        alias: &ty::Type,
        substitution: &Substitution,
        generic_configuration: GenericConfiguration,
        active_where_clause: &WhereClause,
        generic_identifier_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<Resolution> {
        Some(
            match self.alias_type(
                alias,
                substitution,
                generic_configuration,
                active_where_clause,
                generic_identifier_span,
                handler,
            )? {
                ty::Type::Enum(enum_id) => Resolution::Enum(enum_id),
                ty::Type::Struct(struct_ty) => Resolution::Struct(Generics {
                    symbol: struct_ty.struct_id,
                    substitution: struct_ty.substitution,
                }),
                ty::Type::Primitive(primtive) => Resolution::Primitive(primtive),
                ty::Type::Reference(reference) => Resolution::Reference(reference),
                ty::Type::Parameter(ty_parameter) => Resolution::TypeParameter(ty_parameter),
                ty::Type::TraitType(trait_ty) => Resolution::TraitType(UnresolvedTraitType {
                    trait_resolution: Generics {
                        substitution: trait_ty.trait_substitution,
                        symbol: self.trait_types[trait_ty.trait_type_id].parent_trait_id,
                    },
                    member_resolution: Generics {
                        symbol: trait_ty.trait_type_id,
                        substitution: trait_ty.trait_type_substitution,
                    },
                }),
            },
        )
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
            (1.., 0, false) => {
                // Do nothing
            }

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
        generic_configuration: GenericConfiguration,
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
            if generic_configuration == GenericConfiguration::Default {
                let substitution = parent_substitution.map(|parent_substitution| {
                    Self::combine_substitution(&resolved_substitution, parent_substitution)
                });

                if !self.check_where_clause(
                    genericable_id,
                    substitution
                        .as_ref()
                        .map_or(&resolved_substitution, |substitution| substitution),
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
            GlobalID::Type(id) => self.alias_type_as_resolution(
                &self.types[id].alias,
                &resolved_substitution,
                generic_configuration,
                &active_where_clause,
                generic_identifier_span,
                handler,
            )?,
            GlobalID::Trait(id) => {
                match (
                    resolved_substitution.is_concrete_substitution(),
                    generic_configuration,
                ) {
                    (true, GenericConfiguration::Default) => {
                        let implements = self.resolve_trait_implements(
                            id,
                            &resolved_substitution,
                            generic_identifier_span,
                            &active_where_clause,
                            handler,
                        )?;

                        Resolution::Trait(Trait {
                            trait_id: id,
                            substitution: resolved_substitution,
                            resolved_implements: Some(implements),
                        })
                    }
                    (true, GenericConfiguration::TraitResolutionNotAllowed) => {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed {
                                trait_resolution_span: generic_identifier_span.clone(),
                            },
                        ));
                        return None;
                    }
                    (false, _) | (_, GenericConfiguration::TraitResolutionIgnored) => {
                        let required_trait_bound = TraitBound {
                            trait_id: id,
                            substitution: resolved_substitution,
                        };

                        if !active_where_clause
                            .trait_bounds
                            .contains(&required_trait_bound)
                            && generic_configuration == GenericConfiguration::Default
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
                        let combined_substitution = Self::combine_substitution(
                            &Self::transform_trait_member_substitution_to_implements_substitution(
                                resolved_substitution,
                                &self.trait_types[id].generic_parameters,
                                &self.implements_types[self.implements[implements.implements_id]
                                    .implements_types_by_trait_type[&id]]
                                    .generic_parameters,
                            ),
                            &implements.deduced_substitution,
                        );
                        self.alias_type_as_resolution(
                            &self.implements_types[self.implements[implements.implements_id]
                                .implements_types_by_trait_type[&id]]
                                .alias,
                            &combined_substitution,
                            generic_configuration,
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
                    info.generic_configuration,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum ImplementsComparison {
    Incompatible,
    MoreSpecialized,
    LessSpecialized,
    Ambiguous,
}

// use lhs as base
pub(super) fn compare_implements(
    lhs_implements_substitution: &Substitution,
    rhs_implements_substitution: &Substitution,
) -> ImplementsComparison {
    assert_eq!(
        lhs_implements_substitution
            .type_arguments_by_parameter
            .len(),
        rhs_implements_substitution
            .type_arguments_by_parameter
            .len()
    );

    let lhs_to_rhs = get_mappings(lhs_implements_substitution, rhs_implements_substitution);
    let rhs_to_lhs = get_mappings(rhs_implements_substitution, lhs_implements_substitution);

    match (lhs_to_rhs, rhs_to_lhs) {
        (None, None) => ImplementsComparison::Incompatible,
        (None, Some(rhs_to_lhs)) => {
            assert!(!rhs_to_lhs.is_empty());
            ImplementsComparison::MoreSpecialized
        }
        (Some(lhs_to_rhs), None) => {
            assert!(!lhs_to_rhs.is_empty());
            ImplementsComparison::LessSpecialized
        }
        (Some(lhs_to_rhs), Some(rhs_to_lhs)) => match lhs_to_rhs.len().cmp(&rhs_to_lhs.len()) {
            Ordering::Less => ImplementsComparison::MoreSpecialized,
            Ordering::Equal => ImplementsComparison::Ambiguous,
            Ordering::Greater => ImplementsComparison::LessSpecialized,
        },
    }
}

// get mappings from lhs -> rhs
pub(super) fn get_mappings(
    lhs_implements_substitution: &Substitution,
    rhs_implements_substitution: &Substitution,
) -> Option<HashMap<ty::Type, ty::Type>> {
    let mut mappings = HashMap::new();

    for ty_parameter_key in lhs_implements_substitution
        .type_arguments_by_parameter
        .keys()
    {
        let lhs_ty = lhs_implements_substitution
            .type_arguments_by_parameter
            .get(ty_parameter_key)
            .unwrap();
        let rhs_ty = rhs_implements_substitution
            .type_arguments_by_parameter
            .get(ty_parameter_key)
            .unwrap();

        mappings = get_mappings_on_type(lhs_ty, rhs_ty, mappings)?;
    }

    Some(mappings)
}

// maps from lhs_type -> rhs_type
pub(super) fn get_mappings_on_type(
    lhs_type: &ty::Type,
    rhs_ty: &ty::Type,
    mut previous_mappings: HashMap<ty::Type, ty::Type>,
) -> Option<HashMap<ty::Type, ty::Type>> {
    match (lhs_type, rhs_ty) {
        (ty::Type::Parameter(lhs_ty_parameter), rhs_ty) => {
            if let Some(previous_ty) =
                previous_mappings.insert(ty::Type::Parameter(*lhs_ty_parameter), rhs_ty.clone())
            {
                if previous_ty != *rhs_ty {
                    return None;
                }
            }
        }
        (ty::Type::TraitType(lhs_ty_parameter), rhs_ty) => {
            if let Some(previous_ty) = previous_mappings.insert(
                ty::Type::TraitType(lhs_ty_parameter.clone()),
                rhs_ty.clone(),
            ) {
                if previous_ty != *rhs_ty {
                    return None;
                }
            }
        }
        (ty::Type::Reference(lhs_ty_reference), ty::Type::Reference(rhs_ty_reference)) => {
            if lhs_ty_reference.qualifier != rhs_ty_reference.qualifier {
                return None;
            }

            previous_mappings = get_mappings_on_type(
                &lhs_ty_reference.operand,
                &rhs_ty_reference.operand,
                previous_mappings,
            )?;
        }
        (ty::Type::Struct(lhs_ty_struct), ty::Type::Struct(rhs_ty_struct)) => {
            if lhs_ty_struct.struct_id != rhs_ty_struct.struct_id {
                return None;
            }

            assert_eq!(
                lhs_ty_struct.substitution.type_arguments_by_parameter.len(),
                rhs_ty_struct.substitution.type_arguments_by_parameter.len()
            );

            for ty_parameter_key in lhs_ty_struct
                .substitution
                .type_arguments_by_parameter
                .keys()
            {
                let lhs_ty =
                    &lhs_ty_struct.substitution.type_arguments_by_parameter[ty_parameter_key];
                let rhs_ty =
                    &rhs_ty_struct.substitution.type_arguments_by_parameter[ty_parameter_key];

                previous_mappings = get_mappings_on_type(lhs_ty, rhs_ty, previous_mappings)?;
            }
        }
        (lhs_ty, rhs_ty) => {
            if lhs_ty != rhs_ty {
                return None;
            }
        }
    }

    Some(previous_mappings)
}
