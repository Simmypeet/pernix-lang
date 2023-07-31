use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use itertools::CombinationsWithReplacement;
use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, item::TypeBoundConstraint, GenericArgument, GenericIdentifier, PrimitiveTypeSpecifier,
    QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{
    drafting::States,
    resolution::{self, Resolution},
    Table,
};
use crate::{
    error::{
        self, AmbiguousImplements, LifetimeArgumentCountMismatch,
        LifetimeArgumentMustBeSuppliedPriorToTypeArgument, LifetimeArgumentsRequired,
        NoGenericArgumentsRequired, TraitExpected, TraitResolutionNotAllowed,
        TraitTypeBoundHasAlreadyBeenSpecified, TypeArgumentCountMismatch, TypeExpected,
    },
    table,
    ty::{self, Primitive, Reference},
    GenericableID, Generics, GlobalID, Implements, Module, Substitution, Trait, TraitFunction,
    TraitMemberID, TraitType, TypeParameter, WhereClause, ID,
};

impl Table {
    #[must_use]
    fn resolve_with_finalization(
        &mut self,
        info: &resolution::Info,
        qualified_identifier: &QualifiedIdentifier,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Option<Resolution> {
        let mut current_root = resolution::Root {
            referring_site: info.referring_site,
            latest_resolution: None,
        };

        for generics_identifier in std::iter::once(qualified_identifier.first())
            .chain(qualified_identifier.rest().iter().map(|x| &x.1))
        {
            let global_id = self.resolve_single_first_pass(
                generics_identifier.identifier(),
                &current_root,
                qualified_identifier.leading_scope_separator().is_some(),
                handler,
            )?;

            self.finalize_symbol_if_drafted(global_id.into(), states, handler);

            let substitution = self.resolve_substitution_with_finalization(
                generics_identifier,
                global_id,
                info,
                states,
                handler,
            )?;

            current_root.latest_resolution = Some(self.resolve_single_second_pass(
                &generics_identifier.span(),
                &current_root,
                global_id,
                substitution,
                info.check_where_clause,
                handler,
            )?);
        }

        Some(current_root.latest_resolution.unwrap())
    }

    fn resolve_qualified_identifier_type_with_finalization(
        &mut self,
        info: &resolution::Info,
        qualified_identifier: &QualifiedIdentifier,
        states: &mut States,
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

        let resolution =
            self.resolve_with_finalization(info, qualified_identifier, states, handler)?;

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

    fn resolve_type_with_finalization(
        &mut self,
        info: &resolution::Info,
        type_specifier: &TypeSpecifier,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Option<ty::Type> {
        match type_specifier {
            TypeSpecifier::Primitive(primitive_type) => {
                Some(ty::Type::Primitive(match primitive_type {
                    PrimitiveTypeSpecifier::Bool(_) => Primitive::Bool,
                    PrimitiveTypeSpecifier::Void(_) => Primitive::Void,
                    PrimitiveTypeSpecifier::Float32(_) => Primitive::Float32,
                    PrimitiveTypeSpecifier::Float64(_) => Primitive::Float64,
                    PrimitiveTypeSpecifier::Int8(_) => Primitive::Int8,
                    PrimitiveTypeSpecifier::Int16(_) => Primitive::Int16,
                    PrimitiveTypeSpecifier::Int32(_) => Primitive::Int32,
                    PrimitiveTypeSpecifier::Int64(_) => Primitive::Int64,
                    PrimitiveTypeSpecifier::Uint8(_) => Primitive::Uint8,
                    PrimitiveTypeSpecifier::Uint16(_) => Primitive::Uint16,
                    PrimitiveTypeSpecifier::Uint32(_) => Primitive::Uint32,
                    PrimitiveTypeSpecifier::Uint64(_) => Primitive::Uint64,
                }))
            }
            TypeSpecifier::QualifiedIdentifier(qualified_identifier) => self
                .resolve_qualified_identifier_type_with_finalization(
                    info,
                    qualified_identifier,
                    states,
                    handler,
                ),
            TypeSpecifier::Reference(reference_type) => Some(ty::Type::Reference(Reference {
                operand: Box::new(self.resolve_type_with_finalization(
                    info,
                    reference_type.operand_type(),
                    states,
                    handler,
                )?),
                qualifier: reference_type.qualifier().as_ref().map(|x| match x {
                    syntax_tree::ReferenceQualifier::Mutable(_) => ty::ReferenceQualifier::Mutable,
                    syntax_tree::ReferenceQualifier::Restrict(_) => {
                        ty::ReferenceQualifier::Restrict
                    }
                }),
                lifetime_argument: match reference_type.lifetime_argument() {
                    Some(x) => Some(
                        match self.resolve_lifetime_argument(info.referring_site, x, handler) {
                            Ok(x) => x,
                            Err(table::Error::FatalSemantic) => return None,
                            Err(table::Error::InvalidID) => unreachable!(),
                        },
                    ),
                    None => {
                        if info.explicit_lifetime_required {
                            handler.receive(error::Error::LifetimeArgumentsRequired(
                                LifetimeArgumentsRequired {
                                    span: reference_type.span(),
                                },
                            ));
                            return None;
                        }

                        None
                    }
                },
            })),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_substitution_with_finalization(
        &mut self,
        generic_identifier: &GenericIdentifier,
        found_global_id: GlobalID,
        info: &resolution::Info,
        states: &mut States,
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
                let ty =
                    self.resolve_type_with_finalization(info, type_specifier, states, handler)?;

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
}

impl Table {
    fn handle_where_clause_trait_bound(
        &mut self,
        trait_bound: &syntax_tree::item::TraitBound,
        genericable_id: GenericableID,
        where_clause: &mut WhereClause,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        // expect the trait
        let Some(resolution) = self.resolve_with_finalization(
            &resolution::Info {
                referring_site: genericable_id.into(),
                explicit_lifetime_required: true,
                check_where_clause: false,
            },
            trait_bound.qualified_identifier(),
            states,
            handler,
        ) else {
            return;
        };

        // expect the trait
        let Resolution::Trait(trait_resolution) = resolution else {
            handler.receive(error::Error::TraitExpected(TraitExpected {
                symbol_reference_span: trait_bound.span(),
            }));
            return;
        };

        // fully concrete substitution is not allowed
        if trait_resolution.substitution.is_concrete_substitution() {
            handler.receive(error::Error::TraitResolutionNotAllowed(
                TraitResolutionNotAllowed {
                    trait_resolution_span: trait_bound.span(),
                },
            ));
            return;
        }

        if !where_clause.trait_bounds.insert(crate::TraitBound {
            trait_id: trait_resolution.symbol,
            substitution: trait_resolution.substitution,
        }) {
            // TODO: report some warnings
        }
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
        let Some(ty) = self.resolve_type_with_finalization(
            &resolution::Info {
                referring_site: genericable_id.into(),
                explicit_lifetime_required: true,
                check_where_clause: false,
            },
            type_bound.type_specifier(),
            states,
            handler,
        ) else {
            return;
        };

        for type_constraint in type_bound.type_bound_constraints().elements() {
            match type_constraint {
                TypeBoundConstraint::TypeSpecifier(type_bound_specifier) => {
                    let Some(type_bound) = self.resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: genericable_id.into(),
                            explicit_lifetime_required: true,
                            check_where_clause: false,
                        },
                        type_bound_specifier,
                        states,
                        handler,
                    ) else {
                        continue;
                    };

                    // The type must not be fully resolved
                    let ty::Type::TraitType(trait_type) = &ty else {
                        handler.receive(error::Error::TraitResolutionNotAllowed(
                            TraitResolutionNotAllowed {
                                trait_resolution_span: type_bound_specifier.span(),
                            },
                        ));
                        continue;
                    };

                    if parent_where_clause
                        .types_by_trait_type
                        .contains_key(trait_type)
                        || where_clause.types_by_trait_type.contains_key(trait_type)
                    {
                        handler.receive(error::Error::TraitTypeBoundHasAlreadyBeenSpecified(
                            TraitTypeBoundHasAlreadyBeenSpecified {
                                trait_type_bound_span: type_constraint.span(),
                            },
                        ));
                        continue;
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
            lifetime_bound_syntax_tree.operand().identifier(),
            handler,
        ) else {
            return;
        };

        let lifetime_argument_set = where_clause
            .lifetime_argument_sets_by_lifetime_parameter
            .entry(operand_lifetime_parameter)
            .or_default();

        for lifetime_argument_syntax_tree in lifetime_bound_syntax_tree.arguments().elements() {
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

    #[allow(clippy::too_many_lines)]
    fn construct_where_clause(
        &mut self,
        where_clause_syntax_tree: &syntax_tree::item::WhereClause,
        genericable_id: GenericableID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Option<WhereClause> {
        let mut where_clause = WhereClause::default();
        let mut parent_active_where_clause = self
            .get_active_where_clause(
                self.get_genericable(genericable_id)
                    .unwrap()
                    .parent_symbol()
                    .expect("Genericable should have a parent symbol"),
            )
            .unwrap();

        for constraint in where_clause_syntax_tree.constraint_list().elements() {
            match constraint {
                syntax_tree::item::Constraint::TraitBound(trait_bound) => self
                    .handle_where_clause_trait_bound(
                        trait_bound,
                        genericable_id,
                        &mut where_clause,
                        states,
                        handler,
                    ),
                syntax_tree::item::Constraint::LifetimeBound(lifetime_bound_syntax_tree) => self
                    .handle_where_clause_lifetime_bound(
                        lifetime_bound_syntax_tree,
                        genericable_id,
                        &mut where_clause,
                        handler,
                    ),
                syntax_tree::item::Constraint::TypeBound(type_bound) => {
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

        Self::merge_where_caluse(&mut parent_active_where_clause, &where_clause);
        let mut error_found = false;

        for ty in where_clause.lifetime_argument_sets_by_type.keys() {
            let removed_lifetime_argument_set = parent_active_where_clause
                .lifetime_argument_sets_by_type
                .remove(ty)
                .unwrap();

            if !self.check_type_where_clause(
                ty,
                &where_clause_syntax_tree.span(),
                &parent_active_where_clause,
                handler,
            ) {
                error_found = true;
            }

            parent_active_where_clause
                .lifetime_argument_sets_by_type
                .insert(ty.clone(), removed_lifetime_argument_set);
        }

        for (trait_type, ty) in &where_clause.types_by_trait_type {
            let removed_type_bound = parent_active_where_clause
                .types_by_trait_type
                .remove(trait_type)
                .unwrap();

            if !self.check_where_clause(
                self.trait_types[trait_type.trait_type_id]
                    .parent_trait_id
                    .into(),
                &trait_type.trait_substitution,
                &where_clause_syntax_tree.span(),
                &parent_active_where_clause,
                handler,
            ) {
                error_found = true;
            }

            if !self.check_type_where_clause(
                ty,
                &where_clause_syntax_tree.span(),
                &parent_active_where_clause,
                handler,
            ) {
                error_found = true;
            }

            parent_active_where_clause
                .types_by_trait_type
                .insert(trait_type.clone(), removed_type_bound);
        }

        for trait_bound in &where_clause.trait_bounds {
            assert!(parent_active_where_clause.trait_bounds.remove(trait_bound));

            if !self.check_where_clause(
                trait_bound.trait_id.into(),
                &trait_bound.substitution,
                &where_clause_syntax_tree.span(),
                &parent_active_where_clause,
                handler,
            ) {
                error_found = true;
            }

            parent_active_where_clause
                .trait_bounds
                .insert(trait_bound.clone());
        }

        if error_found {
            None
        } else {
            Some(where_clause)
        }
    }

    fn check_type_where_clause(
        &self,
        ty: &ty::Type,
        generics_identifier_span: &Span,
        active_where_clause: &WhereClause,
        handler: &impl Handler<error::Error>,
    ) -> bool {
        match ty {
            ty::Type::Enum(_) | ty::Type::Primitive(_) | ty::Type::Parameter(_) => true,

            ty::Type::Struct(struct_ty) => self.check_where_clause(
                struct_ty.struct_id.into(),
                &struct_ty.substitution,
                generics_identifier_span,
                active_where_clause,
                handler,
            ),
            ty::Type::Reference(reference_ty) => self.check_type_where_clause(
                &reference_ty.operand,
                generics_identifier_span,
                active_where_clause,
                handler,
            ),
            ty::Type::TraitType(trait_ty) => self.check_where_clause(
                self.trait_types[trait_ty.trait_type_id]
                    .parent_trait_id
                    .into(),
                &trait_ty.trait_substitution,
                generics_identifier_span,
                active_where_clause,
                handler,
            ),
        }
    }
}

impl Table {
    pub(super) fn finalize_symbol(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(id).is_some(),
            "symbol is not drafted or being constructed"
        );

        match id {
            ID::Module(_) => todo!(),
            ID::Struct(_) => todo!(),
            ID::Enum(_) => todo!(),
            ID::EnumVariant(_) => todo!(),
            ID::Function(_) => todo!(),
            ID::Type(_) => todo!(),
            ID::Field(_) => todo!(),
            ID::FunctionParameter(_) => todo!(),
            ID::TraitFunctionParameter(_) => todo!(),
            ID::ImplementsFunctionParameter(_) => todo!(),
            ID::Trait(id) => self.finalize_trait(id, states, handler),
            ID::TraitType(_) => todo!(),
            ID::TypeParameter(_) => todo!(),
            ID::LifetimeParameter(_) => todo!(),
            ID::TraitFunction(_) => todo!(),
            ID::Implements(_) => todo!(),
            ID::ImplementsFunction(_) => todo!(),
            ID::ImplementsType(_) => todo!(),
        }
    }

    fn finalize_symbol_if_drafted(
        &mut self,
        id: ID,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        if states.get_current_state(id).is_some() {
            self.finalize_symbol(id, states, handler);
        }
    }

    fn finalize_trait_function(
        &mut self,
        trait_function_id: arena::ID<TraitFunction>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
    }

    fn finalize_trait_type(
        &mut self,
        trait_type_id: arena::ID<TraitType>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
    }

    fn finalize_trait(
        &mut self,
        trait_id: arena::ID<Trait>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(trait_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(trait_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        for trait_member_id in self.traits[trait_id]
            .trait_member_ids_by_name
            .values()
            .copied()
        {
            states.add_drafted_symbol(trait_member_id.into());
        }

        if let Some(where_clause) = self.traits[trait_id]
            .syntax_tree
            .as_ref()
            .map(|x| x.where_clause().as_ref().cloned())
            .expect("syntax tree should exist")
        {
            if let Some(where_clause) =
                self.construct_where_clause(&where_clause, trait_id.into(), states, handler)
            {
                self.traits[trait_id].generics.where_clause = where_clause;
            }
        }

        let implements_syntax_tree_with_module_ids = states
            .remvoe_implements_syntax_tree_with_module_ids_by_trait_id(trait_id)
            .expect("should've exists");

        for implements_syntax_tree_with_module_id in implements_syntax_tree_with_module_ids {
            let signature_span = implements_syntax_tree_with_module_id
                .implements
                .signature()
                .span();

            let Some(new_implements) = self.construct_implements(
                trait_id,
                implements_syntax_tree_with_module_id.module_id,
                implements_syntax_tree_with_module_id.implements,
                states,
                handler,
            ) else {
                continue;
            };

            for existing_implements in self.traits[trait_id].implements.iter().copied() {
                if compare_implements(
                    &self.implements[existing_implements].substitution,
                    &self.implements[new_implements].substitution,
                ) == ImplementsComparison::Ambiguous
                {
                    handler.receive(error::Error::AmbiguousImplements(AmbiguousImplements {
                        existing_implements,
                        new_implements_span: signature_span,
                    }));
                    self.remove_implements(new_implements);
                    break;
                }
            }
        }

        states.remove_constructing_symbol(trait_id.into());

        #[allow(clippy::needless_collect)]
        for trait_member_id in self.traits[trait_id]
            .trait_member_ids_by_name
            .values()
            .copied()
            .collect::<Vec<_>>()
        {
            match trait_member_id {
                TraitMemberID::TraitFunction(trait_function_id) => {
                    self.finalize_trait_function(trait_function_id, states, handler);
                }
                TraitMemberID::TraitType(trait_type_id) => {
                    self.finalize_trait_type(trait_type_id, states, handler);
                }
            }
        }
    }

    fn remove_implements(&mut self, implements_id: arena::ID<Implements>) {
        // remove type parameters
        for ty_parameter in self.implements[implements_id]
            .generics
            .parameters
            .type_parameter_order
            .iter()
            .copied()
        {
            assert!(self.type_parameters.remove(ty_parameter).is_some());
        }
        // remove lifetime parameters
        for lifetime_parameter in self.implements[implements_id]
            .generics
            .parameters
            .lifetime_parameter_order
            .iter()
            .copied()
        {
            assert!(self
                .lifetime_parameters
                .remove(lifetime_parameter)
                .is_some());
        }

        // remove implements type
        for implements_ty in self.implements[implements_id]
            .implements_types_by_trait_type
            .values()
            .copied()
        {
            assert!(self.implements_types.remove(implements_ty).is_some());
        }
        // remove implements Function
        for implements_function in self.implements[implements_id]
            .implements_functions_by_trait_function
            .values()
            .copied()
        {
            // remove function parameters
            for function_parameter in self.implements_functions[implements_function]
                .function_signature
                .parameter_order
                .iter()
                .copied()
            {
                assert!(self
                    .implements_function_parameters
                    .remove(function_parameter)
                    .is_some());
            }

            assert!(self
                .implements_functions
                .remove(implements_function)
                .is_some());
        }

        // remove the implements from the trait
        {
            let parent_trait = &mut self.traits[self.implements[implements_id].trait_id];
            let remove_position = parent_trait
                .implements
                .iter()
                .position(|x| *x == implements_id)
                .unwrap();
            parent_trait.implements.remove(remove_position);
        }

        assert!(self.implements.remove(implements_id).is_some());
    }

    fn type_parameter_exists(ty_parameter: arena::ID<TypeParameter>, ty: &ty::Type) -> bool {
        match ty {
            ty::Type::Enum(_) | ty::Type::Primitive(_) | ty::Type::TraitType(_) => false,
            ty::Type::Struct(struct_ty) => {
                for ty_substitution in struct_ty.substitution.type_arguments_by_parameter.values() {
                    if Self::type_parameter_exists(ty_parameter, ty_substitution) {
                        return true;
                    }
                }
                false
            }
            ty::Type::Reference(reference_ty) => {
                Self::type_parameter_exists(ty_parameter, &reference_ty.operand)
            }
            ty::Type::Parameter(ty) => *ty != ty_parameter,
        }
    }

    #[allow(clippy::too_many_lines)]
    fn construct_implements(
        &mut self,
        parent_trait_id: arena::ID<Trait>,
        module_id: arena::ID<Module>,
        implements_syntax_tree: syntax_tree::item::Implements,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Option<arena::ID<Implements>> {
        let (implements_signature_syntax_tree, implements_body_syntax_tree) =
            implements_syntax_tree.dissolve();

        let implements_id = self.implements.push(Implements {
            generics: Generics::default(),
            trait_id: parent_trait_id,
            name: self.traits[parent_trait_id].name.clone(),
            substitution: Substitution::default(),
            parent_module_id: module_id,
            syntax_tree: None,
            implements_types_by_trait_type: HashMap::new(),
            implements_functions_by_trait_function: HashMap::new(),
        });

        if let Some(generic_parameters) = implements_signature_syntax_tree.generic_parameters() {
            self.implements[implements_id].generics.parameters =
                self.create_generic_parameters(implements_id.into(), generic_parameters, handler);
        }

        if let Some(where_clause) = implements_signature_syntax_tree.where_clause() {
            if let Some(where_clause) =
                self.construct_where_clause(where_clause, implements_id.into(), states, handler)
            {
                self.implements[implements_id].generics.where_clause = where_clause;
            } else {
                self.remove_implements(implements_id);
                return None;
            }
        }

        let generic_identifier = implements_signature_syntax_tree
            .qualified_identifier()
            .rest()
            .last()
            .map_or_else(
                || {
                    implements_signature_syntax_tree
                        .qualified_identifier()
                        .first()
                },
                |x| &x.1,
            );

        let Some(substitution) = self.resolve_substitution_with_finalization(
            generic_identifier,
            parent_trait_id.into(),
            &resolution::Info {
                referring_site: implements_id.into(),
                check_where_clause: true,
                explicit_lifetime_required: true,
            },
            states,
            handler,
        ) else {
            self.remove_implements(implements_id);
            return None;
        };

        if !self.check_where_clause(
            parent_trait_id.into(),
            &substitution,
            &generic_identifier.span(),
            &self.get_active_where_clause(implements_id.into()).unwrap(),
            handler,
        ) {
            self.remove_implements(implements_id);
            return None;
        }

        self.implements[implements_id].substitution = substitution;

        // check if all type parameters are used
        {
            let mut type_parameters = self.implements[implements_id]
                .generics
                .parameters
                .type_parameter_order
                .iter()
                .copied()
                .collect::<HashSet<_>>();

            for type_parameter in self.implements[implements_id]
                .generics
                .parameters
                .type_parameter_order
                .iter()
                .copied()
            {
                for type_argument in self.implements[implements_id]
                    .substitution
                    .type_arguments_by_parameter
                    .values()
                {
                    if Self::type_parameter_exists(type_parameter, type_argument) {
                        type_parameters.remove(&type_parameter);
                    }
                }
            }

            if !type_parameters.is_empty() {}
        }

        let implements_target_root_module_id = self
            .get_parent_target_root_module_id(module_id.into())
            .unwrap();

        if self
            .get_parent_target_root_module_id(parent_trait_id.into())
            .unwrap()
            != implements_target_root_module_id
            && !self.implements[implements_id]
                .substitution
                .type_arguments_by_parameter
                .values()
                .any(|ty| self.is_ty_from_target_root_module(ty, implements_target_root_module_id))
        {
            todo!("report error")
        }

        for implements_member in implements_body_syntax_tree.members() {}

        Some(implements_id)
    }

    fn is_ty_from_target_root_module(
        &self,
        ty: &ty::Type,
        target_root_module_id: arena::ID<Module>,
    ) -> bool {
        match ty {
            ty::Type::Enum(enum_id) => {
                self.get_parent_target_root_module_id((*enum_id).into())
                    .unwrap()
                    == target_root_module_id
            }
            ty::Type::Struct(struct_ty) => {
                if self.get_parent_target_root_module_id(struct_ty.struct_id.into())
                    == Some(target_root_module_id)
                {
                    return true;
                }

                for ty_argument in struct_ty.substitution.type_arguments_by_parameter.values() {
                    if self.is_ty_from_target_root_module(ty_argument, target_root_module_id) {
                        return true;
                    }
                }

                false
            }
            ty::Type::Parameter(_) | ty::Type::Primitive(_) => false,
            ty::Type::Reference(reference_ty) => {
                self.is_ty_from_target_root_module(&reference_ty.operand, target_root_module_id)
            }
            ty::Type::TraitType(trait_type_ty) => {
                if self
                    .get_parent_target_root_module_id(trait_type_ty.trait_type_id.into())
                    .unwrap()
                    == target_root_module_id
                {
                    return true;
                }

                for ty_argument in trait_type_ty
                    .trait_substitution
                    .type_arguments_by_parameter
                    .values()
                    .chain(
                        trait_type_ty
                            .trait_type_substitution
                            .type_arguments_by_parameter
                            .values(),
                    )
                {
                    if self.is_ty_from_target_root_module(ty_argument, target_root_module_id) {
                        return true;
                    }
                }

                false
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum ImplementsComparison {
    Incompatible,
    MoreSpecialized,
    LessSpecialized,
    Ambiguous,
}

// use lhs as base
fn compare_implements(
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
fn get_mappings(
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
fn get_mappings_on_type(
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
