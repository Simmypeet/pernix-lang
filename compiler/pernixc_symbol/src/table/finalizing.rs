use std::collections::{HashMap, HashSet};

use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, item::TypeBoundConstraint, ConnectedList, GenericArgument, GenericIdentifier,
    PrimitiveTypeSpecifier, QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{
    drafting::States,
    resolution::{self, Resolution},
    Table,
};
use crate::{
    error::{
        self, AmbiguousImplements, ImplementsFunctionParameterCountMismatch,
        ImplementsFunctionParameterTypeMismatch, ImplementsFunctionReturnTypeMismatch,
        ImplementsLifetimeParameterCountMismatch, ImplementsTypeParameterCountMismatch,
        LifetimeArgumentCountMismatch, LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
        LifetimeArgumentsRequired, NoGenericArgumentsRequired, PrivateSymbolLeakage,
        SymbolRedefinition, TraitExpected, TraitMemberKind, TraitMemberKindMismatch,
        TraitResolutionNotAllowed, TraitTypeBoundHasAlreadyBeenSpecified,
        TypeArgumentCountMismatch, TypeExpected, UnknownTraitMemberInImplements,
    },
    table::{
        self,
        resolution::{implements, BoundChecking},
    },
    ty::{self, Primitive, Reference},
    Accessibility, FunctionSignatureSyntaxTree, GenericParameters, GenericableID, Generics,
    GlobalID, Implements, ImplementsFunction, ImplementsMemberID, ImplementsType, LifetimeArgument,
    LifetimeParameter, Module, Parameter, Substitution, Symbol, Trait, TraitFunction,
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
                info.bound_checking,
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
                bound_checking: None,
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
            trait_id: trait_resolution.trait_id,
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
                bound_checking: None,
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
                            bound_checking: None,
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
                BoundChecking::Default,
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
                BoundChecking::Default,
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
                BoundChecking::Default,
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
                BoundChecking::Default,
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
            ID::Module(_)
            | ID::Enum(_)
            | ID::EnumVariant(_)
            | ID::Field(_)
            | ID::FunctionParameter(_)
            | ID::TraitFunctionParameter(_)
            | ID::ImplementsFunctionParameter(_)
            | ID::TraitType(_)
            | ID::TypeParameter(_)
            | ID::LifetimeParameter(_)
            | ID::TraitFunction(_)
            | ID::Implements(_)
            | ID::ImplementsFunction(_)
            | ID::ImplementsType(_) => {
                unreachable!("{id:#?}");
            }
            ID::Struct(id) => self.finalize_struct(id, states, handler),
            ID::Function(id) => self.finalize_function(id, states, handler),
            ID::Type(id) => self.finalize_type(id, states, handler),
            ID::Trait(id) => self.finalize_trait(id, states, handler),
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

    fn finalize_struct(
        &mut self,
        struct_id: arena::ID<crate::Struct>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(struct_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(struct_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        if let Some(where_clause) = self.structs[struct_id]
            .syntax_tree
            .as_ref()
            .map(|x| x.where_clause().as_ref().cloned())
            .expect("syntax tree should exist")
        {
            if let Some(where_clause) =
                self.construct_where_clause(&where_clause, struct_id.into(), states, handler)
            {
                self.structs[struct_id].generics.where_clause = where_clause;
            }
        }

        states.remove_constructing_symbol(struct_id.into());

        for field_id in self.structs[struct_id].field_order.clone() {
            let ty = self
                .resolve_type_with_finalization(
                    &resolution::Info {
                        referring_site: field_id.into(),
                        bound_checking: Some(BoundChecking::Default),
                        explicit_lifetime_required: true,
                    },
                    &self.fields[field_id]
                        .syntax_tree
                        .as_ref()
                        .unwrap()
                        .type_annotation()
                        .type_specifier()
                        .clone(),
                    states,
                    handler,
                )
                .unwrap_or(ty::Type::Primitive(Primitive::Void));

            let ty_accessibility = self.get_overall_ty_accessibility(&ty);

            if ty_accessibility.rank() < self.fields[field_id].accessibility.rank() {
                handler.receive(error::Error::PrivateSymbolLeakage(
                    error::PrivateSymbolLeakage {
                        span: self.fields[field_id]
                            .syntax_tree
                            .as_ref()
                            .unwrap()
                            .type_annotation()
                            .type_specifier()
                            .span(),
                    },
                ));
            }

            self.fields[field_id].ty = ty;
        }
    }

    fn finalize_function(
        &mut self,
        function_id: arena::ID<crate::Function>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(function_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(function_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        // finalize where clause
        if let Some(where_clause) = self.functions[function_id]
            .signature
            .syntax_tree
            .as_ref()
            .map(|x| x.where_clause.clone())
            .unwrap()
        {
            if let Some(where_clause) =
                self.construct_where_clause(&where_clause, function_id.into(), states, handler)
            {
                self.functions[function_id].signature.generics.where_clause = where_clause;
            }
        }

        // finalize return ty
        {
            let return_ty = self.functions[function_id]
                .signature
                .syntax_tree
                .as_ref()
                .map(|x| x.return_type.clone())
                .unwrap();

            let return_ty = return_ty.map_or_else(
                || ty::Type::Primitive(Primitive::Void),
                |return_ty| {
                    self.resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: function_id.into(),
                            bound_checking: Some(BoundChecking::Default),
                            explicit_lifetime_required: true,
                        },
                        return_ty.type_annotation().type_specifier(),
                        states,
                        handler,
                    )
                    .unwrap_or(ty::Type::Primitive(Primitive::Void))
                },
            );

            let return_ty_accessibility = self.get_overall_ty_accessibility(&return_ty);

            if return_ty_accessibility.rank() < self.functions[function_id].accessibility.rank() {
                handler.receive(error::Error::PrivateSymbolLeakage(PrivateSymbolLeakage {
                    span: self.functions[function_id]
                        .signature
                        .syntax_tree
                        .as_ref()
                        .map(|x| {
                            x.return_type
                                .as_ref()
                                .map(pernixc_source::SourceElement::span)
                                .unwrap()
                        })
                        .unwrap(),
                }));
            }

            self.functions[function_id].signature.return_type = return_ty;
        }

        // finalize parameters
        for parameter_id in self.functions[function_id].parameter_order.clone() {
            let type_specifier = self.function_parameters[parameter_id]
                .syntax_tree
                .as_ref()
                .map(|x| x.type_annotation().type_specifier().clone())
                .unwrap();

            let Some(ty) = self.resolve_type_with_finalization(
                &resolution::Info {
                    referring_site: parameter_id.into(),
                    bound_checking: Some(BoundChecking::Default),
                    explicit_lifetime_required: false,
                },
                &type_specifier,
                states,
                handler,
            ) else {
                continue;
            };

            let ty_accessibility = self.get_overall_ty_accessibility(&ty);

            if ty_accessibility.rank() < self.functions[function_id].accessibility.rank() {
                handler.receive(error::Error::PrivateSymbolLeakage(PrivateSymbolLeakage {
                    span: self.function_parameters[parameter_id]
                        .syntax_tree
                        .as_ref()
                        .map(|x| x.type_annotation().span())
                        .unwrap(),
                }));
            }

            self.function_parameters[parameter_id].ty = ty;
        }

        states.remove_constructing_symbol(function_id.into());
    }

    fn finalize_type(
        &mut self,
        type_id: arena::ID<crate::Type>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(type_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(type_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        let type_specifier = self.types[type_id]
            .syntax_tree
            .as_ref()
            .unwrap()
            .definition()
            .type_specifier()
            .clone();

        let type_alias = self
            .resolve_type_with_finalization(
                &resolution::Info {
                    referring_site: type_id.into(),
                    bound_checking: None,
                    explicit_lifetime_required: true,
                },
                &type_specifier,
                states,
                handler,
            )
            .unwrap_or(ty::Type::Primitive(Primitive::Void));

        let ty_accessibility = self.get_overall_ty_accessibility(&type_alias);

        if ty_accessibility.rank() < self.get_accessibility(type_id.into()).unwrap().rank() {
            handler.receive(error::Error::PrivateSymbolLeakage(
                error::PrivateSymbolLeakage {
                    span: type_specifier.span(),
                },
            ));
        }

        self.types[type_id].alias = type_alias;

        states.remove_constructing_symbol(type_id.into());
    }

    #[allow(clippy::too_many_lines)]
    fn finalize_trait_function(
        &mut self,
        trait_function_id: arena::ID<TraitFunction>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(trait_function_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(trait_function_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        let trait_function_accessibility =
            self.get_accessibility(trait_function_id.into()).unwrap();

        // finalize where clause
        {
            if let Some(where_clause) = self.trait_functions[trait_function_id]
                .function_signature
                .syntax_tree
                .as_ref()
                .map(|x| x.where_clause.clone())
                .unwrap()
            {
                if let Some(where_clause) = self.construct_where_clause(
                    &where_clause,
                    trait_function_id.into(),
                    states,
                    handler,
                ) {
                    self.trait_functions[trait_function_id]
                        .function_signature
                        .generics
                        .where_clause = where_clause;
                }
            }
        }
        // finalize return type
        {
            if let Some(return_ty) = self.trait_functions[trait_function_id]
                .function_signature
                .syntax_tree
                .as_ref()
                .map(|x| {
                    x.return_type
                        .as_ref()
                        .map(|x| x.type_annotation().type_specifier().clone())
                })
                .unwrap()
            {
                let return_ty = self
                    .resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: trait_function_id.into(),
                            bound_checking: Some(BoundChecking::Default),
                            explicit_lifetime_required: true,
                        },
                        &return_ty,
                        states,
                        handler,
                    )
                    .unwrap_or(ty::Type::Primitive(Primitive::Void));

                let return_ty_accessibility = self.get_overall_ty_accessibility(&return_ty);

                if return_ty_accessibility.rank() < trait_function_accessibility.rank() {
                    handler.receive(error::Error::PrivateSymbolLeakage(
                        error::PrivateSymbolLeakage {
                            span: self.trait_functions[trait_function_id]
                                .function_signature
                                .syntax_tree
                                .as_ref()
                                .map(|x| {
                                    x.return_type
                                        .as_ref()
                                        .map(pernixc_source::SourceElement::span)
                                        .unwrap()
                                })
                                .unwrap(),
                        },
                    ));
                }

                self.trait_functions[trait_function_id]
                    .function_signature
                    .return_type = return_ty;
            }
        }
        // finalize function parameters
        {
            for trait_function_parameter_id in self.trait_functions[trait_function_id]
                .parameter_order
                .clone()
            {
                let parameter_ty = self.trait_function_parameters[trait_function_parameter_id]
                    .syntax_tree
                    .as_ref()
                    .map(|x| x.type_annotation().type_specifier().clone())
                    .unwrap();

                let parameter_ty = self
                    .resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: trait_function_id.into(),
                            bound_checking: Some(BoundChecking::Default),
                            explicit_lifetime_required: true,
                        },
                        &parameter_ty,
                        states,
                        handler,
                    )
                    .unwrap_or(ty::Type::Primitive(Primitive::Void));

                let parameter_ty_accessibility = self.get_overall_ty_accessibility(&parameter_ty);

                if parameter_ty_accessibility.rank() < trait_function_accessibility.rank() {
                    handler.receive(error::Error::PrivateSymbolLeakage(
                        error::PrivateSymbolLeakage {
                            span: self.trait_function_parameters[trait_function_parameter_id]
                                .syntax_tree
                                .as_ref()
                                .map(|x| x.type_annotation().type_specifier().span())
                                .unwrap(),
                        },
                    ));
                }

                self.trait_function_parameters[trait_function_parameter_id].ty = parameter_ty;
            }
        }

        // finalize implements functions
        for implements_id in self.traits[self.trait_functions[trait_function_id].parent_trait_id]
            .implements
            .clone()
        {
            let implements_function_id = self.implements[implements_id]
                .implements_functions_by_trait_function[&trait_function_id];

            self.finalize_implements_function(implements_function_id, states, handler);
        }

        states.remove_constructing_symbol(trait_function_id.into());
    }

    #[allow(clippy::too_many_lines)]
    fn finalize_implements_function(
        &mut self,
        implements_function_id: arena::ID<ImplementsFunction>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        let parent_implements_id =
            self.implements_functions[implements_function_id].parent_implements_id;
        let parent_trait_function_id =
            self.implements_functions[implements_function_id].trait_function_id;

        let trait_function_accessibility = self
            .get_accessibility(parent_trait_function_id.into())
            .unwrap();

        // check generic parameter count
        let Some(member_substitution) = self.check_generic_parameter_match(
            parent_trait_function_id.into(),
            implements_function_id,
            &self.implements_functions[implements_function_id]
                .function_signature
                .syntax_tree
                .as_ref()
                .map(|x| x.identifier.span())
                .unwrap(),
            handler,
        ) else {
            self.remove_implements(parent_implements_id);
            return;
        };

        let all_substitution = Substitution::coombine(
            &member_substitution,
            &self.implements[parent_implements_id].substitution,
        );

        // finalize where clause
        {
            if let Some(where_clause) = self.implements_functions[implements_function_id]
                .function_signature
                .syntax_tree
                .as_ref()
                .map(|x| x.where_clause.clone())
                .unwrap()
            {
                if let Some(where_clause) = self.construct_where_clause(
                    &where_clause,
                    implements_function_id.into(),
                    states,
                    handler,
                ) {
                    self.implements_functions[implements_function_id]
                        .function_signature
                        .generics
                        .where_clause = where_clause;
                }
            }
        }
        let active_where_clause = self
            .get_active_where_clause(implements_function_id.into())
            .unwrap();

        {
            // check if the where clause matches the parent trait function
            let Some(transformed_parent_trait_function_where_clause) = ({
                self.substitute_where_clause(
                    &self.trait_functions[parent_trait_function_id]
                        .generics
                        .where_clause,
                    &all_substitution,
                    BoundChecking::Default,
                    &active_where_clause,
                    &self.implements_functions[implements_function_id]
                        .function_signature
                        .syntax_tree
                        .as_ref()
                        .map(|x| x.identifier.span())
                        .unwrap(),
                    handler,
                )
            }) else {
                self.remove_implements(parent_implements_id);
                return;
            };

            if transformed_parent_trait_function_where_clause
                != self.implements_functions[implements_function_id]
                    .generics
                    .where_clause
            {
                handler.receive(error::Error::IncompatibleImplementsMemberWhereClause(
                    error::IncompatibleImplementsMemberWhereClause {
                        implements_member_span: self.implements_functions[implements_function_id]
                            .function_signature
                            .syntax_tree
                            .as_ref()
                            .map(|x| x.identifier.span())
                            .unwrap(),
                    },
                ));
                self.remove_implements(parent_implements_id);
                return;
            }
        }

        // finalize return type
        {
            if let Some(return_ty) = self.implements_functions[implements_function_id]
                .function_signature
                .syntax_tree
                .as_ref()
                .map(|x| {
                    x.return_type
                        .as_ref()
                        .map(|x| x.type_annotation().type_specifier().clone())
                })
                .unwrap()
            {
                let return_ty = self
                    .resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: implements_function_id.into(),
                            bound_checking: Some(BoundChecking::Default),
                            explicit_lifetime_required: true,
                        },
                        &return_ty,
                        states,
                        handler,
                    )
                    .unwrap_or(ty::Type::Primitive(Primitive::Void));

                let return_ty_accessibility = self.get_overall_ty_accessibility(&return_ty);

                if return_ty_accessibility.rank() < trait_function_accessibility.rank() {
                    handler.receive(error::Error::PrivateSymbolLeakage(
                        error::PrivateSymbolLeakage {
                            span: self.implements_functions[implements_function_id]
                                .function_signature
                                .syntax_tree
                                .as_ref()
                                .map(|x| {
                                    x.return_type
                                        .as_ref()
                                        .map(pernixc_source::SourceElement::span)
                                        .unwrap()
                                })
                                .unwrap(),
                        },
                    ));
                }

                // check if the return type matches
                {
                    let Some(transformed_trait_function_return_ty) = self.substitute_type(
                        &self.trait_functions[parent_trait_function_id]
                            .function_signature
                            .return_type,
                        &all_substitution,
                        Some(BoundChecking::Default),
                        &active_where_clause,
                        &self.implements_functions[implements_function_id]
                            .function_signature
                            .syntax_tree
                            .as_ref()
                            .map(|x| {
                                x.return_type.as_ref().map_or_else(
                                    || x.identifier.span(),
                                    |x| x.type_annotation().type_specifier().span(),
                                )
                            })
                            .unwrap(),
                        handler,
                    ) else {
                        self.remove_implements(parent_implements_id);
                        return;
                    };

                    if return_ty != transformed_trait_function_return_ty {
                        handler.receive(error::Error::ImplementsFunctionReturnTypeMismatch(
                            ImplementsFunctionReturnTypeMismatch {
                                expected_return_type_string: self
                                    .get_type_string(&transformed_trait_function_return_ty)
                                    .unwrap(),
                                actual_return_type_string: self
                                    .get_type_string(&return_ty)
                                    .unwrap(),
                                implements_function_return_type_span: self.implements_functions
                                    [implements_function_id]
                                    .function_signature
                                    .syntax_tree
                                    .as_ref()
                                    .map(|x| {
                                        x.return_type.as_ref().map_or_else(
                                            || x.identifier.span(),
                                            |x| x.type_annotation().type_specifier().span(),
                                        )
                                    })
                                    .unwrap(),
                            },
                        ));
                        self.remove_implements(parent_implements_id);
                        return;
                    }
                }

                self.implements_functions[implements_function_id]
                    .function_signature
                    .return_type = return_ty;
            }
        }
        // finalize function parameters
        {
            if self.implements_functions[implements_function_id]
                .parameter_order
                .len()
                != self.trait_functions[parent_trait_function_id]
                    .parameter_order
                    .len()
            {
                handler.receive(error::Error::ImplementsFunctionParameterCountMismatch(
                    ImplementsFunctionParameterCountMismatch {
                        expected_parameter_count: self.trait_functions[parent_trait_function_id]
                            .parameter_order
                            .len(),
                        actual_parameter_count: self.implements_functions[implements_function_id]
                            .parameter_order
                            .len(),
                        implements_member_span: self.implements_functions[implements_function_id]
                            .function_signature
                            .syntax_tree
                            .as_ref()
                            .map(|x| x.identifier.span())
                            .unwrap(),
                    },
                ));
                self.remove_implements(parent_implements_id);
                return;
            }

            for (idx, implements_function_parameter_id) in self.implements_functions
                [implements_function_id]
                .parameter_order
                .clone()
                .into_iter()
                .enumerate()
            {
                let parent_trait_function_parameter_id =
                    self.trait_functions[parent_trait_function_id].parameter_order[idx];

                let parameter_ty = self.implements_function_parameters
                    [implements_function_parameter_id]
                    .syntax_tree
                    .as_ref()
                    .map(|x| x.type_annotation().type_specifier().clone())
                    .unwrap();

                let parameter_ty = self
                    .resolve_type_with_finalization(
                        &resolution::Info {
                            referring_site: implements_function_id.into(),
                            bound_checking: Some(BoundChecking::Default),
                            explicit_lifetime_required: true,
                        },
                        &parameter_ty,
                        states,
                        handler,
                    )
                    .unwrap_or(ty::Type::Primitive(Primitive::Void));

                let parameter_ty_accessibility = self.get_overall_ty_accessibility(&parameter_ty);

                if parameter_ty_accessibility.rank() < trait_function_accessibility.rank() {
                    handler.receive(error::Error::PrivateSymbolLeakage(
                        error::PrivateSymbolLeakage {
                            span: self.implements_function_parameters
                                [implements_function_parameter_id]
                                .syntax_tree
                                .as_ref()
                                .map(|x| x.type_annotation().type_specifier().span())
                                .unwrap(),
                        },
                    ));
                }

                // check if the parameter type mathes
                {
                    let Some(transformed_trait_function_parameter_ty) = self.substitute_type(
                        &self.trait_function_parameters[parent_trait_function_parameter_id].ty,
                        &all_substitution,
                        Some(BoundChecking::Default),
                        &active_where_clause,
                        &self.implements_function_parameters[implements_function_parameter_id]
                            .syntax_tree
                            .as_ref()
                            .map(|x| x.type_annotation().type_specifier().span())
                            .unwrap(),
                        handler,
                    ) else {
                        self.remove_implements(parent_implements_id);
                        return;
                    };

                    if parameter_ty != transformed_trait_function_parameter_ty {
                        handler.receive(error::Error::ImplementsFunctionParameterTypeMismatch(
                            ImplementsFunctionParameterTypeMismatch {
                                expected_parameter_type_string: self
                                    .get_type_string(&transformed_trait_function_parameter_ty)
                                    .unwrap(),
                                actual_parameter_type_string: self
                                    .get_type_string(&parameter_ty)
                                    .unwrap(),
                                implements_function_parameter_type_span: self
                                    .implements_function_parameters
                                    [implements_function_parameter_id]
                                    .syntax_tree
                                    .as_ref()
                                    .map(|x| x.type_annotation().type_specifier().span())
                                    .unwrap(),
                            },
                        ));
                        self.remove_implements(parent_implements_id);
                        return;
                    }
                }

                self.implements_function_parameters[implements_function_parameter_id].ty =
                    parameter_ty;
            }
        }
    }

    fn check_generic_parameter_match<T: Into<GenericableID> + Copy>(
        &self,
        parent_trait_member_id: TraitMemberID,
        implements_member_id: T,
        implements_member_span: &Span,
        handler: &impl Handler<error::Error>,
    ) -> Option<Substitution> {
        let (required_lifetime_parameter_count, required_type_parameter_count) = {
            let genericable_symbol = self.get_genericable(parent_trait_member_id.into()).unwrap();
            (
                genericable_symbol
                    .generic_parameters()
                    .lifetime_parameter_ids_by_name
                    .len(),
                genericable_symbol
                    .generic_parameters()
                    .type_parameter_ids_by_name
                    .len(),
            )
        };

        let (found_lifetime_parameter_count, found_type_parameter_count) = {
            let genericable_symbol = self.get_genericable(implements_member_id.into()).unwrap();
            (
                genericable_symbol
                    .generic_parameters()
                    .lifetime_parameter_ids_by_name
                    .len(),
                genericable_symbol
                    .generic_parameters()
                    .type_parameter_ids_by_name
                    .len(),
            )
        };

        if required_lifetime_parameter_count != found_lifetime_parameter_count {
            handler.receive(error::Error::ImplementsLifetimeParameterCountMismatch(
                ImplementsLifetimeParameterCountMismatch {
                    implements_member_lifetime_parameter_count: found_lifetime_parameter_count,
                    trait_member_id: parent_trait_member_id,
                    implements_member_span: implements_member_span.clone(),
                },
            ));
            return None;
        }

        if required_type_parameter_count != found_type_parameter_count {
            handler.receive(error::Error::ImplementsTypeParameterCountMismatch(
                ImplementsTypeParameterCountMismatch {
                    implements_member_type_parameter_count: found_type_parameter_count,
                    trait_member_id: parent_trait_member_id,
                    implements_member_span: implements_member_span.clone(),
                },
            ));
            return None;
        }

        let lt_sub = self
            .get_genericable(parent_trait_member_id.into())
            .unwrap()
            .generic_parameters()
            .lifetime_parameter_order
            .iter()
            .copied()
            .zip(
                self.get_genericable(implements_member_id.into())
                    .unwrap()
                    .generic_parameters()
                    .lifetime_parameter_order
                    .iter()
                    .copied()
                    .map(LifetimeArgument::Parameter),
            )
            .collect();
        let ty_sub = self
            .get_genericable(parent_trait_member_id.into())
            .unwrap()
            .generic_parameters()
            .type_parameter_order
            .iter()
            .copied()
            .zip(
                self.get_genericable(implements_member_id.into())
                    .unwrap()
                    .generic_parameters()
                    .type_parameter_order
                    .iter()
                    .copied()
                    .map(ty::Type::Parameter),
            )
            .collect();

        Some(Substitution {
            lifetime_arguments_by_parameter: lt_sub,
            type_arguments_by_parameter: ty_sub,
        })
    }

    fn finalize_trait_type(
        &mut self,
        trait_type_id: arena::ID<TraitType>,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) {
        assert!(
            states.get_current_state(trait_type_id.into()).is_some(),
            "symbol is not drafted or being constructed"
        );

        if let Err(err) = states.mark_as_constructing(trait_type_id.into()) {
            handler.receive(error::Error::CyclicDependency(err));
            return;
        }

        // loop through all implements
        for implements_id in self.traits[self.trait_types[trait_type_id].parent_trait_id]
            .implements
            .clone()
        {
            let implements = &self.implements[implements_id];
            let implements_type_id = implements.implements_types_by_trait_type[&trait_type_id];

            let type_specifier = self.implements_types[implements_type_id]
                .syntax_tree
                .as_ref()
                .unwrap()
                .definition()
                .type_specifier()
                .clone();

            if self
                .check_generic_parameter_match(
                    trait_type_id.into(),
                    implements_type_id,
                    &self.implements_types[implements_type_id]
                        .syntax_tree
                        .as_ref()
                        .map(|x| x.signature().span())
                        .unwrap(),
                    handler,
                )
                .is_none()
            {
                self.remove_implements(implements_id);
                continue;
            }

            let Some(type_alias) = self.resolve_type_with_finalization(
                &resolution::Info {
                    referring_site: implements_type_id.into(),
                    bound_checking: Some(BoundChecking::Default),
                    explicit_lifetime_required: true,
                },
                &type_specifier,
                states,
                handler,
            ) else {
                self.remove_implements(implements_id);
                continue;
            };

            let ty_accessibility = self.get_overall_ty_accessibility(&type_alias);

            if ty_accessibility.rank()
                < self.get_accessibility(trait_type_id.into()).unwrap().rank()
            {
                handler.receive(error::Error::PrivateSymbolLeakage(
                    error::PrivateSymbolLeakage {
                        span: type_specifier.span(),
                    },
                ));
            }

            self.implements_types[implements_type_id].alias = type_alias;
        }

        states.remove_constructing_symbol(trait_type_id.into());
    }

    fn get_overall_ty_accessibility(&self, ty: &ty::Type) -> Accessibility {
        match ty {
            ty::Type::Enum(enum_id) => self.enums[*enum_id].accessibility,
            ty::Type::Struct(struct_ty) => {
                std::iter::once(self.structs[struct_ty.struct_id].accessibility)
                    .chain(
                        struct_ty
                            .substitution
                            .type_arguments_by_parameter
                            .values()
                            .map(|ty| self.get_overall_ty_accessibility(ty)),
                    )
                    .min_by(|lhs, rhs| lhs.rank().cmp(&rhs.rank()))
                    .unwrap()
            }
            ty::Type::TraitType(trait_ty) => std::iter::once(
                self.get_accessibility(trait_ty.trait_type_id.into())
                    .unwrap(),
            )
            .chain(
                trait_ty
                    .trait_substitution
                    .type_arguments_by_parameter
                    .values()
                    .map(|ty| self.get_overall_ty_accessibility(ty)),
            )
            .chain(
                trait_ty
                    .trait_type_substitution
                    .type_arguments_by_parameter
                    .values()
                    .map(|ty| self.get_overall_ty_accessibility(ty)),
            )
            .min_by(|lhs, rhs| lhs.rank().cmp(&rhs.rank()))
            .unwrap(),
            ty::Type::Parameter(_) | ty::Type::Primitive(_) => Accessibility::Public,
            ty::Type::Reference(reference_ty) => {
                self.get_overall_ty_accessibility(&reference_ty.operand)
            }
        }
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
            .unwrap_or_default();

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
                if implements::compare(
                    &self.implements[existing_implements].substitution,
                    &self.implements[new_implements].substitution,
                ) == implements::Comparison::Ambiguous
                {
                    handler.receive(error::Error::AmbiguousImplements(AmbiguousImplements {
                        existing_implements,
                        new_implements_span: signature_span,
                    }));
                    self.remove_implements(new_implements);
                    break;
                }
            }

            // new implements has been successfully added
            if self.implements.get(new_implements).is_some() {
                self.traits[trait_id].implements.insert(new_implements);
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
                TraitMemberID::Function(trait_function_id) => {
                    self.finalize_trait_function(trait_function_id, states, handler);
                }
                TraitMemberID::Type(trait_type_id) => {
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
            // remove type parameters
            for ty_parameter in self.implements_types[implements_ty]
                .generic_parameters
                .type_parameter_order
                .iter()
                .copied()
            {
                assert!(self.type_parameters.remove(ty_parameter).is_some());
            }
            // remove lifetime parameters
            for lifetime_parameter in self.implements_types[implements_ty]
                .generic_parameters
                .lifetime_parameter_order
                .iter()
                .copied()
            {
                assert!(self
                    .lifetime_parameters
                    .remove(lifetime_parameter)
                    .is_some());
            }

            assert!(self.implements_types.remove(implements_ty).is_some());
        }
        // remove implements Function
        for implements_function in self.implements[implements_id]
            .implements_functions_by_trait_function
            .values()
            .copied()
        {
            // remove type parameters
            for ty_parameter in self.implements_functions[implements_function]
                .generics
                .parameters
                .type_parameter_order
                .iter()
                .copied()
            {
                assert!(self.type_parameters.remove(ty_parameter).is_some());
            }
            // remove lifetime parameters
            for lifetime_parameter in self.implements_functions[implements_function]
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
            parent_trait.implements.remove(&implements_id);
        }

        assert!(self.implements.remove(implements_id).is_some());
    }

    fn lifetime_parameter_exists_in_substitution(
        lt_parameter: arena::ID<LifetimeParameter>,
        substitution: &Substitution,
    ) -> bool {
        if substitution
            .lifetime_arguments_by_parameter
            .values()
            .any(|lt| {
                let LifetimeArgument::Parameter(lt_parameter_to_copmare) = lt else {
                    return false;
                };

                lt_parameter == *lt_parameter_to_copmare
            })
        {
            return true;
        }

        if substitution
            .type_arguments_by_parameter
            .values()
            .any(|ty| Self::lifetime_parameter_exists_in_ty(lt_parameter, ty))
        {
            return true;
        }

        false
    }

    fn lifetime_parameter_exists_in_ty(
        lt_parameter: arena::ID<LifetimeParameter>,
        ty: &ty::Type,
    ) -> bool {
        match ty {
            ty::Type::Primitive(_)
            | ty::Type::Enum(_)
            | ty::Type::Parameter(_)
            | ty::Type::TraitType(_) => false,
            ty::Type::Struct(struct_ty) => Self::lifetime_parameter_exists_in_substitution(
                lt_parameter,
                &struct_ty.substitution,
            ),
            ty::Type::Reference(reference_ty) => {
                if matches!(
                    reference_ty.lifetime_argument,
                    Some(LifetimeArgument::Parameter(lt_parameter_to_copmare))
                        if lt_parameter_to_copmare == lt_parameter
                ) {
                    return true;
                }

                Self::lifetime_parameter_exists_in_ty(lt_parameter, &reference_ty.operand)
            }
        }
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
            ty::Type::Parameter(ty) => *ty == ty_parameter,
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
                bound_checking: Some(resolution::BoundChecking::Default),
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
            BoundChecking::Default,
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

            if !type_parameters.is_empty() {
                handler.receive(error::Error::UnusedTypeParameters(
                    error::UnusedTypeParameters {
                        unused_type_parameter_spans: type_parameters
                            .iter()
                            .map(|x| self.type_parameters[*x].symbol_span().unwrap())
                            .collect(),
                    },
                ));
                self.remove_implements(implements_id);
                return None;
            }
        }

        // check if all lifetime parameters are used
        {
            let mut unused_lifetime_parameters = self.implements[implements_id]
                .generics
                .parameters
                .lifetime_parameter_order
                .iter()
                .copied()
                .collect::<HashSet<_>>();

            for lifetime_parameter in self.implements[implements_id]
                .generics
                .parameters
                .lifetime_parameter_order
                .iter()
                .copied()
            {
                if Self::lifetime_parameter_exists_in_substitution(
                    lifetime_parameter,
                    &self.implements[implements_id].substitution,
                ) {
                    unused_lifetime_parameters.remove(&lifetime_parameter);
                }
            }

            if !unused_lifetime_parameters.is_empty() {
                handler.receive(error::Error::UnusedLifetimeParameters(
                    error::UnusedLifetimeParameters {
                        unused_lifetime_parameters: unused_lifetime_parameters
                            .iter()
                            .map(|x| self.lifetime_parameters[*x].symbol_span().unwrap())
                            .collect(),
                    },
                ));
                self.remove_implements(implements_id);
                return None;
            }
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

        let mut trait_members = self.traits[parent_trait_id]
            .trait_member_ids_by_name
            .clone();
        let mut constructed_implements_members: HashMap<String, ImplementsMemberID> =
            HashMap::new();

        for implements_member_syntax_tree in implements_body_syntax_tree.dissolve().1 {
            let identifier = match &implements_member_syntax_tree {
                syntax_tree::item::ImplementsMember::Type(ty) => {
                    ty.signature().identifier().clone()
                }
                syntax_tree::item::ImplementsMember::Function(func) => {
                    func.signature().identifier().clone()
                }
            };

            // check for redefinition
            if let Some(implements_member_id) = constructed_implements_members
                .get(identifier.span.str())
                .copied()
            {
                handler.receive(error::Error::ImplementsMemberRedefinition(
                    SymbolRedefinition {
                        previous_definition_id: implements_member_id,
                        redefinition_span: identifier.span.clone(),
                    },
                ));
                continue;
            }

            let Some(trait_member_id) = trait_members.remove(identifier.span.str()) else {
                handler.receive(error::Error::UnknownTraitMemberInImplements(
                    UnknownTraitMemberInImplements {
                        unknown_member_span: identifier.span.clone(),
                        trait_id: parent_trait_id,
                    },
                ));
                continue;
            };

            let implements_member_id: ImplementsMemberID =
                match (trait_member_id, implements_member_syntax_tree) {
                    (
                        TraitMemberID::Function(function_id),
                        syntax_tree::item::ImplementsMember::Function(function_syntax_tree),
                    ) => {
                        let implements_function_id = self.draft_implements_function(
                            function_syntax_tree,
                            function_id,
                            implements_id,
                            handler,
                        );
                        assert!(self.implements[implements_id]
                            .implements_functions_by_trait_function
                            .insert(function_id, implements_function_id)
                            .is_none());
                        implements_function_id.into()
                    }

                    /* TODO: finalize the type signature */
                    (
                        TraitMemberID::Type(type_id),
                        syntax_tree::item::ImplementsMember::Type(type_syntax_tree),
                    ) => {
                        let implements_type_id = self.draft_implements_type(
                            type_syntax_tree,
                            type_id,
                            implements_id,
                            handler,
                        );
                        assert!(self.implements[implements_id]
                            .implements_types_by_trait_type
                            .insert(type_id, implements_type_id)
                            .is_none());
                        implements_type_id.into()
                    }

                    (trait_member_id, implements_member_syntax_tree) => {
                        handler.receive(error::Error::TraitMemberKindMismatch(
                            TraitMemberKindMismatch {
                                expected_kind: match trait_member_id {
                                    TraitMemberID::Function(_) => TraitMemberKind::Function,
                                    TraitMemberID::Type(_) => TraitMemberKind::Type,
                                },
                                actual_kind: match implements_member_syntax_tree {
                                    syntax_tree::item::ImplementsMember::Function(_) => {
                                        TraitMemberKind::Function
                                    }
                                    syntax_tree::item::ImplementsMember::Type(_) => {
                                        TraitMemberKind::Type
                                    }
                                },
                                span: identifier.span.clone(),
                            },
                        ));
                        continue;
                    }
                };

            constructed_implements_members
                .insert(identifier.span.str().to_string(), implements_member_id);
            trait_members.remove(identifier.span.str());
        }

        if !trait_members.is_empty() {
            handler.receive(error::Error::NotAllTraitMembersWereImplemented(
                error::NotAllTraitMembersWereImplemented {
                    unimplemented_members: trait_members.into_values().collect(),
                    trait_id: parent_trait_id,
                    implements_span: implements_signature_syntax_tree
                        .qualified_identifier()
                        .span(),
                },
            ));

            self.remove_implements(implements_id);
            return None;
        }

        self.implements[implements_id].syntax_tree = Some(implements_signature_syntax_tree);

        Some(implements_id)
    }

    fn draft_implements_type(
        &mut self,
        implements_type_syntax_tree: syntax_tree::item::ImplementsType,
        trait_type_id: arena::ID<TraitType>,
        parent_implements_id: arena::ID<Implements>,
        handler: &impl Handler<error::Error>,
    ) -> arena::ID<ImplementsType> {
        let implements_type_id = self.implements_types.push(ImplementsType {
            generic_parameters: GenericParameters::default(),
            trait_type_id,
            alias: ty::Type::Primitive(Primitive::Void),
            name: self.trait_types[trait_type_id].name.clone(),
            parent_implements_id,
            syntax_tree: None, // to be filled later
        });

        if let Some(generic_parameters) = implements_type_syntax_tree
            .signature()
            .generic_parameters()
            .as_ref()
        {
            self.implements_types[implements_type_id].generic_parameters = self
                .create_generic_parameters(implements_type_id.into(), generic_parameters, handler);
        }

        self.implements_types[implements_type_id].syntax_tree = Some(implements_type_syntax_tree);

        implements_type_id
    }

    fn draft_implements_function(
        &mut self,
        implements_function_syntax_tree: syntax_tree::item::ImplementsFunction,
        trait_function_id: arena::ID<TraitFunction>,
        parent_implements_id: arena::ID<Implements>,
        handler: &impl Handler<error::Error>,
    ) -> arena::ID<ImplementsFunction> {
        // function signature syntax tree (without parameters)
        let (signature_syntax_tree, body_syntax_tree) = implements_function_syntax_tree.dissolve();

        let (signature_syntax_tree, parameters) = {
            let (identifier, generic_parameters, parameter_list, return_type, where_clause) =
                signature_syntax_tree.dissolve();

            (
                FunctionSignatureSyntaxTree {
                    identifier,
                    generic_parameters,
                    return_type,
                    where_clause,
                },
                parameter_list,
            )
        };

        let implements_function_id = self.implements_functions.push(ImplementsFunction {
            function_signature: crate::FunctionSignature {
                name: signature_syntax_tree.identifier.span.str().to_string(),
                parameter_ids_by_name: HashMap::new(), // to be filled later
                parameter_order: Vec::new(),           // to be filled later
                return_type: ty::Type::Primitive(Primitive::Void), // to be filled later
                syntax_tree: None,                     // to be filled later
                generics: Generics::default(),         // to be filled later
            },
            syntax_tree: Some(body_syntax_tree),
            parent_implements_id,
            trait_function_id,
        });

        // create function generics
        if let Some(generic_parameters) = signature_syntax_tree.generic_parameters.as_ref() {
            self.implements_functions[implements_function_id]
                .generics
                .parameters = self.create_generic_parameters(
                implements_function_id.into(),
                generic_parameters,
                handler,
            );
        }
        self.implements_functions[implements_function_id]
            .function_signature
            .syntax_tree = Some(signature_syntax_tree);

        for parameter in parameters
            .dissolve()
            .1
            .into_iter()
            .flat_map(ConnectedList::into_elements)
        {
            let function = &mut self.implements_functions[implements_function_id];

            // redefinition check
            if let Err(error) = Self::redefinition_check(
                &function.function_signature.parameter_ids_by_name,
                parameter.identifier(),
            ) {
                handler.receive(error::Error::ImplementsFunctionParameterRedefinition(error));
                continue;
            }

            let parameter_name = parameter.identifier().span.str().to_string();

            let parameter_id = self.implements_function_parameters.push(Parameter {
                name: parameter_name.clone(),
                parameter_parent_id: implements_function_id,
                declaration_order: function.parameter_order.len(),
                ty: ty::Type::Primitive(Primitive::Void), // to be replaced
                is_mutable: parameter.mutable_keyword().is_some(),
                syntax_tree: Some(parameter),
            });

            function
                .parameter_ids_by_name
                .insert(parameter_name, parameter_id);
            function.parameter_order.push(parameter_id);
        }

        implements_function_id
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
