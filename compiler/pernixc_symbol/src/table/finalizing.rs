use pernixc_source::{SourceElement, Span};
use pernixc_syntax::syntax_tree::{
    self, item::TypeBoundConstraint, GenericArgument, GenericIdentifier, PrimitiveTypeSpecifier,
    QualifiedIdentifier, TypeSpecifier,
};
use pernixc_system::{arena, diagnostic::Handler};

use super::{
    drafting::States,
    resolution::{self, Resolution},
    FatalSemantic, Table,
};
use crate::{
    error::{
        self, LifetimeArgumentCountMismatch, LifetimeArgumentMustBeSuppliedPriorToTypeArgument,
        LifetimeArgumentsRequired, NoGenericArgumentsRequired, TraitExpected,
        TraitResolutionNotAllowed, TypeArgumentCountMismatch, TypeExpected,
    },
    ty::{self, Primitive, Reference},
    GenericableID, GlobalID, Substitution, Trait, WhereClause, ID,
};

impl Table {
    fn resolve_with_finalization(
        &mut self,
        info: &resolution::Info,
        qualified_identifier: &QualifiedIdentifier,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<Resolution, FatalSemantic> {
        let mut current_root = resolution::Root {
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
                .map_err(|x| x.into_fatal_semantic().unwrap())?;

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

        Ok(current_root.latest_resolution.unwrap())
    }

    fn resolve_qualified_identifier_type_with_finalization(
        &mut self,
        info: &resolution::Info,
        qualified_identifier: &QualifiedIdentifier,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, FatalSemantic> {
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
                return Ok(ty::Type::Parameter(ty));
            }
        }

        let resolution =
            self.resolve_with_finalization(info, qualified_identifier, states, handler)?;

        Ok(match resolution {
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
                return Err(FatalSemantic);
            }
        })
    }

    fn resolve_type_with_finalization(
        &mut self,
        info: &resolution::Info,
        type_specifier: &TypeSpecifier,
        states: &mut States,
        handler: &impl Handler<error::Error>,
    ) -> Result<ty::Type, FatalSemantic> {
        match type_specifier {
            TypeSpecifier::Primitive(primitive_type) => {
                Ok(ty::Type::Primitive(match primitive_type {
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
            TypeSpecifier::Reference(reference_type) => Ok(ty::Type::Reference(Reference {
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
                        self.resolve_lifetime_argument(info.referring_site, x, handler)
                            .map_err(|x| x.into_fatal_semantic().unwrap())?,
                    ),
                    None => {
                        if info.explicit_lifetime_required {
                            handler.receive(error::Error::LifetimeArgumentsRequired(
                                LifetimeArgumentsRequired {
                                    span: reference_type.span(),
                                },
                            ));
                            return Err(FatalSemantic);
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
    ) -> Result<Substitution, FatalSemantic> {
        let mut substitution = Substitution::default();

        let Ok(genericable_id) = GenericableID::try_from(found_global_id) else {
            if let Some(generic_arguments) = generic_identifier.generic_arguments() {
                handler.receive(error::Error::NoGenericArgumentsRequired(
                    NoGenericArgumentsRequired {
                        span: generic_arguments.span(),
                    },
                ));
                return Err(FatalSemantic);
            }

            return Ok(substitution);
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
                        return Err(FatalSemantic);
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

                return Err(FatalSemantic);
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
                    let lifetime_argument = self
                        .resolve_lifetime_argument(
                            info.referring_site,
                            lifetime_argument_syntax_tree,
                            handler,
                        )
                        .map_err(|x| x.into_fatal_semantic().unwrap())?;

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

                return Err(FatalSemantic);
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
            return Err(FatalSemantic);
        }

        Ok(substitution)
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
        let Ok(resolution) = self.resolve_with_finalization(
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
        let Ok(ty) = self.resolve_type_with_finalization(
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
                    let Ok(type_bound) = self.resolve_type_with_finalization(
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
    ) -> Result<WhereClause, FatalSemantic> {
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

            if self
                .check_where_clause(
                    self.trait_types[trait_type.trait_type_id]
                        .parent_trait_id
                        .into(),
                    &trait_type.trait_substitution,
                    &where_clause_syntax_tree.span(),
                    &parent_active_where_clause,
                    handler,
                )
                .is_err()
            {
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

            if self
                .check_where_clause(
                    trait_bound.trait_id.into(),
                    &trait_bound.substitution,
                    &where_clause_syntax_tree.span(),
                    &parent_active_where_clause,
                    handler,
                )
                .is_err()
            {
                error_found = true;
            }

            parent_active_where_clause
                .trait_bounds
                .insert(trait_bound.clone());
        }

        if error_found {
            Err(FatalSemantic)
        } else {
            Ok(where_clause)
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

            ty::Type::Struct(struct_ty) => self
                .check_where_clause(
                    struct_ty.struct_id.into(),
                    &struct_ty.substitution,
                    generics_identifier_span,
                    active_where_clause,
                    handler,
                )
                .is_ok(),
            ty::Type::Reference(reference_ty) => self.check_type_where_clause(
                &reference_ty.operand,
                generics_identifier_span,
                active_where_clause,
                handler,
            ),
            ty::Type::TraitType(trait_ty) => self
                .check_where_clause(
                    self.trait_types[trait_ty.trait_type_id]
                        .parent_trait_id
                        .into(),
                    &trait_ty.trait_substitution,
                    generics_identifier_span,
                    active_where_clause,
                    handler,
                )
                .is_ok(),
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
            if let Ok(where_clause) =
                self.construct_where_clause(&where_clause, trait_id.into(), states, handler)
            {
                self.traits[trait_id].generics.where_clause = where_clause;
            }
        }
    }
}
